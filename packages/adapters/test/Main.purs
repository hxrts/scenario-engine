-- | Test 13 (docs/06_simulator.md §14): save a run, reload it byte-identically, and replay it
-- | from the stored records. The Run Store's schema, read-only opens, and failure recording are
-- | covered here too.
module Test.Adapters.Main where

import Prelude

import Core.Run (DeriveKind(..), RunStatus(..))
import Adapters.SQLite (busyTimeout, close, configuredBusyTimeout, open, openReadOnly, schemaVersion, sqliteBackend)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (Manifest)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..), isLeft, isRight)
import Data.String (Pattern(..), contains)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Foreign.Object as Object
import Handlers.Replay (runReplay)
import Effects.Store (createRun, deleteRun, failRun, failRunning, setArchived, setSpec, setStatus)
import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Views.Metrics (roundMetrics)
import Handlers.Store (recorder, runStore, runObserve, startRun)
import Simulator.Simulate (simulate, simulateWith)
import Handlers.Seats (seatEnv)
import Testkit.Meridian (orThrow, presetSeats)
import Test.Adapters.Files (createV1Store, fileExists, removeFile, setRawSpec)
import Test.Adapters.Capstone as CapstoneTests
import Test.Adapters.Pi as PiTests
import Test.Adapters.Primitives as PrimitiveTests
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The initial manifest of the reference premise, for runs created by hand.
manifestOf :: Aff Manifest
manifestOf = do
  g <- presetSeats [] "Passive" Nothing
  pure g.manifest

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CapstoneTests.spec
  PiTests.spec
  PrimitiveTests.spec
  describe "Run Store" do
    it "a run id already in the store is a usable error, not a constraint failure" do
      g <- presetSeats [ "US" ] "Passive" Nothing
      db <- liftEffect (open ":memory:")
      let
        port = sqliteBackend db
        new = { owner: "session:1:0", id: "session-1", name: "live session", created: "2026-01-01T00:00:00Z", spec: Object.empty, manifest: g.manifest, source: Nothing, kind: Nothing }
      first <- liftEffect (startRun port new)
      first `shouldEqual` Right unit
      second <- liftEffect (startRun port new)
      second `shouldEqual` Left "a run named session-1 already exists; choose another --id"

    it "13 Persistence: save, reload byte-identically, and replay from stored records" do
      g <- presetSeats [ "US", "DE", "SA", "JP", "SME" ] "RandomValid" Nothing
      let
        seed = Seed 7
      db <- liftEffect (open ":memory:")
      let port = sqliteBackend db
      stored <- liftEffect $ simulateWith seed g \manifest -> do
        runStore port (createRun { owner: "server:1:0", id: "run-1", name: "persistence", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "RandomValid", manifest, source: Nothing, kind: Nothing })
        recorder port roundMetrics "run-1" manifest.initial
      _ <- orThrow stored
      liftEffect (runStore port (setStatus "run-1" Completed Nothing))
      runs <- liftEffect (runObserve port listRuns)
      map _.status runs `shouldEqual` [ Completed ]
      rows <- liftEffect (runObserve port (readRounds "run-1"))
      length rows `shouldEqual` 5
      -- the stored records and snapshots read back typed, as the run wrote them
      records <- orThrow (traverse _.record rows)
      snapshots <- orThrow (traverse _.snapshot rows)
      -- the stored run equals the pure run, and replaying stored records reproduces it
      pure' <- orThrow (simulate seed g)
      snapshots `shouldEqual` map _.world pure'.records
      map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
      case runReplay (seatEnv seed g) 5 records g of
        Left e -> fail e
        Right out -> map _.world out.records `shouldEqual` snapshots
      manifest <- liftEffect (runObserve port (readManifest "run-1"))
      (manifest <#> \_ -> unit) `shouldEqual` Just unit

    it "a new store is created and migrated; a failed run keeps its reason" do
      let path = "run-store-migrate-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      db <- liftEffect (open path)
      let port = sqliteBackend db
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 5
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "migrated", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing })
      liftEffect (runStore port (failRun "run-1" "boom"))
      runs <- liftEffect (runObserve port listRuns)
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: Failed, reason: Just "boom" } ]
      liftEffect (close db *> removeFile path)

    it "a read on a missing store fails and creates no file" do
      let path = "run-store-missing-test.db"
      liftEffect (removeFile path)
      opened <- liftEffect (try (openReadOnly path))
      (isLeft opened) `shouldEqual` true
      exists <- liftEffect (fileExists path)
      exists `shouldEqual` false

    it "a read-only open sees a migrated store" do
      let path = "run-store-readonly-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      writer <- liftEffect (open path)
      liftEffect $ runStore (sqliteBackend writer) (createRun { owner: "server:1:0", id: "run-1", name: "ro", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing })
      reader' <- liftEffect (openReadOnly path)
      runs <- liftEffect (runObserve (sqliteBackend reader') listRuns)
      map _.id runs `shouldEqual` [ "run-1" ]
      liftEffect (close reader' *> close writer *> removeFile path)

    it "failRunning fails every run still running that the role left, with its reason" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = sqliteBackend db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "orphan", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing })
      n <- liftEffect (runStore port (failRunning { role: "server", reason: "interrupted" }))
      n `shouldEqual` 1
      runs <- liftEffect (runObserve port listRuns)
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: Failed, reason: Just "interrupted" } ]

    it "the busy timeout is set on the store and reads back as configured" do
      let path = "run-store-busy-test.db"
      liftEffect (removeFile path)
      db <- liftEffect (open path)
      timeout <- liftEffect (busyTimeout db)
      timeout `shouldEqual` configuredBusyTimeout
      timeout `shouldEqual` 5000
      reader' <- liftEffect (openReadOnly path)
      readTimeout <- liftEffect (busyTimeout reader')
      readTimeout `shouldEqual` configuredBusyTimeout
      liftEffect (close reader' *> close db *> removeFile path)

    it "a round's transcripts are stored with the round; a round saved without them stores none" do
      g <- presetSeats [ "US", "DE" ] "Passive" (Just 2)
      let
        seed = Seed 3
        said = [ { seat: Id "US", text: "I will buy ten energy" } ]
      db <- liftEffect (open ":memory:")
      let port = sqliteBackend db
      stored <- liftEffect $ simulateWith seed g \manifest -> do
        runStore port (createRun { owner: "server:1:0", id: "run-1", name: "transcripts", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing })
        save <- recorder port roundMetrics "run-1" manifest.initial
        pure \r _ -> save r (if r.record.round == 1 then said else [])
      _ <- orThrow stored
      rows <- liftEffect (runObserve port (readRounds "run-1"))
      map _.transcripts rows `shouldEqual` [ Right said, Right [] ]

    it "a version 1 store upgrades in place: its runs survive, and they gain the archive, source and reason fields" do
      let path = "run-store-v1-test.db"
      liftEffect (removeFile path)
      liftEffect (createV1Store path)
      db <- liftEffect (open path)
      let port = sqliteBackend db
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 5
      runs <- liftEffect (runObserve port listRuns)
      map (\r -> { id: r.id, status: r.status, reason: r.reason }) runs `shouldEqual` [ { id: "run-1", status: Completed, reason: Nothing } ]
      sums <- liftEffect (runObserve port listSummaries)
      map (\r -> { archived: r.archived, source: r.source }) sums `shouldEqual` [ { archived: false, source: Nothing } ]
      -- the upgraded store takes the new operations, and a read-only open sees the new version
      liftEffect (runStore port (setArchived "run-1" true))
      sums' <- liftEffect (runObserve port listSummaries)
      map _.archived sums' `shouldEqual` [ true ]
      reader' <- liftEffect (openReadOnly path)
      version' <- liftEffect (schemaVersion reader')
      version' `shouldEqual` 5
      liftEffect (close reader' *> close db *> removeFile path)

    it "archive is idempotent; delete removes a run with its rounds and metrics, and leaves the run that cites it" do
      g <- presetSeats [ "US", "DE", "SA", "JP", "SME" ] "Conservative" (Just 2)
      let
        seed = Seed 1
      db <- liftEffect (open ":memory:")
      let port = sqliteBackend db
      stored <- liftEffect $ simulateWith seed g \manifest -> do
        runStore port (createRun { owner: "server:1:0", id: "run-1", name: "base", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing })
        recorder port roundMetrics "run-1" manifest.initial
      _ <- orThrow stored
      manifest <- manifestOf
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-2", name: "dup", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Just "run-1", kind: Just Duplicate })
      liftEffect (runStore port (setArchived "run-2" true))
      liftEffect (runStore port (setArchived "run-2" true))
      sums <- liftEffect (runObserve port listSummaries)
      map (\r -> { id: r.id, archived: r.archived, source: r.source }) sums
        `shouldEqual` [ { id: "run-1", archived: false, source: Nothing }, { id: "run-2", archived: true, source: Just "run-1" } ]
      liftEffect (runStore port (setArchived "run-2" false))
      before <- liftEffect (runObserve port (readRounds "run-1"))
      length before `shouldEqual` 2
      removed <- liftEffect (runStore port (deleteRun "run-1"))
      removed `shouldEqual` 1
      after <- liftEffect (runObserve port (readRounds "run-1"))
      after `shouldEqual` []
      left <- liftEffect (runObserve port listSummaries)
      map (\r -> { id: r.id, source: r.source }) left `shouldEqual` [ { id: "run-2", source: Just "run-1" } ]
      again <- liftEffect (runStore port (deleteRun "run-1"))
      again `shouldEqual` 0

    it "a stored spec that does not read back is unreadable in its row; the other runs stay readable" do
      let path = "run-store-corrupt-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      db <- liftEffect (open path)
      let port = sqliteBackend db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "corrupt", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing })
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-2", name: "fine", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing })
      liftEffect (setRawSpec path "run-1" "not json")
      rows <- liftEffect (runObserve port listSummaries)
      case map _.spec rows of
        [ Left why, Right _ ] -> contains (Pattern "spec") why `shouldEqual` true
        _ -> fail "run-1 should be unreadable and run-2 readable"
      map (\r -> isRight r.manifest) rows `shouldEqual` [ true, true ]
      liftEffect (close db *> removeFile path)

    it "a stored spec can be replaced in place (a resumed run records its new round count)" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = sqliteBackend db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "resume", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing })
      liftEffect (runStore port (setSpec "run-1" (Object.singleton "rounds" "5")))
      rows <- liftEffect (runObserve port listSummaries)
      map _.spec rows `shouldEqual` [ Right (Object.singleton "rounds" "5") ]
