-- | Test 13 (docs/06_simulator.md §14): save a run, reload it byte-identically, and replay it
-- | from the stored records. The Run Store's schema, read-only opens, and failure recording are
-- | covered here too.
module Test.Adapters.Main where

import Prelude

import Schema.Run (emptySpec, RunStatus(..))
import Adapters.SQLite (configuredBusyTimeout)
import Handlers.Sql (SqlDevice)
import Handlers.SqlStore (describedTables, schemaVersion)
import Testkit.Store (open, playStored, storeOver)
import Core.Draw (Seed(..))
import Laws.Types (Id(..))
import Laws.World (Manifest)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length, sort)
import Data.Foldable (for_)
import Data.Either (Either(..), either, isRight)
import Data.String (Pattern(..), contains)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message, try)
import Handlers.Replay (runReplay)
import Effects.Store (createRun, deleteRun, failRun, failRunning, setArchived, setSpec)
import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Views.Metrics (roundMetrics)
import Handlers.Store (recorder, runStore, runObserve, startRun)
import Handlers.Simulate (simulate)
import Handlers.Seats (seatEnv)
import Testkit.Meridian (orThrow, presetSeats)
import Test.Adapters.Files (createStoreAtVersion, removeFile, setRawSpec)
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
  describe "the table descriptions" do
    it "name exactly the columns the schema creates" do
      db <- liftEffect (open ":memory:")
      for_ describedTables \t -> do
        reported <- liftEffect (map _.name <$> (db.query ("pragma table_info(" <> t.table <> ")") [] :: Effect (Array { name :: String })))
        sort reported `shouldEqual` sort t.columns
  describe "Run Store" do
    it "a run id already in the store is a usable error, not a constraint failure" do
      g <- presetSeats [ "US" ] "Passive" Nothing
      db <- liftEffect (open ":memory:")
      let
        port = storeOver db
        new = { owner: "session:1:0", id: "session-1", name: "live session", created: "2026-01-01T00:00:00Z", spec: emptySpec, manifest: g.manifest }
      first <- liftEffect (startRun port new)
      first `shouldEqual` Right unit
      second <- liftEffect (startRun port new)
      second `shouldEqual` Left "a run named session-1 already exists; choose another --id"

    it "13 Persistence: save, reload byte-identically, and replay from stored records" do
      g <- presetSeats [ "US", "DE", "SA", "JP", "SME" ] "RandomValid" Nothing
      let
        seed = Seed 7
      db <- liftEffect (open ":memory:")
      let port = storeOver db
      stored <- liftEffect $ playStored port seed g ({ owner: "server:1:0", id: "run-1", name: "persistence", created: "2026-01-01T00:00:00Z", spec: emptySpec { preset = Just "RandomValid" }, manifest: g.manifest })
      _ <- orThrow stored
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

    it "a new store is created at the current schema; a failed run keeps its reason" do
      let path = "run-store-migrate-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      db <- liftEffect (open path)
      let port = storeOver db
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 10
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "migrated", created: "2026-01-01T00:00:00Z", spec: emptySpec { gameId = Just "x" }, manifest })
      liftEffect (runStore port (failRun "run-1" "boom"))
      runs <- liftEffect (runObserve port listRuns)
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: Failed, reason: Just "boom" } ]
      liftEffect (db.close *> removeFile path)

    it "failRunning fails every run still running that the role left, with its reason" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = storeOver db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "orphan", created: "2026-01-01T00:00:00Z", spec: emptySpec { gameId = Just "x" }, manifest })
      n <- liftEffect (runStore port (failRunning { role: "server", reason: "interrupted" }))
      n `shouldEqual` 1
      runs <- liftEffect (runObserve port listRuns)
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: Failed, reason: Just "interrupted" } ]

    it "the busy timeout is set on the store and reads back as configured" do
      let path = "run-store-busy-test.db"
      liftEffect (removeFile path)
      db <- liftEffect (open path)
      timeout <- liftEffect (busyOf db)
      timeout `shouldEqual` configuredBusyTimeout
      timeout `shouldEqual` 5000
      liftEffect (db.close *> removeFile path)

    it "a round's transcripts are stored with the round; a round saved without them stores none" do
      g <- presetSeats [ "US", "DE" ] "Passive" (Just 2)
      let
        seed = Seed 3
        said = [ { seat: Id "US", text: "I will buy ten energy" } ]
      db <- liftEffect (open ":memory:")
      let port = storeOver db
      played <- orThrow (simulate seed g)
      liftEffect do
        runStore port (createRun { owner: "server:1:0", id: "run-1", name: "transcripts", created: "2026-01-01T00:00:00Z", spec: emptySpec { gameId = Just "x" }, manifest: g.manifest })
        save <- recorder port roundMetrics "run-1" g.manifest.initial
        for_ played.records \r -> save r (if r.record.round == 1 then said else [])
      rows <- liftEffect (runObserve port (readRounds "run-1"))
      map _.transcripts rows `shouldEqual` [ Right said, Right [] ]

    it "a store at any other schema version is refused, with what to do" do
      let path = "run-store-other-version-test.db"
      liftEffect (removeFile path)
      liftEffect (createStoreAtVersion path 99)
      opened <- liftEffect (try (open path))
      either (contains (Pattern "Move the file aside") <<< message) (const false) opened `shouldEqual` true
      liftEffect (removeFile path)

    it "archive is idempotent; delete removes a run with its rounds and metrics, and leaves the run that cites it" do
      g <- presetSeats [ "US", "DE", "SA", "JP", "SME" ] "Conservative" (Just 2)
      let
        seed = Seed 1
      db <- liftEffect (open ":memory:")
      let port = storeOver db
      stored <- liftEffect $ playStored port seed g ({ owner: "server:1:0", id: "run-1", name: "base", created: "2026-01-01T00:00:00Z", spec: emptySpec { gameId = Just "x" }, manifest: g.manifest })
      _ <- orThrow stored
      manifest <- manifestOf
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-2", name: "dup", created: "2026-01-01T00:00:00Z", spec: emptySpec { gameId = Just "x", source = Just "run-1" }, manifest })
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
      let port = storeOver db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "corrupt", created: "2026-01-01T00:00:00Z", spec: emptySpec { rounds = Just 3 }, manifest })
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-2", name: "fine", created: "2026-01-01T00:00:00Z", spec: emptySpec { rounds = Just 3 }, manifest })
      liftEffect (setRawSpec path "run-1" "not json")
      rows <- liftEffect (runObserve port listSummaries)
      case map _.spec rows of
        [ Left why, Right _ ] -> contains (Pattern "spec") why `shouldEqual` true
        _ -> fail "run-1 should be unreadable and run-2 readable"
      map (\r -> isRight r.manifest) rows `shouldEqual` [ true, true ]
      liftEffect (db.close *> removeFile path)

    it "a stored spec can be replaced in place (a resumed run records its new round count)" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = storeOver db
      liftEffect $ runStore port (createRun { owner: "server:1:0", id: "run-1", name: "resume", created: "2026-01-01T00:00:00Z", spec: emptySpec { rounds = Just 3 }, manifest })
      liftEffect (runStore port (setSpec "run-1" (emptySpec { rounds = Just 5 })))
      rows <- liftEffect (runObserve port listSummaries)
      map _.spec rows `shouldEqual` [ Right (emptySpec { rounds = Just 5 }) ]

-- | The busy timeout a connection reports.
busyOf :: SqlDevice -> Effect Int
busyOf d = map (\rows -> maybe 0 _.timeout (head rows)) (d.query "select * from pragma_busy_timeout" [] :: Effect (Array { timeout :: Int }))
