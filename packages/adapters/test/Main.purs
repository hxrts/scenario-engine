-- | Test 13 (docs/06_simulator.md §14): save a run, reload it byte-identically, and replay it
-- | from the stored records. The Run Store's schema, read-only opens, and failure recording are
-- | covered here too.
module Test.Adapters.Main where

import Prelude

import Adapters.Store (busyTimeout, configuredBusyTimeout, failStaleRuns, open, openReadOnly, schemaVersion, sqlitePort)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (Manifest)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..), isLeft, isRight)
import Data.String (Pattern(..), contains)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, try)
import Foreign.Object as Object
import Policy.InWorld (inWorldOf)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Handlers.Store (createRun, metricsVersion, persister, reader, recorder)
import Simulator.Simulate (SeatPolicy(..), simulate, simulateWith)
import Adapters.Scenarios (readScenario, readText)
import Scenarios.Load (load, loadPresets)
import Test.Adapters.Files (createV1Store, fileExists, removeFile, setRawSpec)
import Test.Adapters.Capstone as CapstoneTests
import Test.Adapters.Llm as LlmTests
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

orThrow :: forall a. Either String a -> Aff a
orThrow = case _ of
  Left e -> throwError (error e)
  Right a -> pure a

-- | The initial manifest of the reference scenario, for runs created by hand.
manifestOf :: Aff Manifest
manifestOf = do
  loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
  case instantiate loaded.gameboard loaded.config of
    Right initial -> pure initial.manifest
    Left _ -> fail "instantiation" *> throwError (error "instantiation")

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CapstoneTests.spec
  LlmTests.spec
  describe "Run Store" do
    it "13 Persistence: save, reload byte-identically, and replay from stored records" do
      loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
      presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "RandomValid")) [ "US", "DE", "SA", "JP", "SME" ]), playerSeed: Seed 7, overrides: [], rounds: Nothing }
      db <- liftEffect (open ":memory:")
      let port = sqlitePort db
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun port { id: "run-1", name: "persistence", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "RandomValid", manifest, source: Nothing, kind: Nothing }
        persister port "run-1" manifest.initial
      _ <- orThrow stored
      liftEffect (port.setStatus "run-1" "completed")
      let store = reader port
      runs <- liftEffect store.runs
      map _.status runs `shouldEqual` [ "completed" ]
      rows <- liftEffect (store.rounds "run-1")
      length rows `shouldEqual` 5
      -- the stored records and snapshots read back typed, as the run wrote them
      records <- orThrow (traverse _.record rows)
      snapshots <- orThrow (traverse _.snapshot rows)
      -- the stored run equals the pure run, and replaying stored records reproduces it
      pure' <- orThrow (simulate ctx spec)
      snapshots `shouldEqual` map _.world pure'.records
      map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
      case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
        Right initial, Right inWorld -> case runReplay 5 records (runGame inWorld 5 initial.world) of
          Left e -> fail e
          Right out -> map _.world out.records `shouldEqual` snapshots
        _, _ -> fail "instantiation"
      prices <- liftEffect (store.metric "run-1" "price:energy-atlantic" metricsVersion)
      length prices `shouldEqual` 5
      -- a metric is read only at the version it was computed under
      stale <- liftEffect (store.metric "run-1" "price:energy-atlantic" "v0")
      stale `shouldEqual` []
      manifest <- liftEffect (store.manifest "run-1")
      (manifest <#> \_ -> unit) `shouldEqual` Just unit

    it "a new store is created and migrated; a failed run keeps its reason" do
      let path = "run-store-migrate-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      db <- liftEffect (open path)
      let port = sqlitePort db
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 4
      liftEffect $ createRun port { id: "run-1", name: "migrated", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing }
      liftEffect (port.failRun "run-1" "boom")
      runs <- liftEffect (reader port).runs
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: "failed", reason: Just "boom" } ]
      liftEffect (removeFile path)

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
      liftEffect $ createRun (sqlitePort writer) { id: "run-1", name: "ro", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing }
      reader' <- liftEffect (openReadOnly path)
      runs <- liftEffect (reader (sqlitePort reader')).runs
      map _.id runs `shouldEqual` [ "run-1" ]
      liftEffect (removeFile path)

    it "runs left running by a stopped process are failed at startup" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = sqlitePort db
      liftEffect $ createRun port { id: "run-1", name: "orphan", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing }
      n <- liftEffect (failStaleRuns db "interrupted")
      n `shouldEqual` 1
      runs <- liftEffect (reader port).runs
      map (\r -> { status: r.status, reason: r.reason }) runs `shouldEqual` [ { status: "failed", reason: Just "interrupted" } ]

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
      liftEffect (removeFile path)

    it "a round's transcripts are stored with the round; a round saved without them stores none" do
      loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
      presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "Passive")) [ "US", "DE" ]), playerSeed: Seed 3, overrides: [], rounds: Just 2 }
        said = [ { seat: Id "US", text: "I will buy ten energy" } ]
      db <- liftEffect (open ":memory:")
      let port = sqlitePort db
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun port { id: "run-1", name: "transcripts", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing }
        save <- recorder port "run-1" manifest.initial
        pure \r -> save r (if r.record.round == 1 then said else [])
      _ <- orThrow stored
      rows <- liftEffect ((reader port).rounds "run-1")
      map _.transcripts rows `shouldEqual` [ Right said, Right [] ]

    it "a version 1 store upgrades in place: its runs survive, and they gain the archive, source and reason fields" do
      let path = "run-store-v1-test.db"
      liftEffect (removeFile path)
      liftEffect (createV1Store path)
      db <- liftEffect (open path)
      let port = sqlitePort db
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 4
      runs <- liftEffect (reader port).runs
      map (\r -> { id: r.id, status: r.status, reason: r.reason }) runs `shouldEqual` [ { id: "run-1", status: "completed", reason: Nothing } ]
      sums <- liftEffect (reader port).summaries
      map (\r -> { archived: r.archived, source: r.source }) sums `shouldEqual` [ { archived: false, source: Nothing } ]
      -- the upgraded store takes the new operations, and a read-only open sees the new version
      liftEffect (port.setArchived "run-1" true)
      sums' <- liftEffect (reader port).summaries
      map _.archived sums' `shouldEqual` [ true ]
      reader' <- liftEffect (openReadOnly path)
      version' <- liftEffect (schemaVersion reader')
      version' `shouldEqual` 4
      liftEffect (removeFile path)

    it "archive is idempotent; delete removes a run with its rounds and metrics, and leaves the run that cites it" do
      loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
      presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "Conservative")) [ "US", "DE", "SA", "JP", "SME" ]), playerSeed: Seed 1, overrides: [], rounds: Just 2 }
      db <- liftEffect (open ":memory:")
      let port = sqlitePort db
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun port { id: "run-1", name: "base", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Nothing, kind: Nothing }
        persister port "run-1" manifest.initial
      _ <- orThrow stored
      manifest <- manifestOf
      liftEffect $ createRun port { id: "run-2", name: "dup", created: "2026-01-01T00:00:00Z", spec: Object.singleton "label" "x", manifest, source: Just "run-1", kind: Just "duplicate" }
      liftEffect (port.setArchived "run-2" true)
      liftEffect (port.setArchived "run-2" true)
      let store = reader port
      sums <- liftEffect store.summaries
      map (\r -> { id: r.id, archived: r.archived, source: r.source }) sums
        `shouldEqual` [ { id: "run-1", archived: false, source: Nothing }, { id: "run-2", archived: true, source: Just "run-1" } ]
      liftEffect (port.setArchived "run-2" false)
      before <- liftEffect (store.rounds "run-1")
      length before `shouldEqual` 2
      removed <- liftEffect (port.deleteRun "run-1")
      removed `shouldEqual` 1
      after <- liftEffect (store.rounds "run-1")
      after `shouldEqual` []
      prices <- liftEffect (store.metric "run-1" "price:energy-atlantic" metricsVersion)
      prices `shouldEqual` []
      left <- liftEffect store.summaries
      map (\r -> { id: r.id, source: r.source }) left `shouldEqual` [ { id: "run-2", source: Just "run-1" } ]
      again <- liftEffect (port.deleteRun "run-1")
      again `shouldEqual` 0

    it "a stored spec that does not read back is unreadable in its row; the other runs stay readable" do
      let path = "run-store-corrupt-test.db"
      liftEffect (removeFile path)
      manifest <- manifestOf
      db <- liftEffect (open path)
      let port = sqlitePort db
      liftEffect $ createRun port { id: "run-1", name: "corrupt", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing }
      liftEffect $ createRun port { id: "run-2", name: "fine", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing }
      liftEffect (setRawSpec path "run-1" "not json")
      rows <- liftEffect (reader port).summaries
      case map _.spec rows of
        [ Left why, Right _ ] -> contains (Pattern "spec") why `shouldEqual` true
        _ -> fail "run-1 should be unreadable and run-2 readable"
      map (\r -> isRight r.manifest) rows `shouldEqual` [ true, true ]
      liftEffect (removeFile path)

    it "a stored spec can be replaced in place (a resumed run records its new round count)" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      let port = sqlitePort db
      liftEffect $ createRun port { id: "run-1", name: "resume", created: "2026-01-01T00:00:00Z", spec: Object.singleton "rounds" "3", manifest, source: Nothing, kind: Nothing }
      liftEffect (port.setSpec "run-1" (Object.singleton "rounds" "5"))
      rows <- liftEffect (reader port).summaries
      map _.spec rows `shouldEqual` [ Right (Object.singleton "rounds" "5") ]
