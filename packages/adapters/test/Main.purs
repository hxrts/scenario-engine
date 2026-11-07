-- | Test 13 (docs/06_simulator.md §14): save a run, reload it byte-identically, and replay it
-- | from the stored records. The Run Store's schema, read-only opens, and failure recording are
-- | covered here too.
module Test.Adapters.Main where

import Prelude

import Board.Decode.Common (readJson)

import Adapters.Store (busyTimeout, configuredBusyTimeout, createRun, deleteRun, failRun, failStaleRuns, metricsVersion, open, openReadOnly, persister, reader, schemaVersion, setArchived, setStatus, setSpec)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (Manifest, RoundRecord, WorldState)
import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, try)
import Effects.Environment (inWorldOf)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Harness.Simulate (SeatPolicy(..), simulate, simulateWith)
import Adapters.Scenarios (readScenario, readText)
import Scenarios.Load (load, loadPresets)
import Test.Adapters.Files (createV1Store, fileExists, removeFile)
import Test.Adapters.Live as Live
import Test.Adapters.Capstone as CapstoneTests
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
  Live.spec
  CapstoneTests.spec
  describe "Run Store" do
    it "13 Persistence: save, reload byte-identically, and replay from stored records" do
      loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
      presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "RandomValid")) [ "US", "DE", "SA", "JP", "SME" ]), playerSeed: Seed 7, overrides: [], rounds: Nothing }
      db <- liftEffect (open ":memory:")
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun db { id: "run-1", name: "persistence", created: "2026-01-01T00:00:00Z", spec: fromString "RandomValid", manifest, source: Nothing, kind: Nothing }
        persister db "run-1" manifest.initial
      _ <- orThrow stored
      liftEffect (setStatus db "run-1" "completed")
      let store = reader db
      runs <- liftEffect store.runs
      map _.status runs `shouldEqual` [ "completed" ]
      rows <- liftEffect (store.rounds "run-1")
      length rows `shouldEqual` 5
      -- byte-identical reload: decoding and re-encoding reproduces the stored text
      records <- orThrow (traverse (\r -> readJson r.record :: Either String RoundRecord) rows)
      snapshots <- orThrow (traverse (\r -> readJson r.snapshot :: Either String WorldState) rows)
      map (stringify <<< encodeJson) records `shouldEqual` map _.record rows
      map (stringify <<< encodeJson) snapshots `shouldEqual` map _.snapshot rows
      -- the stored run equals the pure run, and replaying stored records reproduces it
      pure' <- orThrow (simulate ctx spec)
      snapshots `shouldEqual` map _.world pure'.records
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
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 3
      liftEffect $ createRun db { id: "run-1", name: "migrated", created: "2026-01-01T00:00:00Z", spec: fromString "x", manifest, source: Nothing, kind: Nothing }
      liftEffect (failRun db "run-1" "boom")
      runs <- liftEffect (reader db).runs
      map (\r -> { status: r.status, reason: toMaybe r.reason }) runs `shouldEqual` [ { status: "failed", reason: Just "boom" } ]
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
      liftEffect $ createRun writer { id: "run-1", name: "ro", created: "2026-01-01T00:00:00Z", spec: fromString "x", manifest, source: Nothing, kind: Nothing }
      reader' <- liftEffect (openReadOnly path)
      runs <- liftEffect (reader reader').runs
      map _.id runs `shouldEqual` [ "run-1" ]
      liftEffect (removeFile path)

    it "runs left running by a stopped process are failed at startup" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      liftEffect $ createRun db { id: "run-1", name: "orphan", created: "2026-01-01T00:00:00Z", spec: fromString "x", manifest, source: Nothing, kind: Nothing }
      n <- liftEffect (failStaleRuns db "interrupted")
      n `shouldEqual` 1
      runs <- liftEffect (reader db).runs
      map (\r -> { status: r.status, reason: toMaybe r.reason }) runs `shouldEqual` [ { status: "failed", reason: Just "interrupted" } ]

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

    it "a version 1 store upgrades in place: its runs survive, and they gain the archive, source and reason fields" do
      let path = "run-store-v1-test.db"
      liftEffect (removeFile path)
      liftEffect (createV1Store path)
      db <- liftEffect (open path)
      version <- liftEffect (schemaVersion db)
      version `shouldEqual` 3
      runs <- liftEffect (reader db).runs
      map (\r -> { id: r.id, status: r.status, reason: toMaybe r.reason }) runs `shouldEqual` [ { id: "run-1", status: "completed", reason: Nothing } ]
      sums <- liftEffect (reader db).summaries
      map (\r -> { archived: r.archived, source: toMaybe r.source }) sums `shouldEqual` [ { archived: false, source: Nothing } ]
      -- the upgraded store takes the new operations, and a read-only open sees the new version
      liftEffect (setArchived db "run-1" true)
      sums' <- liftEffect (reader db).summaries
      map _.archived sums' `shouldEqual` [ true ]
      reader' <- liftEffect (openReadOnly path)
      version' <- liftEffect (schemaVersion reader')
      version' `shouldEqual` 3
      liftEffect (removeFile path)

    it "archive is idempotent; delete removes a run with its rounds and metrics, and leaves the run that cites it" do
      loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
      presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "Conservative")) [ "US", "DE", "SA", "JP", "SME" ]), playerSeed: Seed 1, overrides: [], rounds: Just 2 }
      db <- liftEffect (open ":memory:")
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun db { id: "run-1", name: "base", created: "2026-01-01T00:00:00Z", spec: fromString "x", manifest, source: Nothing, kind: Nothing }
        persister db "run-1" manifest.initial
      _ <- orThrow stored
      manifest <- manifestOf
      liftEffect $ createRun db { id: "run-2", name: "dup", created: "2026-01-01T00:00:00Z", spec: fromString "x", manifest, source: Just "run-1", kind: Just "duplicate" }
      liftEffect (setArchived db "run-2" true)
      liftEffect (setArchived db "run-2" true)
      let store = reader db
      sums <- liftEffect store.summaries
      map (\r -> { id: r.id, archived: r.archived, source: toMaybe r.source }) sums
        `shouldEqual` [ { id: "run-1", archived: false, source: Nothing }, { id: "run-2", archived: true, source: Just "run-1" } ]
      liftEffect (setArchived db "run-2" false)
      before <- liftEffect (store.rounds "run-1")
      length before `shouldEqual` 2
      removed <- liftEffect (deleteRun db "run-1")
      removed `shouldEqual` 1
      after <- liftEffect (store.rounds "run-1")
      after `shouldEqual` []
      prices <- liftEffect (store.metric "run-1" "price:energy-atlantic" metricsVersion)
      prices `shouldEqual` []
      left <- liftEffect store.summaries
      map (\r -> { id: r.id, source: toMaybe r.source }) left `shouldEqual` [ { id: "run-2", source: Just "run-1" } ]
      again <- liftEffect (deleteRun db "run-1")
      again `shouldEqual` 0

    it "a stored spec can be replaced in place (a resumed run records its new round count)" do
      manifest <- manifestOf
      db <- liftEffect (open ":memory:")
      liftEffect $ createRun db { id: "run-1", name: "resume", created: "2026-01-01T00:00:00Z", spec: fromString "{\"rounds\":\"3\"}", manifest, source: Nothing, kind: Nothing }
      liftEffect (setSpec db "run-1" "{\"rounds\":\"5\"}")
      rows <- liftEffect (reader db).summaries
      map _.spec rows `shouldEqual` [ "{\"rounds\":\"5\"}" ]
