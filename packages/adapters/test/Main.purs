-- | Test 13 (docs/06_simulator.md §14): save a run, reload it byte-identically, and replay it
-- | from the stored records.
module Test.Adapters.Main where

import Prelude

import Adapters.Store (createRun, open, persister, reader, setStatus)
import Board.Gameboard (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (RoundRecord, WorldState)
import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effects (inWorldOf, runGame)
import Handlers.Replay (runReplay)
import Harness.Simulate (SeatPolicy(..), simulate, simulateWith)
import Scenarios (load, loadPresets)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

orThrow :: forall a. Either String a -> Aff a
orThrow = case _ of
  Left e -> throwError (error e)
  Right a -> pure a

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Run Store" do
    it "13 Persistence: save, reload byte-identically, and replay from stored records" do
      loaded <- liftEffect (load "fractured-world/v0") >>= orThrow
      presets <- liftEffect loadPresets >>= orThrow
      let
        ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
        spec = { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset "RandomValid")) [ "US", "DE", "SA", "JP", "SME" ]), playerSeed: Seed 7, overrides: [], rounds: Nothing }
      db <- liftEffect (open ":memory:")
      stored <- liftEffect $ simulateWith ctx spec \manifest -> do
        createRun db { id: "run-1", name: "persistence", created: "2026-01-01T00:00:00Z", spec: fromString "RandomValid", manifest }
        persister db "run-1" manifest.initial
      _ <- orThrow stored
      liftEffect (setStatus db "run-1" "completed")
      let store = reader db
      runs <- liftEffect store.runs
      map _.status runs `shouldEqual` [ "completed" ]
      rows <- liftEffect (store.rounds "run-1")
      length rows `shouldEqual` 5
      -- byte-identical reload: decoding and re-encoding reproduces the stored text
      records <- orThrow (traverse (\r -> (jsonParser r.record >>= lmap printJsonDecodeError <<< decodeJson) :: Either String RoundRecord) rows)
      snapshots <- orThrow (traverse (\r -> (jsonParser r.snapshot >>= lmap printJsonDecodeError <<< decodeJson) :: Either String WorldState) rows)
      map (stringify <<< encodeJson) records `shouldEqual` map _.record rows
      map (stringify <<< encodeJson) snapshots `shouldEqual` map _.snapshot rows
      -- the stored run equals the pure run, and replaying stored records reproduces it
      pure' <- orThrow (simulate ctx spec)
      snapshots `shouldEqual` map _.world pure'.records
      case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
        Right initial, Right inWorld -> map _.world (runReplay records (runGame inWorld 5 initial.world)).records `shouldEqual` snapshots
        _, _ -> fail "instantiation"
      prices <- liftEffect (store.metric "run-1" "price:energy-atlantic")
      length prices `shouldEqual` 5
      manifest <- liftEffect (store.manifest "run-1")
      (manifest <#> \_ -> unit) `shouldEqual` Just unit
