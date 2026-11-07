-- | The capstone run through the SQLite Run Store (docs/08_coverage.md): the DSL-authored capstone is
-- | simulated, saved, reloaded byte-identically, and replayed from the stored records. The joint pass
-- | over the recorded transcript is in `Test.Handlers.Capstone`.
module Test.Adapters.Capstone (spec) where

import Prelude

import Adapters.Filesystem (fileOps)
import Scenarios.Tree (readRootFile, readScenario)
import Adapters.SQLite (open, sqliteBackend)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Foreign.Object as Object
import Data.Array (last, length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Effects.Store (createRun, readMetric, readRounds)
import Analysis.Views.Metrics (metricsVersion, roundMetrics)
import Handlers.Store (persister, runStore)
import Data.Map as Map
import Data.Set as Set
import Policy.InWorld (inWorldOf)
import Scenarios.Load (load, loadPresets, loadRun)
import Simulator.Capstone (Capstone, decodeCapstone)
import Simulator.Simulate (SeatPolicy(..), simulate, simulateWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

orThrow :: forall a. Either String a -> Aff a
orThrow = either (throwError <<< error) pure

capstone :: Aff Capstone
capstone = do
  file <- liftEffect (readRootFile fileOps "capstone/run.json") >>= orThrow >>= (orThrow <<< loadRun)
  orThrow (decodeCapstone file)

spec :: Spec Unit
spec = describe "Capstone: the Run Store" do
  it "the capstone run round-trips through the store: saved, reloaded byte-identically, and replayed" do
    loaded <- liftEffect (readScenario fileOps "meridian/v0") >>= orThrow >>= (orThrow <<< load)
    presets <- liftEffect (readRootFile fileOps "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
    cap <- capstone
    let
      ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
      spec' = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }
    db <- liftEffect (open ":memory:")
    let port = sqliteBackend db
    stored <- liftEffect $ simulateWith ctx spec' \manifest -> do
      runStore port (createRun { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "capstone", manifest, source: Nothing, kind: Nothing })
      persister port roundMetrics "capstone" manifest.initial
    _ <- orThrow stored
    rows <- liftEffect (runStore port (readRounds "capstone"))
    length rows `shouldEqual` cap.rounds
    records <- orThrow (traverse _.record rows)
    snapshots <- orThrow (traverse _.snapshot rows)
    pure' <- orThrow (simulate ctx spec')
    map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
    map (stringify <<< encodeJson) snapshots `shouldEqual` map (stringify <<< encodeJson <<< _.world) pure'.records
    snapshots `shouldEqual` map _.world pure'.records
    case instantiate ctx.gameboard ctx.config of
      Right initial ->
        let
          inWorld = inWorldOf ctx.gameboard
        in
          case runReplay cap.rounds records (runGame inWorld cap.rounds initial.world) of
            Left e -> fail e
            Right out -> map _.world out.records `shouldEqual` snapshots
      Left _ -> fail "instantiation"

  it "the capstone's stored price metric rows are the pinned values" do
    cap <- capstone
    presets <- liftEffect (readRootFile fileOps "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
    loaded <- liftEffect (readScenario fileOps "meridian/v0") >>= orThrow >>= (orThrow <<< load)
    let ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
    let spec' = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }
    db <- liftEffect (open ":memory:")
    let port = sqliteBackend db
    stored <- liftEffect $ simulateWith ctx spec' \manifest -> do
      runStore port (createRun { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "capstone", manifest, source: Nothing, kind: Nothing })
      persister port roundMetrics "capstone" manifest.initial
    _ <- orThrow stored
    rows <- liftEffect (runStore port (readRounds "capstone"))
    snapshots <- orThrow (traverse _.snapshot rows)
    let marketIds = maybe [] (\w -> map show (Set.toUnfoldable (Map.keys w.markets) :: Array _)) (last snapshots)
    prices <- for marketIds \m -> liftEffect (runStore port (readMetric "capstone" ("price:" <> m) metricsVersion))
    (map (map _.value) prices) `shouldEqual`
      [ [ "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000" ]
      , [ "2.1600", "2.3200", "2.6400", "2.7200", "2.8000", "2.8800", "2.9600", "2.9733" ]
      , [ "2.6275", "2.9222", "3.2726", "3.1268", "2.7343", "2.5755", "2.5114", "2.4319" ]
      ]
