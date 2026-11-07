-- | The capstone run through the SQLite Run Store (docs/08_coverage.md): the DSL-authored capstone is
-- | simulated, saved, reloaded byte-identically, and replayed from the stored records. The joint pass
-- | over the recorded transcript is in `Test.Handlers.Capstone`.
module Test.Adapters.Capstone (spec) where

import Prelude

import Adapters.SQLite (open, sqliteBackend)
import Views.Metrics (metricsVersion, roundMetrics)
import Core.Draw (Seed(..))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (last, length)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effects.Round (runGame)
import Effects.Store (createRun)
import Effects.Observe (readMetric, readRounds)
import Foreign.Object as Object
import Game.Types (Game)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Handlers.Store (persister, runStore, runObserve)
import Premises.Capstone as C
import Simulator.Simulate (simulate, simulateWith)
import Testkit.Capstone (load)
import Testkit.Meridian (orThrow)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

capstone :: Aff Game
capstone = map _.game load

spec :: Spec Unit
spec = describe "Capstone: the Run Store" do
  it "the capstone run round-trips through the store: saved, reloaded byte-identically, and replayed" do
    g <- capstone
    let
      seed = Seed C.playerSeed
    db <- liftEffect (open ":memory:")
    let port = sqliteBackend db
    stored <- liftEffect $ simulateWith seed g \manifest -> do
      runStore port (createRun { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "capstone", manifest, source: Nothing, kind: Nothing })
      persister port roundMetrics "capstone" manifest.initial
    _ <- orThrow stored
    rows <- liftEffect (runObserve port (readRounds "capstone"))
    length rows `shouldEqual` C.rounds
    records <- orThrow (traverse _.record rows)
    snapshots <- orThrow (traverse _.snapshot rows)
    pure' <- orThrow (simulate seed g)
    map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
    map (stringify <<< encodeJson) snapshots `shouldEqual` map (stringify <<< encodeJson <<< _.world) pure'.records
    snapshots `shouldEqual` map _.world pure'.records
    case runReplay (seatEnv seed g) C.rounds records (runGame g.inWorld C.rounds g.world) of
      Left e -> fail e
      Right out -> map _.world out.records `shouldEqual` snapshots

  it "the capstone's stored price metric rows are the pinned values" do
    g <- capstone
    let seed = Seed C.playerSeed
    db <- liftEffect (open ":memory:")
    let port = sqliteBackend db
    stored <- liftEffect $ simulateWith seed g \manifest -> do
      runStore port (createRun { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "capstone", manifest, source: Nothing, kind: Nothing })
      persister port roundMetrics "capstone" manifest.initial
    _ <- orThrow stored
    rows <- liftEffect (runObserve port (readRounds "capstone"))
    snapshots <- orThrow (traverse _.snapshot rows)
    let marketIds = maybe [] (\w -> map show (Set.toUnfoldable (Map.keys w.markets) :: Array _)) (last snapshots)
    prices <- for marketIds \m -> liftEffect (runObserve port (readMetric "capstone" ("price:" <> m) metricsVersion))
    (map (map _.value) prices) `shouldEqual`
      [ [ "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000" ]
      , [ "2.1600", "2.3200", "2.6400", "2.7200", "2.8000", "2.8800", "2.9600", "2.9733" ]
      , [ "2.6275", "2.9222", "3.2726", "3.1268", "2.7343", "2.5755", "2.5114", "2.4319" ]
      ]
