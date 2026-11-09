-- | The capstone run through the SQLite Run Store: the DSL-authored capstone is
-- | simulated, saved, reloaded byte-identically, and replayed from the stored records. The joint pass
-- | over the recorded transcript is in `Test.Handlers.Capstone`.
module Test.Adapters.Capstone (spec) where

import Schema.Run (emptySpec)
import Prelude

import Testkit.Store (open, playStored, storeOver)
import Views.Metrics (roundMetrics)
import Core.Draw (Seed(..))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concatMap, filter, head, length)
import Data.Foldable (fold)
import Data.String (Pattern(..), stripPrefix)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effects.Observe (readRounds)
import Game.Types (Game)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Handlers.Store (runObserve)
import Settings.Meridian.Capstone as C
import Handlers.Simulate (simulate)
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
    let port = storeOver db
    stored <- liftEffect $ playStored port seed g ({ owner: "server:1:0", id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: emptySpec { preset = Just "capstone" }, manifest: g.manifest })
    _ <- orThrow stored
    rows <- liftEffect (runObserve port (readRounds "capstone"))
    length rows `shouldEqual` C.rounds
    records <- orThrow (traverse _.record rows)
    snapshots <- orThrow (traverse _.snapshot rows)
    pure' <- orThrow (simulate seed g)
    map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
    map (stringify <<< encodeJson) snapshots `shouldEqual` map (stringify <<< encodeJson <<< _.world) pure'.records
    snapshots `shouldEqual` map _.world pure'.records
    case runReplay (seatEnv seed g) C.rounds records g of
      Left e -> fail e
      Right out -> map _.world out.records `shouldEqual` snapshots

  it "the capstone's price metric values are the pinned values" do
    g <- capstone
    pure' <- orThrow (simulate (Seed C.playerSeed) g)
    let
      metricsOf = map roundMetrics pure'.records
      names = map _.metric (fold (head metricsOf))
      series name = map (filter (\m -> m.metric == name)) metricsOf
      prices = map (\name -> concatMap (map _.value) (series name)) (filter (isJust <<< stripPrefix (Pattern "price:")) names)
    prices `shouldEqual`
      [ [ "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000", "3.0000" ]
      , [ "2.1600", "2.3200", "2.6400", "2.7200", "2.8000", "2.8800", "2.9600", "2.9733" ]
      , [ "2.6275", "2.9222", "3.2726", "3.1268", "2.7343", "2.5755", "2.5114", "2.4319" ]
      ]
