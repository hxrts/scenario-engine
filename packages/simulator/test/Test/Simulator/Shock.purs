-- | The major shock (docs/05 §13.6, `Scenarios.Meridian.majorShock`): a designed test case beside the
-- | reference disruption. Calm and shocked games have the same seats, presets and player seed, so a
-- | difference in a seat's proposals is the shock's doing.
module Test.Simulator.Shock (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (Id(..), Intent, IntentF(..))
import Data.Array (concatMap, elem, filter, index, length, nub, range)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Simulator.Simulate (Context, Override(..), Run, SimulationSpec, presetSeats, simulate)
import Scenarios.Meridian (majorShock)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

-- | The seat presets that propose anything (Passive proposes nothing, so it cannot flip).
-- | The Meridian seats, the same list the Experiments and the Main test play.
seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

presetNames :: Array String
presetNames = [ "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "Deployer", "RandomValid" ]

-- | Every seat plays `preset`, with the given overrides.
game :: String -> Array Override -> SimulationSpec
game preset overrides = { seats: presetSeats (const preset) (map Id seats), playerSeed: Seed 1, overrides, rounds: Nothing }

play :: Context -> SimulationSpec -> Aff Run
play ctx s = case simulate ctx s of
  Left e -> throwError (error (show e))
  Right r -> pure r

-- | The kinds of Intent the seats proposed in one round: side and Market for a Trade, the
-- | Transformation for a Transform. Aggregates' Proposals are left out, so only the preset's own
-- | behaviour is compared.
roundKinds :: Run -> Int -> Array String
roundKinds r i = case index r.records i of
  Nothing -> []
  Just rr -> nub (concatMap (\x -> if elem (show x.proposal.author) seats then map kindOf x.proposal.intents else []) rr.record.inputs)
  where
  kindOf = case _ of
    Trade o -> (if o.quantity < Q.zero then "sell " else "buy ") <> show o.market
    Route _ -> "route"
    Transfer _ -> "transfer"
    Transform t -> "transform " <> show t.transformation
    ChangeRule _ -> "rule"

-- | Whether the seats' proposals differ between the calm and the shocked game in some round.
flips :: Run -> Run -> Boolean
flips calm shocked = any (\i -> roundKinds calm i /= roundKinds shocked i) (range 0 (length calm.records - 1))

spec :: Aff Context -> Spec Unit
spec context = describe "Major shock (docs/05 §13.6)" do
  it "flips the behaviour of at least four presets, deterministically" do
    ctx <- context
    let overrides = map (\(Tuple name value) -> Parameter name (fromMaybe Q.zero (Q.parse value))) majorShock
    results <- traverse
      ( \preset -> do
          calm <- play ctx (game preset [])
          shocked <- play ctx (game preset overrides)
          pure (Tuple preset (flips calm shocked))
      )
      presetNames
    let flipped = map (\(Tuple preset _) -> preset) (filter (\(Tuple _ f) -> f) results)
    flipped `shouldSatisfy` (\ns -> length ns >= 4)
