-- | The major shock (docs/05 §13.6, `Premises.Meridian.majorShock`): a designed test case beside the
-- | reference disruption. Calm and shocked games have the same seats, presets and player seed, so a
-- | difference in a seat's proposals is the shock's doing.
module Test.Simulator.Shock (spec) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (IntentF(..))
import Data.Array (concatMap, elem, filter, index, length, nub, range)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Game.Override (Override(..))
import Game.Premise (GameboardParts)
import Premises.Meridian (majorShock)
import Simulator.Simulate (SimRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Testkit.Capstone (runOf)
import Testkit.Meridian (presetGame, seats)

-- | The Presets whose behaviour the shock is checked on (Passive proposes nothing, so it cannot flip).
presetNames :: Array String
presetNames = [ "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "Deployer", "RandomValid" ]

-- | The kinds of Intent the seats proposed in one round: side and Market for a Trade, the
-- | Transformation for a Transform. Aggregates' Proposals are left out, so only the preset's own
-- | behaviour is compared.
roundKinds :: SimRun -> Int -> Array String
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
flips :: SimRun -> SimRun -> Boolean
flips calm shocked = any (\i -> roundKinds calm i /= roundKinds shocked i) (range 0 (length calm.records - 1))

spec :: Aff GameboardParts -> Spec Unit
spec premise = describe "Major shock (docs/05 §13.6)" do
  it "flips the behaviour of at least four presets, deterministically" do
    p <- premise
    let overrides = map (\(Tuple name value) -> Parameter name (fromMaybe Q.zero (Q.parse value))) majorShock
    results <- traverse
      ( \preset -> do
          calm <- play p preset []
          shocked <- play p preset overrides
          pure (Tuple preset (flips calm shocked))
      )
      presetNames
    let flipped = map (\(Tuple preset _) -> preset) (filter (\(Tuple _ f) -> f) results)
    flipped `shouldSatisfy` (\ns -> length ns >= 4)

-- | Every seat plays `preset`, with the given overrides, under player seed 1.
play :: GameboardParts -> String -> Array Override -> Aff SimRun
play p preset overrides = do
  g <- presetGame p preset overrides Nothing
  runOf (Seed 1) g
