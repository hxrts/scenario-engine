-- | The major shock (docs/05 §13.6, `Premises.Meridian.majorShock`): a designed test case beside the
-- | reference disruption. Calm and shocked games have the same seats, presets and player seed, so a
-- | difference in a seat's proposals is the shock's doing.
-- | Pins: docs/05 §13.6 (the major shock).
module Test.Handlers.Shock (spec) where

import Schema.Build.Quantity (q)
import Prelude

import Laws.Query (priceOf)

import Core.Draw (Seed(..))
import Core.Num as Q
import Laws.Types (Id(..), IntentF(..))
import Data.Array (concatMap, elem, filter, index, length, nub, range)
import Data.Foldable (any, maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Assembly.Override (Override(..))
import Assembly.Premise (PossibleWorldParts)
import Premises.Meridian.Parameters (majorShock)
import Premises.Presets (presetNames)
import Handlers.Simulate (SimRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Premises.Meridian.Actors.Seats (playableActors)
import Testkit.Meridian (run, presetGame)

-- | The Presets whose behaviour the shock is checked on (Passive proposes nothing, so it cannot flip).
shockPresets :: Array String
shockPresets = filter (_ /= "Passive") presetNames

-- | The kinds of Intent the seats proposed in one round: side and Exchange for a Trade, the
-- | Transformation for a Transform. Aggregates' candidate Transactions are left out, so only the preset's own
-- | behaviour is compared.
roundKinds :: SimRun -> Int -> Array String
roundKinds r i = case index r.records i of
  Nothing -> []
  Just rr -> nub (concatMap (\x -> if elem x.candidateTransaction.author playableActors then map kindOf x.candidateTransaction.intents else []) rr.record.inputs)
  where
  kindOf = case _ of
    Trade o -> (if o.quantity < Q.zero then "sell " else "buy ") <> show o.exchange
    Route _ -> "route"
    Transfer _ -> "transfer"
    Transform t -> "transform " <> show t.transformation
    ChangeRule _ -> "rule"
    Enter _ -> "agreement"

-- | Whether the seats' proposals differ between the calm and the shocked game in some round.
flips :: SimRun -> SimRun -> Boolean
flips calm shocked = any (\i -> roundKinds calm i /= roundKinds shocked i) (range 0 (length calm.records - 1))

spec :: Aff PossibleWorldParts -> Spec Unit
spec premise = describe "Major shock (docs/05 §13.6)" do
  it "flips the behaviour of at least four presets, deterministically" do
    p <- premise
    let overrides = shockOverrides
    results <- traverse
      ( \preset -> do
          calm <- play p preset []
          shocked <- play p preset overrides
          pure (Tuple preset (flips calm shocked))
      )
      shockPresets
    let flipped = map (\(Tuple preset _) -> preset) (filter (\(Tuple _ f) -> f) results)
    flipped `shouldSatisfy` (\ns -> length ns >= 4)
  it "pins the Conservative prices of docs/05 §13.6, and is deterministic" do
    p <- premise
    calm <- play p "Conservative" []
    shocked <- play p "Conservative" shockOverrides
    again <- play p "Conservative" shockOverrides
    map _.world again.records `shouldEqual` map _.world shocked.records
    prices "energy-atlantic" calm `shouldEqual` [ "2.89", "3.14", "3.32", "3.12", "2.86" ]
    prices "energy-atlantic" shocked `shouldEqual` [ "3.07", "3.19", "3.11", "2.92", "2.77" ]
    range' (prices' "energy-asia" calm) `shouldEqual` Just { min: q "2.16", max: q "2.8" }
    range' (prices' "energy-asia" shocked) `shouldEqual` Just { min: q "2.48", max: q "2.88" }

shockOverrides :: Array Override
shockOverrides = map (\(Tuple name value) -> Parameter name (q value)) majorShock

-- | The end-of-round price of an Exchange across a run.
prices' :: String -> SimRun -> Array Q.Q
prices' m r = map (\x -> fromMaybe Q.zero (priceOf x.world (Id m))) r.records

-- | The same, as the two-decimal display (docs/05 §13.6 prints them so).
prices :: String -> SimRun -> Array String
prices m r = map Q.displayQuantity (prices' m r)

range' :: Array Q.Q -> Maybe { min :: Q.Q, max :: Q.Q }
range' xs = case minimum xs, maximum xs of
  Just a, Just b -> Just { min: a, max: b }
  _, _ -> Nothing

-- | Every seat plays `preset`, with the given overrides, under player seed 1.
play :: PossibleWorldParts -> String -> Array Override -> Aff SimRun
play p preset overrides = do
  g <- presetGame p preset overrides Nothing
  run (Seed 1) g
