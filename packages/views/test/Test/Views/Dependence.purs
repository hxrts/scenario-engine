-- | The Dependence view over a Pacific run (docs/05 §11, Test F): China under a US technology ban, substituting.
-- | Pins: literal values from docs/05.
module Test.Views.Dependence (spec) where

import Prelude

import Adapters.Filesystem (fileOps)
import Assembly.Assemble (assemble, presetBindings)
import Assembly.Composition (Origin(..))
import Core.Draw (Seed(..))
import Data.Array (drop, filter, index)
import Effect.Aff (Aff)
import Views.Common (StoredRun)
import Views.Compare (keyValues)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Laws.Types (Id(..))
import Premises.Load (loadFrom)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Views.Support (toStored)
import Testkit.Meridian (orThrow, run)
import Views.Dependence (dependence)

playing :: String -> String -> Aff StoredRun
playing us cn = do
  loaded <- liftEffect (loadFrom fileOps "pacific/v0")
  parts <- orThrow loaded
  bindings <- orThrow (presetBindings parts.presets (\a -> if a == Id "US" then us else if a == Id "CN" then cn else "Neutral") [ Id "US", Id "CN", Id "US-firm", Id "CN-firm" ])
  g <- orThrow (assemble (FromComposition parts) bindings [])
  r <- run (Seed 1) g
  pure (toStored (us <> "-" <> cn) r)

spec :: Spec Unit
spec = describe "Dependence" do
  it "against the run without the ban, the ban's realized effect is China's shortfall over the run: 20 against 0" do
    banned <- playing "Restrict" "FreeTrade"
    free <- playing "FreeTrade" "FreeTrade"
    let
      t = keyValues (const Nothing) [ banned, free ]
      rowOf label = filter (\x -> index x 0 == Just label) t.rows
    map (drop 2) (rowOf "Shortfall over the run, CN T") `shouldEqual` [ [ "20", "0" ] ]
    map (drop 2) (rowOf "Receipts from sales, US") `shouldEqual` [ [ "4", "24" ] ]
    map (drop 2) (rowOf "Final coverage, CN T") `shouldEqual` [ [ "0.00", "0.00" ] ]

  it "a private sector's demand is its purchases: China's rebalanced private sector buys 1.76 I in round 2, with no capacity to cover it" do
    r <- playing "FreeTrade" "Rebalance"
    let
      t = dependence r
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
    row "2" "CN-private" "I" `shouldEqual` [ [ "2", "CN-private", "I", "1.76", "0", "n/a", "n/a", "1.76", "0", "0", "0", "0", "0", "0" ] ]
    let kv = keyValues (const Nothing) [ r ]
    map (drop 2) (filter (\x -> index x 0 == Just "Purchases over the run, CN-private I") kv.rows) `shouldEqual` [ [ "11.92" ] ]

  it "China under a US technology ban: exposure 4 and short 4, then covered by its own capacity" do
    loaded <- liftEffect (loadFrom fileOps "pacific/v0")
    parts <- orThrow loaded
    bindings <- orThrow (presetBindings parts.presets (\a -> if a == Id "US" then "Restrict" else if a == Id "CN" then "Adaptive" else "Neutral") [ Id "US", Id "CN", Id "US-firm", Id "CN-firm" ])
    g <- orThrow (assemble (FromComposition parts) bindings [])
    r <- run (Seed 1) g
    let
      t = dependence (toStored "pacific" r)
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
    -- Round, Actor, Resource, Requirement, Capacity, Coverage, Exposure, Imports, Exports, Receipts, Shortfall, Bans against, Capacity on order
    row "2" "CN" "T" `shouldEqual` [ [ "2", "CN", "T", "4", "0", "0.00", "4", "0", "0", "0", "4", "1", "2", "0" ] ]
    row "6" "CN" "T" `shouldEqual` [ [ "6", "CN", "T", "4", "4", "1.00", "0", "0", "0", "0", "0", "1", "0", "0" ] ]
    row "2" "CN" "I" `shouldEqual` [ [ "2", "CN", "I", "4", "10", "1.00", "0", "0", "2", "4", "0", "0", "0", "0" ] ]
