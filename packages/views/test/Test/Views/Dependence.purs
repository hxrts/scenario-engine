-- | The Dependence view over a Pacific run (docs/05 §9): China under the US export control on technology.
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
import Testkit.Run (orThrow, run)
import Views.Dependence (dependence)

playing :: String -> String -> Aff StoredRun
playing us cn = do
  loaded <- liftEffect (loadFrom fileOps "pacific/v0")
  parts <- orThrow loaded
  bindings <- orThrow (presetBindings parts.presets (\a -> if a == Id "US" then us else if a == Id "CN" then cn else "FreeTrade") [ Id "US", Id "CN", Id "US-firm", Id "CN-firm" ])
  g <- orThrow (assemble (FromComposition parts) bindings [])
  r <- run (Seed 1) g
  pure (toStored (us <> "-" <> cn) r)

spec :: Spec Unit
spec = describe "Dependence" do
  it "against the run without it, the US export control costs China 15 T of shortfall against 2.6, and the US firm most of its sales abroad" do
    controlled <- playing "Restrict" "FreeTrade"
    free <- playing "FreeTrade" "FreeTrade"
    let
      t = keyValues (const Nothing) [ controlled, free ]
      rowOf label = map (drop 2) (filter (\x -> index x 0 == Just label) t.rows)
    [ rowOf "Shortfall over the run, CN T", rowOf "Receipts from sales, US-firm", rowOf "Sales at home over the run, US-firm", rowOf "Sales abroad over the run, US-firm", rowOf "Orders blocked over the run, US-firm", rowOf "Final coverage, CN T" ] `shouldEqual` [ [ [ "15", "2.6" ] ], [ [ "27.25", "47.02" ] ], [ [ "23.56", "20.53" ] ], [ [ "12.05", "35.47" ] ], [ [ "5", "0" ] ], [ [ "0.00", "0.00" ] ] ]

  it "a private sector's demand is its purchases: China's private sector buys technology abroad, with no capacity to cover it" do
    r <- playing "FreeTrade" "FreeTrade"
    let
      t = dependence r
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
      kv = keyValues (const Nothing) [ r ]
    [ row "3" "CN-private" "T", map (drop 2) (filter (\x -> index x 0 == Just "Purchases over the run, CN-private T") kv.rows) ] `shouldEqual` [ [ [ "3", "CN-private", "T", "0.76", "0", "n/a", "n/a", "0.76", "0", "0", "0", "0", "0", "0" ] ], [ [ "4.6" ] ] ]

  it "China under the US export control: exposure 3 and short 3 every round from round 2, while its industrial inputs sell" do
    r <- playing "Restrict" "FreeTrade"
    let
      t = dependence r
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
    -- Round, Actor, Resource, Requirement, Capacity, Coverage, Exposure, Imports, Exports, Receipts, Shortfall, Bans against, Capacity on order, Unsold
    [ row "2" "CN" "T", row "6" "CN" "T", row "2" "CN" "I" ] `shouldEqual` [ [ [ "2", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "1", "0", "0" ] ], [ [ "6", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "1", "0", "0" ] ], [ [ "2", "CN", "I", "4", "8", "1.00", "0", "0", "4", "8", "0", "0", "0", "0" ] ] ]
