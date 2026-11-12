-- | The Dependence view over a Pacific run (docs/05 §9.1, Test F): China under a US technology ban, substituting.
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
  it "against the run without the ban, China's own import ban costs it 15 T of shortfall against 1.31, and the US firm its sales abroad" do
    banned <- playing "FreeTrade" "Restrict"
    free <- playing "FreeTrade" "FreeTrade"
    let
      t = keyValues (const Nothing) [ banned, free ]
      rowOf label = filter (\x -> index x 0 == Just label) t.rows
    map (drop 2) (rowOf "Shortfall over the run, CN T") `shouldEqual` [ [ "15", "1.31" ] ]
    map (drop 2) (rowOf "Receipts from sales, US-firm") `shouldEqual` [ [ "27.88", "47.06" ] ]
    -- the ban cuts the US firm's sales abroad to round 1's
    map (drop 2) (rowOf "Sales at home over the run, US-firm") `shouldEqual` [ [ "23.88", "22.25" ] ]
    map (drop 2) (rowOf "Sales abroad over the run, US-firm") `shouldEqual` [ [ "4", "24.81" ] ]
    map (drop 2) (rowOf "Orders blocked over the run, US-firm") `shouldEqual` [ [ "5", "0" ] ]
    map (drop 2) (rowOf "Final coverage, CN T") `shouldEqual` [ [ "0.00", "0.00" ] ]

  it "a private sector's demand is its purchases: China's rebalanced private sector buys technology abroad, with no capacity to cover it" do
    r <- playing "FreeTrade" "Rebalance"
    let
      t = dependence r
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
    row "2" "CN-private" "T" `shouldEqual` [ [ "2", "CN-private", "T", "1.31", "0", "n/a", "n/a", "1.31", "0", "0", "0", "0", "0", "0" ] ]
    let kv = keyValues (const Nothing) [ r ]
    map (drop 2) (filter (\x -> index x 0 == Just "Purchases over the run, CN-private T") kv.rows) `shouldEqual` [ [ "10.47" ] ]

  it "China under its own import ban: exposure 3 and short 3 every round, while its industrial inputs sell" do
    loaded <- liftEffect (loadFrom fileOps "pacific/v0")
    parts <- orThrow loaded
    bindings <- orThrow (presetBindings parts.presets (\a -> if a == Id "US" then "FreeTrade" else if a == Id "CN" then "Restrict" else "FreeTrade") [ Id "US", Id "CN", Id "US-firm", Id "CN-firm" ])
    g <- orThrow (assemble (FromComposition parts) bindings [])
    r <- run (Seed 1) g
    let
      t = dependence (toStored "pacific" r)
      row round actor resource = filter (\x -> index x 0 == Just round && index x 1 == Just actor && index x 2 == Just resource) t.rows
    -- Round, Actor, Resource, Requirement, Capacity, Coverage, Exposure, Imports, Exports, Receipts, Shortfall, Bans against, Capacity on order
    row "2" "CN" "T" `shouldEqual` [ [ "2", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "0", "0", "0" ] ]
    row "6" "CN" "T" `shouldEqual` [ [ "6", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "0", "0", "0" ] ]
    row "2" "CN" "I" `shouldEqual` [ [ "2", "CN", "I", "4", "8", "1.00", "0", "0", "4", "8", "0", "0", "0", "0" ] ]
