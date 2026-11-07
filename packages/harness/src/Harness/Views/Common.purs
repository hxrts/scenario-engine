-- | Shared shapes and helpers for the dashboard view models (docs/06_simulator.md §9–§10): the
-- | Table and Chart the frontend renders, the stored run the single-run views read, the metric
-- | names, round scoping, and cell formatting. Pure.
module Harness.Views.Common
  ( Table
  , Chart
  , Statistic
  , StoredRun
  , MetricSpec
  , defaultMetricSpec
  , quantity
  , price
  , ratio
  , exact
  , at
  , final
  , outOfRange
  , kindOf
  , series
  , specValue
  , joinComma
  , players
  , roundResults
  ) where

import Prelude

import Core.Invariants (Failure)
import Core.Num (Kind, Q)
import Core.Num as Q
import Core.Query (isAggregate)
import Core.Types (EntityId, EntityKind(..))
import Core.World (Manifest, RoundRecord, RoundResult, WorldState)
import Data.Array (concatMap, filter, find, index, length, mapWithIndex, nub)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst)

type Table = { title :: String, columns :: Array String, rows :: Array (Array String) }

-- | A metric per round in its own unit. The points are exact decimal strings (`exact`), so the plot
-- | keeps full precision; the chart's axis and tooltips show the value under its display `kind`.
type Chart = { title :: String, unit :: String, kind :: Kind, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }

-- | A statistic the views show: its name, the question it answers, its one-line definition, and its
-- | formula. Each statistic is defined once, in the module that computes it; the dashboard renders it.
type Statistic = { name :: String, question :: String, definition :: String, formula :: String }

-- | A run as read back from the Run Store, with its records and snapshots (single-run views).
type StoredRun =
  { id :: String
  , name :: String
  , status :: String
  , manifest :: Manifest
  , playerSeed :: String
  , spec :: Array (Tuple String String) -- flattened specification, for configuration diffs
  , sourceDeleted :: Boolean -- the run it was derived from is no longer in the store
  , records :: Array RoundRecord
  , snapshots :: Array WorldState
  , assertions :: Array (Array Failure)
  }

-- | The scenario-specific names the metric views use. The default is the reference scenario
-- | (docs/05_test_instance.md); a caller passes its own to view another scenario.
type MetricSpec =
  { procurement :: Array String -- transformations counted as procurement of the military
  , procurementLabel :: String -- the overview's label for that procurement
  , market :: String -- the Market a trajectory comparison shows by default
  }

defaultMetricSpec :: MetricSpec
defaultMetricSpec =
  { procurement: [ "defense-procurement", "defense-procurement-cn" ]
  , procurementLabel: "Military"
  , market: "energy-atlantic"
  }

-- | Display (docs/02 §9.1): a count of units (Money, Resource holdings and flows) as a cell.
quantity :: Q -> String
quantity = Q.displayQuantity

-- | A price (quote per base unit) as a cell.
price :: Q -> String
price = Q.displayPrice

-- | A ratio or share (Gini, influence, shrinkage, factors, support) as a cell.
ratio :: Q -> String
ratio = Q.displayRatio

-- | The exact value, at four decimals, for chart points: the plot keeps full precision.
exact :: Q -> String
exact = Q.format

-- | The World State after round `n` (0 = initial); Nothing for a round the run does not have.
at :: StoredRun -> Int -> Maybe WorldState
at r n
  | n < 0 = Nothing
  | n == 0 = Just r.manifest.initial
  | otherwise = index r.snapshots (n - 1)

-- | The World State after the run's last completed round.
final :: StoredRun -> WorldState
final r = fromMaybe r.manifest.initial (at r (length r.snapshots))

-- | The error for a round outside `lo..` the run's completed rounds.
outOfRange :: StoredRun -> Int -> Int -> String
outOfRange r lo n = "round " <> show n <> " is out of range for run " <> r.id <> " (" <> show lo <> ".." <> show (length r.records) <> ")"

-- | An Entity's kind as the views label it: an Aggregate is residual, every other Entity an Actor.
kindOf :: WorldState -> EntityId -> String
kindOf w e = case _.kind <$> Map.lookup e w.entities of
  Just (Aggregate _) -> "Aggregate (residual)"
  _ -> "Actor"

-- | One named series per key of `f` over the initial state and every snapshot, each point the
-- | value of that key at a round (0 = initial).
series :: forall a. StoredRun -> String -> (WorldState -> Array (Tuple String a)) -> (a -> String) -> Array { name :: String, points :: Array { round :: Int, value :: String } }
series r _ f g =
  let
    states = mapWithIndex Tuple ([ r.manifest.initial ] <> r.snapshots)
    names = nub (concatMap (\(Tuple _ w) -> map (\(Tuple n _) -> n) (f w)) states)
  in
    map (\name -> { name, points: concatMap (\(Tuple i w) -> map (\(Tuple _ v) -> { round: i, value: g v }) (filter (\(Tuple n _) -> n == name) (f w))) states }) names

-- | The value of a key in a flattened specification.
specValue :: Array (Tuple String String) -> String -> Maybe String
specValue spec k = map (\(Tuple _ v) -> v) (find (\(Tuple x _) -> x == k) spec)

joinComma :: Array String -> String
joinComma = joinWith ", "

-- | The players of a World State: its Actors, every Entity except the Aggregates (which are residual
-- | representations, not players), in Entity order.
players :: WorldState -> Array EntityId
players w = map fst (filter (\(Tuple _ e) -> not (isAggregate e)) (Map.toUnfoldable w.entities :: Array _))

-- | Each record with the World State it leaves behind (round i+1 pairs with record i), as the
-- | objectives are evaluated over a run.
roundResults :: StoredRun -> Array RoundResult
roundResults r = mapWithIndex (\i rec -> { world: fromMaybe r.manifest.initial (at r (i + 1)), record: rec }) r.records
