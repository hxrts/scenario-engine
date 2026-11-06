-- | Shared shapes and helpers for the dashboard view models (docs/06_simulator.md §9–§10): the
-- | Table and Chart the frontend renders, the stored run the single-run views read, the metric
-- | names, round scoping, and cell formatting. Pure.
module Harness.Views.Common
  ( Table
  , Chart
  , StoredRun
  , MetricSpec
  , defaultMetricSpec
  , q
  , at
  , final
  , outOfRange
  , kindOf
  , series
  , specValue
  , joinComma
  ) where

import Prelude

import Core.Invariants (Failure)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, EntityKind(..))
import Core.World (Manifest, RoundRecord, WorldState)
import Data.Array (concatMap, filter, find, index, length, mapWithIndex, nub)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

type Table = { title :: String, columns :: Array String, rows :: Array (Array String) }

-- | A metric per round; values are decimal strings in their own units.
type Chart = { title :: String, unit :: String, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }

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

-- | A quantity as a cell.
q :: Q -> String
q = Q.format

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
