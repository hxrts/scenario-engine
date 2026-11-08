-- | Objective evaluation (docs/03_gameboard.md §10.5; docs/06_simulator.md §10.5): every Actor's
-- | objective criteria measured over a run's round records, each met, not met, or not evaluated.
-- | Pure, and never affects the economy.
module Checks.Objectives
  ( evaluateObjectives
  ) where

import Prelude

import Schema.Gameboard (Bound(..), Gameboard, Measure(..), MetricRef, Outcome(..), Span(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, holding)
import Core.Slice (count, event, read, runSlice, total, whereEq, whereNamed)
import Core.Types (EntityId)
import Core.World (RoundResult)
import Data.Array (all, any, concatMap, last, null, zipWith)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

type CriterionResult = { outcome :: Outcome, values :: Array Q, bound :: Array Q }

type ObjectiveResult = { actor :: EntityId, description :: String, criteria :: Array CriterionResult, outcome :: Outcome }

-- | Evaluate every Actor's objective over a run's round records. Never affects the economy.
evaluateObjectives :: Gameboard -> Array RoundResult -> Array ObjectiveResult
evaluateObjectives gb records = concatMap one (Map.toUnfoldable gb.actors :: Array _)
  where
  one (Tuple actor def) = case def.objective of
    Nothing -> []
    Just o ->
      let
        criteria = map criterion o.criteria
      in
        [ { actor, description: o.description, criteria, outcome: objectiveOutcome criteria } ]
  objectiveOutcome criteria
    | null criteria = NotEvaluated
    | any (\c -> c.outcome == NotMet) criteria = NotMet
    | all (\c -> c.outcome == Met) criteria = Met
    | otherwise = NotEvaluated
  criterion c =
    let
      values = series c.over c.metric
      bound = case c.atLeast of
        Fixed q -> map (const q) values
        Of m -> series c.over m
      outcome
        | null records || null values = NotEvaluated
        | all identity (zipWith (>=) values bound) = Met
        | otherwise = NotMet
    in
      { outcome, values, bound }
  -- the metric's value per round, or one aggregate value
  series span m =
    let
      perRound = map (measure m) records
    in
      case span of
        EveryRound -> perRound
        Cumulative -> [ Q.sumQ perRound ]
        Final -> if isFlow m.measure then [ Q.sumQ perRound ] else maybe [] pure (last perRound)

  measure :: MetricRef -> RoundResult -> Q
  measure m r = sign m (Q.sumQ (map (\e -> measureValue m.measure e r) m.entities))
  sign m q = if m.negated then Q.neg q else q
  isFlow = case _ of
    HoldingOf _ -> false
    CapacityAmount _ -> false
    ShortfallOf _ -> true
    ShortfallRounds _ -> true
    PayeeReceipts -> true
    VolumeOf _ -> true

-- | One Entity's value of a measure in one round.
measureValue :: Measure -> EntityId -> RoundResult -> Q
measureValue msr e r = case msr of
  HoldingOf res -> holding r.world e res
  CapacityAmount c -> capacityHeld r.world e c
  ShortfallOf res -> runSlice (total (read @"quantity" (shortfall res))) r.record.events
  ShortfallRounds res -> if runSlice (count (shortfall res)) r.record.events > 0 then Q.fromInt 1 else Q.zero
  PayeeReceipts -> runSlice (total (read @"paid" (whereNamed "paid to the Entity as the facility's payee" payee (event @"Transformed")))) r.record.events
  VolumeOf tid -> runSlice (total (read @"quantity" (whereEq @"transformation" tid (event @"Transformed")))) r.record.events
  where
  shortfall res = whereEq @"resource" res (whereEq @"entity" e (event @"Shortfall"))
  payee t = (Map.lookup t.transformation r.world.transformations >>= _.payee) == Just e
