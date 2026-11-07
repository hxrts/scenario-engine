-- | Objective evaluation (docs/03_gameboard.md §10.5; docs/06_simulator.md §10.5): every Actor's
-- | objective criteria measured over a run's round records, each met, not met, or not evaluated.
-- | Pure, and never affects the economy.
module Checks.Objectives
  ( CriterionResult
  , ObjectiveResult
  , Outcome(..)
  , evaluateObjectives
  ) where

import Prelude

import Schema.Gameboard (Bound(..), Gameboard, Measure(..), MetricRef, Span(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, eventsOf, holding)
import Core.Types (EntityId)
import Core.World (Event(..), RoundResult)
import Data.Array (all, any, concatMap, last, null, zipWith)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

-- | A criterion or objective is met, not met, or not evaluated. A run with no rounds, or a
-- | criterion with no series, is not evaluated: it is neither a pass nor a failure.
data Outcome = Met | NotMet | NotEvaluated

derive instance Eq Outcome

-- | The words the views show; the stored DSL spells the same outcomes in its own form (the capstone's expectations, Premises.Capstone).
instance Show Outcome where
  show = case _ of
    Met -> "met"
    NotMet -> "not met"
    NotEvaluated -> "not evaluated"

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
    _ -> true

-- | One Entity's value of a measure in one round.
measureValue :: Measure -> EntityId -> RoundResult -> Q
measureValue msr e r = case msr of
  HoldingOf res -> holding r.world e res
  CapacityAmount c -> capacityHeld r.world e c
  ShortfallOf res -> sumEvents r case _ of
    Shortfall s | s.entity == e && s.resource == res -> Just s.quantity
    _ -> Nothing
  ShortfallRounds res -> if sumEvents r (shortfall res) > Q.zero then Q.fromInt 1 else Q.zero
  PayeeReceipts -> sumEvents r case _ of
    Transformed t | (Map.lookup t.transformation r.world.transformations >>= _.payee) == Just e -> Just t.paid
    _ -> Nothing
  VolumeOf tid -> sumEvents r case _ of
    Transformed t | t.transformation == tid -> Just t.quantity
    _ -> Nothing
  where
  shortfall res = case _ of
    Shortfall s | s.entity == e && s.resource == res -> Just (Q.fromInt 1)
    _ -> Nothing

sumEvents :: RoundResult -> (Event -> Maybe Q) -> Q
sumEvents r f = Q.sumQ (eventsOf f r.record.events)
