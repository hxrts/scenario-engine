-- | Pure analyses over saved runs (docs/06_simulator.md §10–§11; docs/03_gameboard.md
-- | §10.5): objective evaluation, run differences, counterfactuals, seed experiments, and
-- | agency. All of them compose `simulate`; none has its own run loop.
module Harness.Analysis
  ( Outcome(..)
  , CriterionResult
  , ObjectiveResult
  , evaluateObjectives
  , Difference
  , difference
  , counterfactual
  , playerSeedRuns
  , AgencyProfile
  , agency
  ) where

import Prelude

import Board.Gameboard (Bound(..), Gameboard, Measure(..), MetricRef, Span(..))
import Core.Draw (Seed(..))
import Core.Query (capacityHeld, holding, lookupQ, priceOf)
import Core.Query as Query
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId)
import Core.World (Event(..), RoundResult, WorldState)
import Data.Array (all, any, concatMap, length, filter, fromFoldable, last, nub, null, zipWith)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Harness.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, simulate)

-- Objectives ----------------------------------------------------------------------

-- | A criterion or objective is met, not met, or not evaluated. A run with no rounds, or a
-- | criterion with no series, is not evaluated: it is neither a pass nor a failure.
data Outcome = Met | NotMet | NotEvaluated

derive instance Eq Outcome

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
    Shortfall s | s.entity == e && s.resource == res -> [ s.quantity ]
    _ -> []
  ShortfallRounds res -> if sumEvents r (shortfall res) > Q.zero then Q.fromInt 1 else Q.zero
  PayeeReceipts -> sumEvents r case _ of
    Transformed t | (Map.lookup t.transformation r.world.transformations >>= _.payee) == Just e -> [ t.paid ]
    _ -> []
  VolumeOf tid -> sumEvents r case _ of
    Transformed t | t.transformation == tid -> [ t.quantity ]
    _ -> []
  where
  shortfall res = case _ of
    Shortfall s | s.entity == e && s.resource == res -> [ Q.fromInt 1 ]
    _ -> []

sumEvents :: RoundResult -> (Event -> Array Q) -> Q
sumEvents r f = Q.sumQ (concatMap f r.record.events)

-- Differences and counterfactuals ---------------------------------------------------

-- | How two runs' final states differ: per-Entity holdings and per-Market prices.
type Difference =
  { holdings :: Array { entity :: EntityId, resource :: String, delta :: Q }
  , prices :: Array { market :: String, delta :: Q }
  }

finalWorld :: Run -> Maybe WorldState
finalWorld r = hush r.final

difference :: Run -> Run -> Difference
difference base other = case finalWorld base, finalWorld other of
  Just a, Just b ->
    { holdings: filter (\h -> h.delta /= Q.zero) (concatMap (holdingDelta a b) (fromFoldable (Map.keys a.entities)))
    , prices: filter (\p -> p.delta /= Q.zero) (map (\id -> { market: show id, delta: Q.sub (price b id) (price a id) }) (fromFoldable (Map.keys a.markets)))
    }
  _, _ -> { holdings: [], prices: [] }
  where
  price w id = fromMaybe Q.zero (priceOf w id)
  holdingDelta a b e =
    let
      ha = fromMaybe Map.empty (_.accounts <$> Map.lookup e a.entities)
      hb = fromMaybe Map.empty (_.accounts <$> Map.lookup e b.entities)
    in
      map (\r -> { entity: e, resource: show r, delta: Q.sub (lookupQ r hb) (lookupQ r ha) })
        (nub (fromFoldable (Map.keys ha) <> fromFoldable (Map.keys hb)))

-- | Change exactly one thing and replay with both seeds fixed (§10.4).
counterfactual :: Context -> SimulationSpec -> (SimulationSpec -> SimulationSpec) -> Either String { base :: Run, variant :: Run, difference :: Difference }
counterfactual ctx spec change = do
  base <- simulate ctx spec
  variant <- simulate ctx (change spec) { playerSeed = spec.playerSeed }
  pure { base, variant, difference: difference base variant }

-- | Player-seed distribution: the game seed fixed, the player seed varied (§11).
playerSeedRuns :: Context -> SimulationSpec -> Array Int -> Either String (Array Run)
playerSeedRuns ctx spec seeds = traverse (\s -> simulate ctx spec { playerSeed = Seed s }) seeds

-- Agency (§10.5) ----------------------------------------------------------------------

type AgencyProfile =
  { actor :: EntityId
  , alternative :: String
  , prices :: Q -- total absolute change in final prices
  , others :: Q -- total absolute change in other Actors' holdings
  , aggregates :: Q -- total absolute change in Aggregate holdings
  , institutions :: Int -- procedure outcomes that differ
  , objectives :: Int -- own objective criteria whose result differs
  , measurable :: Boolean
  }

-- | For each seat, replay with the alternative preset while everything else is fixed. The
-- | alternative must be a known preset; an unknown name is an error, not a silent baseline.
agency :: Context -> SimulationSpec -> String -> Array EntityId -> Either String (Array AgencyProfile)
agency ctx spec alternative actors
  | not (Object.member alternative ctx.presets) = Left ("unknown preset " <> alternative)
  | otherwise =
      do
        base <- simulate ctx spec
        traverse (profile base) actors
      where
      profile base actor = do
        variant <- simulate ctx spec { seats = Map.insert actor (Preset alternative) spec.seats }
        let
          d = difference base variant
          isAggregate e = maybe false Query.isAggregate (finalWorld base >>= \w -> Map.lookup e w.entities)
          absSum xs = Q.sumQ (map Q.abs xs)
          prices = absSum (map _.delta d.prices)
          others = absSum (map _.delta (filter (\h -> h.entity /= actor && not (isAggregate h.entity)) d.holdings))
          aggregates = absSum (map _.delta (filter (\h -> isAggregate h.entity) d.holdings))
          outcomes r = concatMap
            ( \x -> concatMap
                ( case _ of
                    ProcedureOutcome o -> [ o.executed ]
                    _ -> []
                )
                x.record.events
            )
            r.records
          institutions = if outcomes base == outcomes variant then 0 else 1
          own r = map _.outcome (concatMap _.criteria (filter (\o -> o.actor == actor) (evaluateObjectives ctx.gameboard r.records)))
          objectives = length (filter identity (zipWith (/=) (own base) (own variant)))
        pure
          { actor
          , alternative
          , prices
          , others
          , aggregates
          , institutions
          , objectives
          , measurable: prices /= Q.zero || others /= Q.zero || aggregates /= Q.zero || institutions > 0 || objectives > 0
          }
