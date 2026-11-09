-- | Counterfactuals, player-seed runs and agency (docs/06_simulator.md §10.4–§11, §10.5). Every
-- | experiment composes the pure `simulate`; none has its own run loop. The views present the results.
module Experiments.Agency
  ( difference
  , counterfactual
  , playerSeedRuns
  , agency
  ) where

import Prelude

import Assembly.Bindings (strategyRulesBindings)
import Data.Tuple (Tuple(..))

import Core.Draw (Seed(..))
import Core.Num as Q
import Laws.Slice (event, read, select)
import Laws.Types (ActorId)
import Laws.World (WorldState)
import Data.Array (concatMap, elem, filter, length, zipWith)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Checks.Objectives (evaluateObjectives)
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.StrategyRules (StrategyRules)
import Schema.PossibleWorld (PossibleWorld)
import Assembly.Error (GameError(..))
import Assembly.Types (Game)
import Handlers.Simulate (SimRun, simulate)
import Views.Agency (AgencyProfile)
import Views.Common (players)
import Views.Difference (Difference, differenceOfWorlds)

finalWorld :: SimRun -> Maybe WorldState
finalWorld r = hush r.final

-- | The difference of two runs' final states; a run that did not finish has no difference.
difference :: SimRun -> SimRun -> Difference
difference base other = case finalWorld base, finalWorld other of
  Just a, Just b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }

-- | Change exactly one thing and replay with both seeds fixed (§10.4).
counterfactual :: Seed -> Game -> (Game -> Game) -> Either GameError { base :: SimRun, variant :: SimRun, difference :: Difference }
counterfactual seed game change = do
  base <- simulate seed game
  counterfactualFrom seed base game change

-- | The same, over a base run already played: only the variant is run.
counterfactualFrom :: Seed -> SimRun -> Game -> (Game -> Game) -> Either GameError { base :: SimRun, variant :: SimRun, difference :: Difference }
counterfactualFrom seed base game change = do
  variant <- simulate seed (change game)
  pure { base, variant, difference: difference base variant }

-- | Player-seed distribution: the game seed fixed, the player seed varied (§11).
playerSeedRuns :: Game -> Array Int -> Either GameError (Array SimRun)
playerSeedRuns game seeds = traverse (\s -> simulate (Seed s) game) seeds

-- | For each seat, replay with the alternative preset while everything else is fixed. The
-- | alternative must be a known preset: `simulate` names an unknown one, so it is an error, not a silent baseline.
agency :: PossibleWorld -> Object StrategyRules -> Seed -> Game -> String -> Array ActorId -> Either GameError (Array AgencyProfile)
agency possibleWorld presets seed game alternative playableActors = do
  base <- simulate seed game
  replacement <- maybe (Left (UnknownPreset alternative)) Right (Object.lookup alternative presets)
  traverse (profile base replacement) playableActors
  where
  profile base replacement playableActor = do
    { variant, difference: d } <- counterfactualFrom seed base game (\g -> g { seats = Map.union (strategyRulesBindings [ Tuple playableActor replacement ]) g.seats })
    let
      isAggregate e = maybe false (\w -> not (elem e (players w))) (finalWorld base)
      absSum xs = Q.sumQ (map Q.abs xs)
      prices = absSum (map _.delta d.prices)
      others = absSum (map _.delta (filter (\h -> h.actor /= playableActor && not (isAggregate h.actor)) d.holdings))
      aggregates = absSum (map _.delta (filter (\h -> isAggregate h.actor) d.holdings))
      outcomes r = concatMap (\x -> select (read @"executed" (event @"ProcedureOutcome")) x.record.events) r.records
      institutions = if outcomes base == outcomes variant then 0 else 1
      ownObjectives r = filter (\o -> o.playableActor == playableActor) (evaluateObjectives possibleWorld r.records)
      own r = map _.outcome (concatMap _.criteria (ownObjectives r))
      objectives = length (filter identity (zipWith (/=) (own base) (own variant)))
      described r = concatMap (\o -> map (\c -> { description: o.description, outcome: c.outcome }) o.criteria) (ownObjectives r)
      criteria = zipWith (\b a -> { description: b.description, baseline: b.outcome, alternative: a.outcome }) (described base) (described variant)
    pure
      { playableActor
      , alternative
      , prices
      , others
      , aggregates
      , institutions
      , objectives
      , criteria
      , measurable: prices /= Q.zero || others /= Q.zero || aggregates /= Q.zero || institutions > 0 || objectives > 0
      }
