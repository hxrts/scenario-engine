-- | Differences, counterfactuals and agency (docs/06_simulator.md §10.4–§11, §10.5). Every analysis
-- | composes `simulate`; none has its own run loop. Pure.
module Analysis.Agency
  ( Difference
  , difference
  , differenceOfWorlds
  , counterfactual
  , playerSeedRuns
  , AgencyProfile
  , agency
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Query (accountsOf, eventsOf, lookupQ, priceOf)
import Core.Query as Query
import Core.Types (EntityId, MarketId, ResourceId)
import Core.World (Event(..), WorldState)
import Data.Array (concatMap, filter, fromFoldable, length, nub, sort, zipWith)
import Data.Either (Either, hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Analysis.Objectives (evaluateObjectives)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimError, SimulationSpec, simulate)

-- | How two runs' final states differ: per-Entity holdings and per-Market prices. Each names its
-- | Resource or Market by id; the views render the text.
type Difference =
  { holdings :: Array { entity :: EntityId, resource :: ResourceId, delta :: Q }
  , prices :: Array { market :: MarketId, delta :: Q }
  }

finalWorld :: Run -> Maybe WorldState
finalWorld r = hush r.final

-- | The difference of two runs' final states; a run that did not finish has no difference.
difference :: Run -> Run -> Difference
difference base other = case finalWorld base, finalWorld other of
  Just a, Just b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }

-- | The symmetric difference of two final World States: every Entity and Resource either holds (in
-- | either state), and every Market either quotes; a value is reported as other minus base.
differenceOfWorlds :: WorldState -> WorldState -> Difference
differenceOfWorlds a b =
  { holdings: filter (\h -> h.delta /= Q.zero) (concatMap (holdingDelta a b) entities)
  , prices: filter (\p -> p.delta /= Q.zero) (map (\id -> { market: id, delta: Q.sub (price b id) (price a id) }) markets)
  }
  where
  entities = sort (nub (fromFoldable (Map.keys a.entities) <> fromFoldable (Map.keys b.entities)))
  markets = sort (nub (fromFoldable (Map.keys a.markets) <> fromFoldable (Map.keys b.markets)))
  price w id = fromMaybe Q.zero (priceOf w id)
  holdingDelta x y e =
    let
      hx = accountsOf x e
      hy = accountsOf y e
    in
      map (\r -> { entity: e, resource: r, delta: Q.sub (lookupQ r hy) (lookupQ r hx) })
        (sort (nub (fromFoldable (Map.keys hx) <> fromFoldable (Map.keys hy))))

-- | Change exactly one thing and replay with both seeds fixed (§10.4).
counterfactual :: Context -> SimulationSpec -> (SimulationSpec -> SimulationSpec) -> Either SimError { base :: Run, variant :: Run, difference :: Difference }
counterfactual ctx spec change = do
  base <- simulate ctx spec
  variant <- simulate ctx (change spec) { playerSeed = spec.playerSeed }
  pure { base, variant, difference: difference base variant }

-- | Player-seed distribution: the game seed fixed, the player seed varied (§11).
playerSeedRuns :: Context -> SimulationSpec -> Array Int -> Either SimError (Array Run)
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
-- | alternative must be a known preset: `simulate` names an unknown one, so it is an error, not a silent baseline.
agency :: Context -> SimulationSpec -> String -> Array EntityId -> Either SimError (Array AgencyProfile)
agency ctx spec alternative actors = do
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
      outcomes r = concatMap (\x -> eventsOf executedOf x.record.events) r.records
      executedOf = case _ of
        ProcedureOutcome o -> Just o.executed
        _ -> Nothing
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
