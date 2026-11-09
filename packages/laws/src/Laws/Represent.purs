-- | Representation estimates (docs/02_type_system.md §7). The engine only computes and
-- | stores this round's adjustment factors; the Aggregate's Policy applies them next round.
module Laws.Represent (Observation, estimate) where

import Prelude

import Core.Map (entries, sumInto)

import Core.Draw (draw)
import Core.Num (Q)
import Core.Num as Q
import Laws.Types (BehaviorDomain, EntityId, Ratio, Representation, ResourceId)
import Laws.World (Event(..), WorldState)
import Data.Array (concatMap, filter, foldl, null)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | One Actor's proposed quantity of a Resource, as read from what it validly proposed.
type Observation = { actor :: EntityId, resource :: ResourceId, quantity :: Q }

-- | A Representation names the Resource whose purchases are observed. The observation is the actor's
-- | validated buy quantity as proposed: the sum of its Trade quantities over screened Proposals,
-- | before funding scales them and before clearing. It is what the actor asked to buy, not its
-- | executed fill.
observation :: Representation -> Array Observation -> Maybe Q
observation rep os =
  let
    orders = map _.quantity (filter (\o -> o.actor == rep.actor && o.resource == rep.observes) os)
  in
    if null orders then Nothing else Just (Q.sumQ (map (max Q.zero) orders))

-- | Clamped deviation from the reference, with seeded sampling noise drawn from the game seed.
-- | Nothing when the reference is zero, since a relative deviation is then undefined.
deviation :: WorldState -> Representation -> Q -> Maybe Q
deviation w rep observed = do
  let e = rep.estimator
  rel <- Q.divMaybe (Q.sub observed e.reference) e.reference
  let
    u = draw w.seed { round: w.round, source: unwrap rep.id, entity: unwrap rep.actor, category: "sampling", index: 0 }
    eps = Q.mul e.noiseSpread (Q.sub (Q.mul (Q.fromInt 2) u) (Q.fromInt 1))
    raw = Q.add rel eps
  pure (maybe raw (\b -> max b.lo (min b.hi raw)) e.deviationBound)

-- | This round's adjustment factors: 1 + Σ shrinkage × weight × deviation per Aggregate and
-- | domain, over the Representations that have an observation and a non-zero reference.
estimate :: WorldState -> Array Observation -> { factors :: Map EntityId (Map BehaviorDomain Ratio), events :: Array Event }
estimate w observed =
  let
    terms = concatMap
      ( \rep -> case observation rep observed of
          Just obs | Just dev <- deviation w rep obs -> [ { rep, term: Q.mul rep.estimator.shrinkage (Q.mul rep.weight dev) } ]
          _ -> []
      )
      w.representations
    sums = foldl (\m t -> sumInto (Tuple t.rep.aggregate t.rep.domain) t.term m) Map.empty terms
    estimates = map (\(Tuple (Tuple agg dom) s) -> { aggregate: agg, domain: dom, factor: Q.add (Q.fromInt 1) s }) (entries sums)
    factors = foldl (\m e -> Map.insertWith Map.union e.aggregate (Map.singleton e.domain e.factor) m) Map.empty estimates
    events = map (\e -> Estimated { round: w.round, aggregate: e.aggregate, domain: e.domain, factor: e.factor }) estimates
  in
    { factors, events }
