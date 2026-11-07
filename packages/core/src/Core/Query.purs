-- | Read-only queries over a World State, shared by the engine, invariants, analysis views,
-- | and tests. Absent entries read as zero.
module Core.Query
  ( lookupQ
  , holding
  , capacityHeld
  , priceOf
  , totalHolding
  , isAggregate
  , homeMarket
  ) where

import Prelude

import Core.Market (marginal)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (CapacityRef, Entity, EntityId, EntityKind(..), MarketId, Price, ResourceId)
import Core.World (WorldState)
import Data.Array (elem, find, fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)

lookupQ :: forall k. Ord k => k -> Map k Q -> Q
lookupQ k m = fromMaybe Q.zero (Map.lookup k m)

-- | What an Entity holds of a Resource.
holding :: WorldState -> EntityId -> ResourceId -> Q
holding w e r = fromMaybe Q.zero (Map.lookup e w.entities <#> \x -> lookupQ r x.accounts)

-- | The amount of a capacity an Entity holds.
capacityHeld :: WorldState -> EntityId -> CapacityRef -> Q
capacityHeld w e ref = fromMaybe Q.zero (Map.lookup e w.entities <#> \x -> lookupQ ref x.capacities)

-- | A Market's current marginal price.
priceOf :: WorldState -> MarketId -> Maybe Price
priceOf w m = Map.lookup m w.markets <#> \x -> marginal x.cost x.position

-- | A Resource's total across every account and Market reserve.
totalHolding :: WorldState -> ResourceId -> Q
totalHolding w r = Q.sumQ (map (lookupQ r <<< _.accounts) (fromFoldable (Map.values w.entities)) <> map (lookupQ r <<< _.reserves) (fromFoldable (Map.values w.markets)))

isAggregate :: Entity -> Boolean
isAggregate e = case e.kind of
  Aggregate _ -> true
  _ -> false

-- | An Entity's home market: the first Market (by id) it has access to, or Nothing when it has none.
homeMarket :: WorldState -> EntityId -> Maybe MarketId
homeMarket w e = map _.id (find (\m -> e `elem` m.access) (fromFoldable (Map.values w.markets)))
