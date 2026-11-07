-- | Read-only queries over a World State, shared by the engine, invariants, analysis views,
-- | and tests. Absent entries read as zero. Also here: the capacity and facility-pool helpers
-- | that Fund prices and Settle allocates against, and the Intent selectors.
module Core.Query
  ( lookupQ
  , holding
  , accountsOf
  , eventsOf
  , countEvents
  , capacityHeld
  , priceOf
  , totalHolding
  , reserveOf
  , aggregateOf
  , isAggregate
  , homeMarket
  , owner
  , capacityOf
  , facilityPrice
  , poolOf
  , poolCeiling
  , tradesOf
  , transfersOf
  , transformsOf
  , transformedLastRound
  , restrictedAtBoundary
  ) where

import Prelude

import Core.Market (averagePrice, marginal)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (Aggregate, BehaviorDomain, CapacityRef, Entity, EntityId, EntityKind(..), Intent, IntentF(..), MarketId, Order, Price, Quantity, Ratio, ResourceId, RoundIndex, Rule, RuleF(..), TransferSpec, Transformation, TransformationId, TransformSpec)
import Core.World (Event(..), WorldState)
import Data.Array (any, concatMap, elem, filter, find, fromFoldable, length, mapMaybe)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Foldable (foldl)

lookupQ :: forall k. Ord k => k -> Map k Q -> Q
lookupQ k m = fromMaybe Q.zero (Map.lookup k m)

-- | What an Entity holds of a Resource.
holding :: WorldState -> EntityId -> ResourceId -> Q
holding w e r = lookupQ r (accountsOf w e)

-- | An Entity's accounts: the amount it holds of each Resource it has an account for; empty for an
-- | Entity the World State does not have.
accountsOf :: WorldState -> EntityId -> Map ResourceId Q
accountsOf w e = maybe Map.empty _.accounts (Map.lookup e w.entities)

-- | The values the matching events yield, in recorded order: the one way a view picks out events of
-- | some kinds, so every event match reads the same way.
eventsOf :: forall a. (Event -> Maybe a) -> Array Event -> Array a
eventsOf = mapMaybe

-- | How many events match the predicate.
countEvents :: (Event -> Boolean) -> Array Event -> Int
countEvents p = length <<< eventsOf (\ev -> if p ev then Just ev else Nothing)

-- | The amount of a capacity an Entity holds.
capacityHeld :: WorldState -> EntityId -> CapacityRef -> Q
capacityHeld w e ref = fromMaybe Q.zero (Map.lookup e w.entities <#> \x -> lookupQ ref x.capacities)

-- | A Market's current marginal price.
priceOf :: WorldState -> MarketId -> Maybe Price
priceOf w m = Map.lookup m w.markets <#> \x -> marginal x.cost x.position

-- | A Resource's total across every account and Market reserve.
totalHolding :: WorldState -> ResourceId -> Q
totalHolding w r = Q.sumQ (map (lookupQ r <<< _.accounts) (fromFoldable (Map.values w.entities)) <> map (lookupQ r <<< _.reserves) (fromFoldable (Map.values w.markets)))

-- | What a Market holds of a Resource in reserve; zero for an unknown Market.
reserveOf :: WorldState -> MarketId -> ResourceId -> Q
reserveOf w m r = maybe Q.zero (lookupQ r <<< _.reserves) (Map.lookup m w.markets)

-- | The stored Representation factor of an Aggregate in a Behavior Domain, if one is stored.
factorOf :: WorldState -> EntityId -> BehaviorDomain -> Maybe Ratio
factorOf w a d = Map.lookup a w.factors >>= Map.lookup d

-- | An Entity's Aggregate definition, when it is an Aggregate.
aggregateOf :: Entity -> Maybe Aggregate
aggregateOf e = case e.kind of
  Aggregate a -> Just a
  _ -> Nothing

isAggregate :: Entity -> Boolean
isAggregate = isJust <<< aggregateOf

-- | An Entity's home market: the first Market (by id) it has access to, or Nothing when it has none.
homeMarket :: WorldState -> EntityId -> Maybe MarketId
homeMarket w e = map _.id (find (\m -> e `elem` m.access) (fromFoldable (Map.values w.markets)))

-- | The capacity's owner: the first Entity holding it. Withholding and capacity lookups both use this.
owner :: WorldState -> CapacityRef -> Maybe EntityId
owner w ref = _.id <$> find (\e -> Map.member ref e.capacities) (fromFoldable (Map.values w.entities))

-- | Effective capacity: the owner's capacity less withheld units.
capacityOf :: WorldState -> CapacityRef -> Q
capacityOf w ref =
  let
    held = maybe Q.zero (\o -> capacityHeld w o ref) (owner w ref)
    withheld = Q.sumQ
      ( map
          ( case _ of
              Withholding r | r.capacity == ref -> r.units
              _ -> Q.zero
          )
          (fromFoldable (Map.values w.rules))
      )
  in
    max Q.zero (Q.sub held withheld)

-- | A facility's uniform price at a total of `q` units: its cost function's average price, or
-- | zero for a facility with no cost function.
facilityPrice :: Transformation -> Q -> Q
facilityPrice tr q = maybe Q.zero (\c -> averagePrice c Q.zero q) tr.cost

-- | The pool a facility's fills draw on: the shared capacity if it is capped, else the facility
-- | alone. Facilities in one pool are allocated together and priced at one uniform price.
poolOf :: TransformationId -> Transformation -> Either TransformationId CapacityRef
poolOf tid tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Right ref
  _ -> Left tid

-- | The most units a facility's pool can fill in total, its ceiling, which escrow prices at in full
-- | and settlement never exceeds (§5.3). The pool is every facility drawing on the same capped
-- | capacity, so its fills together draw on that capacity: a total of q units uses at least
-- | q × (smallest `capacityUse` in the pool). The ceiling is therefore the capacity divided by that
-- | smallest use, rounded down, and every member of the pool escrows at it, whatever its own use.
-- | An uncapped facility's ceiling is the domain top of its cost function. Nothing means no
-- | ceiling, which only a facility with no cost function has, and that one is priced at zero.
poolCeiling :: WorldState -> Transformation -> Maybe Q
poolCeiling w tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Q.divFloor (capacityOf w ref) (smallestUse w ref tr.capacityUse)
  _ -> (\c -> c.domain.hi) <$> tr.cost

-- | The smallest `capacityUse` among the capped facilities on `ref`, starting from `use` (the
-- | facility's own, so the result is never above it).
smallestUse :: WorldState -> CapacityRef -> Q -> Q
smallestUse w ref use = foldl min use (map _.capacityUse (filter drawsOn (fromFoldable (Map.values w.transformations))))
  where
  drawsOn x = x.capacity == Just ref && x.capacityUse > Q.zero

-- | The Trade orders of one Intent list, in order: the one place that selects a Trade.
tradesOf :: Array Intent -> Array Order
tradesOf = mapMaybe
  ( case _ of
      Trade o -> Just o
      _ -> Nothing
  )

transfersOf :: Array Intent -> Array TransferSpec
transfersOf = mapMaybe
  ( case _ of
      Transfer t -> Just t
      _ -> Nothing
  )

transformsOf :: Array Intent -> Array TransformSpec
transformsOf = mapMaybe
  ( case _ of
      Transform t -> Just t
      _ -> Nothing
  )

-- | Whether `by` ran `transformation` in the round before `round` for at least `atLeast` units.
transformedLastRound :: RoundIndex -> EntityId -> TransformationId -> Quantity -> Array Event -> Boolean
transformedLastRound round by transformation atLeast = any
  ( case _ of
      Transformed t -> t.round == round - 1 && t.by == by && t.transformation == transformation && t.quantity >= atLeast
      _ -> false
  )

-- | The Rules `by` enacted against `target` that took effect at the boundary of `round`, i.e. that
-- | were proposed in the round before: an AccessBan on `target`, or a Tariff on its origin.
restrictedAtBoundary :: RoundIndex -> EntityId -> EntityId -> Array Event -> Array Rule
restrictedAtBoundary round by target = concatMap
  ( case _ of
      Enacted e | e.round == round -> case e.rule of
        r@(AccessBan x) | x.jurisdiction == by && x.target == target -> [ r ]
        r@(Tariff x) | x.jurisdiction == by && x.origin == target -> [ r ]
        _ -> []
      _ -> []
  )
