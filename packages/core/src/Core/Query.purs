-- | Read-only queries over a World State, shared by the engine, invariants, analysis views,
-- | and tests. Absent entries read as zero. Also here: the capacity and Transformation-pool helpers
-- | that Fund prices and Settle allocates against, and the Intent selectors.
module Core.Query
  ( lookupQ
  , holding
  , accountsOf
  , capacityHeld
  , capabilitiesOf
  , entitiesOf
  , marketsOf
  , rulesOf
  , transformationsOf
  , priceOf
  , totalHolding
  , reserveOf
  , aggregateOf
  , isAggregate
  , owner
  , capacityOf
  , conversionPrice
  , poolOf
  , poolCeiling
  , tradesOf
  , transfersOf
  , transformsOf
  , enactedAtBoundary
  , droppedProposals
  ) where

import Prelude

import Control.Alt ((<|>))
import Core.Map (values)

import Core.Market (averagePrice, marketPrice)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (Aggregate, CapacityRef, Capability, Entity, EntityId, EntityKind(..), IntentF(..), Market, MarketId, Price, ProposalId, Quantity, ResourceId, RoundIndex, Rule, Scope(..), EffectF(..), Transformation, TransformationId)
import Core.Slice (event, orKind, read, select)
import Core.World (Event, WorldState)
import Data.Array (any, concatMap, filter, find, mapMaybe)
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

-- | Every Entity, in id order.
entitiesOf :: WorldState -> Array Entity
entitiesOf = values <<< _.entities

-- | Every Market, in id order.
marketsOf :: WorldState -> Array Market
marketsOf = values <<< _.markets

-- | The Rules in force, in id order.
rulesOf :: WorldState -> Array Rule
rulesOf = values <<< _.rules

-- | Every Transformation, in id order.
transformationsOf :: WorldState -> Array Transformation
transformationsOf = values <<< _.transformations

-- | What an Entity may do; nothing for an Entity the World State does not have.
capabilitiesOf :: WorldState -> EntityId -> Array Capability
capabilitiesOf w e = maybe [] _.capabilities (Map.lookup e w.entities)

-- | The amount of a capacity an Entity holds.
capacityHeld :: WorldState -> EntityId -> CapacityRef -> Q
capacityHeld w e ref = fromMaybe Q.zero (Map.lookup e w.entities <#> \x -> lookupQ ref x.capacities)

-- | A Market's current marginal price, by id.
priceOf :: WorldState -> MarketId -> Maybe Price
priceOf w m = marketPrice <$> Map.lookup m w.markets

-- | A Resource's total across every account and Market reserve.
totalHolding :: WorldState -> ResourceId -> Q
totalHolding w r = Q.sumQ (map (lookupQ r <<< _.accounts) (entitiesOf w) <> map (lookupQ r <<< _.reserves) (marketsOf w))

-- | What a Market holds of a Resource in reserve; zero for an unknown Market.
reserveOf :: WorldState -> MarketId -> ResourceId -> Q
reserveOf w m r = maybe Q.zero (lookupQ r <<< _.reserves) (Map.lookup m w.markets)

-- | An Entity's Aggregate definition, when it is an Aggregate.
aggregateOf :: Entity -> Maybe Aggregate
aggregateOf e = case e.kind of
  Aggregate a -> Just a
  _ -> Nothing

isAggregate :: Entity -> Boolean
isAggregate = isJust <<< aggregateOf

-- | The capacity's owner: the Entity with a positive holding of it, else the first with an account
-- | for it (an owner run down to zero still owns it). INV-08 keeps a positive holder unique.
-- | Capacity Rules and capacity lookups both use this.
owner :: WorldState -> CapacityRef -> Maybe EntityId
owner w ref = _.id <$> (find (\e -> lookupQ ref e.capacities > Q.zero) entities <|> find (\e -> Map.member ref e.capacities) entities)
  where
  entities = entitiesOf w

-- | Effective capacity: the owner's capacity less what Rules reduce it by.
capacityOf :: WorldState -> CapacityRef -> Q
capacityOf w ref =
  let
    held = maybe Q.zero (\o -> capacityHeld w o ref) (owner w ref)
    reduced = Q.sumQ
      ( map
          ( \r -> case r.scope, r.effect of
              OnCapacity c, Reduce units | c == ref -> units
              _, _ -> Q.zero
          )
          (rulesOf w)
      )
  in
    max Q.zero (Q.sub held reduced)

-- | A Transformation's uniform price at a total of `q` units: its cost function's average price, or
-- | zero for a Transformation with no cost function.
conversionPrice :: Transformation -> Q -> Q
conversionPrice tr q = maybe Q.zero (\c -> averagePrice c Q.zero q) tr.cost

-- | The pool a Transformation's fills draw on: the shared capacity if it is capped, else the Transformation
-- | alone. Allocation in one pool are allocated together and priced at one uniform price.
poolOf :: TransformationId -> Transformation -> Either TransformationId CapacityRef
poolOf tid tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Right ref
  _ -> Left tid

-- | The most units a Transformation's pool can fill in total, its ceiling, which escrow prices at in full
-- | and settlement never exceeds (§5.3). The pool is every Transformation drawing on the same capped
-- | capacity, so its fills together draw on that capacity: a total of q units uses at least
-- | q × (smallest `capacityUse` in the pool). The ceiling is therefore the capacity divided by that
-- | smallest use, rounded down, and every member of the pool escrows at it, whatever its own use.
-- | An uncapped Transformation's ceiling is the domain top of its cost function. Nothing means no
-- | ceiling, which only a Transformation with no cost function has, and that one is priced at zero.
poolCeiling :: WorldState -> Transformation -> Maybe Q
poolCeiling w tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Q.divFloor (capacityOf w ref) (smallestUse w ref tr.capacityUse)
  _ -> (\c -> c.domain.hi) <$> tr.cost

-- | The smallest `capacityUse` among the capped Transformations on `ref`, starting from `use` (the
-- | Transformation's own, so the result is never above it).
smallestUse :: WorldState -> CapacityRef -> Q -> Q
smallestUse w ref use = foldl min use (map _.capacityUse (filter drawsOn (transformationsOf w)))
  where
  drawsOn x = x.capacity == Just ref && x.capacityUse > Q.zero

-- | The Trade orders of one Intent list, in order: the one place that selects a Trade.
tradesOf :: forall m v. Array (IntentF m v) -> Array { market :: m, quantity :: v, limitPrice :: Maybe v }
tradesOf = mapMaybe
  ( case _ of
      Trade o -> Just o
      _ -> Nothing
  )

transfersOf :: forall m v. Array (IntentF m v) -> Array { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: v }
transfersOf = mapMaybe
  ( case _ of
      Transfer t -> Just t
      _ -> Nothing
  )

transformsOf :: forall m v. Array (IntentF m v) -> Array { by :: EntityId, transformation :: TransformationId, quantity :: v }
transformsOf = mapMaybe
  ( case _ of
      Transform t -> Just t
      _ -> Nothing
  )

-- | The Proposals a round turned down whole: rejected or lapsed. A Proposal that lost one order is not among them.
droppedProposals :: Array Event -> Array ProposalId
droppedProposals = select (orKind (read @"proposal" (event @"Rejected")) (read @"proposal" (event @"Lapsed")))

-- | The Rules `by` enacted against `target` that took effect at the boundary of `round`, i.e. that
-- | were proposed in the round before: a flows Rule of `by`'s whose counterparty is `target`.
enactedAtBoundary :: RoundIndex -> EntityId -> EntityId -> Array Event -> Array Rule
enactedAtBoundary round by target = concatMap rulesOf' <<< select (event @"Enacted")
  where
  rulesOf' e
    | e.round /= round = []
    | otherwise = case e.rule.scope of
        Flows f | f.governor == by && f.counterparty == target -> [ e.rule ]
        _ -> []
