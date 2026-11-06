-- | Engine phase 4–5 funding (docs/02_type_system.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. Deals a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund. This
-- | module also prices facilities for escrow: a facility's uniform price at its pool ceiling, and
-- | the capacity and pool helpers that `Core.Engine.Settle` allocates against.
module Core.Engine.Fund
  ( Scaled
  , fund
  , facilityPrice
  , poolOf
  , poolCeiling
  , capacityOf
  ) where

import Prelude

import Core.Engine.Screen (Item, owner)
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, holding, lookupQ)
import Core.Types (CapacityRef, EntityId, Intent(..), ResourceId, Rule(..), Transformation, TransformationId)
import Core.World (Event(..), WorldState)
import Data.Array (any, concatMap, elem, filter, foldl, fromFoldable, partition)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)

-- | An Item with the factor its author's funding scales it by (1 for a deal, which is never scaled).
type Scaled = { item :: Item, scale :: Q }

type Claim = { entity :: EntityId, resource :: ResourceId, amount :: Q }

-- | Reject deals a party cannot fund; scale an Entity's own actions pro rata to what it can
-- | fund. Deals are never scaled.
fund :: WorldState -> Array Item -> { items :: Array Scaled, events :: Array Event }
fund w items =
  let
    { yes: deals, no: singles } = partition _.deal items
    total xs = Map.fromFoldableWith Q.add (map (\c -> Tuple (Tuple c.entity c.resource) c.amount) (concatMap (claims w) xs))
    dealDemand = total deals
    short (Tuple e r) amt = amt > holding w e r
    shortKeys = Map.keys (Map.filterWithKey short dealDemand)
    unfunded d = any (\c -> Tuple c.entity c.resource `elem` fromFoldable shortKeys) (claims w d)
    { yes: dropped, no: kept } = partition unfunded deals
    keptDemand = total kept
    singleDemand = total singles
    room key@(Tuple e r) = Q.sub (holding w e r) (lookupQ key keptDemand)
    -- rounded down, so scale × amt never exceeds the room (a half-even round-up could overdraw it)
    ratio key amt = if amt > room key then fromMaybe Q.zero (Q.divFloor (max Q.zero (room key)) amt) else Q.fromInt 1
    entityScale = Map.fromFoldableWith min (map (\(Tuple key amt) -> Tuple (fst key) (ratio key amt)) (Map.toUnfoldable singleDemand :: Array _))
    scaleOf item = fromMaybe (Q.fromInt 1) (Map.lookup item.proposal.author entityScale)
  in
    { items: map (\i -> { item: i, scale: Q.fromInt 1 }) kept <> map (\i -> { item: i, scale: scaleOf i }) singles
    , events: map (\d -> Rejected { round: w.round, proposal: d.proposal.id, reason: "a party cannot fund the deal" }) dropped
    }

-- | What an Item escrows from opening holdings at its full requested size.
claims :: WorldState -> Item -> Array Claim
claims w item = concatMap claim item.proposal.intents <> map (\t -> { entity: t.from, resource: w.numeraire, amount: t.amount }) item.tariffs
  where
  p = item.proposal
  claim = case _ of
    Trade o -> case Map.lookup o.market w.markets of
      Nothing -> []
      Just m
        | o.quantity > Q.zero ->
            let
              unit = fromMaybe (Market.marginal m.cost m.cost.domain.hi) o.limitPrice
            in
              [ { entity: p.author, resource: m.quote, amount: Q.mul unit o.quantity } ]
        | otherwise -> [ { entity: p.author, resource: m.base, amount: Q.neg o.quantity } ]
    Transfer t -> [ { entity: t.from, resource: t.resource, amount: t.quantity } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        [ { entity: t.by, resource: w.numeraire, amount: Q.mul (fullPrice w tr) t.quantity } ]
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.reserves)
    ChangeRule _ -> []

-- | The uniform price of a facility order whose pool fills `q` units.
facilityPrice :: Transformation -> Q -> Q
facilityPrice tr q = maybe Q.zero (\c -> Market.averagePrice c Q.zero q) tr.cost

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

-- | A facility's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = facilityPrice tr (fromMaybe Q.zero (poolCeiling w tr))

-- | The pool a facility's fills draw on: the shared capacity if it is capped, else the facility
-- | alone. Facilities in one pool are allocated together and priced at one uniform price.
poolOf :: TransformationId -> Transformation -> Either TransformationId CapacityRef
poolOf tid tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Right ref
  _ -> Left tid

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
