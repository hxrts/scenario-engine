-- | Engine phase 4–5 funding (docs/02_type_system.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. Deals a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund. Facility
-- | prices and pools are in Core.Query, which Fund and Settle share.
module Core.Engine.Fund
  ( Scaled
  , fund
  ) where

import Prelude

import Core.Engine.Screen (Item)
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (facilityPrice, holding, lookupQ, poolCeiling)
import Core.Types (EntityId, IntentF(..), ResourceId, Transformation)
import Core.World (Event, RejectReason(..), WorldState, rejection)
import Data.Array (any, concatMap, elem, fromFoldable, partition)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)

-- | An Item with the factor its author's funding scales it by (1 for a deal, which is never scaled).
type Scaled = { item :: Item, scale :: Q }

type Claim = { entity :: EntityId, resource :: ResourceId, amount :: Q }

-- | An Item with the scale its funding gives it.
scaled :: Q -> Item -> Scaled
scaled scale item = { item, scale }

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
    { items: map (scaled (Q.fromInt 1)) kept <> map (\i -> scaled (scaleOf i) i) singles
    , events: map (\d -> rejection w.round d.proposal.id PartyCannotFund) dropped
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
    -- a routed order escrows at its limit (required, checked at screening): limit × quantity on
    -- the pair's quote for a buy, its quantity of the pair's base for a sell (docs/02 §8.2)
    Route r
      | r.quantity > Q.zero -> [ { entity: p.author, resource: r.pair.quote, amount: Q.mul (fromMaybe Q.zero r.limitPrice) r.quantity } ]
      | otherwise -> [ { entity: p.author, resource: r.pair.base, amount: Q.neg r.quantity } ]
    Transfer t -> [ { entity: t.from, resource: t.resource, amount: t.quantity } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        [ { entity: t.by, resource: w.numeraire, amount: Q.mul (fullPrice w tr) t.quantity } ]
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.reserves)
    ChangeRule _ -> []

-- | A facility's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = facilityPrice tr (fromMaybe Q.zero (poolCeiling w tr))

