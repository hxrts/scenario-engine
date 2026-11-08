-- | Engine phase 4–5 funding (docs/02_type_system.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. Joint Proposals a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund. Transformation
-- | prices and pools are in Core.Query, which Fund and Settle share.
module Core.Engine.Fund
  ( Scaled
  , scaleOf
  , fund
  ) where

import Prelude

import Core.Map (entries, sumInto)

import Core.Engine.Screen (Item)
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (conversionPrice, holding, lookupQ, poolCeiling)
import Core.Types (EntityId, IntentF(..), ResourceId, Transformation)
import Core.World (Event, RejectReason(..), WorldState, rejection)
import Data.Array (any, foldl, concatMap, elem, fromFoldable, partition)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)

-- | An Item with the factor each Entity's funding scales what it executes by. A joint Proposal, which is
-- | never scaled, and an Entity with room to spare have no entry.
type Scaled = { item :: Item, scales :: Map EntityId Q }

-- | The factor an Item's Intents executed by `e` are scaled by.
scaleOf :: Scaled -> EntityId -> Q
scaleOf s e = fromMaybe Q.one (Map.lookup e s.scales)

type Claim = { entity :: EntityId, resource :: ResourceId, amount :: Q }

-- | Reject joint Proposals a party cannot fund: every joint Proposal of a party whose total joint demand exceeds its
-- | holdings goes, even if one alone could be funded (docs/02 §8.2). Scale other Items pro rata to
-- | what the Entities they draw on can fund. Joint Proposals are never scaled.
fund :: WorldState -> Array Item -> { items :: Array Scaled, events :: Array Event }
fund w items =
  let
    { yes: joints, no: singles } = partition _.joint items
    total xs = foldl (\m c -> sumInto (Tuple c.entity c.resource) c.amount m) Map.empty (concatMap (claims w) xs)
    jointDemand = total joints
    short (Tuple e r) amt = amt > holding w e r
    shortKeys = Map.keys (Map.filterWithKey short jointDemand)
    unfunded d = any (\c -> Tuple c.entity c.resource `elem` fromFoldable shortKeys) (claims w d)
    { yes: dropped, no: kept } = partition unfunded joints
    keptDemand = total kept
    singleDemand = total singles
    room key@(Tuple e r) = Q.sub (holding w e r) (lookupQ key keptDemand)
    -- rounded down, so scale × amt never exceeds the room (a half-even round-up could overdraw it)
    ratio key amt = if amt > room key then fromMaybe Q.zero (Q.divFloor (max Q.zero (room key)) amt) else Q.one
    entityScale = Map.fromFoldableWith min (map (\(Tuple key amt) -> Tuple (fst key) (ratio key amt)) (entries singleDemand))
  in
    -- keyed by the Entity whose claim it is, which for a procedure Proposal may not be its author
    { items: map (\item -> { item, scales: Map.empty }) kept <> map (\item -> { item, scales: entityScale }) singles
    , events: map (\d -> rejection w.round d.proposal.id PartyCannotFund) dropped
    }

-- | What an Item escrows from opening holdings at its full requested size.
claims :: WorldState -> Item -> Array Claim
claims w item = concatMap claim item.proposal.intents <> map (\t -> { entity: t.from, resource: t.in, amount: t.amount }) item.levies
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
        maybe [] (\pay -> [ { entity: t.by, resource: pay.resource, amount: Q.mul (fullPrice w tr) t.quantity } ]) tr.payment
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.escrowPerUnit)
    ChangeRule _ -> []

-- | A Transformation's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = conversionPrice tr (fromMaybe Q.zero (poolCeiling w tr))

