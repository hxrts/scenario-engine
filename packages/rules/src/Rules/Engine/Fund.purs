-- | Engine phase 4–5 funding (docs/02_type_system.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. Joint Proposals a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund. Transformation
-- | prices and pools are in Rules.Query, which Fund and Settle share.
module Rules.Engine.Fund
  ( Scaled
  , scaleOf
  , fund
  ) where

import Prelude

import Core.Kernel.Ration (ration)
import Rules.Engine.Screen (Item)
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Rules.Query (conversionPrice, holding, poolCeiling)
import Rules.Types (EntityId, IntentF(..), ResourceId, Transformation, executorDraws)
import Rules.World (Event, RejectReason(..), WorldState, rejection)
import Data.Array (concatMap, elem, partition)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

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
    -- joint Proposals are the first tier and whole; every other order is the next tier and scales
    claimsOf item = map (\c -> { key: item.proposal.id, tier: if item.joint then 0 else 1, whole: item.joint, party: c.entity, resource: c.resource, amount: c.amount }) (claims w item)
    rationed = ration (holding w) (concatMap claimsOf (joints <> singles))
    { yes: dropped, no: kept } = partition (\j -> j.proposal.id `elem` rationed.dropped) joints
  in
    -- keyed by the Entity whose claim it is, which for a procedure Proposal may not be its author
    { items: map (\item -> { item, scales: Map.empty }) kept <> map (\item -> { item, scales: rationed.scales }) singles
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
          <> map (\d -> { entity: t.by, resource: d.resource, amount: Q.mul d.perUnit t.quantity }) (executorDraws tr)
    ChangeRule _ -> []

-- | A Transformation's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = conversionPrice tr (fromMaybe Q.zero (poolCeiling w tr))

