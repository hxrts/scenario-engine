-- | Engine phase 4–5 funding (docs/02_kernel.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. joint candidate Transactions a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund. Transformation
-- | prices and pools are in Laws.Query, which Fund and Settle share.
module Laws.Engine.Fund
  ( Scaled
  , scaleOf
  , fund
  ) where

import Prelude

import Core.Kernel.Ration (Claim, ration)
import Core.Map (entries)
import Laws.Engine.Screen (Item)
import Core.Exchange as Exchange
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (transformationPrice, holding, poolCeiling)
import Laws.Types (ActorId, IntentF(..), CandidateTransactionId, ResourceId, Transformation, executorDraws)
import Laws.World (Event, RejectReason(..), WorldState, rejection)
import Data.Array (concatMap, elem, partition, mapMaybe)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

-- | An Item with the factor each Actor's funding scales what it executes by. A joint candidate Transaction, which is
-- | never scaled, and an Actor with room to spare have no entry.
type Scaled = { item :: Item, scales :: Map ActorId Q }

-- | The factor an Item's Intents executed by `e` are scaled by.
scaleOf :: Scaled -> ActorId -> Q
scaleOf s e = fromMaybe Q.one (Map.lookup e s.scales)

type Escrow = { actor :: ActorId, resource :: ResourceId, amount :: Q }

-- | Reject joint candidate Transactions a party cannot fund: every joint candidate Transaction of a party whose total joint demand exceeds its
-- | holdings goes, even if one alone could be funded (docs/02 §8.2). Scale other Items pro rata to
-- | what the Actors they draw on can fund. joint candidate Transactions are never scaled.
fund :: forall due. Ord due => WorldState -> Array (Claim (Either due CandidateTransactionId)) -> Array Item -> { items :: Array Scaled, events :: Array Event, droppedDue :: Array due, dueScales :: Map (Tuple Int ActorId) Q }
fund w dueClaims items =
  let
    { yes: joints, no: singles } = partition _.joint items
    -- joint candidate Transactions are the first tier and whole; every other order is the next tier and scales
    claimsOf item = map (\c -> { key: Right item.candidateTransaction.id, tier: if item.joint then 0 else 1, whole: item.joint, party: c.actor, resource: c.resource, amount: c.amount }) (claims w item)
    rationed = ration (holding w) (dueClaims <> concatMap claimsOf (joints <> singles))
    orderScales = Map.fromFoldable (mapMaybe (\(Tuple (Tuple t e) x) -> if t == 1 then Just (Tuple e x) else Nothing) (entries rationed.scales))
    { yes: dropped, no: kept } = partition (\j -> Right j.candidateTransaction.id `elem` rationed.dropped) joints
  in
    -- keyed by the Actor whose claim it is, which for a voted candidate Transaction may not be its author
    { items: map (\item -> { item, scales: Map.empty }) kept <> map (\item -> { item, scales: orderScales }) singles
    , events: map (\d -> rejection w.round d.candidateTransaction.id PartyCannotFund) dropped
    , droppedDue: mapMaybe (either Just (const Nothing)) rationed.dropped
    , dueScales: rationed.scales
    }

-- | What an Item escrows from opening holdings at its full requested size.
claims :: WorldState -> Item -> Array Escrow
claims w item = concatMap claim item.candidateTransaction.intents <> map (\t -> { actor: t.from, resource: t.in, amount: t.amount }) item.levies
  where
  p = item.candidateTransaction
  claim = case _ of
    Trade o -> case Map.lookup o.exchange w.exchanges of
      Nothing -> []
      Just m
        | o.quantity > Q.zero ->
            let
              unit = fromMaybe (Exchange.marginal m.cost m.cost.domain.hi) o.limitPrice
            in
              [ { actor: p.author, resource: m.quote, amount: Q.mul unit o.quantity } ]
        | otherwise -> [ { actor: p.author, resource: m.base, amount: Q.neg o.quantity } ]
    -- a routed order escrows at its limit (required, checked at screening): limit × quantity on
    -- the pair's quote for a buy, its quantity of the pair's base for a sell (docs/02 §8.2)
    Route r
      | r.quantity > Q.zero -> [ { actor: p.author, resource: r.pair.quote, amount: Q.mul (fromMaybe Q.zero r.limitPrice) r.quantity } ]
      | otherwise -> [ { actor: p.author, resource: r.pair.base, amount: Q.neg r.quantity } ]
    Transfer t -> [ { actor: t.from, resource: t.resource, amount: t.quantity } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        maybe [] (\pay -> [ { actor: t.by, resource: pay.resource, amount: Q.mul (fullPrice w tr) t.quantity } ]) tr.payment
          <> map (\(Tuple r q) -> { actor: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
          <> map (\d -> { actor: t.by, resource: d.resource, amount: Q.mul d.perUnit t.quantity }) (executorDraws tr)
    ChangeRule _ -> []

-- | A Transformation's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = transformationPrice tr (fromMaybe Q.zero (poolCeiling w tr))

