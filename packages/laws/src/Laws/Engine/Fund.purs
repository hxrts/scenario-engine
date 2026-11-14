-- | Engine phase 4–5 funding (docs/02_kernel.md §3.2, §8.2): every screened Item escrows its
-- | claims from opening holdings at full requested size. joint candidate Transactions a party cannot fund are rejected
-- | as a unit, and single-author Items are scaled pro rata to what their author can fund: its Transformations, with
-- | the wages their action laws charge, first, and its other orders on what they leave. What the standing laws
-- | charge each round, such as upkeep, is set aside before either. Transformation
-- | prices and pools are in Laws.Query, which Fund and Settle share.
module Laws.Engine.Fund
  ( Scaled
  , scaleOf
  , transformScaleOf
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
import Core.Kernel.Types (Amount(..), Effect(..), Obligation, Party(..), Subject(..), Trigger(..), Operation(..), Account(..)) as K
import Laws.Engine.Compile (actionTransactions)
import Laws.Types (ActorId, Capability, IntentF(..), CandidateTransactionId, ResourceId, Rule, Transformation, executorDraws, idText)
import Laws.World (AuthoredLaw(..), Event, LegQuery, RejectReason(..), WorldState, rejection)
import Data.Array (concatMap, elem, partition, mapMaybe)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

-- | An Item with the factor each Actor's funding scales what it executes by: its Transformations, and its other
-- | orders. A joint candidate Transaction, which is never scaled, and an Actor with room to spare have no entry.
type Scaled = { item :: Item, scales :: Map ActorId Q, transforms :: Map ActorId Q }

-- | The factor an Item's Trades, Transfers and levies executed by `e` are scaled by.
scaleOf :: Scaled -> ActorId -> Q
scaleOf s e = fromMaybe Q.one (Map.lookup e s.scales)

-- | The factor an Item's Transformations run by `e` are scaled by.
transformScaleOf :: Scaled -> ActorId -> Q
transformScaleOf s e = fromMaybe Q.one (Map.lookup e s.transforms)

type Escrow = { actor :: ActorId, resource :: ResourceId, amount :: Q, producing :: Boolean }

-- | Reject joint candidate Transactions a party cannot fund: every joint candidate Transaction of a party whose total joint demand exceeds its
-- | holdings goes, even if one alone could be funded (docs/02 §8.2). Scale other Items pro rata to
-- | what the Actors they draw on can fund. joint candidate Transactions are never scaled.
fund :: forall due. Ord due => WorldState -> Array (Claim (Either due CandidateTransactionId)) -> Array Item -> { items :: Array Scaled, events :: Array Event, droppedDue :: Array due, dueScales :: Map (Tuple Int ActorId) Q, free :: ActorId -> ResourceId -> Q }
fund w dueClaims items =
  let
    { yes: joints, no: singles } = partition _.joint items
    -- joint candidate Transactions are the first tier and whole; a single order's Transformations and their wages
    -- the next, and its other orders the last, both scaling
    claimsOf item = map (\c -> { key: Right item.candidateTransaction.id, tier: if item.joint then 0 else if c.producing then 1 else 2, whole: item.joint, party: c.actor, resource: c.resource, amount: c.amount }) (claims w item)
    available e r = max Q.zero (Q.sub (holding w e r) (standing w e r))
    allClaims = dueClaims <> concatMap claimsOf (joints <> singles)
    rationed = ration available allClaims
    -- what each claim holds back once funded: a whole claim all of it unless dropped, a scaled one its share
    held c = if c.whole then (if c.key `elem` rationed.dropped then Q.zero else c.amount) else Q.mul (fromMaybe Q.one (Map.lookup (Tuple c.tier c.party) rationed.scales)) c.amount
    escrowed = Map.fromFoldableWith Q.add (map (\c -> Tuple (Tuple c.party c.resource) (held c)) allClaims)
    scalesAt n = Map.fromFoldable (mapMaybe (\(Tuple (Tuple t e) x) -> if t == n then Just (Tuple e x) else Nothing) (entries rationed.scales))
    { yes: dropped, no: kept } = partition (\j -> Right j.candidateTransaction.id `elem` rationed.dropped) joints
  in
    -- keyed by the Actor whose claim it is, which for a voted candidate Transaction may not be its author
    { items: map (\item -> { item, scales: Map.empty, transforms: Map.empty }) kept <> map (\item -> { item, scales: scalesAt 2, transforms: scalesAt 1 }) singles
    , events: map (\d -> rejection w.round d.candidateTransaction.id PartyCannotFund) dropped
    , droppedDue: mapMaybe (either Just (const Nothing)) rationed.dropped
    , dueScales: rationed.scales
    -- what no escrow holds of each holding: a levy charged on a cleared pair is paid from it (docs/03 §5.5)
    , free: \e r -> max Q.zero (Q.sub (available e r) (fromMaybe Q.zero (Map.lookup (Tuple e r) escrowed)))
    }

-- | What an Item escrows from opening holdings at its full requested size.
claims :: WorldState -> Item -> Array Escrow
claims w item = concatMap claim item.candidateTransaction.intents <> map (\t -> { actor: t.from, resource: t.in, amount: t.amount, producing: false }) item.levies
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
              [ { actor: p.author, resource: m.quote, amount: Q.mul unit o.quantity, producing: false } ]
        | otherwise -> [ { actor: p.author, resource: m.base, amount: Q.neg o.quantity, producing: false } ]
    -- a routed order escrows at its limit (required, checked at screening): limit × quantity on
    -- the pair's quote for a buy, its quantity of the pair's base for a sell (docs/02 §8.2)
    Route r
      | r.quantity > Q.zero -> [ { actor: p.author, resource: r.pair.quote, amount: Q.mul (fromMaybe Q.zero r.limitPrice) r.quantity, producing: false } ]
      | otherwise -> [ { actor: p.author, resource: r.pair.base, amount: Q.neg r.quantity, producing: false } ]
    Transfer t -> [ { actor: t.from, resource: t.resource, amount: t.quantity, producing: false } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        map (_ { producing = true })
          ( maybe [] (\pay -> [ { actor: t.by, resource: pay.resource, amount: Q.mul (fullPrice w tr) t.quantity, producing: true } ]) tr.payment
              <> map (\(Tuple r q) -> { actor: t.by, resource: r, amount: Q.mul q t.quantity, producing: true }) (Map.toUnfoldable tr.inputs)
              <> map (\d -> { actor: t.by, resource: d.resource, amount: Q.mul d.perUnit t.quantity, producing: true }) (executorDraws tr)
              -- the wages and other levies its action laws charge on what it runs
              <> concatMap (\c -> mapMaybe (charge t.by) c.obligations) (actionTransactions w t.by (idText t.transformation) t.quantity)
          )
    ChangeRule _ -> []
    Enter _ -> []

-- | What one law's obligation takes from an Actor, when it is a fixed amount from what it holds.
charge :: ActorId -> K.Obligation Capability Rule LegQuery RejectReason -> Maybe Escrow
charge e o = case o.effect of
  K.Move { from: K.HeldBy (K.Named x), resource, owed } | x == e -> (\amount -> { actor: e, resource, amount, producing: true }) <$> fixed owed
  _ -> Nothing
  where
  fixed = case _ of
    K.Exactly q -> Just q
    K.Times t -> Q.mul t.ratio <$> fixed t.amount
    _ -> Nothing

-- | What the standing laws charged every round take of a Resource from an Actor, on what it holds at the round's
-- | start: upkeep on its capacity. It is set aside before any order is funded.
standing :: WorldState -> ActorId -> ResourceId -> Q
standing w e r = Q.sumQ (mapMaybe one w.laws)
  where
  one (AuthoredLaw law) = case law.trigger, law.subject, law.operation of
    K.EachRound, K.OfStock s, K.Emit l | maybe true (elem e) s.holders && l.resource == r -> amount l.owed
    _, _, _ -> Nothing
  amount = case _ of
    K.Exactly q -> Just q
    K.Times t -> Q.mul t.ratio <$> amount t.amount
    K.Stock x -> Just (holding w e x.resource)
    _ -> Nothing

-- | A Transformation's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = transformationPrice tr (fromMaybe Q.zero (poolCeiling w tr))

