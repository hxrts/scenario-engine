-- | Engine phase 7 (docs/02_kernel.md §8.1 step 7) and the round driver. `resolveRound` runs
-- | phases 1–7 in order: prepare, screen, fund, settle, circulate, then estimate and commit. Commit
-- | applies the Settlement's ledger to every Actor and Exchange at once and fails the round if any
-- | Account would go negative. That check is an invariant assertion: settlement and escrow should
-- | make it unreachable, and a failure rejects the round rather than recording a corrupt state.
module Laws.Engine.Commit
  ( EngineError(..)
  , resolveRound
  ) where

import Prelude

import Core.Map (entries, sumMaps)

import Laws.Engine.Circulate (circulate)
import Laws.Engine.Fund (fund)
import Laws.Engine.Prepare (prepareChecked)
import Laws.Engine.Screen (duplicatedIds, screen, withinMaximums)
import Laws.Engine.Settle (Settlement, settle)
import Core.Kernel.Consume (ConsumeError, spawned)
import Core.Kernel.Post (Account(..), Ledger, ledgerByAccount)
import Core.Kernel.Ration (Claim)
import Core.Kernel.Resolve (Fate(..))
import Core.Kernel.Types (Account(..), Amount(..), Effect(..), Party(..)) as K
import Data.Newtype (unwrap)
import Laws.Engine.Compile (runTransaction)
import Core.Num (Q)
import Core.Num as Q
import Laws.CandidateTransaction (submissionOrder)
import Laws.Represent as Represent
import Laws.Types (BehaviorDomain, Actor, ActorId, Exchange, ExchangeId, Ratio, ResourceId, RoundIndex, CandidateTransaction, SignedCandidateTransaction, IntentF(..), CandidateTransactionId)
import Laws.World (addEvents, rejection, Event(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), RoundResult, WorldState, PendingTransaction(..))
import Data.Array (concatMap, elem, filter, find, findMap, sortWith, any, foldl, mapWithIndex, null)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data EngineError
  = NegativeBalance ActorId ResourceId -- an invariant failure: settlement should make this unreachable
  | NegativeReserve ExchangeId ResourceId -- likewise, for Exchange reserves
  | NegativeFlow ActorId ActorId -- a fixed flow with a negative amount: a World State the premise codec would refuse
  | InvalidSuccessor String -- a due transaction spawned what its parent may not

derive instance Eq EngineError
instance Show EngineError where
  show (NegativeBalance e r) = "negative balance: " <> show e <> " " <> show r
  show (NegativeReserve m r) = "negative reserve: " <> show m <> " " <> show r
  show (NegativeFlow from to) = "negative fixed flow: " <> show from <> " to " <> show to
  show (InvalidSuccessor why) = "invalid successor: " <> why

-- | Resolve one round. The World State is prepared first, so the caller passes it in
-- | either prepared or not; the result is the same.
resolveRound :: WorldState -> Array SignedCandidateTransaction -> Either EngineError RoundResult
resolveRound w0 inputs0 = case findMap negativeFixed w0.recurring of
  Just (Tuple f t) -> Left (NegativeFlow f t)
  Nothing -> resolveValid w0 inputs0

-- | A fixed Actor-to-Actor recurring flow with a negative amount, naming its two Actors.
negativeFixed :: RecurringFlow -> Maybe (Tuple ActorId ActorId)
negativeFixed r = case r.source, r.destination, r.amount of
  FromActor f, ToActor t, FixedAmount q | q < Q.zero -> Just (Tuple f t)
  _, _, _ -> Nothing

-- | The event kinds Prepare writes, in the round being resolved.
prepared :: RoundIndex -> Event -> Boolean
prepared round = case _ of
  Settled d -> d.delivered && d.round == round
  Enacted e -> e.round == round
  Repealed e -> e.round == round
  _ -> false

resolveValid :: WorldState -> Array SignedCandidateTransaction -> Either EngineError RoundResult
resolveValid w0 inputs0 = case prepareChecked w0 of
  Left e -> Left (InvalidSuccessor (show e))
  Right prepared' -> resolvePrepared prepared' inputs0

resolvePrepared :: WorldState -> Array SignedCandidateTransaction -> Either EngineError RoundResult
resolvePrepared w inputs0 =
  let
    inputs = sortWith submissionOrder inputs0
    screened = map (screen w (duplicatedIds inputs)) inputs
    bounded = withinMaximums w (concatMap (either (const []) pure) screened)
    items = bounded.items
    overMaximum = map _.candidateTransaction bounded.rejected
    screenEvents = concatMap (either identity (\i -> if i.candidateTransaction.id `elem` overMaximum then [] else i.outcome)) screened
      <> map (\r -> rejection w.round r.candidateTransaction r.reason) bounded.rejected
    -- obligations due in a settlement phase claim holdings beside the round's orders, by their tier
    dueNow = mapWithIndex Tuple (filter isDue w.pending)
    isDue p = (unwrap p).due <= w.round
    funded = fund w (concatMap dueClaims dueNow) items
    consumedDue = settleDue w funded.droppedDue funded.dueScales dueNow
    -- estimates read the validated proposed quantities, before funding scales them (§7.4)
    estimates = Represent.estimate w (concatMap (observationsOf w <<< _.candidateTransaction) items)
    settled = circulate w (settle w { ledger: consumedDue.ledger, events: consumedDue.events, spawned: consumedDue.spawned } funded.items)
  in
    if not (null consumedDue.errors) then Left (InvalidSuccessor (show consumedDue.errors))
    else commit (w { pending = filter (not <<< isDue) w.pending }) inputs (screenEvents <> funded.events <> estimates.events) estimates.factors settled

-- | What a screened candidate Transaction says its author set out to buy: the quantities of its Trades and routed Trades.
observationsOf :: WorldState -> CandidateTransaction -> Array Represent.Observation
observationsOf w p = concatMap trade p.intents
  where
  trade = case _ of
    Trade o | Just m <- Map.lookup o.exchange w.exchanges -> [ { playableActor: p.author, resource: m.base, quantity: o.quantity } ]
    Route r -> [ { playableActor: p.author, resource: r.pair.base, quantity: r.quantity } ]
    _ -> []

-- | What a due transaction asks to set aside: each whole-quantity settled Flow out of a named holder's account.
dueClaims :: Tuple Int PendingTransaction -> Array (Claim (Either Int CandidateTransactionId))
dueClaims (Tuple i (PendingTransaction p)) = concatMap claim p.transaction.obligations
  where
  claim o = case o.effect of
    K.Move { from: K.HeldBy (K.Named e), resource, owed: K.Exactly amount } ->
      [ { key: Left i, tier: p.transaction.ration.tier, whole: p.transaction.ration.whole, party: e, resource, amount } ]
    _ -> []

-- | Settle what fell due in a settlement phase, before the round's orders. A transaction rationing dropped
-- | is consumed as lapsed; one rationing scaled posts at its scale.
settleDue :: WorldState -> Array Int -> Map (Tuple Int ActorId) Q -> Array (Tuple Int PendingTransaction) -> { ledger :: Ledger, events :: Array Event, spawned :: Array PendingTransaction, errors :: Array ConsumeError }
settleDue w dropped scales = foldl one { ledger: Map.empty, events: [], spawned: [], errors: [] } <<< sortWith (\(Tuple _ (PendingTransaction p)) -> Tuple p.transaction.phase p.transaction.ration.tier)
  where
  templates = map unwrap w.templates
  one acc (Tuple i (PendingTransaction p))
    | i `elem` dropped = record acc acc.ledger [] (mapWithIndex (\n _ -> { transaction: 0, obligation: n, fate: Expired, flow: Nothing, change: Nothing, reason: Nothing }) p.transaction.obligations) p.transaction
    | otherwise =
        let
          scaled = p.transaction { obligations = map (scaleBy p.transaction.ration.tier) p.transaction.obligations }
          posted = runTransaction w acc.ledger scaled
        in
          record acc posted.ledger posted.flows posted.consumed scaled
  scaleBy tier o = case o.effect of
    K.Move l@{ from: K.HeldBy (K.Named e), owed: K.Exactly amount } | Just x <- Map.lookup (Tuple tier e) scales -> o { effect = K.Move l { owed = K.Exactly (Q.mul x amount) } }
    _ -> o
  record acc ledger flows consumed c =
    let
      next = spawned w.round templates [ c ] consumed
      events = case c.cause of
        Just cause | any (\f -> f.moved /= Q.zero) flows -> [ Settled { round: w.round, legs: flows, rate: Nothing, cause, delivered: false } ]
        _ -> []
    in
      { ledger, events: acc.events <> events, spawned: acc.spawned <> either (const []) (map PendingTransaction) next, errors: acc.errors <> either pure (const []) next }

commit :: WorldState -> Array SignedCandidateTransaction -> Array Event -> Map ActorId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  let byAccount = ledgerByAccount st.ledger
  actors <- traverse (applyActor byAccount) w.actors
  exchangeList <- traverse (applyExchange st byAccount) (entries w.exchanges)
  let
    events = screenEvents <> st.events
    -- Prepare's events are already in the world's history; the round's record carries them too (docs/04 §11.1)
    world = addEvents events w
      { round = w.round + 1
      , actors = actors
      , exchanges = Map.fromFoldable exchangeList
      , pending = w.pending <> st.spawned
      , factors = factors
      }
  pure { world, record: { round: w.round, inputs, events: filter (prepared w.round) w.events <> events } }

-- | An Actor with the Settlement's net change to its holdings applied, if none goes negative.
applyActor :: Map Account (Map ResourceId Q) -> Actor -> Either EngineError Actor
applyActor byAccount e = (\accounts -> e { accounts = accounts })
  <$> checkNonNegative (Ent e.id) (applyHoldings byAccount (Ent e.id) e.accounts)

-- | An Exchange with the Settlement's net change to its reserves and its new position applied, if no
-- | reserve goes negative.
applyExchange :: Settlement -> Map Account (Map ResourceId Q) -> Tuple ExchangeId Exchange -> Either EngineError (Tuple ExchangeId Exchange)
applyExchange st byAccount (Tuple id m) = (\reserves -> Tuple id m { reserves = reserves, position = fromMaybe m.position (Map.lookup id st.positions) })
  <$> checkNonNegative (Mkt id) (applyHoldings byAccount (Mkt id) m.reserves)

-- | The holdings of one Account after the Settlement's ledger entries for it.
applyHoldings :: Map Account (Map ResourceId Q) -> Account -> Map ResourceId Q -> Map ResourceId Q
applyHoldings byAccount acct hs = sumMaps hs (fromMaybe Map.empty (Map.lookup acct byAccount))

-- | Settlement caps every payment at what an account holds and escrow funds the rest, so no
-- | account should go negative: the first negative holding fails the round.
checkNonNegative :: Account -> Map ResourceId Q -> Either EngineError (Map ResourceId Q)
checkNonNegative acct hs = case find (\(Tuple _ q) -> q < Q.zero) (entries hs) of
  Just (Tuple r _) -> Left
    ( case acct of
        Ent e -> NegativeBalance e r
        Mkt m -> NegativeReserve m r
    )
  Nothing -> Right hs
