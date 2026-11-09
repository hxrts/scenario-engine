-- | Engine phase 7 (docs/02_type_system.md §8.1 step 7) and the round driver. `resolveRound` runs
-- | phases 1–7 in order: prepare, screen, fund, settle, circulate, then estimate and commit. Commit
-- | applies the Settlement's ledger to every Entity and Market at once and fails the round if any
-- | Account would go negative. That check is an invariant assertion: settlement and escrow should
-- | make it unreachable, and a failure rejects the round rather than recording a corrupt state.
module Rules.Engine.Commit
  ( EngineError(..)
  , resolveRound
  ) where

import Prelude

import Core.Map (entries, sumMaps)

import Rules.Engine.Circulate (circulate)
import Rules.Engine.Fund (fund)
import Rules.Engine.Prepare (prepareChecked)
import Rules.Engine.Screen (duplicatedIds, screen, withinMaximums)
import Rules.Engine.Settle (Settlement, settle)
import Core.Kernel.Consume (ConsumeError, spawned)
import Core.Kernel.Post (Account(..), Ledger, ledgerByAccount)
import Core.Kernel.Ration (Claim)
import Core.Kernel.Resolve (Fate(..))
import Core.Kernel.Types (Account(..), Amount(..), Effect(..), Party(..)) as K
import Data.Newtype (unwrap)
import Rules.Engine.Compile (runCoupling)
import Core.Num (Q)
import Core.Num as Q
import Rules.Proposal (submissionOrder)
import Rules.Represent as Represent
import Rules.Types (BehaviorDomain, Entity, EntityId, Market, MarketId, Ratio, ResourceId, RoundIndex, Proposal, SignedProposal, IntentF(..), ProposalId)
import Rules.World (addEvents, rejection, Event(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), RoundResult, WorldState, PendingCoupling(..))
import Data.Array (concatMap, elem, filter, find, findMap, sortWith, any, foldl, mapWithIndex, null)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data EngineError
  = NegativeBalance EntityId ResourceId -- an invariant failure: settlement should make this unreachable
  | NegativeReserve MarketId ResourceId -- likewise, for Market reserves
  | NegativeFlow EntityId EntityId -- a fixed flow with a negative amount: a World State the setting codec would refuse
  | InvalidSuccessor String -- a due coupling spawned what its parent may not

derive instance Eq EngineError
instance Show EngineError where
  show (NegativeBalance e r) = "negative balance: " <> show e <> " " <> show r
  show (NegativeReserve m r) = "negative reserve: " <> show m <> " " <> show r
  show (NegativeFlow from to) = "negative fixed flow: " <> show from <> " to " <> show to
  show (InvalidSuccessor why) = "invalid successor: " <> why

-- | Resolve one round. The World State is prepared first, so the caller passes it in
-- | either prepared or not; the result is the same.
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolveRound w0 inputs0 = case findMap negativeFixed w0.recurring of
  Just (Tuple f t) -> Left (NegativeFlow f t)
  Nothing -> resolveValid w0 inputs0

-- | A fixed Entity-to-Entity recurring flow with a negative amount, naming its two Entities.
negativeFixed :: RecurringFlow -> Maybe (Tuple EntityId EntityId)
negativeFixed r = case r.source, r.destination, r.amount of
  FromEntity f, ToEntity t, FixedAmount q | q < Q.zero -> Just (Tuple f t)
  _, _, _ -> Nothing

-- | The event kinds Prepare writes, in the round being resolved.
prepared :: RoundIndex -> Event -> Boolean
prepared round = case _ of
  Coupled d -> d.delivered && d.round == round
  Enacted e -> e.round == round
  Repealed e -> e.round == round
  _ -> false

resolveValid :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolveValid w0 inputs0 = case prepareChecked w0 of
  Left e -> Left (InvalidSuccessor (show e))
  Right prepared' -> resolvePrepared prepared' inputs0

resolvePrepared :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolvePrepared w inputs0 =
  let
    inputs = sortWith submissionOrder inputs0
    screened = map (screen w (duplicatedIds inputs)) inputs
    bounded = withinMaximums w (concatMap (either (const []) pure) screened)
    items = bounded.items
    overMaximum = map _.proposal bounded.rejected
    screenEvents = concatMap (either identity (\i -> if i.proposal.id `elem` overMaximum then [] else i.outcome)) screened
      <> map (\r -> rejection w.round r.proposal r.reason) bounded.rejected
    -- obligations due in a settlement phase claim holdings beside the round's orders, by their tier
    dueNow = mapWithIndex Tuple (filter isDue w.pending)
    isDue p = (unwrap p).due <= w.round
    funded = fund w (concatMap dueClaims dueNow) items
    consumedDue = settleDue w funded.droppedDue funded.dueScales dueNow
    -- estimates read the validated proposed quantities, before funding scales them (§7.4)
    estimates = Represent.estimate w (concatMap (observationsOf w <<< _.proposal) items)
    settled = circulate w (settle w { ledger: consumedDue.ledger, events: consumedDue.events, spawned: consumedDue.spawned } funded.items)
  in
    if not (null consumedDue.errors) then Left (InvalidSuccessor (show consumedDue.errors))
    else commit (w { pending = filter (not <<< isDue) w.pending }) inputs (screenEvents <> funded.events <> estimates.events) estimates.factors settled

-- | What a screened Proposal says its author set out to buy: the quantities of its Trades and routed Trades.
observationsOf :: WorldState -> Proposal -> Array Represent.Observation
observationsOf w p = concatMap trade p.intents
  where
  trade = case _ of
    Trade o | Just m <- Map.lookup o.market w.markets -> [ { actor: p.author, resource: m.base, quantity: o.quantity } ]
    Route r -> [ { actor: p.author, resource: r.pair.base, quantity: r.quantity } ]
    _ -> []

-- | What a due coupling asks to set aside: each whole-quantity leg out of a named holder's account.
dueClaims :: Tuple Int PendingCoupling -> Array (Claim (Either Int ProposalId))
dueClaims (Tuple i (PendingCoupling p)) = concatMap claim p.coupling.obligations
  where
  claim o = case o.effect of
    K.Move { from: K.HeldBy (K.Named e), resource, owed: K.Exactly amount } ->
      [ { key: Left i, tier: p.coupling.ration.tier, whole: p.coupling.ration.whole, party: e, resource, amount } ]
    _ -> []

-- | Settle what fell due in a settlement phase, before the round's orders. A coupling rationing dropped
-- | is consumed as lapsed; one rationing scaled posts at its scale.
settleDue :: WorldState -> Array Int -> Map (Tuple Int EntityId) Q -> Array (Tuple Int PendingCoupling) -> { ledger :: Ledger, events :: Array Event, spawned :: Array PendingCoupling, errors :: Array ConsumeError }
settleDue w dropped scales = foldl one { ledger: Map.empty, events: [], spawned: [], errors: [] } <<< sortWith (\(Tuple _ (PendingCoupling p)) -> Tuple p.coupling.phase p.coupling.ration.tier)
  where
  templates = map unwrap w.templates
  one acc (Tuple i (PendingCoupling p))
    | i `elem` dropped = record acc acc.ledger [] (mapWithIndex (\n _ -> { coupling: 0, obligation: n, fate: Expired, flow: Nothing, change: Nothing, reason: Nothing }) p.coupling.obligations) p.coupling
    | otherwise =
        let
          scaled = p.coupling { obligations = map (scaleBy p.coupling.ration.tier) p.coupling.obligations }
          posted = runCoupling w acc.ledger scaled
        in
          record acc posted.ledger posted.flows posted.consumed scaled
  scaleBy tier o = case o.effect of
    K.Move l@{ from: K.HeldBy (K.Named e), owed: K.Exactly amount } | Just x <- Map.lookup (Tuple tier e) scales -> o { effect = K.Move l { owed = K.Exactly (Q.mul x amount) } }
    _ -> o
  record acc ledger flows consumed c =
    let
      next = spawned w.round templates [ c ] consumed
      events = case c.cause of
        Just cause | any (\f -> f.moved /= Q.zero) flows -> [ Coupled { round: w.round, legs: flows, rate: Nothing, cause, delivered: false } ]
        _ -> []
    in
      { ledger, events: acc.events <> events, spawned: acc.spawned <> either (const []) (map PendingCoupling) next, errors: acc.errors <> either pure (const []) next }

commit :: WorldState -> Array SignedProposal -> Array Event -> Map EntityId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  let byAccount = ledgerByAccount st.ledger
  entities <- traverse (applyEntity byAccount) w.entities
  marketList <- traverse (applyMarket st byAccount) (entries w.markets)
  let
    events = screenEvents <> st.events
    -- Prepare's events are already in the world's history; the round's record carries them too (docs/04 §11.1)
    world = addEvents events w
      { round = w.round + 1
      , entities = entities
      , markets = Map.fromFoldable marketList
      , pending = w.pending <> st.spawned
      , factors = factors
      }
  pure { world, record: { round: w.round, inputs, events: filter (prepared w.round) w.events <> events } }

-- | An Entity with the Settlement's net change to its holdings applied, if none goes negative.
applyEntity :: Map Account (Map ResourceId Q) -> Entity -> Either EngineError Entity
applyEntity byAccount e = (\accounts -> e { accounts = accounts })
  <$> checkNonNegative (Ent e.id) (applyHoldings byAccount (Ent e.id) e.accounts)

-- | A Market with the Settlement's net change to its reserves and its new position applied, if no
-- | reserve goes negative.
applyMarket :: Settlement -> Map Account (Map ResourceId Q) -> Tuple MarketId Market -> Either EngineError (Tuple MarketId Market)
applyMarket st byAccount (Tuple id m) = (\reserves -> Tuple id m { reserves = reserves, position = fromMaybe m.position (Map.lookup id st.positions) })
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
