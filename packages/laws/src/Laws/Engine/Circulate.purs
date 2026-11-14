-- | Engine phase 6, second half (docs/02_kernel.md §6, §8.2): the recurring stock-flow cycle.
-- | Each `RecurringFlow` moves a Resource from its source to its destination, evaluated on each
-- | Actor's net position after settlement, in list order; the rules from every holder run last, in
-- | Actor then Resource order. One function over the data, no rule-specific paths. `circulate` is
-- | the only export.
module Laws.Engine.Circulate
  ( circulate
  ) where

import Prelude

import Laws.Engine.Compile (WorldLaw, delivery, fractionLaw, runTransaction, timeLaw)
import Laws.Engine.Settle (Settlement, net)
import Core.Kernel.Law (forHolder)
import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Types (Subject(..), Trigger(..)) as K
import Laws.World (AuthoredLaw(..), Flow, addEvents, Cause(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShortfallEffect(..), WorldState, settledEvent)
import Core.Map (sumInto)
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (actorsOf, holding, lookupQ)
import Laws.Slice (runLegQuery)
import Laws.Types (ActorId)
import Data.Array (filter, findIndex, foldl, fromFoldable, index, length, any, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | What the cycle accrues against an Actor's incoming offsetable flows.
type Penalised = { st :: Settlement, offsets :: Map ActorId Q }

-- | The recurring cycle, evaluated on the round's batch result.
circulate :: WorldState -> Settlement -> Settlement
circulate w st0 = eachHolder w (foldl (step w) { st: standing w st0, offsets: Map.empty } (filter (not <<< isEachHolder) w.recurring)).st

-- | The authored laws charged every round (docs/03 §15.1), such as upkeep on capacity held: each of a law's holders
-- | pays what it says, on what it holds after settlement so far, capped at what it holds. A holder that owes nothing
-- | has nothing written for it.
standing :: WorldState -> Settlement -> Settlement
standing w st0 = foldl charge st0 (w.laws >>= each)
  where
  each (AuthoredLaw law) = case law.trigger, law.subject of
    K.EachRound, K.OfStock s -> map (Tuple law) (fromMaybe (map _.id (actorsOf w)) s.holders)
    _, _ -> []
  charge acc (Tuple law holder) = case runLaw w acc.ledger law holder of
    posted
      | any (\f -> f.owed /= Q.zero) posted.flows -> addEvents [ settledEvent w.round posted.flows Nothing law.cause ] acc { ledger = posted.ledger }
      | otherwise -> acc

isEachHolder :: RecurringFlow -> Boolean
isEachHolder r = r.source == FromEachHolder

-- | One rule's flow, on what the round has settled so far.
step :: WorldState -> Penalised -> RecurringFlow -> Penalised
step w acc r =
  case r.source, r.destination of
    Created, ToActor to -> acc { st = acc.st { spawned = acc.st.spawned <> [ delivery { round: max (w.round + 1) (w.round + r.delay), actor: to, resource: r.resource, quantity: amountOf w acc.st r, cause: causeOf r } ] } }
    FromActor from, ToActor to -> paying from to (paidAmount to)
    FromActor from, ToSink -> consume from
    _, _ -> acc
  where
  paidAmount to = if r.offsetable then offsetShare w acc to r (amountOf w acc.st r) else amountOf w acc.st r
  phase = 10 + fromMaybe 0 (findIndex (\x -> x.id == r.id) w.recurring)
  paying from to owed
    | owed == Q.zero = acc
    | otherwise =
        let
          posted = runLaw w acc.st.ledger (timeLaw from (Just to) r.resource owed phase (causeOf r)) from
        in
          acc { st = addEvents [ settledEvent w.round posted.flows Nothing (causeOf r) ] acc.st { ledger = posted.ledger } }
  consume from =
    let
      needed = amountOf w acc.st r
      posted = runLaw w acc.st.ledger (timeLaw from Nothing r.resource needed phase (causeOf r)) from
      moved = fromMaybe Q.zero (_.moved <$> head posted.flows)
      short = Q.sub needed moved
      events = if needed > Q.zero then [ settledEvent w.round posted.flows Nothing (causeOf r) ] else []
    in
      { st: addEvents events acc.st { ledger = posted.ledger }
      , offsets: case r.onShortfall of
          OffsetIncoming perUnit | short > Q.zero -> sumInto from (Q.mul perUnit short) acc.offsets
          _ -> acc.offsets
      }

-- | One holder's share of a time law, on the round so far.
runLaw :: WorldState -> Ledger -> WorldLaw -> ActorId -> { ledger :: Ledger, flows :: Array Flow }
runLaw w ledger law holder = case forHolder holder law of
  Just c -> case runTransaction w ledger c of
    posted -> { ledger: posted.ledger, flows: posted.flows }
  Nothing -> { ledger, flows: [] }

causeOf :: RecurringFlow -> Cause
causeOf r = OfRecurring { rule: r.id, kind: r.kind }

-- | What a rule owes, read from the round so far.
amountOf :: WorldState -> Settlement -> RecurringFlow -> Q
amountOf w st r = case r.amount of
  FixedAmount q -> q
  HeldAmount c -> case r.destination of
    ToActor to -> holding w to c
    ToSink -> Q.zero
  FractionHeld f -> case r.source of
    FromActor from -> Q.mul f (max Q.zero (net w from r.resource st))
    _ -> Q.zero
  -- the record so far: what earlier rounds and this round's settlement have written
  Measured m -> Q.mul m.ratio (fromMaybe Q.zero (runLegQuery w.round (w.events <> st.events) m.query))

-- | One offsetable flow's amount: the recipient's accrued offset, capped at its incoming total and
-- | shared pro rata over its offsetable flows in list order, is taken off.
offsetShare :: WorldState -> Penalised -> ActorId -> RecurringFlow -> Q -> Q
offsetShare w acc to r amount = Q.sub amount (fromMaybe Q.zero (index cuts position))
  where
  siblings = filter (\s -> s.offsetable && s.destination == ToActor to) w.recurring
  amounts = map (amountOf w acc.st) siblings
  incoming = Q.sumQ amounts
  cuts = Q.proRata (min incoming (lookupQ to acc.offsets)) amounts
  position = fromMaybe (length siblings) (findIndex (\s -> s.id == r.id) siblings)

-- | The rules from every holder of a Resource: each Actor loses what its rule says, of every
-- | Resource it has an account for or came to hold this round.
eachHolder :: WorldState -> Settlement -> Settlement
eachHolder w st0 = foldl lose st0 holdings
  where
  holdings = (actorsOf w) >>= \e -> map (Tuple e.id) (fromFoldable (Set.union (Map.keys e.accounts) (acquired e.id)))
  acquired e = Set.mapMaybe (\(Tuple a r) -> if a == Ent e then Just r else Nothing) (Map.keys st0.ledger)
  lose acc (Tuple e r) = foldl (loseBy e) acc (filter (\x -> isEachHolder x && x.resource == r) w.recurring)
  -- a holder that would lose nothing is untouched: nothing is written for it
  loseBy e acc rule = case forHolder e (fractionLaw rule.resource (fractionOf rule) (causeOf rule)) of
    Just c | posted <- runTransaction w acc.ledger c, any (\f -> f.moved /= Q.zero) posted.flows ->
      addEvents [ settledEvent w.round posted.flows Nothing (causeOf rule) ] acc { ledger = posted.ledger }
    _ -> acc
  fractionOf rule = case rule.amount of
    FractionHeld f -> f
    _ -> Q.zero
