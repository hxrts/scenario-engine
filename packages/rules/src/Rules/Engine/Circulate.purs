-- | Engine phase 6, second half (docs/02_type_system.md §6, §8.2): the recurring stock-flow cycle.
-- | Each `RecurringFlow` moves a Resource from its source to its destination, evaluated on each
-- | Entity's net position after settlement, in list order; the rules from every holder run last, in
-- | Entity then Resource order. One function over the data, no rule-specific paths. `circulate` is
-- | the only export.
module Rules.Engine.Circulate
  ( circulate
  ) where

import Prelude

import Rules.Engine.Compile (delivery, fractionLaw, payCoupling, runCoupling)
import Rules.Engine.Settle (Settlement, net, transfer)
import Core.Kernel.Law (forHolder)
import Core.Kernel.Post (Account(..))
import Core.Map (sumInto)
import Core.Num (Q)
import Core.Num as Q
import Rules.Query (entitiesOf, holding, lookupQ)
import Rules.Slice (runLegQuery)
import Rules.Types (EntityId)
import Rules.World (addEvents, Cause(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShortfallEffect(..), WorldState, coupled)
import Data.Array (filter, findIndex, foldl, fromFoldable, index, length, any, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | What the cycle accrues against an Entity's incoming offsetable flows.
type Penalised = { st :: Settlement, offsets :: Map EntityId Q }

-- | The recurring cycle, evaluated on the round's batch result.
circulate :: WorldState -> Settlement -> Settlement
circulate w st0 = eachHolder w (foldl (step w) { st: st0, offsets: Map.empty } (filter (not <<< isEachHolder) w.recurring)).st

isEachHolder :: RecurringFlow -> Boolean
isEachHolder r = r.source == FromEachHolder

-- | One rule's flow, on what the round has settled so far.
step :: WorldState -> Penalised -> RecurringFlow -> Penalised
step w acc r =
  case r.source, r.destination of
    Created, ToEntity to -> acc { st = acc.st { spawned = acc.st.spawned <> [ delivery { round: max (w.round + 1) (w.round + r.delay), entity: to, resource: r.resource, quantity: amountOf w acc.st r, cause: causeOf r } ] } }
    FromEntity from, ToEntity to -> acc { st = transfer w (causeOf r) from to r.resource (paidAmount to) acc.st }
    FromEntity from, ToSink -> consume from
    _, _ -> acc
  where
  paidAmount to = if r.offsetable then offsetShare w acc to r (amountOf w acc.st r) else amountOf w acc.st r
  consume from =
    let
      needed = amountOf w acc.st r
      posted = runCoupling w acc.st.ledger (payCoupling from Nothing r.resource needed (causeOf r))
      moved = fromMaybe Q.zero (_.moved <$> head posted.flows)
      short = Q.sub needed moved
      events = if needed > Q.zero then [ coupled w.round posted.flows Nothing (causeOf r) ] else []
    in
      { st: addEvents events acc.st { ledger = posted.ledger }
      , offsets: case r.onShortfall of
          OffsetIncoming perUnit | short > Q.zero -> sumInto from (Q.mul perUnit short) acc.offsets
          _ -> acc.offsets
      }

causeOf :: RecurringFlow -> Cause
causeOf r = OfRecurring { rule: r.id, kind: r.kind }

-- | What a rule owes, read from the round so far.
amountOf :: WorldState -> Settlement -> RecurringFlow -> Q
amountOf w st r = case r.amount of
  FixedAmount q -> q
  HeldAmount c -> case r.destination of
    ToEntity to -> holding w to c
    ToSink -> Q.zero
  FractionHeld f -> case r.source of
    FromEntity from -> Q.mul f (max Q.zero (net w from r.resource st))
    _ -> Q.zero
  -- the record so far: what earlier rounds and this round's settlement have written
  Measured m -> Q.mul m.ratio (fromMaybe Q.zero (runLegQuery w.round (w.events <> st.events) m.query))

-- | One offsetable flow's amount: the recipient's accrued offset, capped at its incoming total and
-- | shared pro rata over its offsetable flows in list order, is taken off.
offsetShare :: WorldState -> Penalised -> EntityId -> RecurringFlow -> Q -> Q
offsetShare w acc to r amount = Q.sub amount (fromMaybe Q.zero (index cuts position))
  where
  siblings = filter (\s -> s.offsetable && s.destination == ToEntity to) w.recurring
  amounts = map (amountOf w acc.st) siblings
  incoming = Q.sumQ amounts
  cuts = Q.proRata (min incoming (lookupQ to acc.offsets)) amounts
  position = fromMaybe (length siblings) (findIndex (\s -> s.id == r.id) siblings)

-- | The rules from every holder of a Resource: each Entity loses what its rule says, of every
-- | Resource it has an account for or came to hold this round.
eachHolder :: WorldState -> Settlement -> Settlement
eachHolder w st0 = foldl lose st0 holdings
  where
  holdings = (entitiesOf w) >>= \e -> map (Tuple e.id) (fromFoldable (Set.union (Map.keys e.accounts) (acquired e.id)))
  acquired e = Set.mapMaybe (\(Tuple a r) -> if a == Ent e then Just r else Nothing) (Map.keys st0.ledger)
  lose acc (Tuple e r) = foldl (loseBy e) acc (filter (\x -> isEachHolder x && x.resource == r) w.recurring)
  -- a holder that would lose nothing is untouched: nothing is written for it
  loseBy e acc rule = case forHolder e (fractionLaw rule.resource (fractionOf rule) (causeOf rule)) of
    Just c | posted <- runCoupling w acc.ledger c, any (\f -> f.moved /= Q.zero) posted.flows ->
      addEvents [ coupled w.round posted.flows Nothing (causeOf rule) ] acc { ledger = posted.ledger }
    _ -> acc
  fractionOf rule = case rule.amount of
    FractionHeld f -> f
    _ -> Q.zero
