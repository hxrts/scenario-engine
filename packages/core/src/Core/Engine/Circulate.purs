-- | Engine phase 6, second half (docs/02_type_system.md §6, §8.2): the recurring stock-flow cycle.
-- | Each `RecurringFlow` moves a Resource from its source to its destination, evaluated on each
-- | Entity's net position after settlement, in list order; the rules from every holder run last, in
-- | Entity then Resource order. One function over the data, no rule-specific paths. `circulate` is
-- | the only export.
module Core.Engine.Circulate
  ( circulate
  ) where

import Prelude

import Core.Engine.Settle (Account(..), Settlement, adjust, minOwedHeld, net, transfer)
import Core.Map (sumInto)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (entitiesOf, holding, lookupQ)
import Core.Slice (runLegQuery)
import Core.Types (EntityId)
import Core.World (addEvents, Cause(..), Endpoint(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShortfallEffect(..), WorldState, coupled)
import Data.Array (filter, findIndex, foldl, fromFoldable, index, length)
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
    Created, ToEntity to -> acc { st = acc.st { deliveries = acc.st.deliveries <> [ { round: w.round + r.delay, entity: to, resource: r.resource, quantity: amountOf w acc.st r, cause: causeOf r } ] } }
    FromEntity from, ToEntity to -> acc { st = transfer w (causeOf r) from to r.resource (paidAmount to) acc.st }
    FromEntity from, ToSink -> consume from
    _, _ -> acc
  where
  paidAmount to = if r.offsetable then offsetShare w acc to r (amountOf w acc.st r) else amountOf w acc.st r
  consume from =
    let
      needed = amountOf w acc.st r
      moved = minOwedHeld needed (net w from r.resource acc.st)
      short = Q.sub needed moved
      legs = [ { from: Held from, to: Sink, resource: r.resource, owed: needed, moved } ]
      events = if needed > Q.zero then [ coupled w.round legs Nothing (causeOf r) ] else []
    in
      { st: addEvents events acc.st
          { ledger = adjust (Ent from) r.resource (Q.neg moved) acc.st.ledger
          }
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
  loseBy e acc rule =
    let
      held = max Q.zero (net w e rule.resource acc)
      lost = minOwedHeld (fractionOf rule held) held
    in
      if lost == Q.zero then acc
      else addEvents [ coupled w.round [ { from: Held e, to: Sink, resource: rule.resource, owed: lost, moved: lost } ] Nothing (causeOf rule) ] acc
        { ledger = adjust (Ent e) rule.resource (Q.neg lost) acc.ledger
        }
  fractionOf rule held = case rule.amount of
    FractionHeld f -> Q.mul f held
    _ -> Q.zero
