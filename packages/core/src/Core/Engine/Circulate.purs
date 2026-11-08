-- | Engine phase 6, second half (docs/02_type_system.md §6, §8.2): the recurring stock-flow cycle.
-- | Production, variable revenue shares, operating requirements with shortfall penalties, fixed
-- | flows, and decay are all functions of the batch result, evaluated on each Entity's net
-- | position after settlement. Each component is a step over the Settlement; no rule-specific
-- | paths. `circulate` is the only export.
module Core.Engine.Circulate
  ( circulate
  ) where

import Prelude

import Core.Engine.Settle (Account(..), Settlement, adjust, consumedOf, minOwedHeld, net, shortfallOf, transfer)
import Core.Map (entries, groupBy, sumInto)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, lookupQ, entitiesOf)
import Core.Types (EntityId, MarketId, ResourceId)
import Core.World (Event(..), ShareSource(..), Target(..), WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable, zipWith)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | The recurring cycle, evaluated on the round's batch result: production (delivered next
-- | round), variable revenue shares, operating requirements with shortfall penalties, fixed
-- | flows, and decay. One function over the Circulation data; no rule-specific paths.
circulate :: WorldState -> Settlement -> Settlement
circulate w = decay w <<< fixedFlows w <<< requirements w <<< revenueShares w <<< production w

-- | Sum a quantity over the round's events.
sumEvents :: (Event -> Array Q) -> Settlement -> Q
sumEvents f st = Q.sumQ (concatMap f st.events)

-- | The base Resource a Market trades.
marketBase :: WorldState -> MarketId -> Maybe ResourceId
marketBase w m = _.base <$> Map.lookup m w.markets

-- | The Traded events of one Entity in one Resource's Markets, as the quantity and price of each.
tradedIn :: WorldState -> EntityId -> ResourceId -> Event -> Array { quantity :: Q, price :: Q }
tradedIn w e r = case _ of
  Traded t | t.entity == e && marketBase w t.market == Just r -> [ { quantity: t.quantity, price: t.price } ]
  _ -> []

-- | Production capacity becomes a delivery in the next round.
production :: WorldState -> Settlement -> Settlement
production w st = st { deliveries = st.deliveries <> map scheduled w.circulation.production }
  where
  scheduled p = { round: w.round + 1, entity: p.entity, target: Holding p.resource, quantity: capacityHeld w p.entity p.capacity }

-- | Variable shares of sales revenue or payee receipts, paid in the numeraire.
revenueShares :: WorldState -> Settlement -> Settlement
revenueShares w st0 = foldl payShare st0 w.circulation.shares
  where
  payShare st sh = transfer w sh.from sh.to w.numeraire (Q.mul sh.share (base sh)) st
  base sh = case sh.source of
    SalesOf r -> max Q.zero $ sumEvents (\ev -> map (\t -> Q.neg (Q.mul t.price t.quantity)) (tradedIn w sh.from r ev)) st0
    PayeeReceipts -> sumEvents
      ( case _ of
          Transformed t | (Map.lookup t.transformation w.transformations >>= _.payee) == Just sh.from -> [ t.paid ]
          _ -> []
      )
      st0

type Penalised = { st :: Settlement, penalties :: Map EntityId Q }

-- | Operating requirements: consume what is held, record any shortfall, and accrue its
-- | penalty against the Entity's incoming fixed flows.
requirements :: WorldState -> Settlement -> Penalised
requirements w st0 = foldl meet { st: st0, penalties: Map.empty } w.circulation.requirements
  where
  meet acc rq =
    let
      purchases = sumEvents (\ev -> map _.quantity (filter (\t -> t.quantity > Q.zero) (tradedIn w rq.entity rq.resource ev))) st0
      needed = fromMaybe purchases rq.amount
      consumed = minOwedHeld needed (net w rq.entity rq.resource acc.st)
      short = Q.sub needed consumed
      when' cond ev = if cond then [ ev ] else []
    in
      { st: acc.st
          { ledger = adjust (Ent rq.entity) rq.resource (Q.neg consumed) acc.st.ledger
          , events = acc.st.events
              <> when' (consumed > Q.zero) (consumedOf w.round rq.entity rq.resource consumed)
              <> when' (short > Q.zero) (shortfallOf w.round rq.entity rq.resource short)
          }
      , penalties: if short > Q.zero then sumInto rq.entity (Q.mul rq.penalty short) acc.penalties else acc.penalties
      }

-- | Fixed flows, each Entity's incoming flows reduced pro rata by its penalty (payers keep it).
fixedFlows :: WorldState -> Penalised -> Settlement
fixedFlows w { st, penalties } = foldl (\acc (Tuple f amount) -> transfer w f.from f.to f.resource amount acc) st (concatMap reduce byRecipient)
  where
  byRecipient = entries (groupBy _.to w.circulation.flows)
  reduce (Tuple to fs) =
    let
      incoming = Q.sumQ (map _.amount fs)
      cut = min incoming (lookupQ to penalties)
    in
      zipWith (\f k -> Tuple f (Q.sub f.amount k)) fs (Q.proRata cut (map _.amount fs))

-- | Decay of every decaying Resource held. The rate is per round; instances scale annual rates
-- | to the round length (INV-28).
decay :: WorldState -> Settlement -> Settlement
decay w st0 = foldl lose st0 holdings
  where
  -- every Resource the Entity has an account for or came to hold this round
  holdings = concatMap (\e -> map (Tuple e.id) (fromFoldable (Set.union (Map.keys e.accounts) (acquired e.id)))) (entitiesOf w)
  acquired e = Set.mapMaybe (\(Tuple a r) -> if a == Ent e then Just r else Nothing) (Map.keys st0.ledger)
  lose acc (Tuple e r) = case Map.lookup r w.resources >>= _.decay of
    Just d ->
      let
        held = max Q.zero (net w e r acc)
        lost = minOwedHeld (Q.mul d.rate held) held
      in
        if lost == Q.zero then acc
        else acc { ledger = adjust (Ent e) r (Q.neg lost) acc.ledger, events = acc.events <> [ Decayed { round: w.round, entity: e, resource: r, quantity: lost } ] }
    Nothing -> acc
