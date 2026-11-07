-- | Engine phase 6, first half (docs/02_type_system.md §8.1, §8.2): settle the funded batch
-- | against the prepared snapshot. Markets clear at uniform prices, facility fills are allocated
-- | per pool and paid, and Transfers, tariffs, and Rule changes apply. Every payment goes through
-- | `pay`, which caps it at what the payer holds, so an Account cannot go negative. Settlement
-- | accumulates a ledger of net changes; nothing is written to World State until Commit.
-- | `Settlement`, `Account`, and `Ledger` are defined here because this is the first phase that
-- | produces them; Circulate and Commit import them.
module Core.Engine.Settle
  ( Account(..)
  , Ledger
  , Settlement
  , settle
  , adjust
  , net
  , ledgerByAccount
  , transfer
  , minOwedHeld
  , groupBy
  , shortfallOf
  , consumedOf
  ) where

import Prelude

import Core.Engine.Fund (Scaled)
import Core.Engine.Route as Route
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityOf, facilityPrice, holding, lookupQ, poolCeiling, poolOf, tradesOf)
import Core.Types (CapacityRef, EntityId, Id(..), Intent(..), MarketId, ProposalId, ResourceId, Rule, RuleChange(..), RuleId, RoundIndex, TransformationId)
import Core.World (Delivery, Event(..), Target(..), WorldState)
import Data.Array (all, concat, concatMap, filter, foldl, nub, notElem, null, partition, sortWith, zipWith)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)

data Account = Ent EntityId | Mkt MarketId

derive instance Eq Account
derive instance Ord Account

-- | Net changes to every account for the round, applied together at commit.
type Ledger = Map (Tuple Account ResourceId) Q

-- | The ledger grouped by Account, in one pass: each Account's changes, keyed by Resource.
ledgerByAccount :: Ledger -> Map Account (Map ResourceId Q)
ledgerByAccount ledger = Map.fromFoldableWith Map.union (map (\(Tuple (Tuple a r) q) -> Tuple a (Map.singleton r q)) (Map.toUnfoldable ledger :: Array _))

-- | The Shortfall event: an amount owed but not delivered.
shortfallOf :: RoundIndex -> EntityId -> ResourceId -> Q -> Event
shortfallOf round entity resource quantity = Shortfall { round, entity, resource, quantity }

-- | The Consumed event: an operating requirement met from what an Entity held.
consumedOf :: RoundIndex -> EntityId -> ResourceId -> Q -> Event
consumedOf round entity resource quantity = Consumed { round, entity, resource, quantity }

move :: Account -> Account -> ResourceId -> Q -> Ledger -> Ledger
move from to r q = adjust to r q <<< adjust from r (Q.neg q)

adjust :: Account -> ResourceId -> Q -> Ledger -> Ledger
adjust a r q = Map.insertWith Q.add (Tuple a r) q

-- | The round's batch result: the ledger and Market positions, plus the deliveries, Rule changes,
-- | and events it produces.
type Settlement =
  { ledger :: Ledger
  , positions :: Map MarketId Q
  , deliveries :: Array Delivery
  , scheduled :: Map RuleId (Maybe Rule)
  , events :: Array Event
  }

settle :: WorldState -> Array Scaled -> Settlement
settle w scaled0 =
  let
    facility = facilities w scaled0
    scaled = filter (\s -> s.item.proposal.id `notElem` facility.rejected) scaled0
    start = { ledger: Map.empty, positions: Map.empty, deliveries: [], scheduled: Map.empty, events: facility.events }
  in
    foldl (\acc f -> f acc) start
      [ markets w scaled
      , \acc -> foldl (settleFacilities w facility) acc scaled
      , \acc -> foldl (transfersAndRules w) acc scaled
      ]

-- | Facility fills per pool. Deals that would exceed a pool are rejected as a unit; single-author
-- | orders share what remains pro rata.
facilities :: WorldState -> Array Scaled -> Facilities
facilities w scaled = foldl (allocateGroup w) { fills: Map.empty, totals: Map.empty, rejected: [], events: [] } (Map.toUnfoldable groups :: Array _)
  where
  groups = groupBy _.group (facilityOrders w scaled)

type FacilityOrder =
  { key :: Tuple ProposalId Int
  , deal :: Boolean
  , pid :: ProposalId
  , quantity :: Q
  , use :: Q -- capacity drawn per unit ordered: `capacityUse`, or one unit per unit of a pool of its own
  , group :: Either TransformationId CapacityRef
  , tid :: TransformationId
  }

-- | Every Transform, scaled by its Proposal's funding.
facilityOrders :: WorldState -> Array Scaled -> Array FacilityOrder
facilityOrders w = concatMap \s -> concat (mapWithIndex (order s) s.item.proposal.intents)
  where
  order s i = case _ of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations ->
      let
        group = poolOf t.transformation tr
        use = case group of
          Right _ -> tr.capacityUse
          Left _ -> Q.fromInt 1
      in
        [ { key: Tuple s.item.proposal.id i, deal: s.item.deal, pid: s.item.proposal.id, quantity: Q.mul s.scale t.quantity, use, group, tid: t.transformation } ]
    _ -> []

-- | One pool: deals fill in full or are rejected as a unit; single orders share the remaining
-- | capacity pro rata, in whole units of capacity. Each order's quantity is its share divided by
-- | its `use`, floored to the 0.0001 grid, so an order never draws more than its share. The
-- | sub-grid remainder stays unfilled. A capped pool's capacity is its cap; an uncapped facility's
-- | cap is its ceiling, so its fills never exceed the quantity escrow priced.
allocateGroup :: WorldState -> Facilities -> Tuple (Either TransformationId CapacityRef) (Array FacilityOrder) -> Facilities
allocateGroup w acc (Tuple group os) =
  { fills: Map.union acc.fills (Map.fromFoldable fills)
  , totals: foldl (\m (Tuple tid q) -> Map.insertWith Q.add tid q m) acc.totals (zipWith (\o (Tuple _ q) -> Tuple o.tid q) (if overflow then singles else deals <> singles) fills)
  , rejected: acc.rejected <> rejectedDeals
  , events: acc.events <> map (\pid -> Rejected { round: w.round, proposal: pid, reason: "facility capacity exceeded" }) rejectedDeals
  }
  where
  cap = case group of
    Right ref -> capacityOf w ref
    Left tid -> fromMaybe (Q.sumQ (map _.quantity os)) (Map.lookup tid w.transformations >>= poolCeiling w)
  { yes: deals, no: singles } = partition _.deal os
  used o = Q.mul o.use o.quantity
  dealTotal = Q.sumQ (map used deals)
  overflow = dealTotal > cap
  rejectedDeals = if overflow then nub (map _.pid deals) else []
  room = if overflow then cap else Q.sub cap dealTotal
  singleFills
    | Q.sumQ (map used singles) > room = zipWith (\o u -> fromMaybe Q.zero (Q.divFloor u o.use)) singles (Q.proRataWhole room (map used singles))
    | otherwise = map _.quantity singles
  fills = (if overflow then [] else map (\o -> Tuple o.key o.quantity) deals) <> zipWith (\o q -> Tuple o.key q) singles singleFills

type Facilities =
  { fills :: Map (Tuple ProposalId Int) Q
  , totals :: Map TransformationId Q -- filled quantity per facility
  , rejected :: Array ProposalId
  , events :: Array Event
  }

markets :: WorldState -> Array Scaled -> Settlement -> Settlement
markets w scaled acc0 = foldl clearOne acc0 (Map.toUnfoldable w.markets :: Array _)
  where
  explicit = concatMap
    ( \s -> map
        (\o -> { market: o.market, entity: s.item.proposal.author, pid: s.item.proposal.id, quantity: Q.mul s.scale o.quantity, limitPrice: o.limitPrice })
        (tradesOf s.item.proposal.intents)
    )
    scaled
  -- routed Trades are split across their candidate Markets (docs/02 §5.2), against the net of the explicit Trades
  orders = explicit <> Route.plan w (Map.fromFoldableWith Q.add (map (\o -> Tuple o.market o.quantity) explicit)) routed
  routed = concatMap
    ( \s -> concat $ mapWithIndex
        ( \i intent -> case intent of
            Route r
              | Just limit <- r.limitPrice
              , Just cs <- Map.lookup i s.item.routes ->
                  [ { entity: s.item.proposal.author, pid: s.item.proposal.id, index: i, quantity: Q.mul s.scale r.quantity, limitPrice: limit, candidates: cs } ]
            _ -> []
        )
        s.item.proposal.intents
    )
    scaled
  -- Trade fills are not capped by `pay`: clearing bounds them by the Market's domain and reserves,
  -- and escrow funds them (docs/02 §8.2). Their money is escrowed at the limit or the domain ceiling.
  clearOne acc (Tuple id m) =
    let
      os = sortWith (\o -> Tuple o.entity o.pid) (filter (\o -> o.market == id) orders)
      bounds = Route.bounds m
      cleared = Market.clear m.cost m.position bounds os
      -- A Market pays its sellers in quote from its reserves. If the batch would overdraw them,
      -- the sell side is rejected and the batch clears on buys alone, which pay out nothing.
      moneyIn c = Q.sumQ (map (\f -> Q.mul c.price f.quantity) c.fills)
      solvent c = Q.add (lookupQ m.quote m.reserves) (moneyIn c) >= Q.zero
      r = if solvent cleared then cleared else Market.clear m.cost m.position bounds (filter (\o -> o.quantity > Q.zero) os)
      unpaid = if solvent cleared then [] else filter (\o -> o.quantity < Q.zero) os
      fill a f =
        let
          paid = Q.mul r.price f.quantity
        in
          a
            { ledger = move (Mkt id) (Ent f.order.entity) m.base f.quantity (move (Ent f.order.entity) (Mkt id) m.quote paid a.ledger)
            , events = a.events <> [ Traded { round: w.round, market: id, entity: f.order.entity, quantity: f.quantity, price: r.price } ]
            }
    in
      if null os then acc
      else
        let
          filled = foldl fill acc r.fills
        in
          filled
            { positions = Map.insert id (Q.add m.position r.net) acc.positions
            , events = filled.events
                <> map (\o -> Rejected { round: w.round, proposal: o.pid, reason: "limit price not met" }) r.rejected
                <> map (\o -> Rejected { round: w.round, proposal: o.pid, reason: "the market cannot pay its sellers" }) unpaid
            }

-- | Settle one Proposal's filled Transforms: pay the facility's payee, consume inputs, produce
-- | outputs, and schedule capacity effects.
settleFacilities :: WorldState -> Facilities -> Settlement -> Scaled -> Settlement
settleFacilities w facility acc s = foldl one acc (mapWithIndex Tuple s.item.proposal.intents)
  where
  pid = s.item.proposal.id
  -- facilities in one pool with one cost function share one uniform price
  pricedTotal tid tr = Q.sumQ
    ( map (\(Tuple _ q) -> q)
        ( filter
            (\(Tuple other _) -> maybe false (\x -> poolOf other x == poolOf tid tr && x.cost == tr.cost) (Map.lookup other w.transformations))
            (Map.toUnfoldable facility.totals :: Array _)
        )
    )
  -- Capacity effects land on the same boundary as outputs: an output with delay d is visible from
  -- round + max 1 d, and so is an effect, since a delay of zero is treated as one round.
  effects t tr q = concatMap
    ( \e ->
        let
          holder = fromMaybe t.by e.holder
          start = w.round + max 1 tr.delay
          change = { round: start, entity: holder, target: CapacityOf e.capacity, quantity: Q.mul e.perUnit q }
        in
          [ change ] <> maybe [] (\n -> [ change { round = start + n, quantity = Q.neg change.quantity } ]) e.rounds
    )
    tr.effects
  one a (Tuple i intent) = case intent of
    Transform t -> case Tuple (Map.lookup (Tuple pid i) facility.fills) (Map.lookup t.transformation w.transformations) of
      Tuple (Just q) (Just tr) | q > Q.zero ->
        let
          owed = Q.mul (facilityPrice tr (pricedTotal t.transformation tr)) q
          -- escrow funds inputs, so a shortfall here is a backstop: the Transform is rejected, not overdrawn
          inputsHeld = all (\(Tuple r u) -> net w t.by r a >= Q.mul u q) (Map.toUnfoldable tr.inputs :: Array _)
          consume l = foldl (\l' (Tuple r u) -> adjust (Ent t.by) r (Q.neg (Q.mul u q)) l') l (Map.toUnfoldable tr.inputs :: Array _)
          outs = map (\(Tuple r u) -> { round: w.round + tr.delay, entity: t.by, target: Holding r, quantity: Q.mul u q }) (Map.toUnfoldable tr.outputs :: Array _)
          { yes: now, no: later } = partition (\d -> d.round == w.round) outs
          produce l = foldl
            ( \l' d -> case d.target of
                Holding r -> adjust (Ent d.entity) r d.quantity l'
                CapacityOf _ -> l'
            )
            l
            now
        in
          if not inputsHeld then a { events = a.events <> [ Rejected { round: w.round, proposal: pid, reason: show t.transformation <> ": inputs are not held" } ] }
          else case pay w t.by tr.payee w.numeraire owed a of
            Tuple paid afterPay ->
              afterPay
                { ledger = produce (consume afterPay.ledger)
                , deliveries = afterPay.deliveries <> later <> effects t tr q
                , events = afterPay.events <> [ Transformed { round: w.round, by: t.by, transformation: t.transformation, quantity: q, paid } ]
                }
      _ -> a
    _ -> a

-- | Transfers, tariffs, and rule changes of surviving Items.
transfersAndRules :: WorldState -> Settlement -> Scaled -> Settlement
transfersAndRules w acc s = foldl one withTariffs (mapWithIndex Tuple p.intents)
  where
  p = s.item.proposal
  -- tariffs follow the shipment they belong to, so they are scaled with it
  withTariffs = foldl (\a' t -> snd (pay w t.from (Just t.to) w.numeraire (Q.mul s.scale t.amount) a')) acc s.item.tariffs
  one a (Tuple i intent) = case intent of
    Transfer t -> transfer w t.from t.to t.resource (Q.mul s.scale t.quantity) a
    ChangeRule (Enact r) -> a { scheduled = Map.insert (ruleId i) (Just r) a.scheduled }
    ChangeRule (Repeal id) -> a { scheduled = Map.insert id Nothing a.scheduled }
    _ -> a
  ruleId i = Id (show p.id <> "/rule-" <> show i)

-- | Group items into arrays by key, keeping their order.
groupBy :: forall k a. Ord k => (a -> k) -> Array a -> Map k (Array a)
groupBy key xs = Map.fromFoldableWith (flip (<>)) (map (\x -> Tuple (key x) [ x ]) xs)

-- | What an Entity can pay or consume: what is owed, capped at what it holds.
minOwedHeld :: Q -> Q -> Q
minOwedHeld owed held = min owed (max Q.zero held)

-- | The one way an Entity pays out, used by every settlement payment. It pays at most what it
-- | holds after the settlement so far, and the remainder is recorded as a Shortfall. With no
-- | payee (`Nothing`) the amount is destroyed. Returns the amount actually paid.
pay :: WorldState -> EntityId -> Maybe EntityId -> ResourceId -> Q -> Settlement -> Tuple Q Settlement
pay w from to resource owed st = Tuple paid
  st
    { ledger = case to of
        Just p -> move (Ent from) (Ent p) resource paid st.ledger
        Nothing -> adjust (Ent from) resource (Q.neg paid) st.ledger
    , events = st.events <> shortEvents
    }
  where
  paid = minOwedHeld owed (net w from resource st)
  short = Q.sub owed paid
  shortEvents = if short > Q.zero then [ shortfallOf w.round from resource short ] else []

-- | A transfer between Entities through `pay`: the recorded quantity is what was paid.
transfer :: WorldState -> EntityId -> EntityId -> ResourceId -> Q -> Settlement -> Settlement
transfer w from to resource quantity st
  | quantity == Q.zero = st
  | otherwise = case pay w from (Just to) resource quantity st of
      Tuple paid st' -> st' { events = st'.events <> [ Transferred { round: w.round, from, to, resource, quantity: paid } ] }

-- | What an Entity holds after the settlement so far.
net :: WorldState -> EntityId -> ResourceId -> Settlement -> Q
net w e r st = Q.add (holding w e r) (lookupQ (Tuple (Ent e) r) st.ledger)
