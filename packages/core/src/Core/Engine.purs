-- | The engine: a pure function from World State and signed Proposals to the next
-- | World State (docs/02_type_system.md §8). It is an accountant: it checks authority and
-- | signatures, escrows, clears, and keeps the books balanced. It never authors or rewrites
-- | an Intent.
module Core.Engine
  ( EngineError(..)
  , prepareRound
  , observe
  , resolveRound
  , authorized
  , executor
  ) where

import Prelude

import Core.Market as Market
import Core.Represent as Represent
import Core.Num (Q)
import Core.Num as Q
import Core.Types (BehaviorDomain, CollectiveProcedureId, EntityKind(..), Ratio, Capability(..), CapacityRef, Entity, EntityId, Id(..), Intent(..), MarketId, Proposal, ProposalId, ResourceId, Rule(..), RuleChange(..), RuleId, RuleKind(..), SignedProposal, Transferability(..), TransformSpec, Transformation, TransformationId)
import Core.World (Delivery, Event(..), RoundResult, ShareSource(..), Target(..), View, WorldState)
import Core.Query (capacityHeld, holding, lookupQ)
import Data.Array (all, any, concat, concatMap, elem, filter, find, foldl, fromFoldable, notElem, nub, null, partition, sortWith, zipWith)
import Data.Either (Either(..), either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.FunctorWithIndex (mapWithIndex)

data EngineError = UnknownReference String | NegativeBalance EntityId ResourceId

derive instance Eq EngineError
instance Show EngineError where
  show (UnknownReference r) = "unknown reference: " <> r
  show (NegativeBalance e r) = "negative balance: " <> show e <> " " <> show r

-- Phase 1: prepare ------------------------------------------------------------

-- | Apply deliveries due this round and rule changes scheduled at the last boundary.
prepareRound :: WorldState -> WorldState
prepareRound w =
  let
    { yes: due, no: later } = partition (\d -> d.round <= w.round) w.deliveries
    withDeliveries = foldl (\acc d -> acc { entities = Map.update (Just <<< deliver d) d.entity acc.entities }) w due
    rules = foldl (\rs (Tuple id r) -> maybe (Map.delete id rs) (\x -> Map.insert id x rs) r) w.rules (Map.toUnfoldable w.scheduled :: Array _)
    ruleEvents = map (\(Tuple id r) -> maybe (Repealed { round: w.round, id }) (\x -> Enacted { round: w.round, id, rule: x }) r) (Map.toUnfoldable w.scheduled)
    deliveryEvents = map (\d -> Delivered { round: w.round, entity: d.entity, target: d.target, quantity: d.quantity }) due
  in
    withDeliveries { deliveries = later, rules = rules, scheduled = Map.empty, events = w.events <> deliveryEvents <> ruleEvents }

deliver :: Delivery -> Entity -> Entity
deliver d e = case d.target of
  Holding r -> e { accounts = bump r e.accounts }
  CapacityOf c -> e { capacities = bump c e.capacities }
  where
  bump :: forall k. Ord k => k -> Map k Q -> Map k Q
  bump k = Map.alter (Just <<< max Q.zero <<< Q.add d.quantity <<< fromMaybe Q.zero) k

-- Phase 2: observe ------------------------------------------------------------

-- | The beginning-of-round View of one Entity. `toSign` is filled by the round runner.
observe :: WorldState -> Entity -> View
observe w self =
  { round: w.round
  , self
  , prices: map (\m -> Market.marginal m.cost m.position) w.markets
  , rules: fromFoldable (Map.values w.rules)
  , events: w.events
  , factors: fromMaybe Map.empty (Map.lookup self.id w.factors)
  , toSign: []
  }

-- Authority -------------------------------------------------------------------

-- | The Entity whose resources or authority an Intent uses.
executor :: Proposal -> Intent -> EntityId
executor p = case _ of
  Transfer t -> t.from
  Transform t -> t.by
  _ -> p.author

-- | Whether `caps` authorize `who` to execute the Intent (§2.4).
authorized :: Array Capability -> EntityId -> Intent -> Boolean
authorized caps who = case _ of
  Trade o -> MayTrade o.market `elem` caps
  Transfer t -> MayTransfer t.resource `elem` caps && t.from == who
  Transform t -> MayTransform t.transformation `elem` caps && t.by == who
  ChangeRule (Enact r) -> MayEnact (kindOf r) `elem` caps
  ChangeRule (Repeal _) -> any isEnact caps
  where
  isEnact (MayEnact _) = true
  isEnact _ = false

kindOf :: Rule -> RuleKind
kindOf = case _ of
  Tariff _ -> TariffRule
  AccessBan _ -> AccessBanRule
  Withholding _ -> WithholdingRule

-- Phases 4–7: screen, resolve, commit -----------------------------------------

-- | A screened Proposal: signatures, shape, and authority checked; shipping cost attached.
type Item =
  { proposal :: Proposal
  , deal :: Boolean
  , tariffs :: Array { from :: EntityId, to :: EntityId, amount :: Q }
  , outcome :: Array Event -- the procedure outcome, for a passed procedure Proposal
  }

resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolveRound w inputs0 =
  let
    inputs = sortWith (_.proposal.id) inputs0
    screened = map (screen w) inputs
    items = concatMap (either (const []) pure) screened
    screenEvents = concatMap (either identity _.outcome) screened
    funded = fund w items
    estimates = Represent.estimate w (map _.proposal items)
    settled = circulate w (settle w funded.items)
  in
    commit w inputs (screenEvents <> funded.events <> estimates.events) estimates.factors settled

-- | Phase 4: signature completeness, shape, authority, and shipping rules.
screen :: WorldState -> SignedProposal -> Either (Array Event) Item
screen w { proposal: p, signatures } = case p.procedure of
  Just pid -> procedureProposal w p signed pid
  Nothing
    | not (all (_ `elem` signed) signers) -> Left [ Lapsed { round: w.round, proposal: p.id } ]
    | deal && any tradeOrRule p.intents -> reject "deals bundle only Transfers and Transforms"
    | otherwise -> case traverse (checkIntent w p signers) p.intents *> shipping w p of
        Left reason -> reject reason
        Right tariffs -> Right { proposal: p, deal, tariffs, outcome: [] }
  where
  signed = nub ([ p.author ] <> signatures)
  signers = nub ([ p.author ] <> p.signers)
  deal = isNothing p.procedure && any (_ /= p.author) p.signers
  reject reason = Left [ Rejected { round: w.round, proposal: p.id, reason } ]
  tradeOrRule = case _ of
    Trade _ -> true
    ChangeRule _ -> true
    _ -> false

-- | One Intent of an ordinary Proposal: its executor signs and is authorized, and the
-- | Intent respects transferability, rule scope, the facility's bounds, and owner signing.
checkIntent :: WorldState -> Proposal -> Array EntityId -> Intent -> Either String Unit
checkIntent w p signers i = case Map.lookup who w.entities of
  Nothing -> Left ("unknown entity " <> show who)
  Just e
    | not (who `elem` signers) -> Left (show who <> " must sign")
    | not (authorized e.capabilities who i) -> Left (show who <> " is not authorized")
    | otherwise -> transferable *> ruleScope e *> withinBounds *> ownerSigned
  where
  who = executor p i
  transformation t = Map.lookup t.transformation w.transformations
  transferable = case i of
    Transfer t -> case _.transferability <$> Map.lookup t.resource w.resources of
      Just NonTransferable -> Left (show t.resource <> " is not transferable")
      Nothing -> Left ("unknown resource " <> show t.resource)
      _ -> Right unit
    _ -> Right unit
  ruleScope e = case i of
    ChangeRule (Enact (Tariff r)) | r.jurisdiction /= e.id -> Left "a Tariff applies only to the enacting jurisdiction"
    ChangeRule (Enact (AccessBan r)) | r.jurisdiction /= e.id -> Left "an AccessBan applies only to the enacting jurisdiction"
    ChangeRule (Enact (Withholding r)) | not (Map.member r.capacity e.capacities) -> Left "withholding needs the capacity's owner"
    _ -> Right unit
  withinBounds = case i of
    Transform t | Just tr <- transformation t, t.quantity < tr.minimum -> Left (show t.transformation <> " order below its minimum")
    Transform t | Just tr <- transformation t, maybe false (t.quantity > _) tr.maximum -> Left (show t.transformation <> " order above its maximum")
    _ -> Right unit
  ownerSigned = case i of
    Transform t | Just tr <- transformation t, tr.ownerSigns -> case tr.capacity >>= owner w of
      Just o | o `elem` signers -> Right unit
      _ -> Left ("the owner of " <> show t.transformation <> "'s capacity must sign")
    _ -> Right unit

-- | A procedure Proposal (§2.2, §3.3): authored by the Aggregate that holds the procedure,
-- | it executes this round if the residual weight plus the weights of the members who signed
-- | exceeds the threshold, and lapses otherwise. Its Intents must fall within the procedure's
-- | `authorizes`.
procedureProposal :: WorldState -> Proposal -> Array EntityId -> CollectiveProcedureId -> Either (Array Event) Item
procedureProposal w p signed pid = case Map.lookup p.author w.entities of
  Just { kind: Aggregate a } | Just proc <- Map.lookup pid a.procedures ->
    let
      support = Q.add proc.residualWeight (Q.sumQ (map (\m -> lookupQ m proc.weights) (filter (_ /= p.author) signed)))
      passes = support > proc.threshold
      outcome = ProcedureOutcome { round: w.round, proposal: p.id, support, threshold: proc.threshold, executed: passes }
    in
      if not (all (\i -> executor p i == p.author && authorized proc.authorizes p.author i) p.intents) then
        Left [ Rejected { round: w.round, proposal: p.id, reason: "outside the procedure's authority" } ]
      else if not passes then Left [ outcome, Lapsed { round: w.round, proposal: p.id } ]
      else Right { proposal: p, deal: false, tariffs: [], outcome: [ outcome ] }
  _ -> Left [ Rejected { round: w.round, proposal: p.id, reason: "unknown procedure" } ]

-- | Cross-region Transfers must be bundled with a matching transport order signed by the
-- | transport capacity's owner, and are subject to access bans and tariffs.
shipping :: WorldState -> Proposal -> Either String (Array { from :: EntityId, to :: EntityId, amount :: Q })
shipping w p = traverse ship (filter crossRegion transfers)
  where
  transfers = concatMap
    ( case _ of
        Transfer t -> [ t ]
        _ -> []
    )
    p.intents
  crossRegion t = isJust (Map.lookup t.resource w.transport) && not (sameRegion t)
  sameRegion t = any (\m -> m.base == t.resource && t.from `elem` m.access && t.to `elem` m.access) (fromFoldable (Map.values w.markets) :: Array _)
  ship t = do
    tid <- note "no transport facility" (Map.lookup t.resource w.transport)
    let carried = Q.sumQ (map _.quantity (filter (\x -> x.transformation == tid && x.by == t.from) (transforms p)))
    when (carried /= t.quantity) (Left "a cross-region Transfer needs a matching transport order by the sender")
    let
      jf = jur w t.from
      jt = jur w t.to
    when (any (banned jf jt) (fromFoldable (Map.values w.rules) :: Array _)) (Left "shipment banned")
    pure { from: t.from, to: jt, amount: Q.sumQ (map (rate jf jt t.quantity) (fromFoldable (Map.values w.rules))) }
  banned a b = case _ of
    AccessBan r -> (r.jurisdiction == a && r.target == b) || (r.jurisdiction == b && r.target == a)
    _ -> false
  rate origin dest q = case _ of
    Tariff r | r.jurisdiction == dest && r.origin == origin -> Q.mul r.rate q
    _ -> Q.zero

transforms :: Proposal -> Array TransformSpec
transforms p = concatMap
  ( case _ of
      Transform t -> [ t ]
      _ -> []
  )
  p.intents

jur :: WorldState -> EntityId -> EntityId
jur w e = fromMaybe e (Map.lookup e w.jurisdiction)

owner :: WorldState -> CapacityRef -> Maybe EntityId
owner w ref = _.id <$> find (\e -> Map.member ref e.capacities) (fromFoldable (Map.values w.entities))

-- Escrow and joint feasibility (§3.2, §8.2) -----------------------------------

type Claim = { entity :: EntityId, resource :: ResourceId, amount :: Q }

-- | What an Item escrows from opening holdings at its full requested size.
claims :: WorldState -> Item -> Array Claim
claims w item = concatMap claim item.proposal.intents <> map (\t -> { entity: t.from, resource: w.numeraire, amount: t.amount }) item.tariffs
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
    Transfer t -> [ { entity: t.from, resource: t.resource, amount: t.quantity } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        [ { entity: t.by, resource: w.numeraire, amount: Q.mul (fullPrice w tr) t.quantity } ]
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
    ChangeRule _ -> []

-- | The uniform price of a facility order whose pool fills `q` units. Escrow prices the pool
-- | at its full quantity and settlement at the quantity actually filled, through this one
-- | function, so the escrowed price bounds the settled price by construction.
facilityPrice :: Transformation -> Q -> Q
facilityPrice tr q = maybe Q.zero (\c -> Market.averagePrice c Q.zero q) tr.cost

-- | The most units a facility's pool can fill: its capacity in units of `capacityUse`.
poolQuantity :: WorldState -> Transformation -> Q
poolQuantity w tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Q.div (capacityOf w ref) tr.capacityUse
  _ -> maybe Q.zero (\c -> c.domain.hi) tr.cost

-- | A facility's uniform price at full capacity, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = facilityPrice tr (poolQuantity w tr)

-- | Effective capacity: the owner's capacity less withheld units.
capacityOf :: WorldState -> CapacityRef -> Q
capacityOf w ref =
  let
    held = maybe Q.zero (\o -> capacityHeld w o ref) (owner w ref)
    withheld = Q.sumQ
      ( map
          ( case _ of
              Withholding r | r.capacity == ref -> r.units
              _ -> Q.zero
          )
          (fromFoldable (Map.values w.rules))
      )
  in
    max Q.zero (Q.sub held withheld)

-- | Reject deals a party cannot fund; scale an Entity's own actions pro rata to what it can
-- | fund. Deals are never scaled.
fund :: WorldState -> Array Item -> { items :: Array { item :: Item, scale :: Q }, events :: Array Event }
fund w items =
  let
    { yes: deals, no: singles } = partition _.deal items
    total xs = Map.fromFoldableWith Q.add (map (\c -> Tuple (Tuple c.entity c.resource) c.amount) (concatMap (claims w) xs))
    dealDemand = total deals
    short (Tuple e r) amt = amt > holding w e r
    shortKeys = Map.keys (Map.filterWithKey short dealDemand)
    unfunded d = any (\c -> Tuple c.entity c.resource `elem` fromFoldable shortKeys) (claims w d)
    { yes: dropped, no: kept } = partition unfunded deals
    keptDemand = total kept
    singleDemand = total singles
    room key@(Tuple e r) = Q.sub (holding w e r) (lookupQ key keptDemand)
    ratio key amt = if amt > room key then Q.div (max Q.zero (room key)) amt else Q.fromInt 1
    entityScale = Map.fromFoldableWith min (map (\(Tuple key amt) -> Tuple (fst key) (ratio key amt)) (Map.toUnfoldable singleDemand :: Array _))
    scaleOf item = fromMaybe (Q.fromInt 1) (Map.lookup item.proposal.author entityScale)
  in
    { items: map (\i -> { item: i, scale: Q.fromInt 1 }) kept <> map (\i -> { item: i, scale: scaleOf i }) singles
    , events: map (\d -> Rejected { round: w.round, proposal: d.proposal.id, reason: "a party cannot fund the deal" }) dropped
    }

-- Settlement (§8.1 phase 6) ---------------------------------------------------

data Account = Ent EntityId | Mkt MarketId

derive instance Eq Account
derive instance Ord Account

-- | Net changes to every account for the round, applied together at commit.
type Ledger = Map (Tuple Account ResourceId) Q

move :: Account -> Account -> ResourceId -> Q -> Ledger -> Ledger
move from to r q = adjust to r q <<< adjust from r (Q.neg q)

adjust :: Account -> ResourceId -> Q -> Ledger -> Ledger
adjust a r q = Map.insertWith Q.add (Tuple a r) q

type Settlement =
  { ledger :: Ledger
  , positions :: Map MarketId Q
  , deliveries :: Array Delivery
  , scheduled :: Map RuleId (Maybe Rule)
  , events :: Array Event
  }

type Scaled = { item :: Item, scale :: Q }

settle :: WorldState -> Array Scaled -> Settlement
settle w scaled0 =
  let
    facility = facilities w scaled0
    scaled = filter (\s -> s.item.proposal.id `notElem` facility.rejected) scaled0
    start = { ledger: Map.empty, positions: Map.empty, deliveries: [], scheduled: Map.empty, events: facility.events }
  in
    foldl (\acc f -> f acc) start
      [ markets w scaled
      , \acc -> foldl (transforms' w facility) acc scaled
      , \acc -> foldl (transfersAndRules w) acc scaled
      ]

-- | Fill facility orders per capacity. Deals that would exceed a capacity are rejected as a
-- | unit; single-author orders share what remains pro rata in whole units.
-- | Allocate every facility's capacity. Facilities sharing a capacity form one group; an
-- | uncapped facility is its own group.
facilities :: WorldState -> Array Scaled -> Facilities
facilities w scaled = foldl (allocate w) { fills: Map.empty, totals: Map.empty, rejected: [], events: [] } (Map.toUnfoldable groups :: Array _)
  where
  groups = groupBy (\o -> maybe (Left o.tid) Right o.capacity) (facilityOrders w scaled)

type FacilityOrder =
  { key :: Tuple ProposalId Int
  , deal :: Boolean
  , pid :: ProposalId
  , quantity :: Q
  , use :: Q
  , capacity :: Maybe CapacityRef
  , tid :: TransformationId
  }

-- | Every Transform, scaled by its Proposal's funding.
facilityOrders :: WorldState -> Array Scaled -> Array FacilityOrder
facilityOrders w = concatMap \s -> concat (mapWithIndex (order s) s.item.proposal.intents)
  where
  order s i = case _ of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations ->
      [ { key: Tuple s.item.proposal.id i, deal: s.item.deal, pid: s.item.proposal.id, quantity: Q.mul s.scale t.quantity, use: tr.capacityUse, capacity: tr.capacity, tid: t.transformation } ]
    _ -> []

-- | One capacity group: deals fill in full or are rejected as a unit; single orders share
-- | the remaining capacity pro rata, in whole units of their own orders.
allocate :: WorldState -> Facilities -> Tuple (Either TransformationId CapacityRef) (Array FacilityOrder) -> Facilities
allocate w acc (Tuple ref os) =
  { fills: Map.union acc.fills (Map.fromFoldable fills)
  , totals: foldl (\m (Tuple tid q) -> Map.insertWith Q.add tid q m) acc.totals (zipWith (\o (Tuple _ q) -> Tuple o.tid q) (if overflow then singles else deals <> singles) fills)
  , rejected: acc.rejected <> rejectedDeals
  , events: acc.events <> map (\pid -> Rejected { round: w.round, proposal: pid, reason: "facility capacity exceeded" }) rejectedDeals
  }
  where
  cap = either (const (Q.sumQ (map _.quantity os))) (capacityOf w) ref
  { yes: deals, no: singles } = partition _.deal os
  used o = Q.mul o.use o.quantity
  dealTotal = Q.sumQ (map used deals)
  overflow = dealTotal > cap
  rejectedDeals = if overflow then nub (map _.pid deals) else []
  room = if overflow then cap else Q.sub cap dealTotal
  singleFills
    | Q.sumQ (map used singles) > room = zipWith (\o u -> if o.use == Q.zero then o.quantity else Q.div u o.use) singles (Q.proRataWhole room (map used singles))
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
  orders = concatMap
    ( \s -> concatMap
        ( case _ of
            Trade o -> [ { market: o.market, entity: s.item.proposal.author, pid: s.item.proposal.id, quantity: Q.mul s.scale o.quantity, limitPrice: o.limitPrice } ]
            _ -> []
        )
        s.item.proposal.intents
    )
    scaled
  clearOne acc (Tuple id m) =
    let
      os = sortWith (\o -> Tuple o.entity o.pid) (filter (\o -> o.market == id) orders)
      lo = m.cost.domain.lo
      hi = m.cost.domain.hi
      bounds = { minNet: Q.sub lo m.position, maxNet: min (Q.sub hi m.position) (lookupQ m.base m.reserves) }
      r = Market.clear m.cost m.position bounds os
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
            , events = filled.events <> map (\o -> Rejected { round: w.round, proposal: o.pid, reason: "limit price not met" }) r.rejected
            }

transforms' :: WorldState -> Facilities -> Settlement -> Scaled -> Settlement
transforms' w facility acc s = foldl one acc (mapWithIndex Tuple s.item.proposal.intents)
  where
  -- facilities sharing a capacity and a cost function share one uniform price
  pricedTotal tr = Q.sumQ (map (\(Tuple _ q) -> q) (filter (\(Tuple tid _) -> maybe false (\x -> x.capacity == tr.capacity && x.cost == tr.cost) (Map.lookup tid w.transformations)) (Map.toUnfoldable facility.totals)))
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
    Transform t -> case Tuple (Map.lookup (Tuple s.item.proposal.id i) facility.fills) (Map.lookup t.transformation w.transformations) of
      Tuple (Just q) (Just tr) | q > Q.zero ->
        let
          owed = Q.mul (facilityPrice tr (pricedTotal tr)) q
          consume l = foldl (\l' (Tuple r u) -> adjust (Ent t.by) r (Q.neg (Q.mul u q)) l') l (Map.toUnfoldable tr.inputs :: Array _)
          outs = map (\(Tuple r u) -> { round: w.round + tr.delay, entity: t.by, target: Holding r, quantity: Q.mul u q }) (Map.toUnfoldable tr.outputs)
          { yes: now, no: later } = partition (\d -> d.round == w.round) outs
          produce l = foldl
            ( \l' d -> case d.target of
                Holding r -> adjust (Ent d.entity) r d.quantity l'
                CapacityOf _ -> l'
            )
            l
            now
        in
          case pay w t.by tr.payee w.numeraire owed a of
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

-- Circulation (§6) -------------------------------------------------------------

-- | The recurring cycle, evaluated on the round's batch result: production (delivered next
-- | round), variable revenue shares, operating requirements with shortfall penalties, fixed
-- | flows, and decay. One function over the Circulation data; no rule-specific paths.
circulate :: WorldState -> Settlement -> Settlement
circulate w = decay w <<< fixedFlows w <<< requirements w <<< revenueShares w <<< production w

-- | Group items into arrays by key, keeping their order.
groupBy :: forall k a. Ord k => (a -> k) -> Array a -> Map k (Array a)
groupBy key xs = Map.fromFoldableWith (flip (<>)) (map (\x -> Tuple (key x) [ x ]) xs)

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
  paid = min owed (max Q.zero (net w from resource st))
  short = Q.sub owed paid
  shortEvents = if short > Q.zero then [ Shortfall { round: w.round, entity: from, resource, quantity: short } ] else []

-- | A transfer between Entities through `pay`: the recorded quantity is what was paid.
transfer :: WorldState -> EntityId -> EntityId -> ResourceId -> Q -> Settlement -> Settlement
transfer w from to resource quantity st
  | quantity == Q.zero = st
  | otherwise = case pay w from (Just to) resource quantity st of
      Tuple paid st' -> st' { events = st'.events <> [ Transferred { round: w.round, from, to, resource, quantity: paid } ] }

-- | What an Entity holds after the settlement so far.
net :: WorldState -> EntityId -> ResourceId -> Settlement -> Q
net w e r st = Q.add (holding w e r) (lookupQ (Tuple (Ent e) r) st.ledger)

-- | The base Resource a Market trades.
marketBase :: WorldState -> MarketId -> Maybe ResourceId
marketBase w m = _.base <$> Map.lookup m w.markets

-- | Sum a quantity over the round's events.
sumEvents :: (Event -> Array Q) -> Settlement -> Q
sumEvents f st = Q.sumQ (concatMap f st.events)

-- | Production capacity becomes a delivery in the next round.
production :: WorldState -> Settlement -> Settlement
production w st = st { deliveries = st.deliveries <> map scheduled w.circulation.production }
  where
  scheduled p = { round: w.round + 1, entity: p.entity, target: Holding p.resource, quantity: capacityHeld w p.entity p.capacity }

-- | Variable shares of sales revenue or payee receipts, paid in the numeraire.
revenueShares :: WorldState -> Settlement -> Settlement
revenueShares w st0 = foldl pay st0 w.circulation.shares
  where
  pay st sh = transfer w sh.from sh.to w.numeraire (Q.mul sh.share (base sh)) st
  base sh = case sh.source of
    SalesOf r -> max Q.zero $ sumEvents
      ( case _ of
          Traded t | t.entity == sh.from && marketBase w t.market == Just r -> [ Q.neg (Q.mul t.price t.quantity) ]
          _ -> []
      )
      st0
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
      purchases = sumEvents
        ( case _ of
            Traded t | t.entity == rq.entity && marketBase w t.market == Just rq.resource && t.quantity > Q.zero -> [ t.quantity ]
            _ -> []
        )
        st0
      needed = fromMaybe purchases rq.amount
      consumed = min needed (max Q.zero (net w rq.entity rq.resource acc.st))
      short = Q.sub needed consumed
      when' cond ev = if cond then [ ev ] else []
    in
      { st: acc.st
          { ledger = adjust (Ent rq.entity) rq.resource (Q.neg consumed) acc.st.ledger
          , events = acc.st.events
              <> when' (consumed > Q.zero) (Consumed { round: w.round, entity: rq.entity, resource: rq.resource, quantity: consumed })
              <> when' (short > Q.zero) (Shortfall { round: w.round, entity: rq.entity, resource: rq.resource, quantity: short })
          }
      , penalties: if short > Q.zero then Map.insertWith Q.add rq.entity (Q.mul rq.penalty short) acc.penalties else acc.penalties
      }

-- | Fixed flows, each Entity's incoming flows reduced pro rata by its penalty (payers keep it).
fixedFlows :: WorldState -> Penalised -> Settlement
fixedFlows w { st, penalties } = foldl (\acc (Tuple f amount) -> transfer w f.from f.to f.resource amount acc) st (concatMap reduce byRecipient)
  where
  byRecipient = Map.toUnfoldable (groupBy _.to w.circulation.flows) :: Array _
  reduce (Tuple to fs) =
    let
      incoming = Q.sumQ (map _.amount fs)
      cut = min incoming (lookupQ to penalties)
    in
      zipWith (\f k -> Tuple f (Q.sub f.amount k)) fs (Q.proRata cut (map _.amount fs))

-- | Decay of every decaying Resource held.
decay :: WorldState -> Settlement -> Settlement
decay w st0 = foldl lose st0 holdings
  where
  holdings = concatMap (\e -> map (Tuple e.id) (fromFoldable (Map.keys e.accounts))) (fromFoldable (Map.values w.entities))
  lose acc (Tuple e r) = case Map.lookup r w.resources >>= _.decay of
    Just d ->
      let
        lost = Q.mul d.rate (max Q.zero (net w e r acc))
      in
        if lost == Q.zero then acc
        else acc { ledger = adjust (Ent e) r (Q.neg lost) acc.ledger, events = acc.events <> [ Decayed { round: w.round, entity: e, resource: r, quantity: lost } ] }
    Nothing -> acc

-- Phase 7: commit -------------------------------------------------------------

commit :: WorldState -> Array SignedProposal -> Array Event -> Map EntityId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  entities <- traverse applyEntity w.entities
  let
    markets' = mapWithIndex
      ( \id m -> m
          { reserves = applyHoldings (Mkt id) m.reserves
          , position = fromMaybe m.position (Map.lookup id st.positions)
          }
      )
      w.markets
    events = screenEvents <> st.events
    world = w
      { round = w.round + 1
      , entities = entities
      , markets = markets'
      , deliveries = w.deliveries <> st.deliveries
      , scheduled = Map.union st.scheduled w.scheduled
      , factors = factors
      , events = w.events <> events
      }
  pure { world, record: { round: w.round, inputs, events } }
  where
  applyHoldings acct hs = foldl (\h (Tuple (Tuple a r) q) -> if a == acct then Map.insertWith Q.add r q h else h) hs (Map.toUnfoldable st.ledger :: Array _)
  applyEntity e =
    let
      accounts = applyHoldings (Ent e.id) e.accounts
    in
      case find (\(Tuple _ q) -> q < Q.zero) (Map.toUnfoldable accounts :: Array _) of
        Just (Tuple r _) -> Left (NegativeBalance e.id r)
        Nothing -> Right e { accounts = accounts }
