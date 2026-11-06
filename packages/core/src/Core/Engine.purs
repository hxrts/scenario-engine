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
import Data.Array (all, any, concatMap, elem, filter, find, foldl, fromFoldable, mapWithIndex, notElem, nub, null, partition, sortWith, zipWith)
import Data.Either (Either(..), either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

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
screen w { proposal: p, signatures } =
  let
    signed = nub ([ p.author ] <> signatures)
    deal = isNothing p.procedure && any (_ /= p.author) p.signers
    lapse = Left [ Lapsed { round: w.round, proposal: p.id } ]
    reject reason = Left [ Rejected { round: w.round, proposal: p.id, reason } ]
    signers = nub ([ p.author ] <> p.signers)
    tradeOrRule = case _ of
      Trade _ -> true
      ChangeRule _ -> true
      _ -> false
  in
    case p.procedure of
      Just pid -> procedureProposal w p signed pid
      Nothing ->
        if not (all (_ `elem` signed) signers) then lapse
        else if deal && any tradeOrRule p.intents then reject "deals bundle only Transfers and Transforms"
        else ordinary signers deal
  where
  ordinary signers deal =
    let
      reject reason = Left [ Rejected { round: w.round, proposal: p.id, reason } ]
    in
      case traverse (check signers) p.intents of
        Left reason -> reject reason
        Right _ -> case shipping w p of
          Left reason -> reject reason
          Right tariffs -> Right { proposal: p, deal, tariffs, outcome: [] }
  check signers i =
    let
      who = executor p i
    in
      case Map.lookup who w.entities of
        Nothing -> Left ("unknown entity " <> show who)
        Just e
          | not (who `elem` signers) -> Left (show who <> " must sign")
          | not (authorized e.capabilities who i) -> Left (show who <> " is not authorized")
          | otherwise -> transferable i *> ruleScope e i *> atLeastMinimum i *> ownerSigned signers i
  transferable = case _ of
    Transfer t -> case _.transferability <$> Map.lookup t.resource w.resources of
      Just NonTransferable -> Left (show t.resource <> " is not transferable")
      Nothing -> Left ("unknown resource " <> show t.resource)
      _ -> Right unit
    _ -> Right unit
  ownerSigned signers = case _ of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations, tr.ownerSigns ->
      case tr.capacity >>= owner w of
        Just o | o `elem` signers -> Right unit
        _ -> Left ("the owner of " <> show t.transformation <> "'s capacity must sign")
    _ -> Right unit
  atLeastMinimum = case _ of
    Transform t | maybe false (\tr -> t.quantity < tr.minimum) (Map.lookup t.transformation w.transformations) ->
      Left (show t.transformation <> " order below its minimum")
    Transform t | maybe false (\tr -> maybe false (t.quantity > _) tr.maximum) (Map.lookup t.transformation w.transformations) ->
      Left (show t.transformation <> " order above its maximum")
    _ -> Right unit
  ruleScope e = case _ of
    ChangeRule (Enact (Tariff r)) | r.jurisdiction /= e.id -> Left "a Tariff applies only to the enacting jurisdiction"
    ChangeRule (Enact (AccessBan r)) | r.jurisdiction /= e.id -> Left "an AccessBan applies only to the enacting jurisdiction"
    ChangeRule (Enact (Withholding r)) | not (Map.member r.capacity e.capacities) -> Left "withholding needs the capacity's owner"
    _ -> Right unit

-- | A procedure Proposal (§2.2, §3.3): authored by the Aggregate that holds the procedure,
-- | it executes this round if the residual weight plus the weights of the members who signed
-- | exceeds the threshold, and lapses otherwise. Its Intents must fall within the procedure's
-- | `authorizes`.
procedureProposal :: WorldState -> Proposal -> Array EntityId -> CollectiveProcedureId -> Either (Array Event) Item
procedureProposal w p signed pid = case Map.lookup p.author w.entities of
  Just { kind: Aggregate a } | Just proc <- Map.lookup pid a.procedures ->
    let
      support = Q.add proc.residualWeight (Q.sumQ (map (\m -> fromMaybe Q.zero (Map.lookup m proc.weights)) (filter (_ /= p.author) signed)))
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
        | otherwise -> [ { entity: p.author, resource: m.base, amount: Q.sub Q.zero o.quantity } ]
    Transfer t -> [ { entity: t.from, resource: t.resource, amount: t.quantity } ]
    Transform t -> case Map.lookup t.transformation w.transformations of
      Nothing -> []
      Just tr ->
        [ { entity: t.by, resource: w.numeraire, amount: Q.mul (fullPrice w tr) t.quantity } ]
          <> map (\(Tuple r q) -> { entity: t.by, resource: r, amount: Q.mul q t.quantity }) (Map.toUnfoldable tr.inputs)
    ChangeRule _ -> []

-- | A facility's uniform price at full capacity, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = maybe Q.zero (\c -> Market.averagePrice c Q.zero (fromMaybe c.domain.hi (capacityOf w <$> tr.capacity))) tr.cost

-- | Effective capacity: the owner's capacity less withheld units.
capacityOf :: WorldState -> CapacityRef -> Q
capacityOf w ref =
  let
    held = maybe Q.zero (\o -> fromMaybe Q.zero (Map.lookup o w.entities >>= \e -> Map.lookup ref e.capacities)) (owner w ref)
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

available :: WorldState -> EntityId -> ResourceId -> Q
available w e r = fromMaybe Q.zero (Map.lookup e w.entities >>= \x -> Map.lookup r x.accounts)

-- | Reject deals a party cannot fund; scale an Entity's own actions pro rata to what it can
-- | fund. Deals are never scaled.
fund :: WorldState -> Array Item -> { items :: Array { item :: Item, scale :: Q }, events :: Array Event }
fund w items =
  let
    { yes: deals, no: singles } = partition _.deal items
    total xs = Map.fromFoldableWith Q.add (map (\c -> Tuple (Tuple c.entity c.resource) c.amount) (concatMap (claims w) xs))
    dealDemand = total deals
    short key amt = amt > available w (fst' key) (snd' key)
    shortKeys = Map.keys (Map.filterWithKey short dealDemand)
    unfunded d = any (\c -> Tuple c.entity c.resource `elem` fromFoldable shortKeys) (claims w d)
    { yes: dropped, no: kept } = partition unfunded deals
    keptDemand = total kept
    singleDemand = total singles
    room key = Q.sub (available w (fst' key) (snd' key)) (fromMaybe Q.zero (Map.lookup key keptDemand))
    ratio key amt = if amt > room key then Q.div (max Q.zero (room key)) amt else Q.fromInt 1
    entityScale = Map.fromFoldableWith min (map (\(Tuple key amt) -> Tuple (fst' key) (ratio key amt)) (Map.toUnfoldable singleDemand :: Array _))
    scaleOf item = fromMaybe (Q.fromInt 1) (Map.lookup item.proposal.author entityScale)
  in
    { items: map (\i -> { item: i, scale: Q.fromInt 1 }) kept <> map (\i -> { item: i, scale: scaleOf i }) singles
    , events: map (\d -> Rejected { round: w.round, proposal: d.proposal.id, reason: "a party cannot fund the deal" }) dropped
    }
  where
  fst' (Tuple a _) = a
  snd' (Tuple _ b) = b

-- Settlement (§8.1 phase 6) ---------------------------------------------------

data Account = Ent EntityId | Mkt MarketId

derive instance Eq Account
derive instance Ord Account

-- | Net changes to every account for the round, applied together at commit.
type Ledger = Map (Tuple Account ResourceId) Q

move :: Account -> Account -> ResourceId -> Q -> Ledger -> Ledger
move from to r q = adjust to r q <<< adjust from r (Q.sub Q.zero q)

adjust :: Account -> ResourceId -> Q -> Ledger -> Ledger
adjust a r q = Map.alter (Just <<< Q.add q <<< fromMaybe Q.zero) (Tuple a r)

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
facilities :: WorldState -> Array Scaled -> Facilities
facilities w scaled =
  let
    orders = concatMap
      ( \s -> mapWithIndex (\i x -> { s, i, x }) s.item.proposal.intents # concatMap
          ( \o -> case o.x of
              Transform t -> case Map.lookup t.transformation w.transformations of
                Just tr -> [ { key: Tuple s.item.proposal.id o.i, deal: s.item.deal, pid: s.item.proposal.id, by: t.by, quantity: Q.mul s.scale t.quantity, use: tr.capacityUse, capacity: tr.capacity, tid: t.transformation } ]
                Nothing -> []
              _ -> []
          )
      )
      scaled
    -- facilities sharing a capacity form one group; an uncapped facility is its own group
    groups = Map.fromFoldableWith (flip (<>)) (map (\o -> Tuple (maybe (Left o.tid) Right o.capacity) [ o ]) orders)
    step acc (Tuple ref os) =
      let
        cap = either (const (Q.sumQ (map _.quantity os))) (capacityOf w) ref
        { yes: deals, no: singles } = partition _.deal os
        used o = Q.mul o.use o.quantity
        dealTotal = Q.sumQ (map used deals)
        overflow = dealTotal > cap
        rejectedDeals = if overflow then nub (map _.pid deals) else []
        room = if overflow then cap else Q.sub cap dealTotal
        wanted = Q.sumQ (map used singles)
        -- singles share the remaining capacity pro rata, in whole units of their own orders
        singleFills =
          if wanted > room then zipWith (\o u -> if o.use == Q.zero then o.quantity else Q.div u o.use) singles (Q.proRataWhole room (map used singles))
          else map _.quantity singles
        fills = (if overflow then [] else map (\o -> Tuple o.key o.quantity) deals) <> zipWith (\o q -> Tuple o.key q) singles singleFills
      in
        { fills: Map.union acc.fills (Map.fromFoldable fills)
        , totals: foldl (\m (Tuple tid q) -> Map.insertWith Q.add tid q m) acc.totals (zipWith (\o (Tuple _ q) -> Tuple o.tid q) (if overflow then singles else deals <> singles) fills)
        , rejected: acc.rejected <> rejectedDeals
        , events: acc.events <> map (\pid -> Rejected { round: w.round, proposal: pid, reason: "facility capacity exceeded" }) rejectedDeals
        }
  in
    foldl step { fills: Map.empty, totals: Map.empty, rejected: [], events: [] } (Map.toUnfoldable groups :: Array _)
  where
  snd' (Tuple _ b) = b

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
      bounds = { minNet: Q.sub lo m.position, maxNet: min (Q.sub hi m.position) (fromMaybe Q.zero (Map.lookup m.base m.reserves)) }
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
      else (foldl fill acc r.fills)
        { positions = Map.insert id (Q.add m.position r.net) acc.positions
        , events = (foldl fill acc r.fills).events <> map (\o -> Rejected { round: w.round, proposal: o.pid, reason: "limit price not met" }) r.rejected
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
          [ change ] <> maybe [] (\n -> [ change { round = start + n, quantity = Q.sub Q.zero change.quantity } ]) e.rounds
    )
    tr.effects
  one a (Tuple i intent) = case intent of
    Transform t -> case Tuple (Map.lookup (Tuple s.item.proposal.id i) facility.fills) (Map.lookup t.transformation w.transformations) of
      Tuple (Just q) (Just tr) | q > Q.zero ->
        let
          price = maybe Q.zero (\c -> Market.averagePrice c Q.zero (pricedTotal tr)) tr.cost
          paid = Q.mul price q
          pay l = maybe (adjust (Ent t.by) w.numeraire (Q.sub Q.zero paid) l) (\p -> move (Ent t.by) (Ent p) w.numeraire paid l) tr.payee
          consume l = foldl (\l' (Tuple r u) -> adjust (Ent t.by) r (Q.sub Q.zero (Q.mul u q)) l') l (Map.toUnfoldable tr.inputs :: Array _)
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
          a
            { ledger = produce (consume (pay a.ledger))
            , deliveries = a.deliveries <> later <> effects t tr q
            , events = a.events <> [ Transformed { round: w.round, by: t.by, transformation: t.transformation, quantity: q, paid } ]
            }
      _ -> a
    _ -> a

-- | Transfers, tariffs, and rule changes of surviving Items.
transfersAndRules :: WorldState -> Settlement -> Scaled -> Settlement
transfersAndRules w acc s = foldl one withTariffs (mapWithIndex Tuple p.intents)
  where
  p = s.item.proposal
  withTariffs = acc { ledger = foldl (\l t -> move (Ent t.from) (Ent t.to) w.numeraire t.amount l) acc.ledger s.item.tariffs }
  one a (Tuple i intent) = case intent of
    Transfer t ->
      let
        q = Q.mul s.scale t.quantity
      in
        a
          { ledger = move (Ent t.from) (Ent t.to) t.resource q a.ledger
          , events = a.events <> [ Transferred { round: w.round, from: t.from, to: t.to, resource: t.resource, quantity: q } ]
          }
    ChangeRule (Enact r) -> a { scheduled = Map.insert (ruleId i) (Just r) a.scheduled }
    ChangeRule (Repeal id) -> a { scheduled = Map.insert id Nothing a.scheduled }
    _ -> a
  ruleId i = Id (show p.id <> "/rule-" <> show i)

-- Circulation (§6) -------------------------------------------------------------

-- | The recurring cycle, evaluated on the round's batch result: production (delivered next
-- | round), variable revenue shares, operating requirements with shortfall penalties, fixed
-- | flows, and decay. One function over the Circulation data; no rule-specific paths.
circulate :: WorldState -> Settlement -> Settlement
circulate w st0 = decay (fixed (requirements (shares (production st0))))
  where
  c = w.circulation
  net e r st = Q.add (available w e r) (fromMaybe Q.zero (Map.lookup (Tuple (Ent e) r) st.ledger))
  production st = st
    { deliveries = st.deliveries <> map (\p -> { round: w.round + 1, entity: p.entity, target: Holding p.resource, quantity: capacity p.entity p.capacity }) c.production }
  capacity e ref = fromMaybe Q.zero (Map.lookup e w.entities >>= \x -> Map.lookup ref x.capacities)

  shares st = foldl
    ( \acc sh ->
        let
          base = case sh.source of
            SalesOf r -> max Q.zero
              ( Q.sumQ
                  ( concatMap
                      ( case _ of
                          Traded t | t.entity == sh.from && marketBase t.market == Just r -> [ Q.sub Q.zero (Q.mul t.price t.quantity) ]
                          _ -> []
                      )
                      st.events
                  )
              )
            PayeeReceipts -> Q.sumQ
              ( concatMap
                  ( case _ of
                      Transformed t | payeeOf t.transformation == Just sh.from -> [ t.paid ]
                      _ -> []
                  )
                  st.events
              )
          amount = Q.mul sh.share base
        in
          if amount == Q.zero then acc else acc { ledger = move (Ent sh.from) (Ent sh.to) w.numeraire amount acc.ledger, events = acc.events <> [ Transferred { round: w.round, from: sh.from, to: sh.to, resource: w.numeraire, quantity: amount } ] }
    )
    st
    c.shares
  marketBase m = _.base <$> Map.lookup m w.markets
  payeeOf t = Map.lookup t w.transformations >>= _.payee

  -- requirements, returning the penalty owed to each Entity's incoming fixed flows
  requirements st = foldl
    ( \acc rq ->
        let
          purchases = Q.sumQ
            ( concatMap
                ( case _ of
                    Traded t | t.entity == rq.entity && marketBase t.market == Just rq.resource && t.quantity > Q.zero -> [ t.quantity ]
                    _ -> []
                )
                st.events
            )
          needed = fromMaybe purchases rq.amount
          consumed = min needed (max Q.zero (net rq.entity rq.resource acc.st))
          short = Q.sub needed consumed
        in
          { st: acc.st
              { ledger = adjust (Ent rq.entity) rq.resource (Q.sub Q.zero consumed) acc.st.ledger
              , events = acc.st.events
                  <> (if consumed > Q.zero then [ Consumed { round: w.round, entity: rq.entity, resource: rq.resource, quantity: consumed } ] else [])
                  <> (if short > Q.zero then [ Shortfall { round: w.round, entity: rq.entity, resource: rq.resource, quantity: short } ] else [])
              }
          , penalties: if short > Q.zero then Map.insertWith Q.add rq.entity (Q.mul rq.penalty short) acc.penalties else acc.penalties
          }
    )
    { st, penalties: Map.empty }
    c.requirements

  -- fixed flows, each Entity's incoming flows reduced pro rata by its penalty (payers keep it)
  fixed { st, penalties } = foldl
    ( \acc (Tuple f amount) ->
        if amount == Q.zero then acc
        else acc { ledger = move (Ent f.from) (Ent f.to) f.resource amount acc.ledger, events = acc.events <> [ Transferred { round: w.round, from: f.from, to: f.to, resource: f.resource, quantity: amount } ] }
    )
    st
    reduced
    where
    reduced = concatMap
      ( \(Tuple to fs) ->
          let
            incoming = Q.sumQ (map _.amount fs)
            cut = min incoming (fromMaybe Q.zero (Map.lookup to penalties))
            cuts = Q.proRata cut (map _.amount fs)
          in
            zipWith (\f k -> Tuple f (Q.sub f.amount k)) fs cuts
      )
      (Map.toUnfoldable (Map.fromFoldableWith (flip (<>)) (map (\f -> Tuple f.to [ f ]) c.flows)) :: Array _)

  decay st = foldl
    ( \acc (Tuple e r) ->
        case Map.lookup r w.resources >>= _.decay of
          Just d ->
            let
              lost = Q.mul d.rate (max Q.zero (net e r acc))
            in
              if lost == Q.zero then acc
              else acc { ledger = adjust (Ent e) r (Q.sub Q.zero lost) acc.ledger, events = acc.events <> [ Decayed { round: w.round, entity: e, resource: r, quantity: lost } ] }
          Nothing -> acc
    )
    st
    (concatMap (\e -> map (Tuple e.id) (fromFoldable (Map.keys e.accounts))) (fromFoldable (Map.values w.entities)))

-- Phase 7: commit -------------------------------------------------------------

commit :: WorldState -> Array SignedProposal -> Array Event -> Map EntityId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  entities <- traverse applyEntity w.entities
  let
    markets' = mapWithKey'
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
  applyHoldings acct hs = foldl (\h (Tuple (Tuple a r) q) -> if a == acct then Map.alter (Just <<< Q.add q <<< fromMaybe Q.zero) r h else h) hs (Map.toUnfoldable st.ledger :: Array _)
  applyEntity e =
    let
      accounts = applyHoldings (Ent e.id) e.accounts
    in
      case find (\(Tuple _ q) -> q < Q.zero) (Map.toUnfoldable accounts :: Array _) of
        Just (Tuple r _) -> Left (NegativeBalance e.id r)
        Nothing -> Right e { accounts = accounts }
  mapWithKey' f m = Map.mapMaybeWithKey (\k v -> Just (f k v)) m
