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
  , authorizedIn
  , executor
  ) where

import Prelude

import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, holding, lookupQ, priceOf)
import Core.Represent as Represent
import Core.Types (BehaviorDomain, CapacityRef, Capability(..), CollectiveProcedureId, Entity, EntityId, EntityKind(..), Id(..), Intent(..), MarketId, Order, Proposal, ProposalId, Ratio, ResourceId, Rule(..), RuleChange(..), RuleId, RuleKind(..), RoundIndex, SignedProposal, Transferability(..), TransferSpec, TransformSpec, Transformation, TransformationId)
import Core.World (Delivery, Event(..), RoundResult, ShareSource(..), Target(..), View, WorldState)
import Data.Array (all, any, concat, concatMap, elem, filter, find, foldl, fromFoldable, length, last, mapMaybe, notElem, nub, null, partition, sortWith, zipWith)
import Data.Either (Either(..), either, note)
import Data.Int (fromString) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.FunctorWithIndex (mapWithIndex)

data EngineError
  = UnknownReference String
  | NegativeBalance EntityId ResourceId -- an invariant failure: settlement should make this unreachable
  | NegativeReserve MarketId ResourceId -- likewise, for Market reserves

derive instance Eq EngineError
instance Show EngineError where
  show (UnknownReference r) = "unknown reference: " <> r
  show (NegativeBalance e r) = "negative balance: " <> show e <> " " <> show r
  show (NegativeReserve m r) = "negative reserve: " <> show m <> " " <> show r

-- Phase 1: prepare ------------------------------------------------------------

-- | Apply deliveries due this round and rule changes scheduled at the last boundary. Idempotent:
-- | a prepared World State is unchanged by a second call, so `resolveRound` prepares its input
-- | itself and a caller that has already prepared it gets the same result.
prepareRound :: WorldState -> WorldState
prepareRound w =
  let
    { yes: due, no: later } = partition (\d -> d.round <= w.round) w.deliveries
    applied = foldl (applyDelivery w.round) { entities: w.entities, events: [] } due
    rules = foldl (\rs (Tuple id r) -> maybe (Map.delete id rs) (\x -> Map.insert id x rs) r) w.rules (Map.toUnfoldable w.scheduled :: Array _)
    ruleEvents = map (\(Tuple id r) -> maybe (Repealed { round: w.round, id }) (\x -> Enacted { round: w.round, id, rule: x }) r) (Map.toUnfoldable w.scheduled)
  in
    w
      { entities = applied.entities
      , deliveries = later
      , rules = rules
      , scheduled = Map.empty
      , events = w.events <> applied.events <> ruleEvents
      }

type Applied = { entities :: Map EntityId Entity, events :: Array Event }

-- | Apply one due delivery, clamped so that no holding or capacity goes negative. The Delivered
-- | event records the change actually applied, so reversing a delivery later restores exactly
-- | what was removed. A delivery to an Entity that does not exist applies nothing and records
-- | nothing.
applyDelivery :: RoundIndex -> Applied -> Delivery -> Applied
applyDelivery round acc d = case Map.lookup d.entity acc.entities of
  Nothing -> acc
  Just e ->
    let
      before = case d.target of
        Holding r -> lookupQ r e.accounts
        CapacityOf c -> lookupQ c e.capacities
      after = max Q.zero (Q.add before d.quantity)
      change = Q.sub after before
      updated = case d.target of
        Holding r -> e { accounts = Map.insert r after e.accounts }
        CapacityOf c -> e { capacities = Map.insert c after e.capacities }
    in
      if change == Q.zero then acc
      else
        { entities: Map.insert d.entity updated acc.entities
        , events: acc.events <> [ Delivered { round, entity: d.entity, target: d.target, quantity: change } ]
        }

-- Phase 2: observe ------------------------------------------------------------

-- | The beginning-of-round View of one Entity. `toSign` is filled by the round runner.
observe :: WorldState -> Entity -> View
observe w self =
  { round: w.round
  , self
  , prices: Map.mapMaybeWithKey (\id _ -> priceOf w id) w.markets
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

-- | Whether `caps` authorize `who` to execute the Intent (§2.4). A Repeal needs only some
-- | enactment capability here; `authorizedIn` also requires the capability for the repealed Rule's kind.
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

-- | `authorized`, with a Repeal also requiring enactment authority over the kind of the Rule it
-- | repeals. An unknown Rule is not authorized to be repealed.
authorizedIn :: WorldState -> Array Capability -> EntityId -> Intent -> Boolean
authorizedIn w caps who i = authorized caps who i && case i of
  ChangeRule (Repeal id) -> maybe false (\r -> MayEnact (kindOf r) `elem` caps) (Map.lookup id w.rules)
  _ -> true

kindOf :: Rule -> RuleKind
kindOf = case _ of
  Tariff _ -> TariffRule
  AccessBan _ -> AccessBanRule
  Withholding _ -> WithholdingRule

-- | Whether an Entity governs a Rule: a Tariff or AccessBan by its jurisdiction, a Withholding by
-- | the owner of its capacity. Enacting and repealing both require it.
governs :: WorldState -> EntityId -> Rule -> Boolean
governs w who = case _ of
  Tariff r -> r.jurisdiction == who
  AccessBan r -> r.jurisdiction == who
  Withholding r -> owner w r.capacity == Just who

-- | The capacity's owner: the first Entity holding it. Withholding and capacity lookups both use this.
owner :: WorldState -> CapacityRef -> Maybe EntityId
owner w ref = _.id <$> find (\e -> Map.member ref e.capacities) (fromFoldable (Map.values w.entities))

-- Phases 4–7: screen, resolve, commit -----------------------------------------

-- | A screened Proposal: signatures, shape, and authority checked; shipping cost attached.
type Item =
  { proposal :: Proposal
  , deal :: Boolean
  , tariffs :: Array { from :: EntityId, to :: EntityId, amount :: Q }
  , outcome :: Array Event -- the procedure outcome, for a passed procedure Proposal
  }

-- | Whose authority an Intent is checked against: the executor's own capabilities, for an
-- | ordinary Proposal, or the capabilities a procedure authorizes, for a procedure Proposal.
data Standing = OwnAuthority | ProcedureAuthority (Array Capability)

-- | Resolve one round. The World State is prepared first (Phase 1), so the caller passes it in
-- | either prepared or not; the result is the same.
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolveRound w0 inputs0 =
  let
    w = prepareRound w0
    inputs = sortWith submissionOrder inputs0
    ids = map (_.proposal.id) inputs
    -- a duplicated id is ambiguous, so every copy is rejected rather than picking one
    duplicated = nub (filter (\i -> length (filter (_ == i) ids) > 1) ids)
    screened = map (screen w duplicated) inputs
    items = concatMap (either (const []) pure) screened
    screenEvents = concatMap (either identity _.outcome) screened
    funded = fund w items
    -- estimates read the validated proposed quantities, before funding scales them (§7.4)
    estimates = Represent.estimate w (map _.proposal items)
    settled = circulate w (settle w funded.items)
  in
    commit w inputs (screenEvents <> funded.events <> estimates.events) estimates.factors settled

-- | The canonical order of Proposals: by author, then by the numeric index of the author's
-- | submission (the last segment of the id, so index 10 follows index 2), then by id.
submissionOrder :: SignedProposal -> Tuple String (Tuple Int String)
submissionOrder { proposal: p } = Tuple (show p.author) (Tuple index (show p.id))
  where
  index = fromMaybe 0 (Int.fromString (fromMaybe "" (last (split (Pattern "/") (show p.id)))))

-- | Phase 4: signature completeness, shape, authority, and shipping rules.
screen :: WorldState -> Array ProposalId -> SignedProposal -> Either (Array Event) Item
screen w duplicated { proposal: p, signatures } =
  let
    signed = nub ([ p.author ] <> signatures)
    signers = nub ([ p.author ] <> p.signers)
    deal = isNothing p.procedure && any (_ /= p.author) p.signers
    reject reason = Left [ Rejected { round: w.round, proposal: p.id, reason } ]
    tradeOrRule = case _ of
      Trade _ -> true
      ChangeRule _ -> true
      _ -> false
    ordinary
      | not (all (_ `elem` signed) signers) = Left [ Lapsed { round: w.round, proposal: p.id } ]
      | deal && any tradeOrRule p.intents = reject "deals bundle only Transfers and Transforms"
      | otherwise = case validate w p signers OwnAuthority of
          Left reason -> reject reason
          Right tariffs -> Right { proposal: p, deal, tariffs, outcome: [] }
  in
    if p.id `elem` duplicated then reject "duplicate proposal id"
    else case p.procedure of
      Just pid -> procedureProposal w p signed pid
      Nothing -> ordinary

-- | Every Intent of a Proposal, then its shipments. Returns the tariffs the shipments owe.
validate :: WorldState -> Proposal -> Array EntityId -> Standing -> Either String (Array { from :: EntityId, to :: EntityId, amount :: Q })
validate w p signers standing = traverse (checkIntent w p signers standing) p.intents *> shipping w p

-- | One Intent: its executor must sign and be authorized, and the Intent must name known
-- | references and respect market access, transferability, rule scope, the facility's bounds,
-- | and owner signing. `signers` are the Entities whose signatures count for this Proposal.
checkIntent :: WorldState -> Proposal -> Array EntityId -> Standing -> Intent -> Either String Unit
checkIntent w p signers standing i = case Map.lookup who w.entities of
  Nothing -> Left ("unknown entity " <> show who)
  Just e
    | not (who `elem` signers) -> Left (show who <> " must sign")
    | otherwise -> known *> authority *> access *> transferable *> scope e *> withinBounds *> ownerSigned
  where
  who = executor p i
  caps = case standing of
    OwnAuthority -> fromMaybe [] (_.capabilities <$> Map.lookup who w.entities)
    ProcedureAuthority cs -> cs
  authority = if authorizedIn w caps who i then Right unit else Left (show who <> " is not authorized")
  transformation t = Map.lookup t.transformation w.transformations
  known = case i of
    Trade o | not (Map.member o.market w.markets) -> Left ("unknown market " <> show o.market)
    Transform t | not (Map.member t.transformation w.transformations) -> Left ("unknown transformation " <> show t.transformation)
    Transfer t
      | not (Map.member t.resource w.resources) -> Left ("unknown resource " <> show t.resource)
      | not (Map.member t.to w.entities) -> Left ("unknown entity " <> show t.to)
    ChangeRule (Repeal id) | not (Map.member id w.rules) -> Left ("unknown rule " <> show id)
    _ -> Right unit
  access = case i of
    Trade o | Just m <- Map.lookup o.market w.markets, not (who `elem` m.access) -> Left (show who <> " has no access to " <> show o.market)
    _ -> Right unit
  transferable = case i of
    Transfer t | Just NonTransferable <- _.transferability <$> Map.lookup t.resource w.resources -> Left (show t.resource <> " is not transferable")
    _ -> Right unit
  scope e = case i of
    ChangeRule (Enact r) | not (governs w e.id r) -> Left (outOfScope r)
    ChangeRule (Repeal id) | Just r <- Map.lookup id w.rules, not (governs w e.id r) -> Left "only the governing authority may repeal a Rule"
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

outOfScope :: Rule -> String
outOfScope = case _ of
  Tariff _ -> "a Tariff applies only to the enacting jurisdiction"
  AccessBan _ -> "an AccessBan applies only to the enacting jurisdiction"
  Withholding _ -> "withholding needs the capacity's owner"

-- | A procedure Proposal (§2.2, §3.3): authored by the Aggregate that holds the procedure, it
-- | executes this round if the residual weight plus the weights of the members who signed
-- | exceeds the threshold, and lapses otherwise. Its Intents are all executed by its author and
-- | must pass the same per-Intent checks as any Proposal, with authority from the procedure's
-- | `authorizes`.
procedureProposal :: WorldState -> Proposal -> Array EntityId -> CollectiveProcedureId -> Either (Array Event) Item
procedureProposal w p signed pid = case Map.lookup p.author w.entities of
  Just { kind: Aggregate a } | Just proc <- Map.lookup pid a.procedures ->
    let
      support = Q.add proc.residualWeight (Q.sumQ (map (\m -> lookupQ m proc.weights) (filter (_ /= p.author) signed)))
      passes = support > proc.threshold
      outcome = ProcedureOutcome { round: w.round, proposal: p.id, support, threshold: proc.threshold, executed: passes }
    in
      if not (all (\i -> executor p i == p.author) p.intents) then reject "outside the procedure's authority"
      else case validate w p signed (ProcedureAuthority proc.authorizes) of
        Left reason -> reject reason
        Right tariffs
          | not passes -> Left [ outcome, Lapsed { round: w.round, proposal: p.id } ]
          | otherwise -> Right { proposal: p, deal: false, tariffs, outcome: [ outcome ] }
  _ -> reject "unknown procedure"
  where
  reject reason = Left [ Rejected { round: w.round, proposal: p.id, reason } ]

-- | Cross-region Transfers must be bundled with a matching transport order signed by the
-- | transport capacity's owner, and are subject to access bans and tariffs.
shipping :: WorldState -> Proposal -> Either String (Array { from :: EntityId, to :: EntityId, amount :: Q })
shipping w p = traverse ship (filter crossRegion (transfersOf p.intents))
  where
  crossRegion t = isJust (Map.lookup t.resource w.transport) && not (sameRegion t)
  sameRegion t = any (\m -> m.base == t.resource && t.from `elem` m.access && t.to `elem` m.access) (fromFoldable (Map.values w.markets) :: Array _)
  ship t = do
    tid <- note "no transport facility" (Map.lookup t.resource w.transport)
    let carried = Q.sumQ (map _.quantity (filter (\x -> x.transformation == tid && x.by == t.from) (transformsOf p.intents)))
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

-- | The Intents of one kind, in order: the one place that selects an Intent kind.
intentsOf :: forall a. (Intent -> Maybe a) -> Array Intent -> Array a
intentsOf = mapMaybe

tradesOf :: Array Intent -> Array Order
tradesOf = intentsOf
  ( case _ of
      Trade o -> Just o
      _ -> Nothing
  )

transfersOf :: Array Intent -> Array TransferSpec
transfersOf = intentsOf
  ( case _ of
      Transfer t -> Just t
      _ -> Nothing
  )

transformsOf :: Array Intent -> Array TransformSpec
transformsOf = intentsOf
  ( case _ of
      Transform t -> Just t
      _ -> Nothing
  )

jur :: WorldState -> EntityId -> EntityId
jur w e = fromMaybe e (Map.lookup e w.jurisdiction)

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

-- | The uniform price of a facility order whose pool fills `q` units.
facilityPrice :: Transformation -> Q -> Q
facilityPrice tr q = maybe Q.zero (\c -> Market.averagePrice c Q.zero q) tr.cost

-- | The most units a facility's pool can fill in total, its ceiling, which escrow prices at in full
-- | and settlement never exceeds (§5.3). The pool is every facility drawing on the same capped
-- | capacity, so its fills together draw on that capacity: a total of q units uses at least
-- | q × (smallest `capacityUse` in the pool). The ceiling is therefore the capacity divided by that
-- | smallest use, rounded down, and every member of the pool escrows at it, whatever its own use.
-- | An uncapped facility's ceiling is the domain top of its cost function. Nothing means no
-- | ceiling, which only a facility with no cost function has, and that one is priced at zero.
poolCeiling :: WorldState -> Transformation -> Maybe Q
poolCeiling w tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Q.divFloor (capacityOf w ref) (smallestUse w ref tr.capacityUse)
  _ -> (\c -> c.domain.hi) <$> tr.cost

-- | The smallest `capacityUse` among the capped facilities on `ref`, starting from `use` (the
-- | facility's own, so the result is never above it).
smallestUse :: WorldState -> CapacityRef -> Q -> Q
smallestUse w ref use = foldl min use (map _.capacityUse (filter drawsOn (fromFoldable (Map.values w.transformations))))
  where
  drawsOn x = x.capacity == Just ref && x.capacityUse > Q.zero

-- | A facility's uniform price at its full ceiling, used for escrow.
fullPrice :: WorldState -> Transformation -> Q
fullPrice w tr = facilityPrice tr (fromMaybe Q.zero (poolCeiling w tr))

-- | The pool a facility's fills draw on: the shared capacity if it is capped, else the facility
-- | alone. Facilities in one pool are allocated together and priced at one uniform price.
poolOf :: TransformationId -> Transformation -> Either TransformationId CapacityRef
poolOf tid tr = case tr.capacity of
  Just ref | tr.capacityUse > Q.zero -> Right ref
  _ -> Left tid

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
    -- rounded down, so scale × amt never exceeds the room (a half-even round-up could overdraw it)
    ratio key amt = if amt > room key then fromMaybe Q.zero (Q.divFloor (max Q.zero (room key)) amt) else Q.fromInt 1
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
  orders = concatMap
    ( \s -> map
        (\o -> { market: o.market, entity: s.item.proposal.author, pid: s.item.proposal.id, quantity: Q.mul s.scale o.quantity, limitPrice: o.limitPrice })
        (tradesOf s.item.proposal.intents)
    )
    scaled
  -- Trade fills are not capped by `pay`: clearing bounds them by the Market's domain and reserves,
  -- and escrow funds them (docs/02 §8.2). Their money is escrowed at the limit or the domain ceiling.
  clearOne acc (Tuple id m) =
    let
      os = sortWith (\o -> Tuple o.entity o.pid) (filter (\o -> o.market == id) orders)
      bounds = { minNet: Q.sub m.cost.domain.lo m.position, maxNet: min (Q.sub m.cost.domain.hi m.position) (lookupQ m.base m.reserves) }
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

-- Circulation (§6) -------------------------------------------------------------

-- | The recurring cycle, evaluated on the round's batch result: production (delivered next
-- | round), variable revenue shares, operating requirements with shortfall penalties, fixed
-- | flows, and decay. One function over the Circulation data; no rule-specific paths.
circulate :: WorldState -> Settlement -> Settlement
circulate w = decay w <<< fixedFlows w <<< requirements w <<< revenueShares w <<< production w

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
revenueShares w st0 = foldl payShare st0 w.circulation.shares
  where
  payShare st sh = transfer w sh.from sh.to w.numeraire (Q.mul sh.share (base sh)) st
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
      consumed = minOwedHeld needed (net w rq.entity rq.resource acc.st)
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

-- | Decay of every decaying Resource held. The rate is per round; instances scale annual rates
-- | to the round length (INV-28).
decay :: WorldState -> Settlement -> Settlement
decay w st0 = foldl lose st0 holdings
  where
  holdings = concatMap (\e -> map (Tuple e.id) (fromFoldable (Map.keys e.accounts))) (fromFoldable (Map.values w.entities))
  lose acc (Tuple e r) = case Map.lookup r w.resources >>= _.decay of
    Just d ->
      let
        held = max Q.zero (net w e r acc)
        lost = minOwedHeld (Q.mul d.rate held) held
      in
        if lost == Q.zero then acc
        else acc { ledger = adjust (Ent e) r (Q.neg lost) acc.ledger, events = acc.events <> [ Decayed { round: w.round, entity: e, resource: r, quantity: lost } ] }
    Nothing -> acc

-- Phase 7: commit -------------------------------------------------------------

commit :: WorldState -> Array SignedProposal -> Array Event -> Map EntityId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  entities <- traverse (\e -> (\accounts -> e { accounts = accounts }) <$> settled (Ent e.id) e.accounts) w.entities
  marketList <- traverse
    ( \(Tuple id m) -> (\reserves -> Tuple id m { reserves = reserves, position = fromMaybe m.position (Map.lookup id st.positions) })
        <$> settled (Mkt id) m.reserves
    )
    (Map.toUnfoldable w.markets :: Array _)
  let
    events = screenEvents <> st.events
    world = w
      { round = w.round + 1
      , entities = entities
      , markets = Map.fromFoldable marketList
      , deliveries = w.deliveries <> st.deliveries
      , scheduled = Map.union st.scheduled w.scheduled
      , factors = factors
      , events = w.events <> events
      }
  pure { world, record: { round: w.round, inputs, events } }
  where
  applyHoldings acct hs = foldl (\h (Tuple (Tuple a r) q) -> if a == acct then Map.insertWith Q.add r q h else h) hs (Map.toUnfoldable st.ledger :: Array _)
  -- Settlement caps every payment at what an account holds and escrow funds the rest, so no
  -- account should go negative. This is an invariant assertion: if one does, the round fails
  -- rather than recording a corrupt state.
  settled acct hs = case find (\(Tuple _ q) -> q < Q.zero) (Map.toUnfoldable (applyHoldings acct hs) :: Array _) of
    Just (Tuple r _) -> Left
      ( case acct of
          Ent e -> NegativeBalance e r
          Mkt m -> NegativeReserve m r
      )
    Nothing -> Right (applyHoldings acct hs)
