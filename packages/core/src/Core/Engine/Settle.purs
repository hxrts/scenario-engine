-- | Engine phase 6, first half (docs/02_type_system.md §8.1, §8.2): settle the funded batch
-- | against the prepared snapshot. Markets clear at uniform prices, Transformation fills are allocated
-- | per pool and paid, and Transfers, levies, and Rule changes apply. Every payment goes through
-- | `pay`, which caps it at what the payer holds, so an Account cannot go negative. Settlement
-- | accumulates a ledger of net changes; nothing is written to World State until Commit.
-- | `Settlement`, `Account`, and `Ledger` are defined here because this is the first phase that
-- | produces them; Circulate and Commit import them.
module Core.Engine.Settle
  ( Settlement
  , settle
  , net
  , transfer
  ) where

import Prelude

import Core.Engine.Fund (Scaled, scaleOf)
import Core.Map (entries, groupBy, sumInto)
import Core.Engine.Compile (fillCoupling, payCoupling, runCoupling, transformCoupling)
import Core.Kernel.Resolve (Fate(..))
import Core.Kernel.Clear (clearSolvent)
import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Ration (fillPool)
import Core.Kernel.Route as Route
import Core.Num (Q)
import Core.Num as Q
import Core.Query (conversionPrice, drawable, holding, lookupQ, poolCeiling, poolOf)
import Core.Types (EntityId, IntentF(..), MarketId, ProposalId, ResourceId, Rule, RuleChangeF(..), RuleId, TransformationId, poolDraw, ruleIdOf)
import Core.World (Cause(..), Delivery, Event, Mechanism(..), RejectReason(..), WorldState, addEvents, coupled, orderRemoved, rejection)
import Data.Array (any, concat, concatMap, drop, filter, foldl, notElem, nub, null, partition, sortWith, take, zipWith)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..), fst)

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
    allocation = allocate w scaled0
    afterPools = filter (\s -> s.item.proposal.id `notElem` allocation.rejected) scaled0
    -- a joint Proposal settles as a unit (docs/02 §3.3), so its inputs are checked before any leg moves
    { yes: short, no: scaled } = partition (jointShort w allocation) afterPools
    shortEvents = concatMap (\s -> map (rejection w.round s.item.proposal.id <<< InputsNotHeld) (shortInputs w allocation s)) short
    start = { ledger: Map.empty, positions: Map.empty, deliveries: [], scheduled: Map.empty, events: allocation.events <> shortEvents }
  in
    foldl (\acc f -> f acc) start
      [ markets w scaled
      , \acc -> foldl (settleConversions w allocation) acc scaled
      , \acc -> foldl (transfersAndRules w) acc scaled
      ]

-- | The Transformations whose inputs a joint Proposal's filled Transforms need and its executors do not hold.
-- | Escrow funds a joint Proposal's inputs, so this is a backstop.
shortInputs :: WorldState -> Allocation -> Scaled -> Array TransformationId
shortInputs w allocation s = concat (mapWithIndex one s.item.proposal.intents)
  where
  one i = case _ of
    Transform t
      | Just q <- Map.lookup (Tuple s.item.proposal.id i) allocation.fills
      , Just tr <- Map.lookup t.transformation w.transformations
      , any (\(Tuple r u) -> holding w t.by r < Q.mul u q) (entries tr.inputs) -> [ t.transformation ]
    _ -> []

jointShort :: WorldState -> Allocation -> Scaled -> Boolean
jointShort w allocation s = s.item.joint && not (null (shortInputs w allocation s))

-- | Transformation fills per pool. Joint Proposals that would exceed a pool are rejected as a unit; single-author
-- | orders share what remains pro rata.
allocate :: WorldState -> Array Scaled -> Allocation
allocate w scaled = foldl (allocateGroup w) { fills: Map.empty, totals: Map.empty, rejected: [], events: map removedEvent belowMinimum } (entries groups)
  where
  -- funding may scale an order below its Transformation's minimum, which removes it
  { yes: belowMinimum, no: standing } = partition (\o -> o.quantity < o.minimum) (conversionOrders w scaled)
  removedEvent o = orderRemoved w.round (fst o.key) (BelowMinimum o.tid)
  groups = groupBy _.group standing

type ConversionOrder =
  { key :: Tuple ProposalId Int
  , joint :: Boolean
  , quantity :: Q
  , use :: Q -- what is drawn per unit ordered: the holder draw's `perUnit`, or one unit per unit of a pool of its own
  , group :: Either TransformationId ResourceId
  , tid :: TransformationId
  , minimum :: Q
  }

-- | Every Transform, scaled by its Proposal's funding, with its Transformation's minimum.
conversionOrders :: WorldState -> Array Scaled -> Array ConversionOrder
conversionOrders w = concatMap \s -> concat (mapWithIndex (order s) s.item.proposal.intents)
  where
  order s i = case _ of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations ->
      let
        group = poolOf t.transformation tr
        use = maybe (Q.fromInt 1) _.perUnit (poolDraw tr)
      in
        [ { key: Tuple s.item.proposal.id i, joint: s.item.joint, quantity: Q.mul (scaleOf s t.by) t.quantity, use, group, tid: t.transformation, minimum: tr.minimum } ]
    _ -> []

-- | One pool: joint Proposals fill in full or are rejected as a unit; single orders share what remains
-- | pro rata, in whole units of the Resource. Each order's quantity is its share divided by
-- | its `use`, floored to the 0.0001 grid, so an order never draws more than its share. The
-- | sub-grid remainder stays unfilled. A pool's cap is what can be drawn from its Resource; a Transformation
-- | with no holder draw has its ceiling as its cap, so its fills never exceed the quantity escrow priced.
allocateGroup :: WorldState -> Allocation -> Tuple (Either TransformationId ResourceId) (Array ConversionOrder) -> Allocation
allocateGroup w acc (Tuple group os) =
  { fills: Map.union acc.fills (Map.fromFoldable filled.fills)
  , totals: foldl (\m (Tuple tid q) -> sumInto tid q m) acc.totals (zipWith (\o (Tuple _ q) -> Tuple o.tid q) (if filled.overflow then singles else joints <> singles) filled.fills)
  , rejected: acc.rejected <> rejectedJoints
  , events: acc.events <> map (\pid -> rejection w.round pid PoolExceeded) rejectedJoints
  }
  where
  cap = case group of
    Right r -> drawable w r
    Left tid -> fromMaybe (Q.sumQ (map _.quantity os)) (Map.lookup tid w.transformations >>= poolCeiling w)
  { yes: joints, no: singles } = partition _.joint os
  filled = fillPool cap (map (\o -> { key: o.key, whole: o.joint, quantity: o.quantity, use: o.use }) (joints <> singles))
  rejectedJoints = if filled.overflow then nub (map (fst <<< _.key) joints) else []

type Allocation =
  { fills :: Map (Tuple ProposalId Int) Q
  , totals :: Map TransformationId Q -- filled quantity per Transformation
  , rejected :: Array ProposalId
  , events :: Array Event
  }

markets :: WorldState -> Array Scaled -> Settlement -> Settlement
markets w scaled acc0 = foldl clearOne acc0 (entries w.markets)
  where
  explicit :: Array Route.Slice
  explicit = concatMap
    ( \s -> concat $ mapWithIndex
        ( \i intent -> case intent of
            Trade o -> [ { market: o.market, entity: s.item.proposal.author, pid: s.item.proposal.id, intent: i, quantity: Q.mul (scaleOf s s.item.proposal.author) o.quantity, limitPrice: o.limitPrice } ]
            _ -> []
        )
        s.item.proposal.intents
    )
    scaled
  -- routed Trades are split across their candidate Markets (docs/02 §5.2), against the net of the explicit Trades
  orders = explicit <> Route.plan w (foldl (\m o -> sumInto o.market o.quantity m) Map.empty explicit) routed

  routed :: Array Route.Routed
  routed = concatMap
    ( \s -> concat $ mapWithIndex
        ( \i intent -> case intent of
            Route r
              | Just limit <- r.limitPrice
              , Just cs <- Map.lookup i s.item.routes ->
                  [ { entity: s.item.proposal.author, pid: s.item.proposal.id, index: i, quantity: Q.mul (scaleOf s s.item.proposal.author) r.quantity, limitPrice: limit, candidates: cs } ]
            _ -> []
        )
        s.item.proposal.intents
    )
    scaled
  -- Trade fills are not capped by `pay`: clearing bounds them by the Market's domain and reserves,
  -- and escrow funds them (docs/02 §8.2). Their quote leg is escrowed at the limit or the domain ceiling.
  clearOne acc (Tuple id m) =
    let
      os = sortWith (\o -> Tuple o.entity o.pid) (filter (\o -> o.market == id) orders)
      r = clearSolvent m os
      fill a f =
        let
          paid = Q.mul r.price f.quantity
        in
          case runCoupling w a.ledger (fillCoupling id m { entity: f.order.entity, pid: f.order.pid, intent: f.order.intent, quantity: f.quantity, paid }) of
            posted -> addEvents [ coupled w.round posted.flows (Just r.price) (OfIntent { proposal: f.order.pid, intent: f.order.intent, mechanism: Curve id }) ] a { ledger = posted.ledger }
    in
      if null os then acc
      else
        let
          filled = foldl fill acc r.fills
        in
          addEvents (map (\o -> orderRemoved w.round o.pid LimitPriceNotMet) r.rejected <> map (\o -> orderRemoved w.round o.pid QuoteReservesShort) r.unpaid)
            filled { positions = Map.insert id (Q.add m.position r.net) filled.positions }

-- | Settle one Proposal's filled Transforms: pay what the Transformation charges, consume inputs, produce
-- | outputs, and schedule delayed flows.
settleConversions :: WorldState -> Allocation -> Settlement -> Scaled -> Settlement
settleConversions w allocation acc s = foldl one acc (mapWithIndex Tuple s.item.proposal.intents)
  where
  pid = s.item.proposal.id
  -- Transformations in one pool with one cost function share one uniform price
  pricedTotal tid tr = Q.sumQ
    ( map (\(Tuple _ q) -> q)
        ( filter
            (\(Tuple other _) -> maybe false (\x -> poolOf other x == poolOf tid tr && map _.curve x.payment == map _.curve tr.payment) (Map.lookup other w.transformations))
            (entries allocation.totals)
        )
    )
  -- Effects land on the same boundary as outputs: an output with delay d is visible from
  -- round + max 1 d, and so is an effect, since a delay of zero is treated as one round.
  effects cause t tr q = concatMap
    ( \e ->
        let
          holder = fromMaybe t.by e.holder
          start = w.round + max 1 tr.delay
          change = { round: start, entity: holder, resource: e.resource, quantity: Q.mul e.perUnit q, cause }
        in
          [ change ] <> maybe [] (\n -> [ change { round = start + n, quantity = Q.neg change.quantity } ]) e.rounds
    )
    tr.effects
  one a (Tuple i intent) = case intent of
    Transform t -> case Tuple (Map.lookup (Tuple pid i) allocation.fills) (Map.lookup t.transformation w.transformations) of
      Tuple (Just q) (Just tr) | q > Q.zero ->
        let
          owed = Q.mul (conversionPrice tr (pricedTotal t.transformation tr)) q
          cause = OfIntent { proposal: pid, intent: i, mechanism: Conversion { by: t.by, transformation: t.transformation, units: q } }
          outs = map (\(Tuple r u) -> { round: w.round + tr.delay, entity: t.by, resource: r, quantity: Q.mul u q, cause }) (entries tr.outputs)
          { yes: now, no: later } = partition (\d -> d.round == w.round) outs
          inputs = map (\(Tuple r u) -> { resource: r, amount: Q.mul u q }) (entries tr.inputs)
          payment = map (\terms -> { terms, owed }) tr.payment
          posted = runCoupling w a.ledger (transformCoupling { by: t.by, tid: t.transformation, inputs, outputs: map (\d -> { resource: d.resource, amount: d.quantity }) now, payment, cause })
          -- the payment posted first; it is recorded last, and only when something was owed
          paidFirst = if isJust payment then 1 else 0
          paymentLeg = if owed > Q.zero then take paidFirst posted.flows else []
          legs = drop paidFirst posted.flows <> paymentLeg
        in
          if any (\c -> c.fate == Expired) posted.consumed then addEvents [ orderRemoved w.round pid (InputsNotHeld t.transformation) ] a
          else addEvents [ coupled w.round legs Nothing cause ] a
            { ledger = posted.ledger
            , deliveries = a.deliveries <> later <> effects cause t tr q
            }
      _ -> a
    _ -> a

-- | Transfers, levies, and rule changes of surviving Items.
transfersAndRules :: WorldState -> Settlement -> Scaled -> Settlement
transfersAndRules w acc s = foldl one withLevies (mapWithIndex Tuple p.intents)
  where
  p = s.item.proposal
  -- levies follow the Transfer they belong to, so they are scaled with it
  withLevies = foldl levy acc s.item.levies
  levy a' t =
    let
      owed = Q.mul (scaleOf s t.from) t.amount
    in
      case runCoupling w a'.ledger (payCoupling t.from t.to t.in owed (OfRule { kind: t.kind })) of
        posted
          | owed > Q.zero -> addEvents [ coupled w.round posted.flows Nothing (OfRule { kind: t.kind }) ] a' { ledger = posted.ledger }
          | otherwise -> a' { ledger = posted.ledger }
  one a (Tuple i intent) = case intent of
    Transfer t -> transfer w (OfIntent { proposal: p.id, intent: i, mechanism: Direct }) t.from t.to t.resource (Q.mul (scaleOf s t.from) t.quantity) a
    ChangeRule (Enact r) -> a { scheduled = Map.insert (ruleId i) (Just r) a.scheduled }
    ChangeRule (Repeal id) -> a { scheduled = Map.insert id Nothing a.scheduled }
    _ -> a
  ruleId = ruleIdOf p.id

-- | A transfer between Entities, one leg with what was owed and what moved.
transfer :: WorldState -> Cause -> EntityId -> EntityId -> ResourceId -> Q -> Settlement -> Settlement
transfer w cause from to resource quantity st
  | quantity == Q.zero = st
  | otherwise = case runCoupling w st.ledger (payCoupling from (Just to) resource quantity cause) of
      posted -> addEvents [ coupled w.round posted.flows Nothing cause ] st { ledger = posted.ledger }

-- | What an Entity holds after the settlement so far.
net :: WorldState -> EntityId -> ResourceId -> Settlement -> Q
net w e r st = Q.add (holding w e r) (lookupQ (Tuple (Ent e) r) st.ledger)
