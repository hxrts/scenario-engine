-- | Engine phase 6, first half (docs/02_kernel.md §8.1, §8.2): settle the funded batch
-- | against the prepared snapshot. Exchanges clear at uniform prices, Transformation fills are allocated
-- | per pool and paid, and Transfers, levies, and Rule changes apply. Every payment goes through
-- | `pay`, which caps it at what the payer holds, so an Account cannot go negative. Settlement
-- | accumulates a ledger of net changes; nothing is written to World State until Commit.
-- | `Settlement`, `Account`, and `Ledger` are defined here because this is the first phase that
-- | produces them; Circulate and Commit import them.
module Laws.Engine.Settle
  ( Settlement
  , settle
  , net
  ) where

import Prelude

import Laws.Engine.Compile (entered, actionTransactions, delivery, jointInputs, named, pairTransaction, payTransaction, ruleChange, runTransaction, transformTransaction)

import Laws.Engine.Fund (Scaled, scaleOf)
import Core.Map (entries, groupBy, sumInto)
import Core.Kernel.Resolve (Fate(..))
import Core.Kernel.Clear (Side(..), clearSolvent)
import Laws.Engine.Screen (blockOf, leviesOn)
import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Types as K
import Core.Kernel.Ration (fillPool)
import Core.Kernel.Route as Route
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (transformationPrice, drawable, holding, lookupQ, poolCeiling, poolOf)
import Laws.Types (ActorId, IntentF(..), ExchangeId, CandidateTransactionId, ResourceId, RuleChangeF(..), TransformationId, poolDraw, ruleIdOf, idText)
import Laws.World (Cause(..), Event, Mechanism(..), PendingTransaction, RejectReason(..), WorldState, addEvents, settledEvent, orderRemoved, rejection)
import Data.Array (any, concat, concatMap, drop, filter, foldl, notElem, nub, nubEq, null, partition, sortWith, take, zipWith, index, mapMaybe, nubByEq)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Core.Types as Core
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Tuple (Tuple(..), fst)

-- | The round's batch result: the ledger and Exchange positions, plus the deliveries, Rule changes,
-- | and events it produces.
type Settlement =
  { ledger :: Ledger
  , positions :: Map ExchangeId Q
  , intercepts :: Map ExchangeId Q -- each adjusting Exchange's next intercept (docs/02 §5.6)
  , spawned :: Array PendingTransaction
  , events :: Array Event
  }

settle :: WorldState -> { ledger :: Ledger, events :: Array Event, spawned :: Array PendingTransaction } -> Array Scaled -> Settlement
settle w due scaled0 =
  let
    allocation = allocate w scaled0
    afterPools = filter (\s -> s.item.candidateTransaction.id `notElem` allocation.rejected) scaled0
    -- a joint candidate Transaction settles as a unit (docs/02 §3.3), so its inputs are checked before any settled Flow moves
    { yes: short, no: scaled } = partition (jointShort w allocation) afterPools
    shortEvents = concatMap (\s -> map (rejection w.round s.item.candidateTransaction.id <<< InputsNotHeld) (shortInputs w allocation s)) short
    start = { ledger: due.ledger, positions: Map.empty, intercepts: Map.empty, spawned: due.spawned, events: due.events <> allocation.events <> shortEvents }
  in
    foldl (\acc f -> f acc) start
      [ exchanges w scaled
      , \acc -> foldl (settleTransformationFills w allocation) acc scaled
      , \acc -> foldl (transfersAndRules w) acc scaled
      ]

-- | The Transformations whose inputs a joint candidate Transaction's filled Transforms need and its executors do not hold.
-- | Escrow funds a joint candidate Transaction's inputs, so this is a backstop.
shortInputs :: WorldState -> Allocation -> Scaled -> Array TransformationId
shortInputs w allocation s = map _.tid (nubByEq (\a b -> a.index == b.index) (mapMaybe (\c -> if c.fate == Shorted then index needed c.obligation else Nothing) dry.consumed))
  where
  -- resolved on the opening holdings: nothing has posted yet
  dry = runTransaction w Map.empty (jointInputs (map (\x -> { by: x.by, resource: x.resource, amount: x.amount, index: x.index }) needed))
  needed = concat (mapWithIndex one s.item.candidateTransaction.intents)
  one i = case _ of
    Transform t
      | Just q <- Map.lookup (Tuple s.item.candidateTransaction.id i) allocation.fills
      , Just tr <- Map.lookup t.transformation w.transformations ->
          map (\(Tuple r u) -> { by: t.by, resource: r, amount: Q.mul u q, index: i, tid: t.transformation }) (entries tr.inputs)
    _ -> []

jointShort :: WorldState -> Allocation -> Scaled -> Boolean
jointShort w allocation s = s.item.joint && not (null (shortInputs w allocation s))

-- | Transformation fills per pool. joint candidate Transactions that would exceed a pool are rejected as a unit; single-author
-- | orders share what remains pro rata.
allocate :: WorldState -> Array Scaled -> Allocation
allocate w scaled = foldl (allocateGroup w) { fills: Map.empty, totals: Map.empty, rejected: [], events: map removedEvent belowMinimum } (entries groups)
  where
  -- funding may scale an order below its Transformation's minimum, which removes it
  { yes: belowMinimum, no: standing } = partition (\o -> o.quantity < o.minimum) (transformationOrders w scaled)
  removedEvent o = orderRemoved w.round (fst o.key) (BelowMinimum o.tid)
  groups = groupBy _.group standing

type TransformationOrder =
  { key :: Tuple CandidateTransactionId Int
  , joint :: Boolean
  , quantity :: Q
  , use :: Q -- what is drawn per unit ordered: the holder draw's `perUnit`, or one unit per unit of a pool of its own
  , group :: Either TransformationId ResourceId
  , tid :: TransformationId
  , minimum :: Q
  }

-- | Every Transform, scaled by its candidate Transaction's funding, with its Transformation's minimum.
transformationOrders :: WorldState -> Array Scaled -> Array TransformationOrder
transformationOrders w = concatMap \s -> concat (mapWithIndex (order s) s.item.candidateTransaction.intents)
  where
  order s i = case _ of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations ->
      let
        group = poolOf t.transformation tr
        use = maybe (Q.fromInt 1) _.perUnit (poolDraw tr)
      in
        [ { key: Tuple s.item.candidateTransaction.id i, joint: s.item.joint, quantity: Q.mul (scaleOf s t.by) t.quantity, use, group, tid: t.transformation, minimum: tr.minimum } ]
    _ -> []

-- | One pool: joint candidate Transactions fill in full or are rejected as a unit; single orders share what remains
-- | pro rata, in whole units of the Resource. Each order's quantity is its share divided by
-- | its `use`, floored to the 0.0001 grid, so an order never draws more than its share. The
-- | sub-grid remainder stays unfilled. A pool's cap is what can be drawn from its Resource; a Transformation
-- | with no holder draw has its ceiling as its cap, so its fills never exceed the quantity escrow priced.
allocateGroup :: WorldState -> Allocation -> Tuple (Either TransformationId ResourceId) (Array TransformationOrder) -> Allocation
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
  { fills :: Map (Tuple CandidateTransactionId Int) Q
  , totals :: Map TransformationId Q -- filled quantity per Transformation
  , rejected :: Array CandidateTransactionId
  , events :: Array Event
  }

exchanges :: WorldState -> Array Scaled -> Settlement -> Settlement
exchanges w scaled acc0 = foldl clearOne acc0 (entries w.exchanges)
  where
  explicit :: Array (Route.Slice CandidateTransactionId)
  explicit = concatMap
    ( \s -> concat $ mapWithIndex
        ( \i intent -> case intent of
            Trade o -> [ { exchange: o.exchange, actor: s.item.candidateTransaction.author, pid: s.item.candidateTransaction.id, position: i, quantity: Q.mul (scaleOf s s.item.candidateTransaction.author) o.quantity, limitPrice: o.limitPrice } ]
            _ -> []
        )
        s.item.candidateTransaction.intents
    )
    scaled
  -- routed Trades are split across their candidate Exchanges (docs/02 §5.2), against the net of the explicit Trades
  orders = explicit <> Route.plan w.exchanges (foldl (\m o -> sumInto o.exchange o.quantity m) Map.empty explicit) routed

  routed :: Array (Route.Routed CandidateTransactionId)
  routed = concatMap
    ( \s -> concat $ mapWithIndex
        ( \i intent -> case intent of
            Route r
              | Just limit <- r.limitPrice
              , Just cs <- Map.lookup i s.item.routes ->
                  [ { actor: s.item.candidateTransaction.author, pid: s.item.candidateTransaction.id, index: i, quantity: Q.mul (scaleOf s s.item.candidateTransaction.author) r.quantity, limitPrice: limit, candidates: cs } ]
            _ -> []
        )
        s.item.candidateTransaction.intents
    )
    scaled
  -- Trade fills are not capped by `pay`: clearing bounds them by the Exchange's domain and reserves,
  -- and escrow funds them (docs/02 §8.2). Their quote settled Flow is escrowed at the limit or the domain ceiling.
  -- Each cleared pair settles as one transaction between its two sides (docs/02 §5.5). A flows Rule applies to
  -- the base moving from the seller to the buyer as it does to a Transfer: a Block refuses the pair, and a Levy
  -- is charged to the seller with it, once (docs/03 §5.5). The reserve has no jurisdiction, so no Rule applies
  -- to a pair with the reserve. Trade fills are not capped by `pay`: clearing bounds them by the Exchange's
  -- domain and reserves, and escrow funds them (docs/02 §8.2).
  clearOne acc (Tuple id m) =
    let
      os = sortWith (\o -> Tuple o.actor o.pid) (filter (\o -> o.exchange == id) orders)
      r = clearSolvent m os
      actorOf = case _ of
        Party o -> Just o.actor
        Reserve -> Nothing
      blockOn x = case actorOf x.seller, actorOf x.buyer of
        Just from, Just to -> blockOf w { from, to, resource: m.base }
        _, _ -> Nothing
      { yes: settling, no: refused } = partition (isNothing <<< blockOn) r.pairs
      blockedEvents = nubEq (concatMap (\x -> maybe [] (\k -> map (\o -> orderRemoved w.round o.pid (Blocked k)) (ordersOf x)) (blockOn x)) refused)
      ordersOf x = mapMaybe
        ( case _ of
            Party o -> Just o
            Reserve -> Nothing
        )
        [ x.buyer, x.seller ]
      account = case _ of
        Party o -> named o.actor
        Reserve -> K.Reserve id
      ref = case _ of
        Party o -> Just { candidateTransaction: o.pid, intent: o.position }
        Reserve -> Nothing
      -- who answers for each leg: its side's Actor, or the other side's when its side is the reserve
      answerers x = case actorOf x.seller, actorOf x.buyer of
        Just s, Just b -> Just { sellerActor: s, buyerActor: b }
        Just s, Nothing -> Just { sellerActor: s, buyerActor: s }
        Nothing, Just b -> Just { sellerActor: b, buyerActor: b }
        Nothing, Nothing -> Nothing
      settlePair a x = maybe a (\who -> settleWith who a x) (answerers x)
      settleWith { sellerActor, buyerActor } a x =
        let
          levies = case actorOf x.seller, actorOf x.buyer of
            Just from, Just to -> filter (\l -> l.amount > Q.zero) (leviesOn w { from, to, resource: m.base } x.quantity)
            _, _ -> []
          cause = OfMatch { exchange: id, buyer: ref x.buyer, seller: ref x.seller }
          posted = runTransaction w a.ledger (pairTransaction { base: m.base, quote: m.quote, seller: account x.seller, buyer: account x.buyer, sellerActor, buyerActor, quantity: x.quantity, paid: Q.mul r.price x.quantity, levies: map (\l -> { to: l.to, in: l.in, amount: l.amount, byBuyer: l.from /= l.sender }) levies, cause })
          levyEvents = zipWith (\l leg -> settledEvent w.round [ leg ] Nothing (OfRule { kind: l.kind })) levies (drop 2 posted.flows)
        in
          addEvents ([ settledEvent w.round (take 2 posted.flows) (Just r.price) cause ] <> levyEvents) a { ledger = posted.ledger }
      -- what each order settled across its pairs, for the action laws it is charged
      settledBy = foldl (\mp x -> foldl (\m' o -> sumInto (Tuple o.actor (Tuple o.pid o.position)) x.quantity m') mp (ordersOf x)) Map.empty settling
    in
      if null os then acc
      else
        let
          paired = foldl settlePair acc settling
          charged = foldl (\a (Tuple (Tuple actor _) q) -> applyActionLaws w actor (idText id) q a) paired (entries settledBy)
        in
          addEvents (blockedEvents <> map (\o -> orderRemoved w.round o.pid LimitPriceNotMet) r.rejected <> map (\o -> orderRemoved w.round o.pid QuoteReservesShort) r.unpaid)
            charged { positions = Map.insert id (Q.add m.position r.net) charged.positions, intercepts = adjusted m r charged.intercepts }

-- | Settle one candidate Transaction's filled Transforms: pay what the Transformation charges, consume inputs, produce
-- | outputs, and schedule delayed flows.
settleTransformationFills :: WorldState -> Allocation -> Settlement -> Scaled -> Settlement
settleTransformationFills w allocation acc s = foldl one acc (mapWithIndex Tuple s.item.candidateTransaction.intents)
  where
  pid = s.item.candidateTransaction.id
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
          change = { round: start, actor: holder, resource: e.resource, quantity: Q.mul e.perUnit q, cause }
        in
          [ change ] <> maybe [] (\n -> [ change { round = start + n, quantity = Q.neg change.quantity } ]) e.rounds
    )
    tr.effects
  one a (Tuple i intent) = case intent of
    Transform t -> case Tuple (Map.lookup (Tuple pid i) allocation.fills) (Map.lookup t.transformation w.transformations) of
      Tuple (Just q) (Just tr) | q > Q.zero ->
        let
          owed = Q.mul (transformationPrice tr (pricedTotal t.transformation tr)) q
          cause = OfIntent { candidateTransaction: pid, intent: i, mechanism: Transformation { by: t.by, transformation: t.transformation, units: q } }
          outs = map (\(Tuple r u) -> { round: w.round + tr.delay, actor: t.by, resource: r, quantity: Q.mul u q, cause }) (entries tr.outputs)
          { yes: now, no: later } = partition (\d -> d.round == w.round) outs
          inputs = map (\(Tuple r u) -> { resource: r, amount: Q.mul u q }) (entries tr.inputs)
          payment = map (\terms -> { terms, owed }) tr.payment
          posted = runTransaction w a.ledger (transformTransaction { by: t.by, tid: t.transformation, inputs, outputs: map (\d -> { resource: d.resource, amount: d.quantity }) now, payment, cause })
          -- the payment posted first; it is recorded last, and only when something was owed
          paidFirst = if isJust payment then 1 else 0
          paymentLeg = if owed > Q.zero then take paidFirst posted.flows else []
          legs = drop paidFirst posted.flows <> paymentLeg
        in
          if any (\c -> c.fate == Expired) posted.consumed then addEvents [ orderRemoved w.round pid (InputsNotHeld t.transformation) ] a
          else applyActionLaws w t.by (idText t.transformation) q $ addEvents [ settledEvent w.round legs Nothing cause ] a
            { ledger = posted.ledger
            , spawned = a.spawned <> map delivery (later <> effects cause t tr q)
            }
      _ -> a
    _ -> a

-- | Transfers, levies, and rule changes of surviving Items.
transfersAndRules :: WorldState -> Settlement -> Scaled -> Settlement
transfersAndRules w acc s = foldl one withLevies (mapWithIndex Tuple p.intents)
  where
  p = s.item.candidateTransaction
  -- levies follow the Transfer they belong to, so they are scaled with it
  withLevies = foldl levy acc s.item.levies
  levy a' t =
    let
      owed = Q.mul (scaleOf s t.sender) t.amount
    in
      case runTransaction w a'.ledger (payTransaction t.from t.to t.in owed (OfRule { kind: t.kind })) of
        posted
          | owed > Q.zero -> addEvents [ settledEvent w.round posted.flows Nothing (OfRule { kind: t.kind }) ] a' { ledger = posted.ledger }
          | otherwise -> a' { ledger = posted.ledger }
  one a (Tuple i intent) = case intent of
    Transfer t -> transfer w (OfIntent { candidateTransaction: p.id, intent: i, mechanism: Direct }) t.from t.to t.resource (Q.mul (scaleOf s t.from) t.quantity) a
    ChangeRule (Enact r) -> a { spawned = a.spawned <> [ ruleChange w.round p.author (changeCause i) (ruleId i) (Just r) ] }
    ChangeRule (Repeal id) -> a { spawned = a.spawned <> [ ruleChange w.round p.author (changeCause i) id Nothing ] }
    Enter e -> a { spawned = a.spawned <> entered w (nub ([ p.author ] <> p.signers)) e }
    _ -> a
  ruleId = ruleIdOf p.id
  changeCause i = OfIntent { candidateTransaction: p.id, intent: i, mechanism: Direct }

-- | Charge what the authored action laws say an action costs, each its own event.
applyActionLaws :: WorldState -> ActorId -> String -> Q -> Settlement -> Settlement
applyActionLaws w explicitActor action quantity st = foldl charge st (actionTransactions w explicitActor action quantity)
  where
  charge acc c = case runTransaction w acc.ledger c, c.cause of
    posted, Just cause -> addEvents [ settledEvent w.round posted.flows Nothing cause ] acc { ledger = posted.ledger }
    _, Nothing -> acc

-- | A transfer between Actors, one settled Flow with what was owed and what moved.
transfer :: WorldState -> Cause -> ActorId -> ActorId -> ResourceId -> Q -> Settlement -> Settlement
transfer w cause from to resource quantity st
  | quantity == Q.zero = st
  | otherwise = case runTransaction w st.ledger (payTransaction from (Just to) resource quantity cause) of
      posted -> addEvents [ settledEvent w.round posted.flows Nothing cause ] st { ledger = posted.ledger }

-- | What an Actor holds after the settlement so far.
net :: WorldState -> ActorId -> ResourceId -> Settlement -> Q
net w e r st = Q.add (holding w e r) (lookupQ (Tuple (Ent e) r) st.ledger)

-- | An adjusting Exchange's price after its round (docs/02 §5.6): it moves by its adjustment times the gap between what
-- | its orders asked to buy and to sell, as a share of the larger, so a shortage raises it and a glut lowers it, by at
-- | most the adjustment. Its curve's intercept carries the move, so its slope and position still apply.
adjusted :: forall r. Core.Exchange -> { price :: Q, demand :: Q, supply :: Q | r } -> Map ExchangeId Q -> Map ExchangeId Q
adjusted m r acc =
  let
    larger = if r.demand > r.supply then r.demand else r.supply
  in
    case Q.divMaybe (Q.sub r.demand r.supply) larger of
      Just gap | m.adjustment /= Q.zero -> Map.insert m.id (Q.add m.cost.intercept (Q.mul (Q.mul r.price m.adjustment) gap)) acc
      _ -> acc
