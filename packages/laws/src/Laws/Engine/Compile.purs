-- | The compile step: each settled mechanism written as transactions, which the kernel then posts.
module Laws.Engine.Compile
  ( entered
  , WorldKernel
  , kernelOf
  , pairTransaction
  , named
  , transformTransaction
  , payTransaction
  , fractionLaw
  , Delivered
  , delivery
  , deliveredOf
  , ruleChange
  , jointInputs
  , flowLaws
  , actionTransactions
  , timeLaw
  , WorldLaw
  , changeOf
  , runTransaction
  ) where

import Prelude

import Data.Newtype (unwrap)

import Data.Either (either, Either(..))

import Core.Kernel.Consume (enter)
import Laws.Types (ActorId, Capability, Direction(..), EnterF, FlowEffect(..), Id(..), LevyBasis(..), LevyRecipient(..), Payment, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, TransformationId, idText)
import Laws.World (AuthoredLaw(..), Cause(..), Flow, LegQuery, PendingTransaction(..), RejectReason(..), WorldState)

import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Resolve (Consumed, Kernel, resolve)
import Core.Kernel.Law (forHolder, holderParam, quantityParam, recipientParam)
import Core.Kernel.Types (Account(..), Amount(..), Bond(..), Condition(..), Transaction, Effect(..), Law, LawChange(..), Obligation, Operation(..), Party(..), Short(..), Subject(..), Trigger(..), noNext) as K
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (capabilitiesOf, holding, priceOf, reserveOf, rulesOf)
import Data.Array (elem, filter, fromFoldable, head, length, mapMaybe, mapWithIndex, range)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

type WorldKernel = Kernel Capability LegQuery

type Transaction = K.Transaction Capability Rule LegQuery RejectReason Cause

type Oblige = K.Obligation Capability Rule LegQuery RejectReason

-- | The kernel over a World State and the settlement so far.
kernelOf :: WorldState -> Ledger -> WorldKernel
kernelOf w start =
  { opening: \a r -> case a of
      Ent e -> holding w e r
      Mkt m -> reserveOf w m r
  , holds: \e cap -> cap `elem` capabilitiesOf w e
  , read: \_ _ -> Q.zero
  , price: Nothing
  , start
  }

-- | Post one transaction on the settlement so far, returning its settled Flows in order. Compile names every party,
-- | so resolution cannot meet an unbound one.
runTransaction :: WorldState -> Ledger -> Transaction -> { ledger :: Ledger, flows :: Array Flow, consumed :: Array (Consumed Rule RejectReason) }
runTransaction w start c = case resolve (kernelOf w start) [ c ] of
  Right r -> { ledger: r.ledger, flows: mapMaybe _.flow r.consumed, consumed: r.consumed }
  Left _ -> { ledger: start, flows: [], consumed: [] }

named :: ActorId -> K.Account
named = K.HeldBy <<< K.Named

-- | One cleared pair: the base moving from the seller to the buyer, the quote moving back at the batch price, and
-- | the levies the flows Rules charge the seller on that movement. Delivery and payment are bound `Whole` to each
-- | other, so neither moves without the other, and each levy stands only with the delivery. A side with no
-- | Actor is the Exchange's reserve. The obligor of a leg is the Actor who answers for it: its side's Actor, or
-- | the other side's when its side is the reserve.
pairTransaction
  :: { base :: ResourceId
     , quote :: ResourceId
     , seller :: K.Account
     , buyer :: K.Account
     , sellerActor :: ActorId
     , buyerActor :: ActorId
     , quantity :: Q
     , paid :: Q
     , levies :: Array { to :: Maybe ActorId, in :: ResourceId, amount :: Q }
     , cause :: Cause
     }
  -> Transaction
pairTransaction p =
  { obligations:
      [ leg "delivery" p.sellerActor p.seller p.buyer p.base p.quantity [ bound 1 ]
      , leg "payment" p.buyerActor p.buyer p.seller p.quote p.paid [ bound 0 ]
      ]
        <> map (\l -> leg "levy" p.sellerActor p.seller (maybe K.Destroyed named l.to) l.in l.amount [ bound 0 ]) p.levies
  , ration: { tier: 1, whole: false, scaleGroup: Nothing }
  , phase: 1
  , cause: Just p.cause
  , consented: []
  }
  where
  bound i = { condition: K.Performed { index: i, bond: K.Whole }, reason: Nothing }

  leg :: String -> ActorId -> K.Account -> K.Account -> ResourceId -> Q -> Array _ -> Oblige
  leg oid obligor from to resource owed when =
    { id: oid, obligor: K.Named obligor, effect: K.Move { from, to, resource, owed: K.Exactly owed }, when, onShort: K.Cap, next: K.noNext }

-- | One filled Transform, for what settles this round. The payment posts first, since it caps at what
-- | is held before the inputs go, and every settled Flow stands only if all the inputs are held.
transformTransaction
  :: { by :: ActorId
     , tid :: TransformationId
     , inputs :: Array { resource :: ResourceId, amount :: Q }
     , outputs :: Array { resource :: ResourceId, amount :: Q }
     , payment :: Maybe { terms :: Payment, owed :: Q }
     , cause :: Cause
     }
  -> Transaction
transformTransaction t =
  { obligations: payment <> map input t.inputs <> map output t.outputs
  , ration: { tier: 1, whole: false, scaleGroup: Nothing }
  , phase: 2
  , cause: Just t.cause
  , consented: []
  }
  where
  who = named t.by
  held = map (\i -> { condition: K.StockAtLeast { stock: { at: who, resource: i.resource }, amount: K.Exactly i.amount }, reason: Just (InputsNotHeld t.tid) }) t.inputs
  obligation oid from to resource owed onShort =
    { id: oid, obligor: K.Named t.by, effect: K.Move { from, to, resource, owed: K.Exactly owed }, when: held, onShort, next: K.noNext }
  input i = obligation "input" who K.Destroyed i.resource i.amount K.Reject
  output o = obligation "output" K.Created who o.resource o.amount K.Reject
  payment = case t.payment of
    Just p -> [ obligation "payment" who (named p.terms.to) p.terms.resource p.owed K.Cap ]
    Nothing -> []

-- | One payment that caps at what is held. No payee destroys what moves.
payTransaction :: ActorId -> Maybe ActorId -> ResourceId -> Q -> Cause -> Transaction
payTransaction from to resource owed cause =
  { obligations: [ { id: "pay", obligor: K.Named from, effect: K.Move { from: named from, to: maybe K.Destroyed named to, resource, owed: K.Exactly owed }, when: [], onShort: K.Cap, next: K.noNext } ]
  , ration: { tier: 1, whole: false, scaleGroup: Nothing }
  , phase: 3
  , cause: Just cause
  , consented: []
  }

-- | A time law taking a fraction of what each holder of a Resource holds, out of the economy.
fractionLaw :: ResourceId -> Q -> Cause -> K.Law Capability LegQuery RejectReason Cause
fractionLaw resource fraction cause =
  { subject: K.OfStock { holders: Nothing, resource }
  , trigger: K.EachRound
  , operation: K.Emit
      { from: holder
      , to: K.Destroyed
      , resource
      , owed: K.Least { left: K.Times { ratio: fraction, amount: stock }, right: stock }
      }
  , guard: []
  , phase: 5
  , cause
  }
  where
  holder = K.HeldBy (K.Bound holderParam)
  stock = K.Stock { at: holder, resource }

-- | A change to a holding, due at a round boundary. A negative quantity leaves the economy.
type Delivered = { round :: RoundIndex, actor :: ActorId, resource :: ResourceId, quantity :: Q, cause :: Cause }

boundary :: Int
boundary = 0

standing :: Cause -> Array Oblige -> Transaction
standing cause obligations =
  { obligations, ration: { tier: 0, whole: true, scaleGroup: Nothing }, phase: boundary, cause: Just cause, consented: [] }

-- | A delivery as a spawned transaction. One that would take from a holder with nothing lapses silently, so
-- | a delivery that changes nothing records nothing.
delivery :: Delivered -> PendingTransaction
delivery d = PendingTransaction { due: d.round, transaction: standing d.cause [ obligation ] }
  where
  who = named d.actor
  obligation
    | d.quantity < Q.zero =
        { id: "delivery"
        , obligor: K.Named d.actor
        , effect: K.Move { from: who, to: K.Destroyed, resource: d.resource, owed: K.Exactly (Q.neg d.quantity) }
        , when: [ { condition: K.StockAtLeast { stock: { at: who, resource: d.resource }, amount: K.Exactly Q.ulp }, reason: Nothing } ]
        , onShort: K.Cap
        , next: K.noNext
        }
    | otherwise =
        { id: "delivery"
        , obligor: K.Named d.actor
        , effect: K.Move { from: K.Created, to: who, resource: d.resource, owed: K.Exactly d.quantity }
        , when: []
        , onShort: K.Cap
        , next: K.noNext
        }

-- | What a pending transaction delivers, if it is a delivery.
deliveredOf :: PendingTransaction -> Maybe { actor :: ActorId, resource :: ResourceId, quantity :: Q }
deliveredOf (PendingTransaction p) = head p.transaction.obligations >>= \o -> case o.effect of
  K.Move { from: K.Created, to: K.HeldBy (K.Named actor), resource, owed: K.Exactly quantity } -> Just { actor, resource, quantity }
  K.Move { from: K.HeldBy (K.Named actor), to: K.Destroyed, resource, owed: K.Exactly quantity } -> Just { actor, resource, quantity: Q.neg quantity }
  _ -> Nothing

-- | A Rule change, due at the next boundary. Without a Rule it is a repeal.
entered :: WorldState -> Array ActorId -> EnterF Q -> Array PendingTransaction
entered w consented e = either (const []) (pure <<< PendingTransaction) $ enter w.round (map unwrap w.templates)
  { template: e.template
  , args: Map.fromFoldable (map (\a -> Tuple a.name (K.Exactly a.value)) e.args)
  , parties: Map.fromFoldable (map (\p -> Tuple p.role p.actor) e.parties)
  , consented
  , cause: Just (OfAgreement { template: e.template })
  }

ruleChange :: RoundIndex -> ActorId -> Cause -> RuleId -> Maybe Rule -> PendingTransaction
ruleChange round who cause id rule = PendingTransaction
  { due: round + 1
  , transaction: standing cause
      [ { id: "change"
        , obligor: K.Named who
        , effect: K.Change (maybe (K.RemoveLaw (idText id)) (\law -> K.SetLaw { id: idText id, law: law }) rule)
        , when: []
        , onShort: K.Reject
        , next: K.noNext
        }
      ]
  }

-- | The Rule change a pending transaction makes, if it makes one.
changeOf :: PendingTransaction -> Maybe (Tuple RuleId (Maybe Rule))
changeOf (PendingTransaction p) = head p.transaction.obligations >>= \o -> case o.effect of
  K.Change (K.SetLaw x) -> Just (Tuple (Id x.id) (Just x.law))
  K.Change (K.RemoveLaw id) -> Just (Tuple (Id id) Nothing)
  _ -> Nothing

-- | The inputs of a joint candidate Transaction's filled Transforms, each bound whole to every other: if one party
-- | cannot perform, none does. Each obligation's id is its Transform's position among the inputs.
jointInputs :: Array { by :: ActorId, resource :: ResourceId, amount :: Q, index :: Int } -> Transaction
jointInputs inputs =
  { obligations: mapWithIndex obligation inputs
  , ration: { tier: 0, whole: true, scaleGroup: Nothing }
  , phase: 2
  , cause: Nothing
  , consented: []
  }
  where
  others i = filter (_ /= i) (range 0 (length inputs - 1))
  obligation i x =
    { id: show x.index
    , obligor: K.Named x.by
    , effect: K.Move { from: named x.by, to: K.Destroyed, resource: x.resource, owed: K.Exactly x.amount }
    , when: map (\j -> { condition: K.Performed { index: j, bond: K.Whole }, reason: Nothing }) (others i)
    , onShort: K.Reject
    , next: K.noNext
    }

type WorldLaw = K.Law Capability LegQuery RejectReason Cause

-- | Each flows Rule in force as the laws of its directions, one list per Rule. A levy is charged on the
-- | moved quantity, priced first when it is ad valorem.
flowLaws :: WorldState -> Array (Array WorldLaw)
flowLaws w = mapMaybe ofRule (rulesOf w)
  where
  membersOf group = filter (\e -> fromMaybe e (Map.lookup e w.groups) == group) (fromFoldable (Map.keys w.actors))
  ofRule = case _ of
    FlowRule r -> Just (map (lawOf r) (directions r.scope))
    HoldingRule _ -> Nothing
  directions scope = case scope.direction of
    Inbound -> [ Tuple scope.counterparty scope.governor ]
    Outbound -> [ Tuple scope.governor scope.counterparty ]
    EitherWay -> [ Tuple scope.counterparty scope.governor, Tuple scope.governor scope.counterparty ]
  lawOf r (Tuple from to) =
    { subject: K.OfFlow { from: membersOf from, to: membersOf to, resources: r.scope.over }
    , trigger: K.OnAction { obligor: Nothing, cause: Nothing }
    , operation: case r.effect of
        Block -> K.Refuse
        Levy l -> K.Emit
          { from: K.HeldBy (K.Bound quantityParam)
          , to: case l.to of
              LevyToDestination -> K.HeldBy (K.Bound recipientParam)
              LevyToGovernor -> named r.scope.governor
              LevyToSink -> K.Destroyed
          , resource: l.in
          , owed: case l.basis of
              PerUnit -> K.Times { ratio: l.rate, amount: K.Arg quantityParam }
              AdValorem m -> K.Times { ratio: l.rate, amount: K.Times { ratio: fromMaybe Q.zero (priceOf w m), amount: K.Arg quantityParam } }
          }
    , guard: []
    , phase: 3
    , cause: OfRule { kind: r.kind }
    }

-- | What the authored action laws charge when `actor` settles `quantity` of the action named `action`,
-- | as one transaction per law. A law charges on the action's quantity, through its quantity parameter.
actionTransactions :: WorldState -> ActorId -> String -> Q -> Array Transaction
actionTransactions w explicitActor action quantity = mapMaybe one w.laws
  where
  one (AuthoredLaw law) = case law.trigger of
    K.OnAction scope | maybe true (_ == explicitActor) scope.obligor && maybe true (_ == action) scope.cause -> forHolder explicitActor (priced law)
    _ -> Nothing
  priced law = law
    { operation = case law.operation of
        K.Emit l -> K.Emit l { owed = amount l.owed }
        other -> other
    }
  amount = case _ of
    K.Arg p | p == quantityParam -> K.Exactly quantity
    K.Times t -> K.Times t { amount = amount t.amount }
    other -> other

-- | A time law moving `owed` from one holder, to a holder or out of the economy.
timeLaw :: ActorId -> Maybe ActorId -> ResourceId -> Q -> Int -> Cause -> WorldLaw
timeLaw from to resource owed phase cause =
  { subject: K.OfStock { holders: Just [ from ], resource }
  , trigger: K.EachRound
  , operation: K.Emit { from: K.HeldBy (K.Bound holderParam), to: maybe K.Destroyed named to, resource, owed: K.Exactly owed }
  , guard: []
  , phase
  , cause
  }
