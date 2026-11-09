-- | The compile step: each settled mechanism written as couplings, which the kernel then posts.
module Laws.Engine.Compile
  ( WorldKernel
  , kernelOf
  , fillCoupling
  , transformCoupling
  , payCoupling
  , fractionLaw
  , Delivered
  , delivery
  , deliveredOf
  , ruleChange
  , jointInputs
  , flowLaws
  , actionCouplings
  , timeLaw
  , WorldLaw
  , changeOf
  , runCoupling
  ) where

import Prelude

import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Resolve (Consumed, Kernel, resolve)
import Core.Kernel.Dynamic (forHolder, holderParam, quantityParam, recipientParam)
import Core.Kernel.Types (Account(..), Amount(..), Bond(..), Condition(..), Coupling, Effect(..), Dynamic, DynamicChange(..), Obligation, Operation(..), Party(..), Short(..), Subject(..), Trigger(..), noNext) as K
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (capabilitiesOf, holding, priceOf, reserveOf, rulesOf)
import Laws.Types (Capability, Direction(..), EntityId, FlowEffect(..), Id(..), LevyBasis(..), LevyRecipient(..), Market, MarketId, Payment, ProposalId, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, TransformationId, idText)
import Laws.World (AuthoredLaw(..), Cause(..), Flow, LegQuery, Mechanism(..), PendingCoupling(..), RejectReason(..), WorldState)
import Data.Array (elem, filter, fromFoldable, head, length, mapMaybe, mapWithIndex, range)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

type WorldKernel = Kernel Capability LegQuery

type Couple = K.Coupling Capability Rule LegQuery RejectReason Cause

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

-- | Post one coupling on the settlement so far, returning its legs in order. Compile names every party,
-- | so resolution cannot meet an unbound one.
runCoupling :: WorldState -> Ledger -> Couple -> { ledger :: Ledger, flows :: Array Flow, consumed :: Array (Consumed Rule RejectReason) }
runCoupling w start c = case resolve (kernelOf w start) [ c ] of
  Right r -> { ledger: r.ledger, flows: mapMaybe _.flow r.consumed, consumed: r.consumed }
  Left _ -> { ledger: start, flows: [], consumed: [] }

named :: EntityId -> K.Account
named = K.HeldBy <<< K.Named

-- | One cleared fill: the base leg first, then the quote leg. A negative quantity is a sale, so each
-- | leg's direction flips.
fillCoupling :: MarketId -> Market -> { entity :: EntityId, pid :: ProposalId, intent :: Int, quantity :: Q, paid :: Q } -> Couple
fillCoupling id m f =
  { obligations:
      if f.quantity >= Q.zero then [ leg "base" (K.Reserve id) (named f.entity) m.base f.quantity, leg "quote" (named f.entity) (K.Reserve id) m.quote f.paid ]
      else [ leg "base" (named f.entity) (K.Reserve id) m.base (Q.neg f.quantity), leg "quote" (K.Reserve id) (named f.entity) m.quote (Q.neg f.paid) ]
  , holds: []
  , solver: Nothing
  , ration: { tier: 1, whole: false, scaleGroup: Nothing }
  , phase: 1
  , cause: Just (OfIntent { proposal: f.pid, intent: f.intent, mechanism: Curve id })
  , consented: []
  }
  where
  leg :: String -> K.Account -> K.Account -> ResourceId -> Q -> Oblige
  leg oid from to resource owed =
    { id: oid, obligor: K.Named f.entity, effect: K.Move { from, to, resource, owed: K.Exactly owed }, when: [], onShort: K.Cap, next: K.noNext }

-- | One filled Transform, for what settles this round. The payment posts first, since it caps at what
-- | is held before the inputs go, and every leg stands only if all the inputs are held.
transformCoupling
  :: { by :: EntityId
     , tid :: TransformationId
     , inputs :: Array { resource :: ResourceId, amount :: Q }
     , outputs :: Array { resource :: ResourceId, amount :: Q }
     , payment :: Maybe { terms :: Payment, owed :: Q }
     , cause :: Cause
     }
  -> Couple
transformCoupling t =
  { obligations: payment <> map input t.inputs <> map output t.outputs
  , holds: []
  , solver: Nothing
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
payCoupling :: EntityId -> Maybe EntityId -> ResourceId -> Q -> Cause -> Couple
payCoupling from to resource owed cause =
  { obligations: [ { id: "pay", obligor: K.Named from, effect: K.Move { from: named from, to: maybe K.Destroyed named to, resource, owed: K.Exactly owed }, when: [], onShort: K.Cap, next: K.noNext } ]
  , holds: []
  , solver: Nothing
  , ration: { tier: 1, whole: false, scaleGroup: Nothing }
  , phase: 3
  , cause: Just cause
  , consented: []
  }

-- | A time law taking a fraction of what each holder of a Resource holds, out of the economy.
fractionLaw :: ResourceId -> Q -> Cause -> K.Dynamic Capability LegQuery RejectReason Cause
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
type Delivered = { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Q, cause :: Cause }

boundary :: Int
boundary = 0

standing :: Cause -> Array Oblige -> Couple
standing cause obligations =
  { obligations, holds: [], solver: Nothing, ration: { tier: 0, whole: true, scaleGroup: Nothing }, phase: boundary, cause: Just cause, consented: [] }

-- | A delivery as a spawned coupling. One that would take from a holder with nothing lapses silently, so
-- | a delivery that changes nothing records nothing.
delivery :: Delivered -> PendingCoupling
delivery d = PendingCoupling { due: d.round, coupling: standing d.cause [ obligation ] }
  where
  who = named d.entity
  obligation
    | d.quantity < Q.zero =
        { id: "delivery"
        , obligor: K.Named d.entity
        , effect: K.Move { from: who, to: K.Destroyed, resource: d.resource, owed: K.Exactly (Q.neg d.quantity) }
        , when: [ { condition: K.StockAtLeast { stock: { at: who, resource: d.resource }, amount: K.Exactly Q.ulp }, reason: Nothing } ]
        , onShort: K.Cap
        , next: K.noNext
        }
    | otherwise =
        { id: "delivery"
        , obligor: K.Named d.entity
        , effect: K.Move { from: K.Created, to: who, resource: d.resource, owed: K.Exactly d.quantity }
        , when: []
        , onShort: K.Cap
        , next: K.noNext
        }

-- | What a pending coupling delivers, if it is a delivery.
deliveredOf :: PendingCoupling -> Maybe { entity :: EntityId, resource :: ResourceId, quantity :: Q }
deliveredOf (PendingCoupling p) = head p.coupling.obligations >>= \o -> case o.effect of
  K.Move { from: K.Created, to: K.HeldBy (K.Named entity), resource, owed: K.Exactly quantity } -> Just { entity, resource, quantity }
  K.Move { from: K.HeldBy (K.Named entity), to: K.Destroyed, resource, owed: K.Exactly quantity } -> Just { entity, resource, quantity: Q.neg quantity }
  _ -> Nothing

-- | A Rule change, due at the next boundary. Without a Rule it is a repeal.
ruleChange :: RoundIndex -> EntityId -> Cause -> RuleId -> Maybe Rule -> PendingCoupling
ruleChange round who cause id rule = PendingCoupling
  { due: round + 1
  , coupling: standing cause
      [ { id: "change"
        , obligor: K.Named who
        , effect: K.Change (maybe (K.RemoveDynamic (idText id)) (\law -> K.SetDynamic { id: idText id, dynamic: law }) rule)
        , when: []
        , onShort: K.Reject
        , next: K.noNext
        }
      ]
  }

-- | The Rule change a pending coupling makes, if it makes one.
changeOf :: PendingCoupling -> Maybe (Tuple RuleId (Maybe Rule))
changeOf (PendingCoupling p) = head p.coupling.obligations >>= \o -> case o.effect of
  K.Change (K.SetDynamic x) -> Just (Tuple (Id x.id) (Just x.dynamic))
  K.Change (K.RemoveDynamic id) -> Just (Tuple (Id id) Nothing)
  _ -> Nothing

-- | The inputs of a joint Proposal's filled Transforms, each bound whole to every other: if one party
-- | cannot perform, none does. Each obligation's id is its Transform's position among the inputs.
jointInputs :: Array { by :: EntityId, resource :: ResourceId, amount :: Q, index :: Int } -> Couple
jointInputs inputs =
  { obligations: mapWithIndex obligation inputs
  , holds: []
  , solver: Nothing
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

type WorldLaw = K.Dynamic Capability LegQuery RejectReason Cause

-- | Each flows Rule in force as the laws of its directions, one list per Rule. A levy is charged on the
-- | moved quantity, priced first when it is ad valorem.
flowLaws :: WorldState -> Array (Array WorldLaw)
flowLaws w = mapMaybe ofRule (rulesOf w)
  where
  membersOf group = filter (\e -> fromMaybe e (Map.lookup e w.groups) == group) (fromFoldable (Map.keys w.entities))
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
-- | as one coupling per law. A law charges on the action's quantity, through its quantity parameter.
actionCouplings :: WorldState -> EntityId -> String -> Q -> Array Couple
actionCouplings w actor action quantity = mapMaybe one w.laws
  where
  one (AuthoredLaw law) = case law.trigger of
    K.OnAction scope | maybe true (_ == actor) scope.obligor && maybe true (_ == action) scope.cause -> forHolder actor (priced law)
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
timeLaw :: EntityId -> Maybe EntityId -> ResourceId -> Q -> Int -> Cause -> WorldLaw
timeLaw from to resource owed phase cause =
  { subject: K.OfStock { holders: Just [ from ], resource }
  , trigger: K.EachRound
  , operation: K.Emit { from: K.HeldBy (K.Bound holderParam), to: maybe K.Destroyed named to, resource, owed: K.Exactly owed }
  , guard: []
  , phase
  , cause
  }
