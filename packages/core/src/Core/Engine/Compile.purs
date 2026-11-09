-- | The compile step: each settled mechanism written as couplings, which the kernel then posts.
module Core.Engine.Compile
  ( WorldKernel
  , kernelOf
  , fillCoupling
  , transformCoupling
  , payCoupling
  , fractionLaw
  , runCoupling
  ) where

import Prelude

import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Resolve (Consumed, Kernel, resolve)
import Core.Kernel.Law (holderParam)
import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Coupling, Effect(..), Law, Operation(..), Party(..), Short(..), Subject(..), Trigger(..), noNext) as K
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capabilitiesOf, holding, reserveOf)
import Core.Types (Capability, EntityId, Market, MarketId, Payment, ProposalId, ResourceId, Rule, TransformationId)
import Core.World (Cause(..), Flow, LegQuery, Mechanism(..), RejectReason(..), WorldState)
import Data.Array (elem, mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)

type WorldKernel = Kernel Capability LegQuery

type Couple = K.Coupling Capability Rule LegQuery RejectReason Cause

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
  leg :: String -> K.Account -> K.Account -> ResourceId -> Q -> _
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
