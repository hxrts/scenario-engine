-- | The compile step: each settled mechanism written as couplings, which the kernel then posts.
module Core.Engine.Compile
  ( WorldKernel
  , kernelOf
  , fillCoupling
  , runCoupling
  ) where

import Prelude

import Core.Kernel.Post (Account(..), Ledger)
import Core.Kernel.Resolve (Consumed, Kernel, resolve)
import Core.Kernel.Types (Account(..), Amount(..), Coupling, Effect(..), Party(..), Short(..), noNext) as K
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capabilitiesOf, holding, reserveOf)
import Core.Types (Capability, EntityId, Market, MarketId, ProposalId, ResourceId, Rule)
import Core.World (Cause(..), Flow, LegQuery, Mechanism(..), RejectReason, WorldState)
import Data.Array (elem, mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

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

