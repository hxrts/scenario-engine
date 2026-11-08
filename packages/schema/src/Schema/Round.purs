-- | The checks a round makes (§13 Phase 6): `RoundF f p`, one record of what a round
-- | holds, with the filling at the leaves and `p` for a named Proposal. `Observed` is what the engine's
-- | round produced (`Checks.Round.observe`); `Expect` is what a setting asserts, with sparse maps (an
-- | absent key or a `Nothing` leaf asserts nothing) and a `Nothing` field asserting nothing, and
-- | `Authored` is an `Expect` as a setting writes it, whose Proposals are named by author and position
-- | because their ids depend on the round. All are values of this one type, so the checker compares
-- | them field by field.
module Schema.Round
  ( Observed
  , Expect
  , Authored
  , ProposalRef
  , Delivery
  , ProcedureOf
  , ProcedureOutcome
  , RoundF
  , noExpect
  , atRound
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (BehaviorDomain, EntityId, MarketId, ProposalId, ResourceId, RoundIndex, proposalId)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | A delivery an Entity is owed: the Resource and the quantity it receives.
type Delivery = { entity :: EntityId, resource :: ResourceId, quantity :: Q }

-- | A Proposal named in an expectation: its author and its position among the author's Proposals that round.
type ProposalRef = { author :: EntityId, index :: Int }

-- | A collective procedure's outcome: the Proposal it put to a vote, the support behind it, and whether it ran.
type ProcedureOf p = { proposal :: p, support :: Q, executed :: Boolean }

type ProcedureOutcome = ProcedureOf ProposalId

-- | One round's fields. The maps hold a leaf per (key, key) pair; the whole-round lists and the count
-- | hold one value each.
type RoundF f p =
  { accounts :: Map EntityId (Map ResourceId (f Q))
  , reserves :: Map MarketId (Map ResourceId (f Q))
  , factors :: Map EntityId (Map BehaviorDomain (f Q))
  , deliveries :: f (Array Delivery)
  , rejected :: f (Array p)
  , lapsed :: f (Array p)
  , procedures :: f (Array (ProcedureOf p))
  , scheduled :: f Int
  }

type Observed = RoundF Identity ProposalId

type Expect = RoundF Maybe ProposalId

type Authored = RoundF Maybe ProposalRef

-- | An expectation that checks nothing: every field is absent.
noExpect :: forall p. RoundF Maybe p
noExpect = { accounts: Map.empty, reserves: Map.empty, factors: Map.empty, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, procedures: Nothing, scheduled: Nothing }

-- | An authored expectation in `round`: each Proposal it names takes its id in that round.
atRound :: RoundIndex -> Authored -> Expect
atRound round e = e
  { rejected = map (map idOf) e.rejected
  , lapsed = map (map idOf) e.lapsed
  , procedures = map (map (\p -> p { proposal = idOf p.proposal })) e.procedures
  }
  where
  idOf r = proposalId round r.author r.index
