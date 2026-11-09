-- | The checks a round makes: `RoundF f p`, one record of what a round
-- | holds, with the filling at the leaves and `p` for a named candidate Transaction. `Observed` is what the engine's
-- | round produced (`Checks.Round.observe`); `Expect` is what a premise asserts, with sparse maps (an
-- | absent key or a `Nothing` leaf asserts nothing) and a `Nothing` field asserting nothing, and
-- | `Authored` is an `Expect` as a premise writes it, whose candidate Transactions are named by author and position
-- | because their ids depend on the round. All are values of this one type, so the checker compares
-- | them field by field.
module Schema.Round
  ( Observed
  , Expect
  , Authored
  , CandidateTransactionRef
  , Delivery
  , ProcedureOf
  , ProcedureOutcome
  , RoundF
  , RoundRow
  , noExpect
  , atRound
  ) where

import Prelude

import Core.Num (Q)
import Laws.Types (BehaviorDomain, ActorId, ExchangeId, CandidateTransactionId, ResourceId, RoundIndex, candidateTransactionId)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | A delivery an Actor is owed: the Resource and the quantity it receives.
type Delivery = { actor :: ActorId, resource :: ResourceId, quantity :: Q }

-- | A candidate Transaction named in an expectation: its author and its position among the author's candidate Transactions that round.
type CandidateTransactionRef = { author :: ActorId, index :: Int }

-- | A collective procedure's outcome: the candidate Transaction it put to a vote, the support behind it, and whether it ran.
type ProcedureOf p = { candidateTransaction :: p, support :: Q, executed :: Boolean }

type ProcedureOutcome = ProcedureOf CandidateTransactionId

-- | One round's fields. The maps hold a leaf per (key, key) pair; the whole-round lists and the count
-- | hold one value each.
type RoundF f p = Record
  ( RoundRow
      (Map ActorId (Map ResourceId (f Q)))
      (Map ExchangeId (Map ResourceId (f Q)))
      (Map ActorId (Map BehaviorDomain (f Q)))
      (f (Array Delivery))
      (f (Array p))
      (f (Array (ProcedureOf p)))
      (f Int)
  )

-- | One slot per field of a round. `Checks.Round.check` holds one comparer per slot, so a new field fails to compile.
type RoundRow :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Row Type
type RoundRow accounts reserves factors deliveries candidateTransactions procedures count =
  ( accounts :: accounts
  , reserves :: reserves
  , factors :: factors
  , deliveries :: deliveries
  , rejected :: candidateTransactions
  , lapsed :: candidateTransactions
  , procedures :: procedures
  , scheduled :: count
  )

type Observed = RoundF Identity CandidateTransactionId

type Expect = RoundF Maybe CandidateTransactionId

type Authored = RoundF Maybe CandidateTransactionRef

-- | An expectation that checks nothing: every field is absent.
noExpect :: forall p. RoundF Maybe p
noExpect = { accounts: Map.empty, reserves: Map.empty, factors: Map.empty, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, procedures: Nothing, scheduled: Nothing }

-- | An authored expectation in `round`: each candidate Transaction it names takes its id in that round.
atRound :: RoundIndex -> Authored -> Expect
atRound round e = e
  { rejected = map (map idOf) e.rejected
  , lapsed = map (map idOf) e.lapsed
  , procedures = map (map (\p -> p { candidateTransaction = idOf p.candidateTransaction })) e.procedures
  }
  where
  idOf r = candidateTransactionId round r.author r.index
