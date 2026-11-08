-- | The checks a round makes (§13 Phase 6): `RoundF f`, one record of what a round
-- | holds, with the filling at the leaves. `Observed = RoundF Identity` is what the engine's round
-- | produced (`Checks.Round.observe`); `Expect = RoundF Maybe` is what a premise asserts, with sparse
-- | maps (an absent key or a `Nothing` leaf asserts nothing) and a `Nothing` field asserting nothing.
-- | Both sides are values of this one type, so the checker compares them field by field.
module Schema.Round
  ( Observed
  , Expect
  , Delivery
  , ProcedureOutcome
  , RoundF
  ) where

import Core.Num (Q)
import Core.Types (BehaviorDomain, EntityId, MarketId, ProposalId, ResourceId)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Maybe (Maybe)

-- | A delivery an Entity is owed: the Resource and the quantity it receives.
type Delivery = { entity :: EntityId, resource :: ResourceId, quantity :: Q }

-- | A collective procedure's outcome: the Proposal it put to a vote, the support behind it, and whether it ran.
type ProcedureOutcome = { proposal :: ProposalId, support :: Q, executed :: Boolean }

-- | One round's fields. The maps hold a leaf per (key, key) pair; the whole-round lists and the count
-- | hold one value each.
type RoundF f =
  { accounts :: Map EntityId (Map ResourceId (f Q))
  , reserves :: Map MarketId (Map ResourceId (f Q))
  , factors :: Map EntityId (Map BehaviorDomain (f Q))
  , deliveries :: f (Array Delivery)
  , rejected :: f (Array ProposalId)
  , lapsed :: f (Array ProposalId)
  , procedures :: f (Array ProcedureOutcome)
  , scheduled :: f Int
  }

type Observed = RoundF Identity

type Expect = RoundF Maybe
