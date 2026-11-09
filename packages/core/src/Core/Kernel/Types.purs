-- | The kernel's vocabulary (docs/02_type_system.md): stocks, flows, laws over them, and couplings of
-- | conditional obligations. Nothing here names an agreement, an institution or a schedule: those are
-- | values an authoring layer writes. `cap` is what a party may be required to hold, `reason` what a
-- | failed condition reports, and `cause` what produced a settlement; the kernel only carries them.
module Core.Kernel.Types
  ( Param
  , TemplateId
  , ObligationId
  , ObligationIndex
  , Phase
  , Tier
  , Party(..)
  , Account(..)
  , StockRef
  , Amount(..)
  , Leg
  , Bond(..)
  , Short(..)
  , Limit(..)
  , Condition(..)
  , Guarded
  , Effect(..)
  , LawChange(..)
  , Spawn
  , Next
  , noNext
  , Obligation
  , Hold
  , Solver(..)
  , Ration
  , Coupling
  , Template
  , Pending
  , StockScope
  , FlowScope
  , ActionScope
  , Subject(..)
  , Trigger(..)
  , Operation(..)
  , Law
  ) where

import Core.Num (Q)
import Core.Types (EntityId, MarketId, ResourceId, RoundOffset, RoundIndex)
import Data.Map (Map)
import Data.Maybe (Maybe)

type Param = String
type TemplateId = String
type ObligationId = String
type ObligationIndex = Int

-- | Where in a round's fixed sequence a coupling or a law settles. Lower runs first.
type Phase = Int

-- | Rationing priority: lower fills first.
type Tier = Int

-- | Who an obligation binds: a named party, or one a template binds from its parent.
data Party = Named EntityId | Bound Param

-- | Where a quantity sits or moves to. `Created` and `Destroyed` are the economy's boundary.
data Account
  = HeldBy Party
  | Reserve MarketId
  | Created
  | Destroyed

type StockRef = { at :: Account, resource :: ResourceId }

-- | A quantity computed when its obligation is consumed. Arguments are frozen values. `query` is a
-- | read of the record, opaque to the kernel.
data Amount query
  = Exactly Q
  | Arg Param
  | Stock StockRef
  | Times Q (Amount query)
  | Plus (Amount query) (Amount query)
  | Minus (Amount query) (Amount query)
  | Least (Amount query) (Amount query)
  | ShareOf { limit :: Amount query, among :: Array (Amount query), index :: Int }
  | Read query

type Leg query = { from :: Account, to :: Account, resource :: ResourceId, owed :: Amount query }

-- | How a dependency on another obligation binds: all or nothing, or in proportion at a rate.
data Bond = Whole | Proportional Q

-- | What a shortfall does: the obligation is not performed, or it moves what is held.
data Short = Reject | Cap

data Limit = AtMost Q | AtLeast Q

data Condition cap query
  = Performed ObligationIndex Bond
  | StockAtLeast StockRef (Amount query)
  | Consent (Array EntityId)
  | WeightedConsent { weights :: Map EntityId Q, residual :: Q, threshold :: Q }
  | PriceWithin Limit
  | QuantityAtLeast (Amount query)
  | Holds Party cap

-- | A condition and what its failure reports. No reason is a silent removal.
type Guarded cap query reason = { condition :: Condition cap query, reason :: Maybe reason }

-- | A change to the laws in force, applied when its obligation is consumed.
data LawChange law = SetLaw String law | RemoveLaw String

data Effect law query
  = Move (Leg query)
  | Change (LawChange law)

-- | A successor, as data: what to instantiate, how many rounds on (at least 1), and its frozen arguments.
type Spawn query =
  { template :: TemplateId
  , due :: RoundOffset
  , args :: Map Param (Amount query)
  , parties :: Map Param Party
  }

-- | What each way of being consumed spawns.
type Next query = { performed :: Array (Spawn query), short :: Array (Spawn query), lapsed :: Array (Spawn query) }

noNext :: forall query. Next query
noNext = { performed: [], short: [], lapsed: [] }

type Obligation cap law query reason =
  { id :: ObligationId
  , obligor :: Party
  , effect :: Effect law query
  , when :: Array (Guarded cap query reason)
  , onShort :: Short
  , next :: Next query
  }

-- | A quantity set aside for the round and released at commit. It moves nothing.
type Hold query = { party :: Party, resource :: ResourceId, amount :: Amount query }

-- | How one limit is divided among many: the closed set of solvers.
data Solver
  = Clearing { market :: MarketId }
  | Routing { base :: ResourceId, quote :: ResourceId, limit :: Q, candidates :: Array MarketId }
  | Pool { resource :: Maybe ResourceId, perUnit :: Q }

-- | Who shares a shortage: the tier that fills first, whether a coupling is whole or scaled, and the
-- | group whose claims scale together.
type Ration = { tier :: Tier, whole :: Boolean, scaleGroup :: Maybe EntityId }

type Coupling cap law query reason cause =
  { obligations :: Array (Obligation cap law query reason)
  , holds :: Array (Hold query)
  , solver :: Maybe Solver
  , ration :: Ration
  , phase :: Phase
  , cause :: Maybe cause -- Nothing inherits the parent's
  }

type Template cap law query reason cause =
  { params :: Array Param
  , parties :: Array Param
  , coupling :: Coupling cap law query reason cause
  }

type Pending cap law query reason cause = { due :: RoundIndex, coupling :: Coupling cap law query reason cause }

type StockScope = { holders :: Maybe (Array EntityId), resource :: ResourceId }
type FlowScope = { from :: Array EntityId, to :: Array EntityId, resources :: Array ResourceId }
type ActionScope = { obligor :: Maybe EntityId, cause :: Maybe String }

data Subject = OfStock StockScope | OfFlow FlowScope

data Trigger = EachRound | OnAction ActionScope

data Operation query
  = Emit (Leg query)
  | Refuse
  | Withhold (Amount query)

type Law cap query reason cause =
  { subject :: Subject
  , trigger :: Trigger
  , operation :: Operation query
  , guard :: Array (Guarded cap query reason)
  , phase :: Phase
  , cause :: cause
  }
