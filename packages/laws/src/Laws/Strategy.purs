-- | The Strategy language's types (docs/02_type_system.md §4.4): the declarative rules an
-- | in-world Policy runs. The Gameboard's Policy catalog holds these values, so they live in
-- | core with the other domain types; `Policy.Strategy` decodes them and `Policy.Interpret` runs them.
module Laws.Strategy
  ( Strategy
  , Clause
  , Cond(..)
  , RecordQuery(..)
  , Expr(..)
  , Template(..)
  , SignRule(..)
  , InWorld
  , MarketRef(..)
  , IntentTemplate
  , selfName
  , selfId
  ) where

import Prelude

import Core.Num (Q)
import Laws.World (LegQuery)
import Laws.Types (Id(..), BehaviorDomain, CollectiveProcedureId, EntityId, IntentF, MarketId, PolicyId, ProposalF, ResourceId, TransformationId)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)

-- | The sentinel name a Strategy uses: `self` names the acting Entity in an entity position. The
-- | interpreter resolves it (`Policy.Interpret`).
selfName :: String
selfName = "self"

-- | The acting Entity, as an id a template names.
selfId :: EntityId
selfId = Id selfName

-- | How a Strategy names a Market (docs/02 §4.4): one Market by id, or a selector over the Markets
-- | the acting Entity has access to whose base is the Resource. A clause that uses selectors is
-- | instantiated once per Market they pick; an empty pick instantiates nothing.
data MarketRef
  = Named MarketId -- one Market, by id
  | Each ResourceId -- every accessible Market for the Resource
  | Cheapest ResourceId -- the accessible Market with the lowest beginning-of-round price for the Resource
  | Costliest ResourceId -- the accessible Market with the highest beginning-of-round price for the Resource

-- | An Intent as a Strategy writes it: Markets by reference, quantities as expressions.
type IntentTemplate = IntentF MarketRef Expr

-- | Clauses propose; sign rules choose which addressed Proposals to sign.
type Strategy = { rules :: Array Clause, sign :: Array SignRule }

-- | Every condition must hold for the clause's templates to be proposed.
type Clause = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a setting parameter (checked against the Gameboard at load time)
  | Round -- the current round
  | Price MarketRef -- beginning-of-round marginal price of the Market the reference picks
  | Holding ResourceId -- the Entity's own holding
  | Factor BehaviorDomain -- the stored Representation factor, 1 when none
  | Mul Expr Expr
  | Sub Expr Expr
  | Max Expr Expr

data Cond
  = Gt Expr Expr
  | Ge Expr Expr
  | Le Expr Expr
  | Eq Expr Expr
  | Not Cond
  | Recorded { query :: RecordQuery, atLeast :: Expr } -- what a query of the record reads is at least the expression

-- | A question a Strategy asks of the record (docs/02 §4.4): a query of the settlement legs, the Rules enacted by a governor against a counterparty
-- | at this round's boundary, or the Rules in force that a governor governs.
data RecordQuery
  = OfLegs LegQuery
  | RulesEnacted { governor :: EntityId, counterparty :: EntityId }
  | RulesInForce { governor :: EntityId }

-- | A Strategy's template: an Intent or a Proposal with its quantities as expressions, or one of the
-- | combinators that choose among them or depend on the last round's events. `self` names the acting
-- | Entity in any entity position, and a `MarketRef` names a Market (`Policy.Interpret`).
data Template
  = IntentT IntentTemplate -- one Intent (docs/02 §3)
  | ProposalT (ProposalF MarketRef Expr) -- a joint Proposal (no procedure), or a Procedure (its id); its Intents are Intent templates
  | Mirror { governor :: EntityId, counterparty :: EntityId } -- each Rule of a `RulesEnacted` query, turned back on its governor
  | Choose (Array Template) -- one option, picked by a seeded draw
  | NoAction -- propose nothing (an option for Choose)

-- | The in-world Policies (NPC and Aggregate) and the setting parameters they read. The Strategies
-- | are the Gameboard's catalog; this is the Type System's record of them (docs/02 §4.4).
type InWorld = { strategies :: Map PolicyId Strategy, params :: Object Q }

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignConversion TransformationId -- Proposals ordering this Transformation
  | SignAll
  | SignIf { when :: Array Cond, rule :: SignRule } -- the inner rule, while every condition holds

derive instance Eq MarketRef
derive instance Ord MarketRef
derive instance Generic MarketRef _
instance Show MarketRef where
  show = genericShow

derive instance Eq RecordQuery
derive instance Generic RecordQuery _
instance Show RecordQuery where
  show = genericShow

derive instance Generic Expr _
derive instance Generic Cond _
derive instance Generic SignRule _
derive instance Eq Expr
derive instance Eq Cond
derive instance Eq Template
derive instance Eq SignRule
