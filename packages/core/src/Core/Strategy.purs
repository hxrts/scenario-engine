-- | The Strategy language's types (docs/02_type_system.md §4.4): the declarative rules an
-- | in-world Policy runs. The Gameboard's Policy catalog holds these values, so they live in
-- | core with the other domain types; `Policy.Strategy` decodes them and `Policy.Interpret` runs them.
module Core.Strategy
  ( Strategy
  , Clause
  , Cond(..)
  , Expr(..)
  , Template(..)
  , SignRule(..)
  , InWorld
  , selfName
  , homeMarketName
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (BehaviorDomain, CollectiveProcedureId, EntityId, IntentF, MarketId, PolicyId, ProposalF, ResourceId, TransformationId)
import Data.Map (Map)
import Foreign.Object (Object)

-- | The sentinel names a Strategy uses: `self` names the acting Entity in an entity position, and
-- | `$home` the Entity's home Market. The interpreter resolves them (`Policy.Interpret`).
selfName :: String
selfName = "self"

homeMarketName :: String
homeMarketName = "$home"

-- | Clauses propose; sign rules choose which addressed Proposals to sign.
type Strategy = { rules :: Array Clause, sign :: Array SignRule }

-- | Every condition must hold for the clause's templates to be proposed.
type Clause = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a premise parameter (checked against the Gameboard at load time)
  | Round -- the current round
  | Price MarketId -- beginning-of-round marginal price
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
  | TransformedLastRound { by :: EntityId, transformation :: TransformationId, atLeast :: Expr }
  | RestrictedLastRound { by :: EntityId, target :: EntityId } -- a restriction enacted last round
  | RestrictionInForce { by :: EntityId } -- an AccessBan or Tariff by that jurisdiction is in force

-- | A Strategy's template: an Intent or a Proposal with its quantities as expressions, or one of the
-- | combinators that choose among them or depend on the last round's events. `self` names the acting
-- | Entity in any entity position, and `$home` the Entity's home Market (`Policy.Interpret`).
data Template
  = IntentT (IntentF Expr) -- one Intent (docs/02 §3)
  | ProposalT (ProposalF Expr) -- a Deal (no procedure), or a Procedure (its id); its Intents are Intent templates
  | ReciprocateLastRound { target :: EntityId } -- enact the restriction `target` enacted against this Entity last round
  | Choose (Array Template) -- one option, picked by a seeded draw
  | NoAction -- propose nothing (an option for Choose)

-- | The in-world Policies (NPC and Aggregate) and the premise parameters they read. The Strategies
-- | are the Gameboard's catalog; this is the Type System's record of them (docs/02 §4.4).
type InWorld = { strategies :: Map PolicyId Strategy, params :: Object Q }

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignFacility TransformationId -- Proposals ordering this facility (e.g. shipments)
  | SignAll
  | SignIf { when :: Array Cond, rule :: SignRule } -- the inner rule, while every condition holds

derive instance Eq Expr
derive instance Eq Cond
derive instance Eq Template
derive instance Eq SignRule
