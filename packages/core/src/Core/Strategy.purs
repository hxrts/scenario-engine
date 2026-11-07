-- | The Strategy language's types (docs/02_type_system.md §4.4): the declarative rules an
-- | in-world Policy runs. The Gameboard's Policy catalog holds these values, so they live in
-- | core with the other domain types; `Policy.Strategy` decodes them and `Policy.Interpret` runs them.
module Core.Strategy
  ( Strategy
  , Clause
  , Cond(..)
  , Expr(..)
  , Template(..)
  , TransferTemplate
  , SignRule(..)
  ) where

import Core.Num (Q)
import Data.Maybe (Maybe)
import Core.Types (BehaviorDomain, CapacityRef, CollectiveProcedureId, EntityId, MarketId, Pair, ResourceId, RuleId, TransformationId)

-- | Clauses propose; sign rules choose which addressed Proposals to sign.
type Strategy = { rules :: Array Clause, sign :: Array SignRule }

-- | Every condition must hold for the clause's templates to be proposed.
type Clause = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a scenario parameter (checked against the Gameboard at load time)
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

-- | A Transfer a template carries: the agent `from` gives `quantity` to `to`.
type TransferTemplate = { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }

data Template
  = TradeT { market :: MarketId, quantity :: Expr, limit :: Maybe Expr }
  | RouteT { pair :: Pair, quantity :: Expr, limit :: Maybe Expr } -- best execution: no Market named (docs/02 §5.2)
  | TransformT { transformation :: TransformationId, quantity :: Expr }
  | TransferT TransferTemplate -- an Intent a procedure may carry (docs/05 §12.1)
  | Procedure { procedure :: CollectiveProcedureId, signers :: Array EntityId, intents :: Array Template } -- intents: trades, transforms, transfers, tariffs, reciprocations only
  | EnactTariff { origin :: EntityId, rate :: Expr }
  | ReciprocateLastRound { target :: EntityId } -- enact the restriction `target` enacted against this Entity last round
  | Deal { signers :: Array EntityId, transfers :: Array TransferTemplate, transforms :: Array { transformation :: TransformationId, quantity :: Expr } } -- `self` names the author; a deal bundles Transfers and Transforms (docs/02 §3)
  | Choose (Array Template) -- one option, picked by a seeded draw
  | EnactAccessBan { target :: EntityId } -- a ban on shipments from the Entity's jurisdiction into `target`
  | EnactWithholding { capacity :: CapacityRef, units :: Expr } -- withhold units of a capacity the Entity owns
  | RepealRule { rule :: RuleId } -- repeal a Rule the Entity governs, by the id the engine assigned it
  | NoAction -- propose nothing (an option for Choose)

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignFacility TransformationId -- Proposals ordering this facility (e.g. shipments)
  | SignAll
  | SignIf { when :: Array Cond, rule :: SignRule } -- the inner rule, while every condition holds

