-- | The StrategyRules language's types (docs/02_kernel.md §4.4): the declarative rules an
-- | in-world Strategy runs. The composition's Strategy catalog holds these values, so they live in
-- | core with the other domain types; `Strategy.StrategyRules` decodes them and `Strategy.Interpret` runs them.
module Laws.StrategyRules
  ( StrategyRules
  , Clause
  , Cond(..)
  , RecordQuery(..)
  , Expr(..)
  , Template(..)
  , SignRule(..)
  , InWorld
  , ExchangeRef(..)
  , IntentTemplate
  , selfName
  , selfId
  ) where

import Prelude

import Core.Num (Q)
import Laws.World (LegQuery)
import Laws.Types (Id(..), BehaviorDomain, CollectiveProcedureId, ActorId, IntentF, ExchangeId, RuleKindId, StrategyId, CandidateTransactionF, ResourceId, TransformationId)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)

-- | The sentinel name a StrategyRules uses: `self` names the acting Actor in an Actor position. The
-- | interpreter resolves it (`Strategy.Interpret`).
selfName :: String
selfName = "self"

-- | The acting Actor, as an id a template names.
selfId :: ActorId
selfId = Id selfName

-- | How a StrategyRules names an Exchange (docs/02 §4.4): one Exchange by id, or a selector over the Exchanges
-- | the acting Actor has access to whose base is the Resource. A clause that uses selectors is
-- | instantiated once per Exchange they pick; an empty pick instantiates nothing.
data ExchangeRef
  = Named ExchangeId -- one Exchange, by id
  | Each ResourceId -- every accessible Exchange for the Resource
  | Cheapest ResourceId -- the accessible Exchange with the lowest beginning-of-round price for the Resource
  | Costliest ResourceId -- the accessible Exchange with the highest beginning-of-round price for the Resource

-- | An Intent as a StrategyRules writes it: Exchanges by reference, quantities as expressions.
type IntentTemplate = IntentF ExchangeRef Expr

-- | Clauses propose; sign rules choose which addressed candidate Transactions to sign.
type StrategyRules = { rules :: Array Clause, sign :: Array SignRule }

-- | Every condition must hold for the clause's templates to be proposed.
type Clause = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a premise parameter (checked against the composition at load time)
  | Round -- the current round
  | Price ExchangeRef -- beginning-of-round marginal price of the Exchange the reference picks
  | Holding ResourceId -- the Actor's own holding
  | Factor BehaviorDomain -- the stored Representation factor, 1 when none
  | Due ResourceId -- the fixed amount of a Resource the Actor's recurring flows take from it each round
  | Scheduled ResourceId -- what scheduled Flows still pending will deliver of a Resource to the Actor
  | Planned ResourceId -- what the Transforms the Strategy drafted earlier this round consume of a Resource
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
  | Acting ActorId -- the acting Actor is this one, so one StrategyRules can say what each seat it serves does

-- | A question a StrategyRules asks of the record (docs/02 §4.4): a query of the settlement settled Flows, the Rules enacted by a governor against a counterparty
-- | at this round's boundary, or the Rules in force that a governor governs.
data RecordQuery
  = OfLegs LegQuery
  | RulesEnacted { governor :: ActorId, counterparty :: ActorId }
  | RulesInForce { governor :: ActorId, kinds :: Array RuleKindId } -- of these kinds, or of any when none is named

-- | A StrategyRules's template: an Intent or a candidate Transaction with its quantities as expressions, or one of the
-- | combinators that choose among them or depend on the last round's events. `self` names the acting
-- | Actor in any Actor position, and a `ExchangeRef` names an Exchange (`Strategy.Interpret`).
data Template
  = IntentT IntentTemplate -- one Intent (docs/02 §3)
  | CandidateTransactionT (CandidateTransactionF ExchangeRef Expr) -- a joint candidate Transaction (no procedure), or a Procedure (its id); its Intents are Intent templates
  | Mirror { governor :: ActorId, counterparty :: ActorId } -- each Rule of a `RulesEnacted` query, turned back on its governor
  | Choose (Array Template) -- one option, picked by a seeded draw
  | NoAction -- propose nothing (an option for Choose)

-- | The in-world Strategies (non-playable Actor and Aggregate) and the premise parameters they read. The Strategies
-- | are the composition's catalog; this is the Type System's record of them (docs/02 §4.4).
type InWorld = { strategyRules :: Map StrategyId StrategyRules, params :: Object Q }

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignTransformation TransformationId -- candidate Transactions ordering this Transformation
  | SignAgreements (Array String) -- candidate Transactions entering one of these templates
  | SignAll
  | SignIf { when :: Array Cond, rule :: SignRule } -- the inner rule, while every condition holds

derive instance Eq ExchangeRef
derive instance Ord ExchangeRef
derive instance Generic ExchangeRef _
instance Show ExchangeRef where
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
