-- | The decision language (docs/02_kernel.md §4.4): the expressions, conditions, candidate Transaction
-- | templates, sign rules, clauses and Strategies a Strategy runs. The builders return the
-- | `Laws.StrategyRules` values directly; `Schema.Premise.StrategyRules` writes them in the premise format.
module Schema.Build.Decision
  ( lit
  , param
  , roundQ
  , holding
  , factor
  , due
  , scheduled
  , planned
  , acting
  , mulQ
  , subQ
  , gt
  , ge
  , le
  , eqQ
  , transformedLastRound
  , restrictedLastRound
  , restrictionInForce
  , ruleInForce
  , asTemplate
  , orderAt
  , selfTransform
  , selfEnact
  , procedure
  , joint
  , reciprocate
  , choose
  , noAction
  , signProcedures
  , signTransformation
  , signAll
  , signIf
  , rule
  , passive
  , selfActor
  , named
  , each
  , cheapest
  , costliest
  , tradeAt
  , priceAt
  , max'
  , divQ
  , not'
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Laws.StrategyRules (Clause, Cond(..), Expr(..), IntentTemplate, ExchangeRef(..), SignRule(..), StrategyRules, Template(..))
import Laws.StrategyRules as S
import Laws.World (CausePattern(..), LegAggregate(..), Side(..), Window(..))
import Laws.Types (ActorId, ActorTag, Id(..), IntentF(..), ExchangeTag, ProcedureTag, ResourceId, ResourceTag, RuleChangeF(..), RuleF, RuleKindTag, TransformationId, TransformationTag, idText)
import Schema.Build.Named (class Named)
import Schema.Build.Named (named) as N
import Schema.Build.Quantity (class ToQty, q)

-- | The Actor an Intent names by `self`: the Actor that runs the StrategyRules.
selfActor :: ActorId
selfActor = Id S.selfName

-- Exchange references ----------------------------------------------------------------------

-- | One Exchange, by id.
named :: forall a. Named a ExchangeTag => a -> ExchangeRef
named = Named <<< N.named

-- | Every accessible Exchange for the Resource: the clause runs once per Exchange.
each :: forall a. Named a ResourceTag => a -> ExchangeRef
each = Each <<< N.named

-- | The accessible Exchange with the lowest beginning-of-round price for the Resource.
cheapest :: forall a. Named a ResourceTag => a -> ExchangeRef
cheapest = Cheapest <<< N.named

-- | The accessible Exchange with the highest beginning-of-round price for the Resource.
costliest :: forall a. Named a ResourceTag => a -> ExchangeRef
costliest = Costliest <<< N.named

-- | An order on the Exchange a reference picks; `limit` is the optional limit price.
tradeAt :: { exchange :: ExchangeRef, quantity :: Expr, limit :: Maybe Expr } -> IntentTemplate
tradeAt r = Trade { exchange: r.exchange, quantity: r.quantity, limitPrice: r.limit }

-- Expressions ----------------------------------------------------------------------------

-- | A literal: `lit 12`, `lit "3.2"`.
lit :: forall a. ToQty a => a -> Expr
lit = Lit <<< q

param :: String -> Expr
param = Param

roundQ :: Expr
roundQ = Round

-- | The beginning-of-round price of the Exchange a reference picks.
priceAt :: ExchangeRef -> Expr
priceAt = Price

holding :: forall a. Named a ResourceTag => a -> Expr
holding = Holding <<< N.named

factor :: forall a. Named a ResourceTag => a -> Expr
factor x = Factor (Id (idText (N.named x :: ResourceId)))

-- | The fixed amount of a Resource the Actor's recurring flows take from it each round.
due :: forall a. Named a ResourceTag => a -> Expr
due = Due <<< N.named

-- | What scheduled Flows still pending will deliver of a Resource to the Actor.
scheduled :: forall a. Named a ResourceTag => a -> Expr
scheduled = Scheduled <<< N.named

-- | What the Transforms drafted earlier this round consume of a Resource.
planned :: forall a. Named a ResourceTag => a -> Expr
planned = Planned <<< N.named

mulQ :: Expr -> Expr -> Expr
mulQ = Mul

subQ :: Expr -> Expr -> Expr
subQ = Sub

max' :: Expr -> Expr -> Expr
max' = Max

-- | A quotient, zero when the divisor is zero.
divQ :: Expr -> Expr -> Expr
divQ = Div

-- Conditions -----------------------------------------------------------------------------

gt :: Expr -> Expr -> Cond
gt = Gt

ge :: Expr -> Expr -> Cond
ge = Ge

le :: Expr -> Expr -> Cond
le = Le

eqQ :: Expr -> Expr -> Cond
eqQ = Eq

not' :: Cond -> Cond
not' = Not

-- | `atLeast` is an expression: a literal, or `param` for a threshold a parameter sweep can change.
transformedLastRound :: forall b t. Named b ActorTag => Named t TransformationTag => { by :: b, transformation :: t, atLeast :: Expr } -> Cond
transformedLastRound r = Recorded { query: S.OfLegs { party: N.named r.by, window: LastRound, side: Outgoing, resource: Nothing, cause: TransformationOf (N.named r.transformation), aggregate: LargestUnits }, atLeast: r.atLeast }

restrictedLastRound :: forall b t. Named b ActorTag => Named t ActorTag => { by :: b, target :: t } -> Cond
restrictedLastRound r = Recorded { query: S.RulesEnacted { governor: N.named r.by, counterparty: N.named r.target }, atLeast: lit 1 }

-- | The acting Actor is this one: lets one StrategyRules say what each seat it serves does.
acting :: forall a. Named a ActorTag => a -> Cond
acting = Acting <<< N.named

restrictionInForce :: forall b. Named b ActorTag => { by :: b } -> Cond
restrictionInForce r = Recorded { query: S.RulesInForce { governor: N.named r.by, kinds: [] }, atLeast: lit 1 }

-- | A Rule of one of these kinds is in force, governed by `by`.
ruleInForce :: forall b k. Named b ActorTag => Named k RuleKindTag => { by :: b, kinds :: Array k } -> Cond
ruleInForce r = Recorded { query: S.RulesInForce { governor: N.named r.by, kinds: map N.named r.kinds }, atLeast: lit 1 }

-- Templates ------------------------------------------------------------------------------

-- | One Intent, with its quantities as expressions, as a template.
asTemplate :: IntentTemplate -> Template
asTemplate = IntentT

-- | An order on the Exchange a reference picks, as a template; `limit` is the optional limit price.
orderAt :: ExchangeRef -> Expr -> Maybe Expr -> Template
orderAt exchange quantity limit = IntentT (tradeAt { exchange, quantity, limit })

-- | Run a Transformation as the acting Actor. A StrategyRules can only name `self` here: the premise
-- | format writes no `by` for a template, so these three builders take none. (`Schema.Build.Intent`'s
-- | `transform`, `enactTariff` and `enactAccessBan` are the fixture forms, with `Q` quantities.)
selfTransform :: forall t. Named t TransformationTag => { transformation :: t, quantity :: Expr } -> IntentTemplate
selfTransform r = Transform { by: Id S.selfName, transformation: N.named r.transformation, quantity: r.quantity }

-- | Enact a Rule of a declared kind, with the acting Actor as governor (a flows scope) or holder (a holding Rule).
selfEnact :: RuleF Expr -> IntentTemplate
selfEnact r = ChangeRule (Enact r)

-- | A Procedure's candidate Transaction: its id, the signers it asks, and its Intents (a Procedure names
-- | the collective procedure the candidate Transaction runs under).
procedure :: forall s i. Named i ProcedureTag => Named s ActorTag => { id :: i, signers :: Array s, intents :: Array IntentTemplate } -> Template
procedure r = CandidateTransactionT { signers: map N.named r.signers, intents: r.intents, procedure: Just (N.named r.id) }

-- | A joint candidate Transaction: a candidate Transaction with no procedure, bundling only transfers and transforms (`jointIntent`). Its
-- | transfers come first, then its transforms, each run by the acting Actor. A joint candidate Transaction of any other
-- | Intent has no form here, so it does not typecheck.
joint
  :: { signers :: Array ActorId
     , transfers :: Array { from :: ActorId, to :: ActorId, resource :: ResourceId, quantity :: Expr }
     , transforms :: Array { transformation :: TransformationId, quantity :: Expr }
     }
  -> Template
joint r = CandidateTransactionT
  { signers: r.signers
  , intents: map (\x -> Transfer { from: x.from, to: x.to, resource: x.resource, quantity: x.quantity }) r.transfers <> map selfTransform r.transforms
  , procedure: Nothing
  }

-- | Enact the restriction `target` enacted against this Actor last round.
reciprocate :: forall t. Named t ActorTag => { target :: t } -> Template
reciprocate r = Mirror { governor: N.named r.target, counterparty: Id S.selfName }

choose :: Array Template -> Template
choose = Choose

-- | Propose nothing: an option for `choose`, or a clause's whole proposal.
noAction :: Template
noAction = NoAction

-- Sign rules -----------------------------------------------------------------------------

signProcedures :: forall a. Named a ProcedureTag => Array a -> SignRule
signProcedures xs = SignProcedures (map N.named xs)

signTransformation :: forall a. Named a TransformationTag => a -> SignRule
signTransformation = SignTransformation <<< N.named

signAll :: SignRule
signAll = SignAll

-- | The inner sign rule, while every condition holds.
signIf :: Array Cond -> SignRule -> SignRule
signIf when inner = SignIf { when, rule: inner }

-- Clauses and Strategies -----------------------------------------------------------------

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Clause
rule when propose = { when, propose }

-- | A StrategyRules that proposes nothing and signs nothing.
passive :: StrategyRules
passive = { rules: [], sign: [] }
