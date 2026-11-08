-- | The decision language (docs/02_type_system.md §4.4): the expressions, conditions, Proposal
-- | templates, sign rules, clauses and Strategies a Policy runs. The builders return the
-- | `Core.Strategy` values directly; `Schema.Setting.Strategy` writes them in the setting format.
module Schema.Build.Decision
  ( lit
  , param
  , roundQ
  , holding
  , factor
  , mulQ
  , subQ
  , gt
  , ge
  , le
  , eqQ
  , transformedLastRound
  , restrictedLastRound
  , restrictionInForce
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
  , signFacility
  , signAll
  , signIf
  , rule
  , passive
  , selfEntity
  , named
  , each
  , cheapest
  , costliest
  , tradeAt
  , priceAt
  , max'
  , not'
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Core.Strategy (Clause, Cond(..), Expr(..), IntentTemplate, MarketRef(..), SignRule(..), Strategy, Template(..))
import Core.Strategy as S
import Core.World (CausePattern(..), LegAggregate(..), Side(..), Window(..))
import Core.Types (Id(..), IntentF(..), RuleChangeF(..), RuleF)
import Schema.Build.Quantity (class ToQty, q)

-- | The Entity an Intent names by `self`: the Entity that runs the Strategy.
selfEntity :: String
selfEntity = S.selfName

-- Market references ----------------------------------------------------------------------

-- | One Market, by id.
named :: String -> MarketRef
named = Named <<< Id

-- | Every accessible Market for the Resource: the clause runs once per Market.
each :: String -> MarketRef
each = Each <<< Id

-- | The accessible Market with the lowest beginning-of-round price for the Resource.
cheapest :: String -> MarketRef
cheapest = Cheapest <<< Id

-- | The accessible Market with the highest beginning-of-round price for the Resource.
costliest :: String -> MarketRef
costliest = Costliest <<< Id

-- | An order on the Market a reference picks; `limit` is the optional limit price.
tradeAt :: { market :: MarketRef, quantity :: Expr, limit :: Maybe Expr } -> IntentTemplate
tradeAt r = Trade { market: r.market, quantity: r.quantity, limitPrice: r.limit }

-- Expressions ----------------------------------------------------------------------------

-- | A literal: `lit 12`, `lit "3.2"`.
lit :: forall a. ToQty a => a -> Expr
lit = Lit <<< q

param :: String -> Expr
param = Param

roundQ :: Expr
roundQ = Round

-- | The beginning-of-round price of the Market a reference picks.
priceAt :: MarketRef -> Expr
priceAt = Price

holding :: String -> Expr
holding = Holding <<< Id

factor :: String -> Expr
factor = Factor <<< Id

mulQ :: Expr -> Expr -> Expr
mulQ = Mul

subQ :: Expr -> Expr -> Expr
subQ = Sub

max' :: Expr -> Expr -> Expr
max' = Max

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
transformedLastRound :: { by :: String, transformation :: String, atLeast :: Expr } -> Cond
transformedLastRound r = Recorded { query: S.OfLegs { party: Id r.by, window: LastRound, side: Outgoing, resource: Nothing, cause: ConversionOf (Id r.transformation), aggregate: LargestUnits }, atLeast: r.atLeast }

restrictedLastRound :: { by :: String, target :: String } -> Cond
restrictedLastRound r = Recorded { query: S.RulesEnacted { governor: Id r.by, counterparty: Id r.target }, atLeast: lit 1 }

restrictionInForce :: { by :: String } -> Cond
restrictionInForce r = Recorded { query: S.RulesInForce { governor: Id r.by }, atLeast: lit 1 }

-- Templates ------------------------------------------------------------------------------

-- | One Intent, with its quantities as expressions, as a template.
asTemplate :: IntentTemplate -> Template
asTemplate = IntentT

-- | An order on the Market a reference picks, as a template; `limit` is the optional limit price.
orderAt :: MarketRef -> Expr -> Maybe Expr -> Template
orderAt market quantity limit = IntentT (tradeAt { market, quantity, limit })

-- | Run a Transformation as the acting Entity. A Strategy can only name `self` here: the setting
-- | format writes no `by` for a template, so these three builders take none. (`Schema.Build.Intent`'s
-- | `transform`, `enactTariff` and `enactAccessBan` are the fixture forms, with `Q` quantities.)
selfTransform :: { transformation :: String, quantity :: Expr } -> IntentTemplate
selfTransform r = Transform { by: Id S.selfName, transformation: Id r.transformation, quantity: r.quantity }

-- | Enact a Rule of a declared kind, with the acting Entity as governor (a flows scope) or owner (a capacity).
selfEnact :: RuleF Expr -> IntentTemplate
selfEnact r = ChangeRule (Enact r)

-- | A Procedure's Proposal: its id, the signers it asks, and its Intents (a Procedure names
-- | the collective procedure the Proposal runs under).
procedure :: { id :: String, signers :: Array String, intents :: Array IntentTemplate } -> Template
procedure r = ProposalT { signers: map Id r.signers, intents: r.intents, procedure: Just (Id r.id) }

-- | A joint Proposal: a Proposal with no procedure, bundling only transfers and transforms (`jointIntent`). Its
-- | transfers come first, then its transforms, each run by the acting Entity. A joint Proposal of any other
-- | Intent has no form here, so it does not typecheck.
joint
  :: { signers :: Array String
     , transfers :: Array { from :: String, to :: String, resource :: String, quantity :: Expr }
     , transforms :: Array { transformation :: String, quantity :: Expr }
     }
  -> Template
joint r = ProposalT
  { signers: map Id r.signers
  , intents: map (\x -> Transfer { from: Id x.from, to: Id x.to, resource: Id x.resource, quantity: x.quantity }) r.transfers <> map selfTransform r.transforms
  , procedure: Nothing
  }

-- | Enact the restriction `target` enacted against this Entity last round.
reciprocate :: { target :: String } -> Template
reciprocate r = Mirror { governor: Id r.target, counterparty: Id S.selfName }

choose :: Array Template -> Template
choose = Choose

-- | Propose nothing: an option for `choose`, or a clause's whole proposal.
noAction :: Template
noAction = NoAction

-- Sign rules -----------------------------------------------------------------------------

signProcedures :: Array String -> SignRule
signProcedures xs = SignProcedures (map Id xs)

signFacility :: String -> SignRule
signFacility = SignConversion <<< Id

signAll :: SignRule
signAll = SignAll

-- | The inner sign rule, while every condition holds.
signIf :: Array Cond -> SignRule -> SignRule
signIf when inner = SignIf { when, rule: inner }

-- Clauses and Strategies -----------------------------------------------------------------

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Clause
rule when propose = { when, propose }

-- | A Strategy that proposes nothing and signs nothing.
passive :: Strategy
passive = { rules: [], sign: [] }
