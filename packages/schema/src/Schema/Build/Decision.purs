-- | The decision language (docs/02_type_system.md §4.4): the expressions, conditions, Proposal
-- | templates, sign rules, clauses and Strategies a Policy runs. The builders return the
-- | `Core.Strategy` values directly; `Schema.Premise.Strategy` writes them in the premise format.
module Schema.Build.Decision
  ( lit
  , param
  , round
  , price
  , holding
  , factor
  , mul
  , sub
  , gt
  , ge
  , le
  , eq
  , transformedLastRound
  , restrictedLastRound
  , restrictionInForce
  , asTemplate
  , procedure
  , deal
  , reciprocate
  , choose
  , noAction
  , signProcedures
  , signFacility
  , signAll
  , signIf
  , rule
  , strategy
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
import Core.Types (Id(..), IntentF(..))
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

round :: Expr
round = Round

-- | The beginning-of-round price of a Market, by id.
price :: String -> Expr
price = Price <<< named

-- | The beginning-of-round price of the Market a reference picks.
priceAt :: MarketRef -> Expr
priceAt = Price

holding :: String -> Expr
holding = Holding <<< Id

factor :: String -> Expr
factor = Factor <<< Id

mul :: Expr -> Expr -> Expr
mul = Mul

sub :: Expr -> Expr -> Expr
sub = Sub

max' :: Expr -> Expr -> Expr
max' = Max

-- Conditions -----------------------------------------------------------------------------

gt :: Expr -> Expr -> Cond
gt = Gt

ge :: Expr -> Expr -> Cond
ge = Ge

le :: Expr -> Expr -> Cond
le = Le

eq :: Expr -> Expr -> Cond
eq = Eq

not' :: Cond -> Cond
not' = Not

-- | `atLeast` is an expression: a literal, or `param` for a threshold a parameter sweep can change.
transformedLastRound :: { by :: String, transformation :: String, atLeast :: Expr } -> Cond
transformedLastRound r = TransformedLastRound { by: Id r.by, transformation: Id r.transformation, atLeast: r.atLeast }

restrictedLastRound :: { by :: String, target :: String } -> Cond
restrictedLastRound r = RestrictedLastRound { by: Id r.by, target: Id r.target }

restrictionInForce :: { by :: String } -> Cond
restrictionInForce r = RestrictionInForce { by: Id r.by }

-- Templates ------------------------------------------------------------------------------

-- | One Intent, with its quantities as expressions, as a template.
asTemplate :: IntentTemplate -> Template
asTemplate = IntentT

-- | A Procedure's Proposal: its id, the signers it asks, and its Intents (a Procedure names
-- | the collective procedure the Proposal runs under).
procedure :: { id :: String, signers :: Array String, intents :: Array IntentTemplate } -> Template
procedure r = ProposalT { signers: map Id r.signers, intents: r.intents, procedure: Just (Id r.id) }

-- | A Deal: a Proposal with no procedure. Its transfers come first, then its transforms.
deal :: { signers :: Array String, intents :: Array IntentTemplate } -> Template
deal r = ProposalT { signers: map Id r.signers, intents: r.intents, procedure: Nothing }

-- | Enact the restriction `target` enacted against this Entity last round.
reciprocate :: { target :: String } -> Template
reciprocate r = ReciprocateLastRound { target: Id r.target }

choose :: Array Template -> Template
choose = Choose

-- | Propose nothing: an option for `choose`, or a clause's whole proposal.
noAction :: Template
noAction = NoAction

-- Sign rules -----------------------------------------------------------------------------

signProcedures :: Array String -> SignRule
signProcedures xs = SignProcedures (map Id xs)

signFacility :: String -> SignRule
signFacility = SignFacility <<< Id

signAll :: SignRule
signAll = SignAll

-- | The inner sign rule, while every condition holds.
signIf :: Array Cond -> SignRule -> SignRule
signIf when inner = SignIf { when, rule: inner }

-- Clauses and Strategies -----------------------------------------------------------------

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Clause
rule when propose = { when, propose }

strategy :: { rules :: Array Clause, sign :: Array SignRule } -> Strategy
strategy r = { rules: r.rules, sign: r.sign }
