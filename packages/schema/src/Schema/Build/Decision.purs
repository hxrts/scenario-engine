-- | The decision language (docs/02_type_system.md §4.4): the expressions, conditions, Proposal
-- | templates, sign rules, clauses and Strategies a Policy runs. The builders return the
-- | `Core.Strategy` values directly; `Schema.Premise.Strategy` writes them in the premise format.
module Schema.Build.Decision
  ( lit
  , param
  , roundQ
  , price
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
  , selfTariff
  , selfAccessBan
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
  , homeMarket
  , max'
  , not'
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Core.Strategy (Clause, Cond(..), Expr(..), SignRule(..), Strategy, Template(..))
import Core.Strategy as S
import Core.Types (Id(..), IntentF(..), RuleChangeF(..), RuleF(..))
import Schema.Build.Quantity (class ToQty, q)

-- | The Entity an Intent names by `self`: the Entity that runs the Strategy.
selfEntity :: String
selfEntity = S.selfName

-- | The Market a Strategy names by `$home`: the Entity's home Market.
homeMarket :: String
homeMarket = S.homeMarketName

-- Expressions ----------------------------------------------------------------------------

-- | A literal: `lit 12`, `lit "3.2"`.
lit :: forall a. ToQty a => a -> Expr
lit = Lit <<< q

param :: String -> Expr
param = Param

roundQ :: Expr
roundQ = Round

price :: String -> Expr
price = Price <<< Id

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
transformedLastRound r = TransformedLastRound { by: Id r.by, transformation: Id r.transformation, atLeast: r.atLeast }

restrictedLastRound :: { by :: String, target :: String } -> Cond
restrictedLastRound r = RestrictedLastRound { by: Id r.by, target: Id r.target }

restrictionInForce :: { by :: String } -> Cond
restrictionInForce r = RestrictionInForce { by: Id r.by }

-- Templates ------------------------------------------------------------------------------

-- | One Intent, with its quantities as expressions, as a template.
asTemplate :: IntentF Expr -> Template
asTemplate = IntentT

-- | An order on a Market as a template; `limit` is the optional limit price.
orderAt :: String -> Expr -> Maybe Expr -> Template
orderAt market quantity limit = IntentT (Trade { market: Id market, quantity, limitPrice: limit })

-- | Run a Transformation as the acting Entity. A Strategy can only name `self` here: the premise
-- | format writes no `by` for a template, so these three builders take none. (`Schema.Build.Intent`'s
-- | `transform`, `enactTariff` and `enactAccessBan` are the fixture forms, with `Q` quantities.)
selfTransform :: { transformation :: String, quantity :: Expr } -> IntentF Expr
selfTransform r = Transform { by: Id S.selfName, transformation: Id r.transformation, quantity: r.quantity }

-- | Enact a tariff as the acting Entity's jurisdiction.
selfTariff :: { origin :: String, rate :: Expr } -> IntentF Expr
selfTariff r = ChangeRule (Enact (Tariff { jurisdiction: Id S.selfName, origin: Id r.origin, rate: r.rate }))

-- | Enact an access ban as the acting Entity's jurisdiction.
selfAccessBan :: { target :: String } -> IntentF Expr
selfAccessBan r = ChangeRule (Enact (AccessBan { jurisdiction: Id S.selfName, target: Id r.target }))

-- | A Procedure's Proposal: its id, the signers it asks, and its Intents (a Procedure names
-- | the collective procedure the Proposal runs under).
procedure :: { id :: String, signers :: Array String, intents :: Array (IntentF Expr) } -> Template
procedure r = ProposalT { signers: map Id r.signers, intents: r.intents, procedure: Just (Id r.id) }

-- | A Deal: a Proposal with no procedure. Its transfers come first, then its transforms.
deal :: { signers :: Array String, intents :: Array (IntentF Expr) } -> Template
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
