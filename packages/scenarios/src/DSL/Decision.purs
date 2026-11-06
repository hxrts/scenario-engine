-- | The decision language (docs/02_type_system.md §4.4): the expressions, conditions,
-- | Proposal templates, sign rules and clauses from which a Policy's Program is built. The
-- | builders emit the JSON that the `policy` interpreter reads.
module DSL.Decision
  ( Expr
  , lit
  , param
  , round
  , price
  , holding
  , factor
  , mul
  , sub
  , max'
  , Cond
  , gt
  , ge
  , le
  , eq
  , not'
  , transformedLastRound
  , restrictedLastRound
  , restrictionInForce
  , Template
  , tradeT
  , transformT
  , procedureT
  , enactTariffT
  , deal
  , reciprocate
  , choose
  , noAction
  , SignRule
  , signProcedures
  , signFacility
  , signAll
  , Clause
  , rule
  , program
  ) where

import Prelude

import DSL.Base (Built, class ToQty, Qty, build, q, tag)
import Data.Argonaut.Core (Json, fromArray, fromString, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Prim.Row (class Union)

newtype Expr = Expr Json

instance EncodeJson Expr where
  encodeJson (Expr j) = j

-- | A literal: `lit 12`, `lit "3.2"`.
lit :: forall a. ToQty a => a -> Expr
lit = Expr <<< encodeJson <<< q

param :: String -> Expr
param = Expr <<< tag "param" <<< fromString

round :: Expr
round = Expr (tag "round" jsonNull)

price :: String -> Expr
price = Expr <<< tag "price" <<< fromString

holding :: String -> Expr
holding = Expr <<< tag "holding" <<< fromString

factor :: String -> Expr
factor = Expr <<< tag "factor" <<< fromString

binary :: String -> Expr -> Expr -> Json
binary k (Expr a) (Expr b) = tag k (fromArray [ a, b ])

mul :: Expr -> Expr -> Expr
mul a b = Expr (binary "mul" a b)

sub :: Expr -> Expr -> Expr
sub a b = Expr (binary "sub" a b)

max' :: Expr -> Expr -> Expr
max' a b = Expr (binary "max" a b)

newtype Cond = Cond Json

instance EncodeJson Cond where
  encodeJson (Cond j) = j

gt :: Expr -> Expr -> Cond
gt a b = Cond (binary "gt" a b)

ge :: Expr -> Expr -> Cond
ge a b = Cond (binary "ge" a b)

le :: Expr -> Expr -> Cond
le a b = Cond (binary "le" a b)

eq :: Expr -> Expr -> Cond
eq a b = Cond (binary "eq" a b)

not' :: Cond -> Cond
not' (Cond c) = Cond (tag "not" c)

transformedLastRound
  :: forall r rest
   . Union r rest (by :: String, transformation :: String, atLeast :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Cond
transformedLastRound = Cond <<< tag "transformedLastRound" <<< encodeJson

restrictedLastRound
  :: forall r rest
   . Union r rest (by :: String, target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Cond
restrictedLastRound = Cond <<< tag "restrictedLastRound" <<< encodeJson

restrictionInForce
  :: forall r rest
   . Union r rest (by :: String)
  => EncodeJson (Record r)
  => Record r
  -> Cond
restrictionInForce = Cond <<< tag "restrictionInForce" <<< encodeJson

newtype Template = Template Json

instance EncodeJson Template where
  encodeJson (Template j) = j

-- | An order on a Market; `limit` is optional.
tradeT
  :: forall r rest
   . Union r rest (market :: String, quantity :: Expr, limit :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
tradeT = Template <<< tag "trade" <<< encodeJson

transformT
  :: forall r rest
   . Union r rest (transformation :: String, quantity :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
transformT = Template <<< tag "transform" <<< encodeJson

-- | An Aggregate's own procedure Proposal, for its members to sign.
procedureT
  :: forall r rest
   . Union r rest (id :: String, signers :: Array String, intents :: Array Template)
  => EncodeJson (Record r)
  => Record r
  -> Template
procedureT = Template <<< tag "procedure" <<< encodeJson

enactTariffT
  :: forall r rest
   . Union r rest (origin :: String, rate :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
enactTariffT = Template <<< tag "enactTariff" <<< encodeJson

deal
  :: forall r rest
   . Union r rest (signers :: Array String, transfers :: Array { from :: String, to :: String, resource :: String, quantity :: Expr })
  => EncodeJson (Record r)
  => Record r
  -> Template
deal = Template <<< tag "deal" <<< encodeJson

reciprocate
  :: forall r rest
   . Union r rest (target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Template
reciprocate = Template <<< tag "reciprocate" <<< encodeJson

choose :: Array Template -> Template
choose = Template <<< tag "choose" <<< encodeJson

-- | Propose nothing: an option for `choose`, or a clause's whole proposal.
noAction :: Template
noAction = Template (tag "noAction" jsonNull)

newtype SignRule = SignRule Json

instance EncodeJson SignRule where
  encodeJson (SignRule j) = j

signProcedures :: Array String -> SignRule
signProcedures = SignRule <<< tag "procedures" <<< encodeJson

signFacility :: String -> SignRule
signFacility = SignRule <<< tag "facility" <<< fromString

signAll :: SignRule
signAll = SignRule (tag "all" jsonNull)

-- | One clause of a Program: when every condition holds, propose the templates.
newtype Clause = Clause Json

instance EncodeJson Clause where
  encodeJson (Clause j) = j

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Clause
rule when propose = Clause (encodeJson { when, propose })

program
  :: forall r rest
   . Union r rest (rules :: Array Clause, sign :: Array SignRule)
  => EncodeJson (Record r)
  => Record r
  -> Built "program"
program = build
