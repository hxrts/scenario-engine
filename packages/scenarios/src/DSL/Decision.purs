-- | The decision language (docs/02_type_system.md §4.4): the expressions, conditions,
-- | Proposal templates, sign rules and clauses from which a Policy's Strategy is built. The
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
  , routeT
  , transformT
  , transferT
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
  , signIf
  , enactAccessBanT
  , enactWithholdingT
  , repealT
  , Clause
  , rule
  , strategy
  ) where

import Prelude

import DSL.Base (Built, class ToQty, Qty, build, q, tag)
import Data.Argonaut.Core (Json, fromArray, fromString, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Policy.Tags as T
import Prim.Row (class Union)

-- | A Transfer's fields: the agent `from` gives `quantity` of `resource` to `to`.
type TransferRow = (from :: String, to :: String, resource :: String, quantity :: Expr)

newtype Expr = Expr Json

instance EncodeJson Expr where
  encodeJson (Expr j) = j

-- | A literal: `lit 12`, `lit "3.2"`.
lit :: forall a. ToQty a => a -> Expr
lit = Expr <<< encodeJson <<< q

param :: String -> Expr
param = Expr <<< tag T.paramTag <<< fromString

round :: Expr
round = Expr (tag T.roundTag jsonNull)

price :: String -> Expr
price = Expr <<< tag T.priceTag <<< fromString

holding :: String -> Expr
holding = Expr <<< tag T.holdingTag <<< fromString

factor :: String -> Expr
factor = Expr <<< tag T.factorTag <<< fromString

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
not' (Cond c) = Cond (tag T.notTag c)

-- | `atLeast` is an expression: a literal, or `param` for a threshold a parameter sweep can change.
transformedLastRound
  :: forall r rest
   . Union r rest (by :: String, transformation :: String, atLeast :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Cond
transformedLastRound = Cond <<< tag T.transformedLastRoundTag <<< encodeJson

restrictedLastRound
  :: forall r rest
   . Union r rest (by :: String, target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Cond
restrictedLastRound = Cond <<< tag T.restrictedLastRoundTag <<< encodeJson

restrictionInForce
  :: forall r rest
   . Union r rest (by :: String)
  => EncodeJson (Record r)
  => Record r
  -> Cond
restrictionInForce = Cond <<< tag T.restrictionInForceTag <<< encodeJson

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
tradeT = Template <<< tag T.tradeTag <<< encodeJson

-- | A routed order (docs/02 §5.2): no Market is named, so `base` and `quote` give the pair and
-- | the engine routes the order across the Markets for it. `limit` is required.
routeT
  :: forall r rest
   . Union r rest (base :: String, quote :: String, quantity :: Expr, limit :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
routeT = Template <<< tag T.routeTag <<< encodeJson

transformT
  :: forall r rest
   . Union r rest (transformation :: String, quantity :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
transformT = Template <<< tag T.transformTag <<< encodeJson

-- | A Transfer, for a procedure to carry (docs/05 §12.1): the agent `from` gives `quantity` to `to`.
transferT
  :: forall r rest
   . Union r rest TransferRow
  => EncodeJson (Record r)
  => Record r
  -> Template
transferT = Template <<< tag T.transferTag <<< encodeJson

-- | An Aggregate's own procedure Proposal, for its members to sign.
procedureT
  :: forall r rest
   . Union r rest (id :: String, signers :: Array String, intents :: Array Template)
  => EncodeJson (Record r)
  => Record r
  -> Template
procedureT = Template <<< tag T.procedureTag <<< encodeJson

enactTariffT
  :: forall r rest
   . Union r rest (origin :: String, rate :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
enactTariffT = Template <<< tag T.enactTariffTag <<< encodeJson

deal
  :: forall r rest
   . Union r rest (signers :: Array String, transfers :: Array (Record TransferRow), transforms :: Array { transformation :: String, quantity :: Expr })
  => EncodeJson (Record r)
  => Record r
  -> Template
deal = Template <<< tag T.dealTag <<< encodeJson

reciprocate
  :: forall r rest
   . Union r rest (target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Template
reciprocate = Template <<< tag T.reciprocateTag <<< encodeJson

-- | Ban shipments from the author's jurisdiction into `target` (docs/02 §3, AccessBan).
enactAccessBanT
  :: forall r rest
   . Union r rest (target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Template
enactAccessBanT = Template <<< tag T.enactAccessBanTag <<< encodeJson

-- | Withhold `units` of a capacity the author owns, per round (docs/02 §3, Withholding).
enactWithholdingT
  :: forall r rest
   . Union r rest (capacity :: String, units :: Expr)
  => EncodeJson (Record r)
  => Record r
  -> Template
enactWithholdingT = Template <<< tag T.enactWithholdingTag <<< encodeJson

-- | Repeal a Rule the author governs, by the id the engine gave it: `<proposal id>/rule-<index>`.
repealT
  :: forall r rest
   . Union r rest (rule :: String)
  => EncodeJson (Record r)
  => Record r
  -> Template
repealT = Template <<< tag T.repealTag <<< encodeJson

choose :: Array Template -> Template
choose = Template <<< tag T.chooseTag <<< encodeJson

-- | Propose nothing: an option for `choose`, or a clause's whole proposal.
noAction :: Template
noAction = Template (tag T.noActionTag jsonNull)

newtype SignRule = SignRule Json

instance EncodeJson SignRule where
  encodeJson (SignRule j) = j

signProcedures :: Array String -> SignRule
signProcedures = SignRule <<< tag T.proceduresTag <<< encodeJson

signFacility :: String -> SignRule
signFacility = SignRule <<< tag T.facilityTag <<< fromString

signAll :: SignRule
signAll = SignRule (tag T.allTag jsonNull)

-- | The inner sign rule, while every condition holds.
signIf :: Array Cond -> SignRule -> SignRule
signIf when inner = SignRule (tag T.signIfTag (encodeJson { when, rule: inner }))

-- | One clause of a Strategy: when every condition holds, propose the templates.
newtype Clause = Clause Json

instance EncodeJson Clause where
  encodeJson (Clause j) = j

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Clause
rule when propose = Clause (encodeJson { when, propose })

strategy
  :: forall r rest
   . Union r rest (rules :: Array Clause, sign :: Array SignRule)
  => EncodeJson (Record r)
  => Record r
  -> Built "strategy"
strategy = build
