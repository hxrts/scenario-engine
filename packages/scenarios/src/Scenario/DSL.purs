-- | A small typed vocabulary for authoring scenarios in PureScript and compiling them to the
-- | JSON that `board` and `policy` decode. Every term is a thin wrapper around that JSON, so
-- | the format stays defined in one place: the decoders.
-- |
-- | Record builders accept any subset of their allowed fields: a misspelt or mistyped field
-- | fails to compile, and only the fields given are emitted (absent fields take the
-- | decoder's default).
module Scenario.DSL
  ( Qty
  , class ToQty
  , q
  , Keyed
  , keyed
  , (:=)
  , entry
  , Built
  , Capability
  , trade
  , transfer
  , transform
  , enactTariff
  , enactAccessBan
  , enactWithholding
  , Tagged
  , free
  , restricted
  , nonTransferable
  , coverage
  , institutional
  , omission
  , holdingOf
  , capacityOf
  , shortfallOf
  , shortfallRounds
  , payeeReceipts
  , volumeOf
  , final
  , everyRound
  , cumulative
  , sales
  , payee
  -- world
  , resource
  , cost
  , market
  , facility
  , effect
  , entity
  , aggregate
  , collective
  , definition
  , objective
  , criterion
  , metric
  , atLeast
  , matching
  , own
  , finishes
  , neverShort
  , production
  , flow
  , share
  , requirement
  , scheme
  , estimator
  , representative
  -- decision language
  , Expr
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
  , none
  , SignRule
  , signProcedures
  , signFacility
  , signAll
  , Rule
  , rule
  , program
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prim.Row (class Union)

-- Quantities and maps ----------------------------------------------------------------

-- | An exact decimal quantity, written as the decoders read it (a string).
newtype Qty = Qty String

instance EncodeJson Qty where
  encodeJson (Qty s) = fromString s

-- | Quantities are written as an `Int` or as a decimal `String` ("0.02"), never a
-- | floating-point `Number`.
class ToQty a where
  q :: a -> Qty

instance ToQty Qty where
  q = identity

instance ToQty Int where
  q = Qty <<< show

instance ToQty String where
  q = Qty

-- | A JSON object from key–value pairs: holdings, catalogs, parameters, seats.
newtype Keyed a = Keyed (Array (Tuple String a))

instance EncodeJson a => EncodeJson (Keyed a) where
  encodeJson (Keyed kvs) = fromObject (Object.fromFoldable (map (\(Tuple k v) -> Tuple k (encodeJson v)) kvs))

keyed :: forall a. Array (Tuple String a) -> Keyed a
keyed = Keyed

-- | A holding: `"M" := 260`, `"E" := "0.5"`.
entry :: forall a. ToQty a => String -> a -> Tuple String Qty
entry k v = Tuple k (q v)

infix 6 entry as :=

-- | A built JSON fragment of one kind (a market, a facility, an entity, …).
newtype Built (kind :: Symbol) = Built Json

instance EncodeJson (Built kind) where
  encodeJson (Built j) = j

-- | Encode a record whose fields the caller's `Union` constraint has already checked.
build :: forall kind r. EncodeJson (Record r) => Record r -> Built kind
build = Built <<< encodeJson

-- Tagged strings ---------------------------------------------------------------------

-- | A string the decoders split on `:` (capabilities, fallbacks, measures, …).
newtype Tagged (kind :: Symbol) = Tagged String

instance EncodeJson (Tagged kind) where
  encodeJson (Tagged s) = fromString s

type Capability = Tagged "capability"

trade :: String -> Capability
trade m = Tagged ("trade:" <> m)

transfer :: String -> Capability
transfer r = Tagged ("transfer:" <> r)

transform :: String -> Capability
transform t = Tagged ("transform:" <> t)

enactTariff :: Capability
enactTariff = Tagged "enact:tariff"

enactAccessBan :: Capability
enactAccessBan = Tagged "enact:accessBan"

enactWithholding :: Capability
enactWithholding = Tagged "enact:withholding"

free :: Tagged "transferability"
free = Tagged "Free"

restricted :: Tagged "transferability"
restricted = Tagged "Restricted"

nonTransferable :: Tagged "transferability"
nonTransferable = Tagged "NonTransferable"

-- | The Actor's accounts and activity stay in an Aggregate.
coverage :: String -> Tagged "fallback"
coverage a = Tagged ("coverage:" <> a)

-- | The same accounts, driven by an in-world Policy.
institutional :: String -> Tagged "fallback"
institutional p = Tagged ("policy:" <> p)

omission :: Tagged "fallback"
omission = Tagged "omission"

holdingOf :: String -> Tagged "measure"
holdingOf r = Tagged ("holding:" <> r)

capacityOf :: String -> Tagged "measure"
capacityOf c = Tagged ("capacity:" <> c)

shortfallOf :: String -> Tagged "measure"
shortfallOf r = Tagged ("shortfall:" <> r)

shortfallRounds :: String -> Tagged "measure"
shortfallRounds r = Tagged ("shortfallRounds:" <> r)

payeeReceipts :: Tagged "measure"
payeeReceipts = Tagged "payeeReceipts"

volumeOf :: String -> Tagged "measure"
volumeOf t = Tagged ("volume:" <> t)

final :: Tagged "span"
final = Tagged "final"

everyRound :: Tagged "span"
everyRound = Tagged "everyRound"

cumulative :: Tagged "span"
cumulative = Tagged "cumulative"

sales :: String -> Tagged "share source"
sales r = Tagged ("sales:" <> r)

payee :: Tagged "share source"
payee = Tagged "payee"

-- World (docs/03_gameboard.md) ------------------------------------------------------

resource
  :: forall r rest
   . Union r rest (transferability :: Tagged "transferability", reservable :: Boolean, decay :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "resource"
resource = build

cost :: { base :: Qty, slope :: Qty, lo :: Qty, hi :: Qty } -> Built "cost"
cost = Built <<< encodeJson

market
  :: forall r rest
   . Union r rest (base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty, access :: Array String, position :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "market"
market = build

facility
  :: forall r rest
   . Union r rest
       ( inputs :: Keyed Qty
       , outputs :: Keyed Qty
       , cost :: Built "cost"
       , capacity :: String
       , payee :: String
       , delay :: Int
       , minimum :: Qty
       , maximum :: Qty
       , capacityUse :: Qty
       , ownerSigns :: Boolean
       , effects :: Array (Built "effect")
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "facility"
facility = build

-- | A facility's change to a capacity per unit ordered.
effect
  :: forall r rest
   . Union r rest (capacity :: String, holder :: String, perUnit :: Qty, rounds :: Int)
  => EncodeJson (Record r)
  => Record r
  -> Built "effect"
effect = build

entity
  :: forall r rest
   . Union r rest
       ( accounts :: Keyed Qty
       , capabilities :: Array Capability
       , capacities :: Keyed Qty
       , definition :: Built "definition"
       , aggregate :: Built "aggregate"
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "entity"
entity = build

aggregate
  :: forall r rest
   . Union r rest (policy :: String, members :: Array String, procedures :: Keyed (Built "procedure"))
  => EncodeJson (Record r)
  => Record r
  -> Built "aggregate"
aggregate = build

-- | A collective procedure on an Aggregate's own Proposals.
collective :: { weights :: Keyed Qty, residualWeight :: Qty, threshold :: Qty, authorizes :: Array Capability } -> Built "procedure"
collective = Built <<< encodeJson

-- | An Actor definition: seat eligibility, fallback, and objective.
definition
  :: forall r rest
   . Union r rest
       ( required :: Boolean
       , seat :: Boolean
       , npc :: Array String
       , fallback :: Tagged "fallback"
       , objective :: Built "objective"
       , capability :: String
       , dependency :: String
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "definition"
definition = build

objective :: { description :: String, criteria :: Array (Built "criterion") } -> Built "objective"
objective = Built <<< encodeJson

criterion
  :: forall r rest
   . Union r rest (metric :: Built "metric", over :: Tagged "span", atLeast :: Built "bound")
  => EncodeJson (Record r)
  => Record r
  -> Built "criterion"
criterion = build

metric
  :: forall r rest
   . Union r rest (entities :: Array String, measure :: Tagged "measure", negated :: Boolean)
  => EncodeJson (Record r)
  => Record r
  -> Built "metric"
metric = build

-- | A criterion's bound: a fixed quantity, or another metric to match.
atLeast :: forall a. ToQty a => a -> Built "bound"
atLeast = Built <<< encodeJson <<< q

matching :: Built "metric" -> Built "bound"
matching (Built j) = Built j

-- | One Entity's own measure.
own :: String -> Tagged "measure" -> Built "metric"
own e m = metric { entities: [ e ], measure: m, negated: false }

-- | The Entity finishes the game with at least the bound of a measure.
finishes :: String -> Tagged "measure" -> Built "bound" -> Built "criterion"
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Entity is never short of a Resource in any round.
neverShort :: String -> String -> Built "criterion"
neverShort e r = criterion { metric: metric { entities: [ e ], measure: shortfallOf r, negated: true }, over: everyRound, atLeast: atLeast 0 }

production :: { entity :: String, resource :: String, capacity :: String } -> Built "production"
production = Built <<< encodeJson

flow :: { from :: String, to :: String, resource :: String, amount :: Qty } -> Built "flow"
flow = Built <<< encodeJson

share :: { from :: String, to :: String, share :: Qty, source :: Tagged "share source" } -> Built "share"
share = Built <<< encodeJson

requirement
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, amount :: Qty, penalty :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "requirement"
requirement = build

-- | A Representation scheme on an Aggregate.
scheme :: { id :: String, eligibleActors :: Array String, target :: String, domain :: String, estimator :: Built "estimator" } -> Built "scheme"
scheme = Built <<< encodeJson

estimator
  :: forall r rest
   . Union r rest (reference :: Qty, shrinkage :: Qty, bound :: Qty, noiseSpread :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "estimator"
estimator = build

-- | An instantiation's representative assignment.
representative
  :: forall r rest
   . Union r rest (scheme :: String, actor :: String, weight :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "representative"
representative = build

-- Decision language (docs/02_type_system.md §4.4) -------------------------------------

tag :: String -> Json -> Json
tag k v = fromObject (Object.singleton k v)

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

transformedLastRound :: { by :: String, transformation :: String, atLeast :: Qty } -> Cond
transformedLastRound = Cond <<< tag "transformedLastRound" <<< encodeJson

restrictedLastRound :: { by :: String, target :: String } -> Cond
restrictedLastRound = Cond <<< tag "restrictedLastRound" <<< encodeJson

restrictionInForce :: { by :: String } -> Cond
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

transformT :: { transformation :: String, quantity :: Expr } -> Template
transformT = Template <<< tag "transform" <<< encodeJson

-- | An Aggregate's own procedure Proposal, for its members to sign.
procedureT :: { id :: String, signers :: Array String, intents :: Array Template } -> Template
procedureT = Template <<< tag "procedure" <<< encodeJson

enactTariffT :: { origin :: String, rate :: Expr } -> Template
enactTariffT = Template <<< tag "enactTariff" <<< encodeJson

deal :: { signers :: Array String, transfers :: Array { from :: String, to :: String, resource :: String, quantity :: Expr } } -> Template
deal = Template <<< tag "deal" <<< encodeJson

reciprocate :: { target :: String } -> Template
reciprocate = Template <<< tag "reciprocate" <<< encodeJson

choose :: Array Template -> Template
choose = Template <<< tag "choose" <<< encodeJson

none :: Template
none = Template (tag "none" jsonNull)

newtype SignRule = SignRule Json

instance EncodeJson SignRule where
  encodeJson (SignRule j) = j

signProcedures :: Array String -> SignRule
signProcedures = SignRule <<< tag "procedures" <<< encodeJson

signFacility :: String -> SignRule
signFacility = SignRule <<< tag "facility" <<< fromString

signAll :: SignRule
signAll = SignRule (tag "all" jsonNull)

newtype Rule = Rule Json

instance EncodeJson Rule where
  encodeJson (Rule j) = j

-- | When every condition holds, propose the templates. `when` may be empty.
rule :: Array Cond -> Array Template -> Rule
rule when propose = Rule (encodeJson { when, propose })

program
  :: forall r rest
   . Union r rest (rules :: Array Rule, sign :: Array SignRule)
  => EncodeJson (Record r)
  => Record r
  -> Built "program"
program = build
