-- | A small typed vocabulary for authoring scenarios in PureScript and compiling them to the
-- | JSON that `board` and `policy` decode. Every term is a thin wrapper around that JSON, so
-- | the format stays defined in one place: the decoders.
-- |
-- | Record builders take any subset of their allowed fields, given as a row `Union`: a
-- | misspelt or mistyped field fails to compile, and only the fields given are emitted. An
-- | absent field takes the decoder's default, or is a decode error if the decoder requires
-- | it. Tagged strings (capabilities, fallbacks, measures, spans, share sources) are built
-- | from the constants in `Board.Tags`, the same constants the decoders match on.
module Scenario.DSL
  ( Qty
  , class ToQty
  , q
  , Keyed
  , keyed
  , (:=)
  , entry
  , Built
  , builtJson
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
  , class ToBound
  , bound
  , atLeast
  , own
  , finishes
  , neverShort
  , production
  , flow
  , share
  , requirement
  , scheme
  , estimator
  , assignment
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
  , noAction
  , SignRule
  , signProcedures
  , signFacility
  , signAll
  , Clause
  , rule
  , program
  -- fixtures
  , Intent
  , intentJson
  , tradeI
  , transferI
  , transformI
  , enactTariffI
  , enactAccessBanI
  , enactWithholdingI
  , proposal
  , expect
  , ref
  , refAt
  , delivery
  , procedureOutcome
  , roundOf
  , representation
  , accessBanRule
  , tariffRule
  , withholdingRule
  ) where

import Prelude

import Board.Tags as T
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

derive newtype instance Semigroup (Keyed a)

keyed :: forall a. Array (Tuple String a) -> Keyed a
keyed = Keyed

-- | A holding: `"M" := 260`, `"E" := "0.5"`.
entry :: forall a. ToQty a => String -> a -> Tuple String Qty
entry k v = Tuple k (q v)

infix 6 entry as :=

-- | A built JSON fragment of one kind (a market, a facility, an entity, …).
newtype Built (kind :: Symbol) = Built Json

-- | The JSON a builder emits, for decoding in tests and tooling.
builtJson :: forall kind. Built kind -> Json
builtJson (Built j) = j

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
trade m = Tagged (T.tag T.tradeKind m)

transfer :: String -> Capability
transfer r = Tagged (T.tag T.transferKind r)

transform :: String -> Capability
transform t = Tagged (T.tag T.transformKind t)

enactTariff :: Capability
enactTariff = Tagged (T.tag T.enactKind T.tariffKind)

enactAccessBan :: Capability
enactAccessBan = Tagged (T.tag T.enactKind T.accessBanKind)

enactWithholding :: Capability
enactWithholding = Tagged (T.tag T.enactKind T.withholdingKind)

free :: Tagged "transferability"
free = Tagged "Free"

restricted :: Tagged "transferability"
restricted = Tagged "Restricted"

nonTransferable :: Tagged "transferability"
nonTransferable = Tagged "NonTransferable"

-- | The Actor's accounts and activity stay in an Aggregate.
coverage :: String -> Tagged "fallback"
coverage a = Tagged (T.tag T.coverageKind a)

-- | The same accounts, driven by an in-world Policy.
institutional :: String -> Tagged "fallback"
institutional p = Tagged (T.tag T.policyKind p)

omission :: Tagged "fallback"
omission = Tagged T.omissionKind

holdingOf :: String -> Tagged "measure"
holdingOf r = Tagged (T.tag T.holdingKind r)

capacityOf :: String -> Tagged "measure"
capacityOf c = Tagged (T.tag T.capacityKind c)

shortfallOf :: String -> Tagged "measure"
shortfallOf r = Tagged (T.tag T.shortfallKind r)

shortfallRounds :: String -> Tagged "measure"
shortfallRounds r = Tagged (T.tag T.shortfallRoundsKind r)

payeeReceipts :: Tagged "measure"
payeeReceipts = Tagged T.payeeReceiptsKind

volumeOf :: String -> Tagged "measure"
volumeOf t = Tagged (T.tag T.volumeKind t)

final :: Tagged "span"
final = Tagged T.finalKind

everyRound :: Tagged "span"
everyRound = Tagged T.everyRoundKind

cumulative :: Tagged "span"
cumulative = Tagged T.cumulativeKind

sales :: String -> Tagged "share source"
sales r = Tagged (T.tag T.salesKind r)

payee :: Tagged "share source"
payee = Tagged T.payeeKind

-- World (docs/03_gameboard.md) ------------------------------------------------------

resource
  :: forall r rest
   . Union r rest (transferability :: Tagged "transferability", reservable :: Boolean, decay :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "resource"
resource = build

cost
  :: forall r rest
   . Union r rest (base :: Qty, slope :: Qty, lo :: Qty, hi :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "cost"
cost = build

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
collective
  :: forall r rest
   . Union r rest (weights :: Keyed Qty, residualWeight :: Qty, threshold :: Qty, authorizes :: Array Capability)
  => EncodeJson (Record r)
  => Record r
  -> Built "procedure"
collective = build

-- | An Actor definition: seat eligibility, fallback, and objective.
definition
  :: forall r rest
   . Union r rest
       ( required :: Boolean
       , seat :: Boolean
       , npc :: Array String
       , fallback :: Tagged "fallback"
       , objective :: Built "objective"
       , description :: String
       , dependency :: String
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "definition"
definition = build

objective
  :: forall r rest
   . Union r rest (description :: String, criteria :: Array (Built "criterion"))
  => EncodeJson (Record r)
  => Record r
  -> Built "objective"
objective = build

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

-- | A criterion's bound: a fixed quantity, or a metric the Entity must match.
class ToBound a where
  bound :: a -> Built "bound"

instance ToBound Int where
  bound = Built <<< encodeJson <<< q

instance ToBound String where
  bound = Built <<< encodeJson <<< q

instance ToBound Qty where
  bound = Built <<< encodeJson <<< q

instance ToBound (Built "metric") where
  bound = Built <<< encodeJson

-- | `atLeast 200` is a fixed bound; `atLeast (own "CN" (holdingOf "Mil"))` matches another measure.
atLeast :: forall a. ToBound a => a -> Built "bound"
atLeast = bound

-- | One Entity's own measure.
own :: String -> Tagged "measure" -> Built "metric"
own e m = metric { entities: [ e ], measure: m, negated: false }

-- | The Entity finishes the game with at least the bound of a measure.
finishes :: String -> Tagged "measure" -> Built "bound" -> Built "criterion"
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Entity is never short of a Resource in any round.
neverShort :: String -> String -> Built "criterion"
neverShort e r = criterion { metric: metric { entities: [ e ], measure: shortfallOf r, negated: true }, over: everyRound, atLeast: atLeast 0 }

production
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, capacity :: String)
  => EncodeJson (Record r)
  => Record r
  -> Built "production"
production = build

flow
  :: forall r rest
   . Union r rest (from :: String, to :: String, resource :: String, amount :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "flow"
flow = build

share
  :: forall r rest
   . Union r rest (from :: String, to :: String, share :: Qty, source :: Tagged "share source")
  => EncodeJson (Record r)
  => Record r
  -> Built "share"
share = build

requirement
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, amount :: Qty, penalty :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "requirement"
requirement = build

-- | A Representation scheme on an Aggregate.
scheme
  :: forall r rest
   . Union r rest (id :: String, eligibleActors :: Array String, target :: String, domain :: String, estimator :: Built "estimator")
  => EncodeJson (Record r)
  => Record r
  -> Built "scheme"
scheme = build

estimator
  :: forall r rest
   . Union r rest (reference :: Qty, shrinkage :: Qty, bound :: Qty, noiseSpread :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "estimator"
estimator = build

-- | An instantiation's assignment of an Actor to a Representation scheme.
assignment
  :: forall r rest
   . Union r rest (scheme :: String, actor :: String, weight :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "assignment"
assignment = build

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

-- Fixtures (docs/05_test_instance.md §21–§27) ---------------------------------------

-- | A canonical Intent, as a fixture's Proposals carry it.
newtype Intent = Intent Json

intentJson :: Intent -> Json
intentJson (Intent j) = j

instance EncodeJson Intent where
  encodeJson (Intent j) = j

tradeI
  :: forall r rest
   . Union r rest (market :: String, quantity :: Qty, limit :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
tradeI = Intent <<< tag "trade" <<< encodeJson

transferI
  :: forall r rest
   . Union r rest (from :: String, to :: String, resource :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
transferI = Intent <<< tag "transfer" <<< encodeJson

transformI
  :: forall r rest
   . Union r rest (by :: String, transformation :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
transformI = Intent <<< tag "transform" <<< encodeJson

enactTariffI
  :: forall r rest
   . Union r rest (jurisdiction :: String, origin :: String, rate :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactTariffI r = Intent (tag "enact" (tag "tariff" (encodeJson r)))

enactAccessBanI
  :: forall r rest
   . Union r rest (jurisdiction :: String, target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactAccessBanI r = Intent (tag "enact" (tag "accessBan" (encodeJson r)))

enactWithholdingI
  :: forall r rest
   . Union r rest (capacity :: String, units :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactWithholdingI r = Intent (tag "enact" (tag "withholding" (encodeJson r)))

-- | A Proposal and the signatures it gathered. Ids follow from the author and order.
proposal
  :: forall r rest
   . Union r rest (author :: String, intents :: Array Intent, signers :: Array String, signatures :: Array String, procedure :: String)
  => EncodeJson (Record r)
  => Record r
  -> Built "proposal"
proposal = build

-- | A Proposal named in an expectation: its author's first Proposal in the round.
ref :: String -> Built "proposal ref"
ref author = Built (encodeJson author)

-- | The Proposal at a position among its author's Proposals in the round.
refAt :: String -> Int -> Built "proposal ref"
refAt author index = Built (encodeJson { author, index })

-- | Exact expected results after a round. Only the fields given are checked: `deliveries: []`
-- | asserts that nothing was delivered, while an absent field asserts nothing.
expect
  :: forall r rest
   . Union r rest
       ( accounts :: Keyed (Keyed Qty)
       , reserves :: Keyed (Keyed Qty)
       , deliveries :: Array (Built "delivery")
       , rejected :: Array (Built "proposal ref")
       , lapsed :: Array (Built "proposal ref")
       , factors :: Keyed (Keyed Qty)
       , scheduled :: Int
       , procedures :: Array (Built "procedure outcome")
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "expect"
expect = build

delivery
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "delivery"
delivery = build

-- | The outcome of a procedure Proposal: its author's first Proposal unless `index` says otherwise.
procedureOutcome
  :: forall r rest
   . Union r rest (author :: String, index :: Int, support :: Qty, executed :: Boolean)
  => EncodeJson (Record r)
  => Record r
  -> Built "procedure outcome"
procedureOutcome = build

-- | One round of a multi-round fixture.
roundOf
  :: forall r rest
   . Union r rest (proposals :: Array (Built "proposal"), expect :: Built "expect")
  => EncodeJson (Record r)
  => Record r
  -> Built "round"
roundOf = build

-- | A Representation bound in a fixture's world (flattened, as the engine stores it).
representation
  :: forall r rest
   . Union r rest (id :: String, actor :: String, aggregate :: String, domain :: String, weight :: Qty, reference :: Qty, shrinkage :: Qty, bound :: Qty, noiseSpread :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "representation"
representation = build

-- | Rules in force from the start of a world.
accessBanRule :: { jurisdiction :: String, target :: String } -> Built "rule"
accessBanRule r = Built (encodeJson { accessBan: r })

tariffRule :: { jurisdiction :: String, origin :: String, rate :: Qty } -> Built "rule"
tariffRule r = Built (encodeJson { tariff: r })

withholdingRule :: { capacity :: String, units :: Qty } -> Built "rule"
withholdingRule r = Built (encodeJson { withholding: r })
