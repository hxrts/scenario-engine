-- | Slices of the round record (docs/02 §5.5). A selector names an `Event` kind by its
-- | constructor; the kind name and every field name are checked at compile time against the `Event`
-- | type. A `Slice` is a value: its kind or kinds, the field filters, the field it reads, and its
-- | aggregate. `runSlice` evaluates it and `describeSlice` renders the same value as formula text, so a
-- | Statistic's formula cannot drift from its calculation. A computation that treats many kinds
-- | differently is a fold written with one handler per kind (`foldEvents`), so a new kind fails to compile.
module Core.Slice
  ( Select
  , event
  , orKind
  , whereEq
  , whereGt
  , whereIn
  , whereNamed
  , read
  , Slice
  , count
  , total
  , items
  , first
  , groupBy
  , legs
  , Leg
  , exchanges
  , conversions
  , conversionPaymentsTo
  , runLegQuery
  , transfers
  , levies
  , shortfalls
  , sunk
  , runSlice
  , describeSlice
  , select
  , pickOf
  , matches
  , class Pick
  , pick
  , class HandlerRow
  , class Handles
  , handles
  , matchEvent
  , foldEvents
  , ignore
  ) where

import Prelude

import Control.Alt ((<|>))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, MarketId, ProposalId, RecurringKindId, ResourceId, RoundIndex, RuleKindId, TransformationId)
import Core.World (Cause(..), CausePattern(..), Endpoint(..), Event, Flow, LegAggregate(..), LegQuery, Mechanism(..), Side(..), Window(..))
import Data.Array (concatMap, elem, filter, foldl, head, index, length, nubEq, snoc)
import Data.Foldable (any, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
import Record as Record
import Type.Proxy (Proxy(..))

-- Selectors -------------------------------------------------------------------------------------

-- | Find the payload of the constructor `name` in a sum's representation. A name that is no
-- | constructor of the sum is a compile error naming it.
class Pick :: Symbol -> Type -> Type -> Constraint
class Pick name rep b | name rep -> b where
  pick :: Proxy name -> rep -> Maybe b

instance Pick name (Constructor name (Argument b)) b where
  pick _ (Constructor (Argument b)) = Just b

else instance Pick name (Sum (Constructor name (Argument b)) r) b where
  pick _ = case _ of
    Inl (Constructor (Argument b)) -> Just b
    Inr _ -> Nothing

else instance Pick name r b => Pick name (Sum (Constructor other a) r) b where
  pick p = case _ of
    Inr r -> pick p r
    Inl _ -> Nothing

else instance Fail (Beside (Text "Event has no kind named ") (QuoteLabel name)) => Pick name rep b where
  pick _ _ = Nothing

-- | Some Events and what is read from each, with the description of the choice made so far.
newtype Select r = Select
  { kinds :: Array String
  , filters :: Array String
  , field :: Maybe String
  , pick :: Event -> Array r
  }

-- | The Events of one kind, read as their record: `event @"Traded"`.
event :: forall @name b rep. IsSymbol name => Generic Event rep => Pick name rep b => Select b
event = Select { kinds: [ reflectSymbol p ], filters: [], field: Nothing, pick: \e -> maybe [] pure (pick p (from e)) }
  where
  p = Proxy :: Proxy name

-- | The Events of either selector, in recorded order: what each reads must be the same.
orKind :: forall r. Select r -> Select r -> Select r
orKind (Select a) (Select b) = Select
  { kinds: a.kinds <> b.kinds
  , filters: a.filters <> b.filters
  , field: a.field <|> b.field
  , pick: \e -> a.pick e <> b.pick e
  }

-- | A filter a value cannot state: its own description, and its predicate over what is selected.
whereNamed :: forall r. String -> (r -> Boolean) -> Select r -> Select r
whereNamed text p (Select s) = Select s
  { filters = snoc s.filters text
  , pick = filter p <<< s.pick
  }

-- | The Events whose field equals the value: `whereEq @"entity" e`.
whereEq :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Eq b => Show b => b -> Select (Record r) -> Select (Record r)
whereEq x = whereNamed (reflectSymbol f <> " = " <> show x) (\r -> Record.get f r == x)
  where
  f = Proxy :: Proxy field

-- | The Events whose field is above the value: `whereGt @"quantity" Q.zero`.
whereGt :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Ord b => Show b => b -> Select (Record r) -> Select (Record r)
whereGt x = whereNamed (reflectSymbol f <> " > " <> show x) (\r -> Record.get f r > x)
  where
  f = Proxy :: Proxy field

-- | The Events whose field is one of the values, shown by their names.
whereIn :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Eq b => String -> Array b -> Select (Record r) -> Select (Record r)
whereIn label xs = whereNamed (reflectSymbol f <> " in " <> label) (\r -> elem (Record.get f r) xs)
  where
  f = Proxy :: Proxy field

-- | Read one field of what is selected: `read @"quantity"`.
read :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Select (Record r) -> Select b
read (Select s) = Select { kinds: s.kinds, filters: s.filters, field: Just (reflectSymbol f), pick: map (Record.get f) <<< s.pick }
  where
  f = Proxy :: Proxy field

-- | Everything selected, in recorded order.
select :: forall r. Select r -> Array Event -> Array r
select (Select s) = concatMap s.pick

-- | What a selector reads from one Event, if the Event is of its kind and passes its filters.
pickOf :: forall r. Select r -> Event -> Maybe r
pickOf (Select s) = head <<< s.pick

-- | Whether a selector picks the Event and something it reads satisfies the predicate.
matches :: forall r. Select r -> (r -> Boolean) -> Event -> Boolean
matches (Select s) p = any p <<< s.pick

-- Legs and causes ---------------------------------------------------------------------------------

-- | One leg of a settlement, with what it was part of.
type Leg =
  { round :: RoundIndex
  , from :: Endpoint
  , to :: Endpoint
  , resource :: ResourceId
  , owed :: Q
  , moved :: Q
  , rate :: Maybe Q
  , cause :: Cause
  , delivered :: Boolean
  , position :: Int
  , count :: Int
  }

-- | A selector over `Coupled` events, read by a named projection.
coupledSelect :: forall r. String -> ({ round :: RoundIndex, legs :: Array Flow, rate :: Maybe Q, cause :: Cause, delivered :: Boolean } -> Array r) -> Select r
coupledSelect text f = case event @"Coupled" of
  Select s -> Select s { filters = [ text ], pick = concatMap f <<< s.pick }

-- | Every leg of every settlement, in recorded order.
legs :: Select Leg
legs = coupledSelect "every leg" \c -> mapWithIndex (\i l -> { round: c.round, from: l.from, to: l.to, resource: l.resource, owed: l.owed, moved: l.moved, rate: c.rate, cause: c.cause, delivered: c.delivered, position: i, count: length c.legs }) c.legs

-- | Market fills: one settlement along a Market's curve, its quantity signed (+ buys, − sells).
exchanges :: Select { round :: RoundIndex, market :: MarketId, entity :: EntityId, resource :: ResourceId, quantity :: Q, price :: Q, proposal :: ProposalId }
exchanges = coupledSelect "the cause is an exchange along a Market's curve" \c -> case c.cause, index c.legs 0, c.rate of
  OfIntent { proposal, mechanism: Curve market }, Just base, Just price
    | not c.delivered -> case base.from, base.to of
        InMarket _, Held entity -> [ { round: c.round, market, entity, resource: base.resource, quantity: base.moved, price, proposal } ]
        Held entity, InMarket _ -> [ { round: c.round, market, entity, resource: base.resource, quantity: Q.neg base.moved, price, proposal } ]
        _, _ -> []
  _, _, _ -> []

-- | Conversions: the units run, and what was paid to the payee.
conversions :: Select { round :: RoundIndex, by :: EntityId, transformation :: TransformationId, quantity :: Q, paid :: Q, proposal :: ProposalId }
conversions = coupledSelect "the cause is a conversion" \c -> case c.cause of
  OfIntent { proposal, mechanism: Conversion x }
    | not c.delivered -> [ { round: c.round, by: x.by, transformation: x.transformation, quantity: x.units, paid: Q.sumQ (map _.moved (filter payment c.legs)), proposal } ]
  _ -> []

-- | A conversion's payment leg: from an Entity to the payee. A leg to the sink is an input, never a payment.
payment :: Flow -> Boolean
payment l = l.from /= Source && l.to /= Sink

-- | The payment legs into an Entity, as what it received as a payee.
conversionPaymentsTo :: EntityId -> Select Flow
conversionPaymentsTo e = coupledSelect "payment legs to the Entity from conversions" \c -> case c.cause of
  OfIntent { mechanism: Conversion _ } | not c.delivered -> filter (\l -> payment l && l.to == Held e) c.legs
  _ -> []

-- | What a query of the record reads at `round`, over `events`. Delivered settlements are never read. Nothing
-- | when `LargestUnits` finds no conversion: a window that read nothing has no largest.
runLegQuery :: RoundIndex -> Array Event -> LegQuery -> Maybe Q
runLegQuery round events q = case q.aggregate of
  LargestUnits -> maximum (map _.quantity (filter (\c -> c.round == target && c.by == q.party && Just c.transformation == conversionOf) (select conversions events)))
  _ -> Just case q.side of
    Incoming -> sumOf (_.to)
    Outgoing -> sumOf (_.from)
    NetOutgoing -> max Q.zero (Q.sub (sumOf (_.from)) (sumOf (_.to)))
  where
  conversionOf = case q.cause of
    ConversionOf t -> Just t
    CurveBase -> Nothing
    ConversionPayment -> Nothing
  target = case q.window of
    ThisRoundSoFar -> round
    LastRound -> round - 1
  read' l = l.round == target && not l.delivered && maybe true (_ == l.resource) q.resource && patternOf l
  patternOf l = case q.cause, l.cause of
    CurveBase, OfIntent { mechanism: Curve _ } -> l.position == 0
    ConversionPayment, OfIntent { mechanism: Conversion _ } -> isPayment l
    ConversionOf t, OfIntent { mechanism: Conversion x } -> x.transformation == t
    _, _ -> false
  isPayment l = l.from /= Source && l.to /= Sink
  amount l = case q.aggregate of
    SumValue -> maybe Q.zero (\r -> Q.mul r l.moved) l.rate
    _ -> l.moved
  held = Held q.party
  sumOf end = Q.sumQ (map amount (filter (\l -> end l == held && read' l) (select legs events)))

-- | Single moves between Entities, by an Intent or a recurring rule.
transfers :: Select { round :: RoundIndex, from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Q, owed :: Q }
transfers = coupledSelect "the cause is a direct move or a recurring rule, one leg between Entities" \c -> case c.cause, c.legs of
  OfIntent { mechanism: Direct }, [ l ] -> oneMove c l
  OfRecurring _, [ l ] -> oneMove c l
  _, _ -> []
  where
  oneMove c l = case l.from, l.to of
    Held from, Held to | not c.delivered -> [ { round: c.round, from, to, resource: l.resource, quantity: l.moved, owed: l.owed } ]
    _, _ -> []

-- | Levies: a single leg paid under a declared Rule kind.
levies :: Select { round :: RoundIndex, kind :: RuleKindId, from :: EntityId, to :: EntityId, amount :: Q, owed :: Q }
levies = coupledSelect "the cause is a declared Rule kind" \c -> case c.cause, c.legs of
  OfRule { kind }, [ l ] -> case l.from, l.to of
    Held from, Held to -> [ { round: c.round, kind, from, to, amount: l.moved, owed: l.owed } ]
    _, _ -> []
  _, _ -> []

-- | Every leg short of what was owed, from an Entity: the quantity is what did not move.
shortfalls :: Select { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Q }
shortfalls = coupledSelect "moved < owed on a leg from an Entity" \c -> concatMap (short c) c.legs
  where
  short c l = case l.from of
    Held entity | l.moved < l.owed && not c.delivered -> [ { round: c.round, entity, resource: l.resource, quantity: Q.sub l.owed l.moved } ]
    _ -> []

-- | What a recurring rule of a kind took out of an Entity to nowhere.
sunk :: RecurringKindId -> Select { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Q }
sunk kind = coupledSelect ("the cause is a recurring rule of kind " <> show kind <> ", from an Entity to the sink") \c -> case c.cause of
  OfRecurring r | r.kind == kind -> concatMap (one c) c.legs
  _ -> []
  where
  one c l = case l.from, l.to of
    Held entity, Sink | l.moved > Q.zero -> [ { round: c.round, entity, resource: l.resource, quantity: l.moved } ]
    _, _ -> []

-- Slices ----------------------------------------------------------------------------------------

-- | An aggregate over a selection: how it is written, and how it is computed.
newtype Slice a = Slice { describe :: String, run :: Array Event -> a }

-- | The kinds and filters of a selection, as a phrase: `Traded events where entity = the Actor`.
source :: forall r. Select r -> String
source (Select s) = names s.kinds <> " events" <> (if s.filters == [] then "" else " where " <> joined " and " s.filters)
  where
  names xs = joined " and " xs

joined :: String -> Array String -> String
joined sep = foldl (\acc x -> if acc == "" then x else acc <> sep <> x) ""

reading :: forall r. Select r -> String
reading sel@(Select s) = case s.field of
  Just f -> f <> " of " <> source sel
  Nothing -> source sel

-- | How many are selected.
count :: forall r. Select r -> Slice Int
count sel = Slice { describe: "count of " <> reading sel, run: length <<< select sel }

-- | The sum of the quantities selected.
total :: Select Q -> Slice Q
total sel = Slice { describe: "sum of " <> reading sel, run: Q.sumQ <<< select sel }

-- | Everything selected, in recorded order.
items :: forall r. Select r -> Slice (Array r)
items sel = Slice { describe: reading sel, run: select sel }

-- | The first one selected, if any.
first :: forall r. Select r -> Slice (Maybe r)
first sel = Slice { describe: "first of " <> reading sel, run: head <<< select sel }

-- | What is selected, grouped by a field, each group's key in order of first appearance.
groupBy :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Eq b => Select (Record r) -> Slice (Array (Tuple b (Array (Record r))))
groupBy sel = Slice
  { describe: reading sel <> ", grouped by " <> reflectSymbol f
  , run: \es ->
      let
        xs = select sel es
      in
        map (\k -> Tuple k (filter (\x -> Record.get f x == k) xs)) (nubEq (map (Record.get f) xs))
  }
  where
  f = Proxy :: Proxy field

runSlice :: forall a. Slice a -> Array Event -> a
runSlice (Slice s) = s.run

describeSlice :: forall a. Slice a -> String
describeSlice (Slice s) = s.describe

-- Total folds -----------------------------------------------------------------------------------

-- | One handler per constructor: the record a total match over a sum needs.
class HandlerRow :: Type -> Type -> Row Type -> Constraint
class HandlerRow rep res row | rep res -> row

instance (HandlerRow a res l, HandlerRow b res r, Row.Union l r row) => HandlerRow (Sum a b) res row

instance Row.Cons name (b -> res) () row => HandlerRow (Constructor name (Argument b)) res row

-- | Applying the handler the value's constructor names.
class Handles :: Type -> Row Type -> Type -> Constraint
class Handles rep row res | rep row -> res where
  handles :: Record row -> rep -> res

instance (Handles a row res, Handles b row res) => Handles (Sum a b) row res where
  handles r = case _ of
    Inl x -> handles r x
    Inr x -> handles r x

instance (IsSymbol name, Row.Cons name (b -> res) t row) => Handles (Constructor name (Argument b)) row res where
  handles r (Constructor (Argument b)) = Record.get (Proxy :: Proxy name) r b

-- | Match an Event, one handler per kind: a new kind fails to compile here.
matchEvent :: forall rep row res. Generic Event rep => HandlerRow rep res row => Handles rep row res => Record row -> Event -> res
matchEvent r e = handles r (from e)

-- | Fold the Events, one handler per kind. A kind with no effect names `ignore`.
foldEvents :: forall rep row acc. Generic Event rep => HandlerRow rep (acc -> acc) row => Handles rep row (acc -> acc) => Record row -> acc -> Array Event -> acc
foldEvents r = foldl (\acc e -> matchEvent r e acc)

-- | The handler of a kind that has no effect on a fold.
ignore :: forall b acc. b -> acc -> acc
ignore _ acc = acc
