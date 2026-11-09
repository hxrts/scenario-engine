-- | Slices of the round record (docs/02 §5.5). A selector names an `Event` kind by its
-- | constructor; the kind name and every field name are checked at compile time against the `Event`
-- | type. A `Slice` is a value: its kind or kinds, the field filters, the field it reads, and its
-- | aggregate. `runSlice` evaluates it and `describeSlice` renders the same value as formula text, so a
-- | Statistic's formula cannot drift from its calculation. A computation that treats many kinds
-- | differently is a fold written with one handler per kind (`foldEvents`), so a new kind fails to compile.
module Laws.Slice
  ( Select
  , event
  , orKind
  , whereEq
  , whereGt
  , whereNamed
  , read
  , Slice
  , count
  , total
  , items
  , first
  , groupBy
  , legs
  , FlowOwed
  , exchangeFills
  , transformationFills
  , transformationPaymentsTo
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
  , foldEvents
  , ignore
  ) where

import Prelude

import Core.Sum (class HandlerRow, class Handles, class Pick, handles, pick)

import Control.Alt ((<|>))
import Core.Num (Q)
import Core.Num as Q
import Laws.Types (ActorId, ExchangeId, CandidateTransactionId, RecurringKindId, ResourceId, RoundIndex, RuleKindId, TransformationId)
import Laws.World (Cause(..), CausePattern(..), Endpoint(..), Event, Flow, LegAggregate(..), LegQuery, Mechanism(..), Side(..), Window(..))
import Data.Array (concatMap, filter, foldl, head, index, length, nubEq, snoc)
import Data.Foldable (any, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic, from)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

-- Selectors -------------------------------------------------------------------------------------

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

-- | The Events whose field equals the value: `whereEq @"Actor" e`.
whereEq :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Eq b => Show b => b -> Select (Record r) -> Select (Record r)
whereEq x = whereNamed (reflectSymbol f <> " = " <> show x) (\r -> Record.get f r == x)
  where
  f = Proxy :: Proxy field

-- | The Events whose field is above the value: `whereGt @"quantity" Q.zero`.
whereGt :: forall @field r b tail. IsSymbol field => Row.Cons field b tail r => Ord b => Show b => b -> Select (Record r) -> Select (Record r)
whereGt x = whereNamed (reflectSymbol f <> " > " <> show x) (\r -> Record.get f r > x)
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

-- settled Flows and causes ---------------------------------------------------------------------------------

-- | One settled Flow of a settlement, with what it was part of.
type FlowOwed =
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

-- | A selector over `Settled` events, read by a named projection.
settledSelect :: forall r. String -> ({ round :: RoundIndex, legs :: Array Flow, rate :: Maybe Q, cause :: Cause, delivered :: Boolean } -> Array r) -> Select r
settledSelect text f = case event @"Settled" of
  Select s -> Select s { filters = [ text ], pick = concatMap f <<< s.pick }

-- | Every settled Flow of every settlement, in recorded order.
legs :: Select FlowOwed
legs = settledSelect "every leg" \c -> mapWithIndex (\i l -> { round: c.round, from: l.from, to: l.to, resource: l.resource, owed: l.owed, moved: l.moved, rate: c.rate, cause: c.cause, delivered: c.delivered, position: i, count: length c.legs }) c.legs

-- | Exchange fills: one settlement along an Exchange's curve, its quantity signed (+ buys, − sells).
exchangeFills :: Select { round :: RoundIndex, exchange :: ExchangeId, actor :: ActorId, resource :: ResourceId, quantity :: Q, price :: Q, candidateTransaction :: CandidateTransactionId }
exchangeFills = settledSelect "the cause is an exchange along an Exchange's curve" \c -> case c.cause, index c.legs 0, c.rate of
  OfIntent { candidateTransaction, mechanism: Curve exchange }, Just base, Just price
    | not c.delivered -> case base.from, base.to of
        InExchange _, Held actor -> [ { round: c.round, exchange, actor, resource: base.resource, quantity: base.moved, price, candidateTransaction } ]
        Held actor, InExchange _ -> [ { round: c.round, exchange, actor, resource: base.resource, quantity: Q.neg base.moved, price, candidateTransaction } ]
        _, _ -> []
  _, _, _ -> []

-- | TransformationFills: the units run, and what was paid to the payee.
transformationFills :: Select { round :: RoundIndex, by :: ActorId, transformation :: TransformationId, quantity :: Q, paid :: Q, candidateTransaction :: CandidateTransactionId }
transformationFills = settledSelect "the cause is a transformation" \c -> case c.cause of
  OfIntent { candidateTransaction, mechanism: Transformation x }
    | not c.delivered -> [ { round: c.round, by: x.by, transformation: x.transformation, quantity: x.units, paid: Q.sumQ (map _.moved (filter payment c.legs)), candidateTransaction } ]
  _ -> []

-- | A transformation's payment settled Flow: from an Actor to the payee. A settled Flow to the sink is an input, never a payment.
payment :: Flow -> Boolean
payment l = l.from /= Source && l.to /= Sink

-- | The payment settled Flows into an Actor, as what it received as a payee.
transformationPaymentsTo :: ActorId -> Select Flow
transformationPaymentsTo e = settledSelect "payment legs to the Actor from transformationFills" \c -> case c.cause of
  OfIntent { mechanism: Transformation _ } | not c.delivered -> filter (\l -> payment l && l.to == Held e) c.legs
  _ -> []

-- | What a query of the record reads at `round`, over `events`. Delivered settlements are never read. Nothing
-- | when `LargestUnits` finds no transformation: a window that read nothing has no largest.
runLegQuery :: RoundIndex -> Array Event -> LegQuery -> Maybe Q
runLegQuery round events q = case q.aggregate of
  LargestUnits -> maximum (map _.quantity (filter (\c -> c.round == target && c.by == q.party && Just c.transformation == transformationOf) (select transformationFills events)))
  _ -> Just case q.side of
    Incoming -> sumOf (_.to)
    Outgoing -> sumOf (_.from)
    NetOutgoing -> max Q.zero (Q.sub (sumOf (_.from)) (sumOf (_.to)))
  where
  transformationOf = case q.cause of
    TransformationOf t -> Just t
    CurveBase -> Nothing
    TransformationPayment -> Nothing
  target = case q.window of
    ThisRoundSoFar -> round
    LastRound -> round - 1
  read' l = l.round == target && not l.delivered && maybe true (_ == l.resource) q.resource && patternOf l
  patternOf l = case q.cause, l.cause of
    CurveBase, OfIntent { mechanism: Curve _ } -> l.position == 0
    TransformationPayment, OfIntent { mechanism: Transformation _ } -> isPayment l
    TransformationOf t, OfIntent { mechanism: Transformation x } -> x.transformation == t
    _, _ -> false
  isPayment l = l.from /= Source && l.to /= Sink
  amount l = case q.aggregate of
    SumValue -> maybe Q.zero (\r -> Q.mul r l.moved) l.rate
    _ -> l.moved
  held = Held q.party
  sumOf end = Q.sumQ (map amount (filter (\l -> end l == held && read' l) (select legs events)))

-- | Single moves between Actors, by an Intent or a recurring rule.
transfers :: Select { round :: RoundIndex, from :: ActorId, to :: ActorId, resource :: ResourceId, quantity :: Q, owed :: Q }
transfers = settledSelect "the cause is a direct move or a recurring rule, one leg between Actors" \c -> case c.cause, c.legs of
  OfIntent { mechanism: Direct }, [ l ] -> oneMove c l
  OfRecurring _, [ l ] -> oneMove c l
  _, _ -> []
  where
  oneMove c l = case l.from, l.to of
    Held from, Held to | not c.delivered -> [ { round: c.round, from, to, resource: l.resource, quantity: l.moved, owed: l.owed } ]
    _, _ -> []

-- | Levies: a single settled Flow paid under a declared Rule kind.
levies :: Select { round :: RoundIndex, kind :: RuleKindId, from :: ActorId, to :: ActorId, amount :: Q, owed :: Q }
levies = settledSelect "the cause is a declared Rule kind" \c -> case c.cause, c.legs of
  OfRule { kind }, [ l ] -> case l.from, l.to of
    Held from, Held to -> [ { round: c.round, kind, from, to, amount: l.moved, owed: l.owed } ]
    _, _ -> []
  _, _ -> []

-- | Every settled Flow short of what was owed, from an Actor: the quantity is what did not move.
shortfalls :: Select { round :: RoundIndex, actor :: ActorId, resource :: ResourceId, quantity :: Q }
shortfalls = settledSelect "moved < owed on a leg from an Actor" \c -> concatMap (short c) c.legs
  where
  short c l = case l.from of
    Held actor | l.moved < l.owed && not c.delivered -> [ { round: c.round, actor, resource: l.resource, quantity: Q.sub l.owed l.moved } ]
    _ -> []

-- | What a recurring rule of a kind took out of an Actor to nowhere.
sunk :: RecurringKindId -> Select { round :: RoundIndex, actor :: ActorId, resource :: ResourceId, quantity :: Q }
sunk kind = settledSelect ("the cause is a recurring rule of kind " <> show kind <> ", from an Actor to the sink") \c -> case c.cause of
  OfRecurring r | r.kind == kind -> concatMap (one c) c.legs
  _ -> []
  where
  one c l = case l.from, l.to of
    Held actor, Sink | l.moved > Q.zero -> [ { round: c.round, actor, resource: l.resource, quantity: l.moved } ]
    _, _ -> []

-- Slices ----------------------------------------------------------------------------------------

-- | An aggregate over a selection: how it is written, and how it is computed.
newtype Slice a = Slice { describe :: String, run :: Array Event -> a }

-- | The kinds and filters of a selection, as a phrase: `Traded events where Actor = the Actor`.
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

-- | Match an Event, one handler per kind: a new kind fails to compile here.
matchEvent :: forall rep row res. Generic Event rep => HandlerRow rep res row => Handles rep row res => Record row -> Event -> res
matchEvent r e = handles r (from e)

-- | Fold the Events, one handler per kind. A kind with no effect names `ignore`.
foldEvents :: forall rep row acc. Generic Event rep => HandlerRow rep (acc -> acc) row => Handles rep row (acc -> acc) => Record row -> acc -> Array Event -> acc
foldEvents r = foldl (\acc e -> matchEvent r e acc)

-- | The handler of a kind that has no effect on a fold.
ignore :: forall b acc. b -> acc -> acc
ignore _ acc = acc
