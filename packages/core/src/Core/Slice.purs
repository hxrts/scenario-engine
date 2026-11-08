-- | Slices of the round record (docs/02 §5.5; work/7 S10, S11). A selector names an `Event` kind by its
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
import Core.World (Event)
import Data.Array (elem, filter, foldl, head, length, mapMaybe, nubEq, snoc)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.Maybe (Maybe(..))
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
  , pick :: Event -> Maybe r
  }

-- | The Events of one kind, read as their record: `event @"Traded"`.
event :: forall @name b rep. IsSymbol name => Generic Event rep => Pick name rep b => Select b
event = Select { kinds: [ reflectSymbol p ], filters: [], field: Nothing, pick: pick p <<< from }
  where
  p = Proxy :: Proxy name

-- | The Events of either selector, in recorded order: what each reads must be the same.
orKind :: forall r. Select r -> Select r -> Select r
orKind (Select a) (Select b) = Select
  { kinds: a.kinds <> b.kinds
  , filters: a.filters <> b.filters
  , field: a.field <|> b.field
  , pick: \e -> a.pick e <|> b.pick e
  }

-- | A filter a value cannot state: its own description, and its predicate over what is selected.
whereNamed :: forall r. String -> (r -> Boolean) -> Select r -> Select r
whereNamed text p (Select s) = Select s
  { filters = snoc s.filters text
  , pick = \e -> case s.pick e of
      Just r | p r -> Just r
      _ -> Nothing
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
select (Select s) = mapMaybe s.pick

-- | What a selector reads from one Event, if the Event is of its kind and passes its filters.
pickOf :: forall r. Select r -> Event -> Maybe r
pickOf (Select s) = s.pick

-- | Whether a selector picks the Event and what it reads satisfies the predicate.
matches :: forall r. Select r -> (r -> Boolean) -> Event -> Boolean
matches sel p e = case pickOf sel e of
  Just r -> p r
  Nothing -> false

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
