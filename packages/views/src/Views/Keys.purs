-- | The Shape of a reply, found from its type, and the reply of each view of a stored run
-- | (docs/07_simulator.md §8). The server encodes a view through its entry and the dashboard decodes it through the same
-- | one. Shapes write the round-end World form: keys descending, a `Maybe` as its value or null.
module Views.Keys
  ( ViewKey
  , class ViewShape
  , viewShape
  , class ViewFields
  , viewFields
  , ViewReplies
  , viewKey
  , class ViewRow
  , class ViewAnswers
  , viewAnswer
  , answerView
  ) where

import Prelude

import Schema.Run (RunClass, RunStatus, runClassShape, runStatusShape)

import Core.Num (Kind, kindShape)
import Core.Shape (class Exhausts, class Inject, Format(..), RecF, Shape, boolShape, encodeWith, fieldOf, inject, intShape, jsonShape, listOf, object, objectValuesOf, optionalOf, stringShape)
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic, Constructor, NoArguments, Sum(..), from, to)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Schema.View (View)
import Views.Actions (ActionsView)
import Views.Regimes (Regime)
import Views.Balance (BalanceView)
import Views.Common (Chart, StoredRun, Table)
import Laws.World (Event, eventShape)
import Views.Overview (RunOverview)

-- | A view of a stored run, and the shape of its reply.
type ViewKey a = { view :: View, shape :: Shape a }

-- | The shape of a view model, derived from its type. A view model is built of text, numbers, flags, arrays,
-- | records, optional values, and Kinds, and holds no Quantity, no Id, and no sum, so every one of its types has
-- | one shape. Field names are the keys, in both formats.
class ViewShape a where
  viewShape :: Shape a

-- | A round report carries the round's events as they are stored.
instance ViewShape Event where
  viewShape = eventShape

instance ViewShape String where
  viewShape = stringShape

instance ViewShape Int where
  viewShape = intShape

instance ViewShape Boolean where
  viewShape = boolShape

instance ViewShape Kind where
  viewShape = kindShape

instance ViewShape RunStatus where
  viewShape = runStatusShape

instance ViewShape RunClass where
  viewShape = runClassShape

instance ViewShape a => ViewShape (Array a) where
  viewShape = listOf viewShape

instance ViewShape a => ViewShape (Maybe a) where
  viewShape = optionalOf viewShape

instance ViewShape a => ViewShape (Object a) where
  viewShape = objectValuesOf viewShape

instance (RL.RowToList r rl, ViewFields rl r r) => ViewShape (Record r) where
  viewShape = object "view" (map (\b -> Builder.build b {}) (viewFields (Proxy :: Proxy rl)))

-- | The fields of a view model's record, in sorted key order, as `Core.Shape.recordShape` describes them.
class ViewFields :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class ViewFields rl whole built | rl -> built where
  viewFields :: Proxy rl -> RecF (Record whole) (Builder (Record ()) (Record built))

instance ViewFields RL.Nil whole () where
  viewFields _ = pure identity

instance
  ( IsSymbol key
  , ViewShape a
  , Row.Cons key a _w whole
  , Row.Cons key a tail built
  , Row.Lacks key tail
  , ViewFields rest whole tail
  ) =>
  ViewFields (RL.Cons key a rest) whole built where
  viewFields _ =
    (\value tail -> Builder.insert field value <<< tail)
      <$> fieldOf (reflectSymbol field) "" viewShape (Record.get field)
      <*> viewFields (Proxy :: Proxy rest)
    where
    field = Proxy :: Proxy key

instance ViewShape Json where
  viewShape = jsonShape

-- | Every view of a stored run and its reply, by `View` constructor: the one statement of what a view answers.
type ViewReplies =
  ( "Overview" :: RunOverview
  , "Balances" :: Table
  , "Exchanges" :: Array Chart
  , "Transformations" :: Table
  , "Economy" :: { totals :: Chart, flows :: Table, pressure :: Table }
  , "Representation" :: Table
  , "Procedures" :: Table
  , "Assertions" :: Table
  , "WholeEconomy" :: Table
  , "Objectives" :: Table
  , "Standings" :: Table
  , "NetWorth" :: Chart
  , "Actions" :: ActionsView
  , "Drivers" :: Table
  , "Balance" :: BalanceView
  , "Events" :: Table
  , "Dependence" :: Table
  , "Regimes" :: Array Regime
  , "Clearing" :: { goods :: Table, capital :: Table, rules :: Table }
  , "Indicators" :: { countries :: Table, seats :: Table }
  , "Finance" :: { bonds :: Table, external :: Table, current :: Table, earnings :: Table }
  )

-- | The `View` of a label, with the Shape its reply has.
viewKey
  :: forall @name rep a t
   . Generic View rep
  => Inject name rep Unit
  => Row.Cons name a t ViewReplies
  => ViewShape a
  => ViewKey a
viewKey = { view: to (inject (Proxy :: Proxy name) unit), shape: viewShape }

-- | A record with one per-view function, from a stored run to its reply in `m (f a)`. A view with no entry in
-- | `ViewReplies`, or an entry with no view, does not compile.
class ViewRow :: Type -> (Type -> Type) -> (Type -> Type) -> Row Type -> Constraint
class ViewRow rep m f row | rep m f -> row

instance (ViewRow a m f l, ViewRow b m f r, Row.Union l r row) => ViewRow (Sum a b) m f row

instance (Row.Cons name a t ViewReplies, Row.Cons name (StoredRun -> m (f a)) () row) => ViewRow (Constructor name NoArguments) m f row

class ViewAnswers :: Type -> (Type -> Type) -> (Type -> Type) -> Row Type -> Constraint
class ViewAnswers rep m f impl where
  viewAnswer :: Record impl -> StoredRun -> rep -> m (f Json)

instance (ViewAnswers a m f impl, ViewAnswers b m f impl) => ViewAnswers (Sum a b) m f impl where
  viewAnswer impl r = case _ of
    Inl x -> viewAnswer impl r x
    Inr x -> viewAnswer impl r x

instance
  ( IsSymbol name
  , Functor m
  , Functor f
  , ViewShape a
  , Row.Cons name a t ViewReplies
  , Row.Cons name (StoredRun -> m (f a)) s impl
  ) =>
  ViewAnswers (Constructor name NoArguments) m f impl where
  viewAnswer impl r _ = map (map (encodeWith Snapshot (viewShape :: Shape a))) (Record.get (Proxy :: Proxy name) impl r)

-- | Run the function of a view, encoding its reply through the view's Shape.
answerView
  :: forall rep m f row
   . Generic View rep
  => Exhausts rep ViewReplies ()
  => ViewRow rep m f row
  => ViewAnswers rep m f row
  => Record row
  -> StoredRun
  -> View
  -> m (f Json)
answerView impl r = viewAnswer impl r <<< from
