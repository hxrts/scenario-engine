-- | The replies of the API, each with one shape (docs/06_simulator.md §8): the server encodes a view
-- | through its key, and the dashboard decodes it through the same key, so the two cannot disagree and
-- | neither names a view's type by hand. The shapes write the round-end World form, which is the form the
-- | implicit record codec wrote: keys descending, a `Maybe` as its value or null.
module Views.Keys
  ( ViewKey
  , launched
  , batched
  , resumed
  , archived
  , deleted
  , derived
  , agencyDetail
  , sets
  , runSet
  , renamedSet
  , experiment
  , overview
  , balances
  , exchanges
  , transformations
  , economy
  , representation
  , procedures
  , assertions
  , wholeEconomy
  , objectives
  , standings
  , netWorth
  , actions
  , drivers
  , balance
  , events
  , table
  , runList
  , premise
  , premises
  , glossary
  , actorList
  , history
  , roundReport
  , compare
  ) where

import Prelude

import Core.Num (Kind, kindShape)
import Core.Shape (Shape, RecF, boolShape, fieldOf, intShape, listOf, object, objectValuesOf, optionalOf, stringShape)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Schema.View (View(..))
import Views.Actions (ActionsView)
import Views.Balance (BalanceView)
import Views.Common (Chart, Table)
import Views.Compare (CompareView)
import Views.Actor (ActorList, History)
import Views.Report (RoundReport)
import Laws.World (Event, eventShape)
import Views.Glossary (Glossary)
import Views.Overview (RunList, RunOverview)
import Views.Premise (Premise)
import Schema.Run (DeriveKind, deriveKindShape)

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

instance ViewShape DeriveKind where
  viewShape = deriveKindShape

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

table :: Shape Table
table = viewShape

chart :: Shape Chart
chart = viewShape

actionsView :: Shape ActionsView
actionsView = viewShape

balanceView :: Shape BalanceView
balanceView = viewShape

economyView :: Shape { totals :: Chart, flows :: Table, pressure :: Table }
economyView = viewShape

runOverview :: Shape RunOverview
runOverview = viewShape

runList :: Shape RunList
runList = viewShape

-- | `GET /api/premise`: the premises the launch form offers.
premises :: Shape (Array Premise)
premises = viewShape

premise :: Shape Premise
premise = viewShape

glossary :: Shape Glossary
glossary = viewShape

actorList :: Shape ActorList
actorList = viewShape

roundReport :: Shape RoundReport
roundReport = viewShape

history :: Shape History
history = viewShape

compare :: Shape CompareView
compare = viewShape

key :: forall a. View -> Shape a -> ViewKey a
key view shape = { view, shape }

overview :: ViewKey RunOverview
overview = key Overview runOverview

balances :: ViewKey Table
balances = key Balances table

exchanges :: ViewKey (Array Chart)
exchanges = key Exchanges (listOf chart)

transformations :: ViewKey Table
transformations = key Transformations table

economy :: ViewKey { totals :: Chart, flows :: Table, pressure :: Table }
economy = key Economy economyView

representation :: ViewKey Table
representation = key Representation table

procedures :: ViewKey Table
procedures = key Procedures table

wholeEconomy :: ViewKey Table
wholeEconomy = key WholeEconomy table

assertions :: ViewKey Table
assertions = key Assertions table

-- | The named Run sets.
sets :: Shape { sets :: Array { name :: String, baseline :: String, runs :: Array String } }
sets = viewShape

runSet :: Shape { name :: String, baseline :: String, runs :: Array String }
runSet = viewShape

experiment :: Shape { set :: String, runs :: Array String }
experiment = viewShape

renamedSet :: Shape { name :: String }
renamedSet = viewShape

-- | The agency analysis beyond its table.
agencyDetail :: Shape { criteria :: Table, profile :: Table }
agencyDetail = viewShape

objectives :: ViewKey Table
objectives = key Objectives table

standings :: ViewKey Table
standings = key Standings table

netWorth :: ViewKey Chart
netWorth = key NetWorth chart

actions :: ViewKey ActionsView
actions = key Actions actionsView

drivers :: ViewKey Table
drivers = key Drivers table

balance :: ViewKey BalanceView
balance = key Balance balanceView

events :: ViewKey Table
events = key Events table

-- Reply shapes of the write endpoints ------------------------------------------------------------

launched :: Shape { id :: String, playerSeed :: Int }
launched = viewShape

-- | One run of a batch: its Premise and its run.
batched :: Shape { premise :: String, id :: String, playerSeed :: Int }
batched = viewShape

resumed :: Shape { id :: String, rounds :: Int }
resumed = viewShape

archived :: Shape { id :: String, archived :: Boolean }
archived = viewShape

deleted :: Shape { deleted :: String }
deleted = viewShape

-- | The reply to a derive; `recordsMatch` only for a rerun.
derived :: Shape { id :: String, source :: String, kind :: DeriveKind, recordsMatch :: Maybe Boolean }
derived = viewShape
