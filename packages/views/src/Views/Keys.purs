-- | The replies of the API, each with one shape (docs/06_simulator.md §8): the server encodes a view
-- | through its key, and the dashboard decodes it through the same key, so the two cannot disagree and
-- | neither names a view's type by hand. The shapes write the Snapshot form, which is the form the
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
  , scenarioSet
  , renamedSet
  , experiment
  , overview
  , balances
  , markets
  , facilities
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
  , setting
  , settings
  , glossary
  , entityList
  , history
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
import Views.Entity (EntityList, History)
import Views.Glossary (Glossary)
import Views.Overview (RunList, RunOverview)
import Views.Setting (Setting)
import Schema.Run (DeriveKind, deriveKindShape)

-- | A view of a stored run, and the shape of its reply.
type ViewKey a = { view :: View, shape :: Shape a }

-- | The shape of a view model, derived from its type. A view model is built of text, numbers, flags, arrays,
-- | records, optional values, and Kinds, and holds no Quantity, no Id, and no sum, so every one of its types has
-- | one shape. Field names are the keys, in both formats.
class ViewShape a where
  viewShape :: Shape a

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

-- | `GET /api/setting`: the settings the launch form offers.
settings :: Shape (Array Setting)
settings = viewShape

setting :: Shape Setting
setting = viewShape

glossary :: Shape Glossary
glossary = viewShape

entityList :: Shape EntityList
entityList = viewShape

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

markets :: ViewKey (Array Chart)
markets = key Markets (listOf chart)

facilities :: ViewKey Table
facilities = key Facilities table

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

-- | The named scenario sets.
sets :: Shape { sets :: Array { name :: String, baseline :: String, runs :: Array String } }
sets = viewShape

scenarioSet :: Shape { name :: String, baseline :: String, runs :: Array String }
scenarioSet = viewShape

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

-- | One run of a batch: its Setting and its run.
batched :: Shape { setting :: String, id :: String, playerSeed :: Int }
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
