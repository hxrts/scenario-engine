-- | The replies of the API, each with one shape (docs/06_simulator.md §8): the server encodes a view
-- | through its key, and the dashboard decodes it through the same key, so the two cannot disagree and
-- | neither names a view's type by hand. The shapes write the Snapshot form, which is the form the
-- | implicit record codec wrote: keys descending, a `Maybe` as its value or null.
module Views.Keys
  ( ViewKey
  , overview
  , balances
  , markets
  , facilities
  , economy
  , representation
  , procedures
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
  , glossary
  , entityList
  , history
  , compare
  ) where

import Core.Num (kindShape)
import Core.Run (runStatusShape)
import Core.Shape (Shape, boolShape, intShape, listOf, objectValuesOf, optionalOf, recordShape, stringShape)
import Core.View (View(..))
import Views.Actions (ActionsChart, ActionsView)
import Views.Balance (BalanceView)
import Views.Common (Bars, Chart, Point, Reason, Table)
import Views.Compare (CompareView, Marked)
import Views.Entity (EntityChoice, EntityList, History)
import Views.Glossary (Glossary)
import Views.Overview (Fact, RunFlags, RunList, RunOverview)
import Views.Premise (Premise, RoundRange)

-- | A view of a stored run, and the shape of its reply.
type ViewKey a = { view :: View, shape :: Shape a }

strings :: Shape (Array String)
strings = listOf stringShape

table :: Shape Table
table = recordShape "Table" { title: stringShape, columns: strings, rows: listOf strings }

point :: Shape Point
point = recordShape "Point" { round: intShape, value: stringShape }

chart :: Shape Chart
chart = recordShape "Chart"
  { title: stringShape
  , unit: stringShape
  , kind: kindShape
  , series: listOf (recordShape "Series" { name: stringShape, points: listOf point })
  }

bars :: Shape Bars
bars = recordShape "Bars"
  { title: stringShape
  , unit: stringShape
  , kind: kindShape
  , categories: strings
  , series: listOf (recordShape "BarSeries" { name: stringShape, values: strings })
  }

actionsChart :: Shape ActionsChart
actionsChart = recordShape "ActionsChart"
  { title: stringShape
  , purpose: stringShape
  , kinds: listOf (recordShape "ActionKind" { key: stringShape, label: stringShape })
  , bars: listOf
      ( recordShape "ActionBar"
          { player: stringShape
          , total: intShape
          , counts: listOf (recordShape "ActionCount" { key: stringShape, count: intShape })
          }
      )
  }

actionsView :: Shape ActionsView
actionsView = recordShape "ActionsView" { chart: actionsChart, totals: table, byRound: table }

balanceView :: Shape BalanceView
balanceView = recordShape "BalanceView" { measures: table, completion: table }

economyView :: Shape { totals :: Chart, flows :: Table, pressure :: Table }
economyView = recordShape "Economy" { totals: chart, flows: table, pressure: table }

fact :: Shape Fact
fact = recordShape "Fact" { label: stringShape, value: stringShape }

runOverview :: Shape RunOverview
runOverview = recordShape "RunOverview"
  { id: stringShape
  , rounds: intShape
  , target: intShape
  , resumable: boolShape
  , facts: listOf fact
  }

runFlags :: Shape RunFlags
runFlags = recordShape "RunFlags" { id: stringShape, archived: boolShape, readable: boolShape }

runList :: Shape RunList
runList = recordShape "RunList" { title: stringShape, columns: strings, rows: listOf strings, runs: listOf runFlags }

roundRange :: Shape RoundRange
roundRange = recordShape "RoundRange" { min: intShape, max: intShape, default: intShape }

premise :: Shape Premise
premise = recordShape "Premise"
  { name: stringShape
  , version: stringShape
  , rounds: roundRange
  , gameSeed: intShape
  , defaultName: stringShape
  , presets: strings
  }

reason :: Shape Reason
reason = recordShape "Reason" { label: stringShape, reason: stringShape }

glossary :: Shape Glossary
glossary = recordShape "Glossary"
  { definitions: objectValuesOf stringShape
  , reasons: objectValuesOf reason
  , unnamed: recordShape "Unnamed" { definition: stringShape, reason }
  , round: recordShape "RoundEntry" { prefix: stringShape, definition: stringShape }
  }

entityChoice :: Shape EntityChoice
entityChoice = recordShape "EntityChoice" { id: stringShape, seat: boolShape }

entityList :: Shape EntityList
entityList = recordShape "EntityList" { default: stringShape, entities: listOf entityChoice }

history :: Shape History
history = recordShape "History" { table, chart, netWorth: chart }

marked :: Shape Marked
marked = recordShape "Marked" { table, marked: listOf (listOf boolShape) }

compare :: Shape CompareView
compare = recordShape "CompareView"
  { compatibility: table
  , trajectory: optionalOf chart
  , markets: listOf chart
  , netWorth: listOf (recordShape "PlayerChart" { player: stringShape, chart })
  , standings: bars
  , balance: recordShape "BalanceBars" { spread: bars, gini: bars, unit: stringShape }
  , keyValues: table
  , final: marked
  , baseline: stringShape
  , diff: table
  , config: marked
  , seeds: table
  , skipped: strings
  }

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
