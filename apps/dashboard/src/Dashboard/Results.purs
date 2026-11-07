-- | One run's results (docs/06_simulator.md §9.2). The summary answers the five questions in order:
-- | standings, net worth over time, the actions players take, the drivers beside the price chart, and
-- | the balance of the game. Each deeper topic is under its own Details section. Every number comes
-- | from the read API; this module chooses what to show and where, and renders each statistic's
-- | definition from its record in `Harness.Views`.
module Dashboard.Results (mkRunResults, Props, roundCount, roundTarget, clampRound, rowValue) where

import Prelude

import Dashboard.Api (runPath, useApi)
import Dashboard.Components (as, definition, fetched, fieldList, metricChart, status, tableOf)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.UI as UI
import Data.Array (drop, find, index)
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Harness.Views.Actions (actionsStat)
import Harness.Views.Balance (completionStat, giniStat, spreadStat)
import Harness.Views.Common (Chart, Statistic, Table)
import Harness.Views.Drivers (driversStat)
import Harness.Views.Standings (netWorthStat, rankStat)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React

-- | The run whose results are shown. Comparison is chosen on the Compare page, not here.
type Props = { id :: String }

-- | The economy view model (`/api/runs/:id/economy`).
type Economy = { totals :: Chart, flows :: Table, pressure :: Table }

-- | The actions view model (`/api/runs/:id/actions`): the run totals per Actor, and each round.
type Actions = { totals :: Table, byRound :: Table }

-- | The balance view model (`/api/runs/:id/balance`): the spread and Gini, and objective completion.
type Balance = { measures :: Table, completion :: Table }

-- | A value of a row of the Overview (a field and its value).
rowValue :: Table -> String -> Maybe String
rowValue t field = find (\r -> index r 0 == Just field) t.rows >>= \r -> index r 1

-- | The run's completed rounds, from the Overview's `Rounds` row.
roundCount :: Table -> Maybe Int
roundCount t = rowValue t "Rounds" >>= Int.fromString

-- | The run's target rounds, from the Overview's `Rounds target` row, when the server reports one.
roundTarget :: Table -> Maybe Int
roundTarget t = rowValue t "Rounds target" >>= Int.fromString

-- | A requested round, kept within the run's rounds (0 is the initial state).
clampRound :: Int -> Int -> Int
clampRound n r = max 0 (min n r)

-- | A view's description: the question it answers, then its formula.
describe :: Statistic -> String
describe s = s.question <> " Formula: " <> s.formula

-- | A table with its title (the server's title for it) above it.
titled :: DataTable -> Table -> JSX
titled grid t = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children: [ D.p { className: "text-sm font-medium", children: [ D.text t.title ] }, tableOf grid t ]
  }

-- | The results of one run. The overview is fetched first, because the rounds it reports scope the
-- | other views. A run that cannot be read shows its error.
mkRunResults :: DataTable -> Component Props
mkRunResults grid = do
  body <- mkBody grid
  component "RunResults" \p -> React.do
    overview <- useApi p.id (runPath p.id <> "/overview")
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-stack"
      , children:
          [ D.div
              { className: "flex flex-wrap items-center justify-between gap-2"
              , children: [ D.h2 { className: "text-lg font-semibold", children: [ D.text ("Run " <> p.id) ] } ]
              }
          , case overview of
              Nothing -> status Nothing
              Just (Left e) -> status (Just e)
              Just (Right (t :: Table)) -> body { id: p.id, table: t }
          ]
      }

type BodyProps = { id :: String, table :: Table }

-- | The summary, then the deep views. The round selector (for balance sheets and events) is clamped
-- | to the run's rounds.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> component "RunResultsBody" \b -> React.do
  requested /\ setRequested <- useState' (Nothing :: Maybe Int)
  let
    n = fromMaybe 0 (roundCount b.table)
    r = clampRound n (fromMaybe n requested)
    path view = runPath b.id <> "/" <> view
    roundInput label = D.label
      { className: "flex items-center gap-2 text-sm"
      , children:
          [ D.text label
          , element UI.input { className: "w-20", "aria-label": label, value: show r, onChange: handler targetValue (\v -> setRequested (Int.fromString =<< v)) }
          , D.text (" of " <> show n)
          ]
      }
    summary =
      [ Layout.section
          { title: "Standings"
          , purpose: describe netWorthStat <> " Rank: " <> rankStat.definition
          , children:
              [ definition netWorthStat
              , fetch { path: path "standings", render: as \(t :: Table) -> titled grid t }
              ]
          }
      , Layout.section
          { title: "Net worth over time"
          , purpose: describe netWorthStat <> " One line per player, from the start (round 0) to the final round."
          , children: [ fetch { path: path "networth", render: as \(c :: Chart) -> metricChart c } ]
          }
      , Layout.section
          { title: "Actions"
          , purpose: describe actionsStat <> " Totals over the whole run; the round-by-round counts are under Details."
          , children:
              [ definition actionsStat
              , fetch { path: path "actions", render: as \(a :: Actions) -> titled grid a.totals }
              ]
          }
      , Layout.grid
          [ Layout.panel
              { span: 6
              , title: "Price"
              , purpose: "What moves prices: the marginal price of each Market after every round, in quote units per base unit. The drivers of the same rounds are beside it."
              , children: [ fetch { path: path "markets", render: as \(cs :: Array Chart) -> maybe (D.span_ []) metricChart (index cs 0) } ]
              }
          , Layout.panel
              { span: 6
              , title: "Drivers"
              , purpose: describe driversStat
              , children:
                  [ definition driversStat
                  , fetch { path: path "drivers", render: as \(t :: Table) -> titled grid t }
                  ]
              }
          ]
      , Layout.section
          { title: "Balance of the game"
          , purpose: spreadStat.question <> " Formulas: " <> spreadStat.name <> ": " <> spreadStat.formula <> " " <> giniStat.name <> ": " <> giniStat.formula
          , children:
              [ definition spreadStat
              , definition giniStat
              , definition completionStat
              , fetch
                  { path: path "balance"
                  , render: as \(bal :: Balance) -> D.div
                      { className: "flex min-w-0 flex-col gap-section"
                      , children: [ titled grid bal.measures, titled grid bal.completion ]
                      }
                  }
              ]
          }
      ]
    deep =
      [ Layout.details
          { title: "Overview"
          , purpose: "Every overview field of this run, with the engine and draws it was built with. A field with no recorded value says not recorded, with the reason on hover."
          , children: [ fieldList b.table ]
          }
      , Layout.details
          { title: "Actions by round"
          , purpose: describe actionsStat
          , children: [ fetch { path: path "actions", render: as \(a :: Actions) -> titled grid a.byRound } ]
          }
      , Layout.details
          { title: "Markets"
          , purpose: "Reserves and traded volume for each Market, by round."
          , children: [ fetch { path: path "markets", render: as \(cs :: Array Chart) -> D.div_ (map metricChart (drop 1 cs)) } ]
          }
      , Layout.details
          { title: "Facilities"
          , purpose: "Each order the facilities filled: who ordered it, what it bought, and the Money paid."
          , children: [ fetch { path: path "facilities", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "World economy"
          , purpose: "Resource totals across all accounts, Circulation (the Money moved between Entity categories), and the pressure each round: shortfalls, rejected and lapsed Proposals, consumption and decay."
          , children: [ fetch { path: path "economy", render: as \(e :: Economy) -> D.div_ [ metricChart e.totals, titled grid e.flows, titled grid e.pressure ] } ]
          }
      , Layout.details
          { title: "Balance sheets by round"
          , purpose: "Every balance sheet after the round you choose, with the holdings at the start of the run for comparison. Defaults to the final round."
          , children: [ roundInput "Round", fetch { path: path ("balances?round=" <> show r), render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Objectives"
          , purpose: "Each Actor's objective criteria, each met, not met, or not evaluated, with the measured values. Objectives are measured from the run and never change the economy."
          , children: [ fetch { path: path "objectives", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Representation"
          , purpose: "The four-stage record of each Representation: the observation, the stored factor, the Aggregate's next purchase, and the influence ratio."
          , children: [ fetch { path: path "representation", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Procedures"
          , purpose: "Each collective procedure an Aggregate held: the members who signed, the support against the threshold, and whether it executed or lapsed."
          , children: [ fetch { path: path "procedures", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Events"
          , purpose: "The engine's events in the round you choose, in canonical phase order."
          , children: [ roundInput "Event round", fetch { path: path ("events?round=" <> show r), render: as \(t :: Table) -> titled grid t } ]
          }
      ]
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children: summary <> deep
    }
