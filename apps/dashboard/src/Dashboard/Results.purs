-- | One run's results (docs/06_simulator.md §9.2), a presentational view of the overview the run page
-- | fetched. A compact header names the run from the overview's facts; then the sections answer the five
-- | questions in order: the key figures, the marginal prices, the standings, net worth over time, the
-- | actions players take, the drivers, the balance of the game, the objectives, and the final balance
-- | sheet. Every number comes from the read API; this module chooses what to show and where, and renders
-- | each statistic's definition from its record in `Views`.
module Dashboard.Results (mkRunResults, Props) where

import Prelude

import Dashboard.Client (runView)
import Dashboard.Api (Loaded)
import Dashboard.Components (StatVariant(..), as, fetched, metricChart, stackedBars, stale, status, statistic, tableOf)
import Dashboard.DataTable (DataTable)
import Dashboard.Entity (mkEntity)
import Dashboard.Glossary (Lookup, emptyCell, glossaryContext)
import Dashboard.Layout as Layout
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Views.Actions (ActionsChart, actionsStat)
import Views.Balance (giniStat, spreadStat)
import Views.Common (Chart, Table)
import Views.Drivers (driversStat)
import Views.Overview (Fact, RunOverview)
import Views.Standings (netWorthStat)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useContext)
import React.Basic.Hooks as React

-- | The run whose results are shown, its version (bumped by an action on it, so every section refetches),
-- | and its overview as the run page loaded it (dimmed while the next run's loads). Comparison is chosen
-- | on the Compare page, not here.
type Props = { id :: String, version :: Int, overview :: Loaded (Either String RunOverview) }

-- | The actions view model's run chart and run totals (`/api/runs/:id/actions`). The per-round counts
-- | are in the API only.
type Actions = { chart :: ActionsChart, totals :: Table }

-- | The balance view model's measures (`/api/runs/:id/balance`): the spread and Gini at the final round.
-- | The objective completion is in the API only; the objectives table shows the same criteria.
type Balance = { measures :: Table }

-- | A table with its title (the server's title for it) above it.
titled :: DataTable -> Table -> JSX
titled grid t = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children: [ Layout.caption t.title, tableOf grid t ]
  }

-- | The compact header: the run's id, then its facts (name, scenario, status, rounds completed out of
-- | its target, game id), both from the same overview, so a run switch never shows a new name over the
-- | old facts. An empty fact says what it means, with the reason on hover.
header :: Lookup -> String -> Array Fact -> JSX
header glossary id facts = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.h2 { className: "text-lg font-semibold", children: [ D.text ("Run " <> id) ] }
      , D.dl
          { className: "grid grid-cols-[max-content_1fr] gap-x-6 gap-y-1 text-sm"
          , children: facts >>= \f ->
              [ D.dt { key: f.label, className: "text-muted-foreground", children: [ D.text f.label ] }
              , D.dd { key: f.label <> "-value", className: "min-w-0 break-words", children: [ valueOf f ] }
              ]
          }
      ]
  }
  where
  valueOf f
    | f.value == "" = let e = emptyCell glossary f.label in D.span { className: "italic text-muted-foreground", title: e.reason, children: [ D.text e.label ] }
    | otherwise = D.text f.value

-- | The results of one run. A run whose overview cannot be read shows its error; the previous run's
-- | results stay on screen, dimmed, until the selected run's overview arrives.
mkRunResults :: DataTable -> Component Props
mkRunResults grid = do
  body <- mkBody grid
  component "RunResults" \p -> React.do
    glossary <- useContext glossaryContext
    pure $ stale p.overview.busy case p.overview.value of
      Nothing -> status Nothing
      Just (Left e) -> status (Just e)
      Just (Right o) -> D.div
        { className: "flex min-w-0 flex-col gap-section"
        , children: [ header glossary o.id (o.facts), body { id: p.id, version: p.version } ]
        }

type BodyProps = { id :: String, version :: Int }

-- | The sections, in the order of the five questions. Each has one purpose line.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> mkEntity >>= \entity -> component "RunResultsBody" \b -> React.do
  let
    keyed path = b.id <> "/" <> path <> "@" <> show b.version
    view name render = fetch { key: keyed name, load: runView b.id name Nothing, render }
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children:
        [ Layout.section
            { title: "Key figures"
            , purpose: "The measure behind each of the five questions, with the formula the sections below use."
            , children: [ statistic Figure netWorthStat, statistic Figure actionsStat, statistic Figure driversStat, statistic Figure spreadStat, statistic Figure giniStat ]
            }
        , Layout.section
            { title: "Marginal prices"
            , purpose: "What moves prices: the marginal price of each Market after every round, in quote units per base unit."
            , children: [ view "markets" (as \(cs :: Array Chart) -> maybe (D.span_ []) metricChart (index cs 0)) ]
            }
        , Layout.section
            { title: "Standings"
            , purpose: "How well each player is doing against the others: net worth after the final round, its change from the start, and its rank."
            , children: [ view "standings" (as \(t :: Table) -> titled grid t) ]
            }
        , Layout.section
            { title: "Net worth over time"
            , purpose: "One line per player: net worth from the start (round 0) to the final round."
            , children: [ view "networth" (as \(c :: Chart) -> metricChart c) ]
            }
        , Layout.section
            { title: "Actions"
            , purpose: "What each player does over the whole run, by kind of action: one bar per player, and the table gives the exact counts."
            , children: [ view "actions" (as \(a :: Actions) -> D.div { className: "flex min-w-0 flex-col gap-section", children: [ stackedBars a.chart, titled grid a.totals ] }) ]
            }
        , Layout.section
            { title: "Drivers"
            , purpose: "What forces drive gameplay, by round: the rules enacted and repealed, the collective procedure outcomes, and the shortfalls per Resource."
            , children: [ view "drivers" (as \(t :: Table) -> titled grid t) ]
            }
        , Layout.section
            { title: "Balance measures"
            , purpose: "How evenly final net worth is spread among the players: the spread and the Gini coefficient."
            , children: [ view "balance" (as \(bal :: Balance) -> titled grid bal.measures) ]
            }
        , Layout.section
            { title: "Objectives"
            , purpose: "Each Actor's objective criteria, met, not met or not evaluated, with the measured values. Objectives are measured from the run and never change the economy."
            , children: [ view "objectives" (as \(t :: Table) -> titled grid t) ]
            }
        , Layout.section
            { title: "Final balance sheet"
            , purpose: "Every account's holdings by Resource after the final round, with the holdings at the start of the run for comparison."
            , children: [ fetch { key: keyed "balances", load: runView b.id "balances" Nothing, render: as \(t :: Table) -> titled grid t } ]
            }
        , entity { id: b.id, version: b.version }
        ]
    }
