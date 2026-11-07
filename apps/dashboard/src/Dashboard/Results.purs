-- | One run's results (docs/06_simulator.md §9.2). A compact header names the run; then the sections
-- | answer the five questions in order: the key figures, the marginal prices, the standings, net worth
-- | over time, the actions players take, the drivers, the balance of the game, the objectives, and the
-- | final balance sheet. Every number comes from the read API; this module chooses what to show and
-- | where, and renders each statistic's definition from its record in `Analysis.Views`.
module Dashboard.Results (mkRunResults, Props, roundCount, roundTarget) where

import Prelude

import Dashboard.Api (runPath, useApi)
import Dashboard.Components (as, fetched, metricChart, stackedBars, stale, status, tableOf)
import Dashboard.Glossary (emptyCell)
import Dashboard.DataTable (DataTable)
import Dashboard.Entity (mkEntity)
import Dashboard.Layout as Layout
import Data.Array (find, index)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Analysis.Views.Actions (ActionsChart, actionsStat)
import Analysis.Views.Balance (giniStat, spreadStat)
import Analysis.Views.Common (Chart, Statistic, Table)
import Analysis.Views.Drivers (driversStat)
import Analysis.Views.Standings (netWorthStat)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useEffect, useState', (/\))
import React.Basic.Hooks as React

-- | The run whose results are shown, and its status from the run list. Comparison is chosen on the
-- | Compare page, not here.
type Props = { id :: String, status :: String }

-- | The actions view model's run chart and run totals (`/api/runs/:id/actions`). The per-round counts
-- | are in the API only.
type Actions = { chart :: ActionsChart, totals :: Table }

-- | The balance view model's measures (`/api/runs/:id/balance`): the spread and Gini at the final round.
-- | The objective completion is in the API only; the objectives table shows the same criteria.
type Balance = { measures :: Table }

-- | A value of a row of the overview (a field and its value).
rowValue :: Table -> String -> Maybe String
rowValue t field = find (\r -> index r 0 == Just field) t.rows >>= \r -> index r 1

-- | The run's completed rounds, from the overview's `Rounds` row.
roundCount :: Table -> Maybe Int
roundCount t = rowValue t "Rounds" >>= Int.fromString

-- | The run's target rounds, from the overview's `Rounds target` row, when the server reports one.
roundTarget :: Table -> Maybe Int
roundTarget t = rowValue t "Rounds target" >>= Int.fromString

-- | A statistic as a key figure: its name, the question it answers, its definition and its formula.
figure :: Statistic -> JSX
figure s = D.div
  { className: "flex min-w-0 flex-col gap-1 text-sm"
  , children:
      [ D.p { className: "font-medium", children: [ D.text s.name ] }
      , D.p { className: "text-muted-foreground", children: [ D.text (s.question <> " " <> s.definition) ] }
      , D.p { className: "text-muted-foreground", children: [ D.text ("Formula: " <> s.formula) ] }
      ]
  }

-- | A table with its title (the server's title for it) above it.
titled :: DataTable -> Table -> JSX
titled grid t = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children: [ D.p { className: "text-sm font-medium", children: [ D.text t.title ] }, tableOf grid t ]
  }

-- | The compact header: the run's id, name, scenario, status, rounds completed out of its target, and
-- | its game id.
header :: String -> String -> Table -> JSX
header id status t = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.h2 { className: "text-lg font-semibold", children: [ D.text ("Run " <> id) ] }
      , D.dl
          { className: "grid grid-cols-[max-content_1fr] gap-x-6 gap-y-1 text-sm"
          , children: facts >>= \(Tuple label value) ->
              [ D.dt { key: label, className: "text-muted-foreground", children: [ D.text label ] }
              , D.dd { key: label <> "-value", className: "min-w-0 break-words", children: [ valueOf label value ] }
              ]
          }
      ]
  }
  where
  -- an empty value says what it means, with the reason on hover (the same wording as the tables)
  valueOf label value
    | value == "" = let e = emptyCell label in D.span { className: "italic text-muted-foreground", title: e.reason, children: [ D.text e.label ] }
    | otherwise = D.text value
  field name = fromMaybe "" (rowValue t name)
  rounds = case roundCount t, roundTarget t of
    Just n, Just target -> show n <> " of " <> show target
    Just n, Nothing -> show n
    _, _ -> ""
  facts =
    [ Tuple "Name" (field "Name")
    , Tuple "Scenario" (field "Gameboard")
    , Tuple "Status" status
    , Tuple "Rounds completed" rounds
    , Tuple "Game ID" (field "Game ID")
    ]

-- | The results of one run. The overview is fetched first, because the rounds it reports scope the
-- | other views. A run that cannot be read shows its error.
mkRunResults :: DataTable -> Component Props
mkRunResults grid = do
  body <- mkBody grid
  component "RunResults" \p -> React.do
    overview <- useApi p.id (runPath p.id <> "/overview")
    -- the previous run's header stays, dimmed, until the selected run's overview arrives
    shown /\ setShown <- useState' (Nothing :: Maybe (Either String Table))
    useEffect overview do
      case overview of
        Just r -> setShown (Just r)
        Nothing -> pure unit
      pure (pure unit)
    let busy = isNothing overview
    pure $ stale busy case (if busy then shown else overview) of
      Nothing -> status Nothing
      Just (Left e) -> status (Just e)
      Just (Right (t :: Table)) -> D.div
        { className: "flex min-w-0 flex-col gap-section"
        , children: [ header p.id p.status t, body { id: p.id, table: t } ]
        }

type BodyProps = { id :: String, table :: Table }

-- | The sections, in the order of the five questions. Each has one purpose line.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> mkEntity >>= \entity -> component "RunResultsBody" \b -> React.do
  let
    n = fromMaybe 0 (roundCount b.table)
    path view = runPath b.id <> "/" <> view
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children:
        [ Layout.section
            { title: "Key figures"
            , purpose: "The measure behind each of the five questions, with the formula the sections below use."
            , children: [ figure netWorthStat, figure actionsStat, figure driversStat, figure spreadStat, figure giniStat ]
            }
        , Layout.section
            { title: "Marginal prices"
            , purpose: "What moves prices: the marginal price of each Market after every round, in quote units per base unit."
            , children: [ fetch { path: path "markets", render: as \(cs :: Array Chart) -> maybe (D.span_ []) metricChart (index cs 0) } ]
            }
        , Layout.section
            { title: "Standings"
            , purpose: "How well each player is doing against the others: net worth after the final round, its change from the start, and its rank."
            , children: [ fetch { path: path "standings", render: as \(t :: Table) -> titled grid t } ]
            }
        , Layout.section
            { title: "Net worth over time"
            , purpose: "One line per player: net worth from the start (round 0) to the final round."
            , children: [ fetch { path: path "networth", render: as \(c :: Chart) -> metricChart c } ]
            }
        , Layout.section
            { title: "Actions"
            , purpose: "What each player does over the whole run, by kind of action: one bar per player, and the table gives the exact counts."
            , children: [ fetch { path: path "actions", render: as \(a :: Actions) -> D.div { className: "flex min-w-0 flex-col gap-section", children: [ stackedBars a.chart, titled grid a.totals ] } } ]
            }
        , Layout.section
            { title: "Drivers"
            , purpose: "What forces drive gameplay, by round: the rules enacted and repealed, the collective procedure outcomes, and the shortfalls per Resource."
            , children: [ fetch { path: path "drivers", render: as \(t :: Table) -> titled grid t } ]
            }
        , Layout.section
            { title: "Balance measures"
            , purpose: "How evenly final net worth is spread among the players: the spread and the Gini coefficient."
            , children: [ fetch { path: path "balance", render: as \(bal :: Balance) -> titled grid bal.measures } ]
            }
        , Layout.section
            { title: "Objectives"
            , purpose: "Each Actor's objective criteria, met, not met or not evaluated, with the measured values. Objectives are measured from the run and never change the economy."
            , children: [ fetch { path: path "objectives", render: as \(t :: Table) -> titled grid t } ]
            }
        , Layout.section
            { title: "Final balance sheet"
            , purpose: "Every account's holdings by Resource after the final round, with the holdings at the start of the run for comparison."
            , children: [ fetch { path: path ("balances?round=" <> show n), render: as \(t :: Table) -> titled grid t } ]
            }
        , entity { id: b.id }
        ]
    }
