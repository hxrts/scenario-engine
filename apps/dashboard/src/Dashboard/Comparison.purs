-- | What the Simulations and Live pages share (docs/07_simulator.md §9): the run browser (a run list as a table, a
-- | comparison of the runs, and the chosen run in full), and one run's balance sheets and results. The
-- | comparison reads in two parts: every run at a glance, one row each, and the plots of the few runs ticked, named
-- | in their legends. Every view model comes from the read API, through `Dashboard.Client`.
module Dashboard.Comparison (runTable, mkRunPage, mkComparison, mkRunBrowser) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (useLoad)
import Dashboard.BalanceSheet (mkLinks)
import Dashboard.Client as Client
import Dashboard.Components (groupedBars, loaded, note, overlayChart, placeholderFrame, selectInput, tableOf)
import Dashboard.DataTable (DataTable, defaults)
import Dashboard.Layout as Layout
import Dashboard.Palette (runColour)
import React.Basic.DOM (css)
import Dashboard.Results (mkRunResults)
import Dashboard.UI as UI
import Data.Array (elem, filter, find, index, null, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Schema.Run (RunStatus(..))
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React
import Views.Compare (CompareView)
import Views.Overview (RunList)

-- | The run list's id column: a run's id is its first cell, and a click on it selects the run.
runKeyColumn :: Int
runKeyColumn = 0

-- | The run list as a table. A row click selects that run; the selected row is highlighted.
runTable :: DataTable -> RunList -> { selected :: String, select :: String -> Effect Unit } -> JSX
runTable grid t s = D.div
  { className: "overflow-x-auto"
  , children:
      [ grid
          ( (defaults t)
              { onRow = Just \row -> s.select (fromMaybe "" (index row runKeyColumn))
              , activeRow = \row -> index row runKeyColumn == Just s.selected
              }
          )
      ]
  }

-- | A run's group: its title and description, each Actor's balance-sheet page, then the run's overview, fetched
-- | once, and its results.
mkRunPage :: DataTable -> Component { id :: String, title :: String, purpose :: String }
mkRunPage grid = do
  results <- mkRunResults grid
  links <- mkLinks
  component "RunPage" \p -> React.do
    overview <- useLoad p.id (Client.overview p.id)
    pure $ Layout.group
      { title: p.title
      , purpose: p.purpose
      , children:
          [ Layout.subsection { title: "Balance sheets", purpose: "Each Actor's own page for this run. A page shows only that Actor's data.", children: [ links p.id ] }
          , results { id: p.id, version: 0, overview }
          ]
      }

-- | A comparison of the runs given (in order, the first the baseline), as two groups: comparing the runs (every run at a
-- | glance, the runs to plot, and their plots), and the agency of each seat when agency runs are among them.
-- | `initial` names the runs ticked to start with.
mkComparison :: DataTable -> Component { ids :: Array String, names :: String -> String, initial :: Array String }
mkComparison grid = component "Comparison" \p -> React.do
  everyRun <- useLoad (String.joinWith "," p.ids) (Client.compare p.ids)
  ticked /\ setTicked <- useState' (Nothing :: Maybe (Array String))
  picked /\ setPicked <- useState' (Nothing :: Maybe String)
  let
    plotted = fromMaybe p.initial ticked
    toggle id on = setTicked (Just (if on then (if elem id plotted then plotted else plotted <> [ id ]) else filter (_ /= id) plotted))
  plots <- useLoad (String.joinWith "," plotted) (if null plotted then pure (Left "") else Client.compare plotted)
  pure $ React.fragment
    [ Layout.group
        { title: "Compare the runs"
        , purpose: "Every run side by side: one row each in a summary, then the runs you tick drawn on shared axes."
        , children:
            [ Layout.subsection
                { title: "The runs at a glance"
                , purpose: "One row per run: who leads after the final round and with what, how even final net worth is, how many units of fixed requirements went short, and how many Rules were enacted."
                , children: [ loaded everyRun.value \c -> tableOf grid c.summary ]
                }
            , Layout.subsection
                { title: "Runs to plot"
                , purpose: "Tick the runs to draw in the plots below. The first ticked is the baseline."
                , children: [ chooser p.ids p.names plotted toggle ]
                }
            , if null plotted then note "Tick a run above to plot it." else plotSections plotted picked setPicked plots.value
            ]
        }
    , loaded everyRun.value \c ->
        if null c.agency.rows then D.span_ []
        else Layout.group
          { title: "Agency"
          , purpose: "How far one seat changes a run on its own: each agency run against the preset run it deviates from, and that seat's objective criteria under each."
          , children:
              [ Layout.subsection { title: "What each seat moves", purpose: "The change in prices, in other players' and Aggregates' holdings, in institutional decisions and in the seat's own criteria.", children: [ tableOf grid c.agency ] }
              , Layout.subsection { title: "Each seat's criteria", purpose: "Every criterion of the seat's objective, met or not under the baseline and under its alternative.", children: [ tableOf grid c.agencyCriteria ] }
              ]
          }
    ]
  where
  chooser ids names plotted toggle = D.div
    { className: "grid grid-cols-1 gap-2 sm:grid-cols-2 lg:grid-cols-3"
    , children: ids <#> \id -> D.label
        { key: id
        , className: "flex items-center gap-2 text-sm"
        , children:
            [ element UI.checkbox { checked: elem id plotted, onCheckedChange: mkEffectFn1 (toggle id) }
            , D.span { className: "size-3 shrink-0 rounded-full", style: css { backgroundColor: if elem id plotted then runColour plotted id else "transparent" } }
            , D.span { children: [ D.text (names id) ] }
            ]
        }
    }
  plotSections plotted picked setPicked shown =
    let
      baseline = fromMaybe "" (index plotted 0)
      waiting = placeholderFrame "Loading the plots…"

      withPlots :: (CompareView -> Array JSX) -> Array JSX
      withPlots f = case shown of
        Just (Right c) -> f c
        Just (Left e) -> [ note e ]
        Nothing -> [ waiting ]
      label c id = maybe id _.name (find (\r -> r.id == id) c.runs) <> (if id == baseline then " (baseline)" else "")
      chosen c = case picked of
        Just pl | elem pl (map _.player c.netWorth) -> pl
        _ -> maybe "" _.player (index c.netWorth 0)
    in
      React.fragment
        [ Layout.subsection
            { title: "Prices that move"
            , purpose: "Each Exchange price that changes in any plotted run, after every round: one line per run, on the same axes."
            , children: withPlots \c -> if null c.exchanges then [ note "No price moves in the plotted runs." ] else [ D.div { className: "flex min-w-0 flex-col gap-stack", children: c.exchanges <#> \m -> Layout.captioned m.title [ overlayChart plotted (label c) baseline m ] } ]
            }
        , Layout.subsection
            { title: "Net worth over time"
            , purpose: "One player's net worth from the start (round 0) to the final round, with one line per plotted run. Choose the player."
            , children: withPlots \c ->
                [ D.label { className: "flex w-72 items-center gap-2 text-sm", children: [ D.text "Player", selectInput { value: chosen c, options: map _.player c.netWorth, onChange: setPicked } ] }
                , maybe waiting (\nw -> overlayChart plotted (label c) baseline nw.chart) (find (\nw -> nw.player == chosen c) c.netWorth)
                ]
            }
        , Layout.subsection
            { title: "Final standings"
            , purpose: "Each player's net worth after the final round: one group per player, with one bar per plotted run."
            , children: withPlots \c -> [ groupedBars plotted (label c) "Player" c.standings ]
            }
        ]

-- | A listed run: its id, its name, and whether it completed.
type Listed = { id :: String, name :: String, ok :: Boolean }

-- | The readable runs of a list, each with its name and whether it completed (the list's Name and Status cells).
listed :: RunList -> Array Listed
listed t = map (\(Tuple f row) -> { id: f.id, name: fromMaybe f.id (index row 1), ok: index row 6 == Just (show Completed) }) (filter (\(Tuple f _) -> f.readable) (zip t.runs t.rows))

-- | What both pages show of a list of runs, in order: the runs as a table to choose from, their comparison, and the
-- | chosen run in full. `baseline` names the run that heads the comparison and opens when none is chosen; with none,
-- | nothing opens until a run is chosen.
mkRunBrowser
  :: DataTable
  -> Component
       { list :: RunList
       , selected :: Maybe String
       , select :: String -> Effect Unit
       , baseline :: Maybe String
       , empty :: String
       , choose :: String
       }
mkRunBrowser grid = do
  comparison <- mkComparison grid
  runPage <- mkRunPage grid
  component "RunBrowser" \p -> React.do
    let
      runs = listed p.list
      baseline = maybe "" _.id ((p.baseline >>= \b -> find (\r -> r.id == b) runs) <|> (if p.baseline == Nothing then Nothing else find (const true) runs))
      current = p.selected <|> (if baseline == "" then Nothing else Just baseline)
      ordered = map _.id (filter (\r -> r.id == baseline) runs <> filter (\r -> r.id /= baseline) runs)
      nameOf id = maybe id (\r -> r.name <> (if r.ok then "" else " (failed)")) (find (\r -> r.id == id) runs)
    pure $
      if null p.list.runs then note p.empty
      else React.fragment
        [ Layout.group
            { title: "Choose a run"
            , purpose: p.choose
            , children: [ runTable grid p.list { selected: fromMaybe "" current, select: p.select } ]
            }
        , if null ordered then D.span_ [] else comparison { ids: ordered, names: nameOf, initial: if baseline == "" then [] else [ baseline ] }
        , maybe (D.span_ []) (\id -> runPage { id, title: "The selected run: " <> nameOf id, purpose: "One run in full: each Actor's balance sheet, its facts, the headline at a chosen round, each country's regime, the clearing between them, and the seats' scorecards." }) current
        ]
