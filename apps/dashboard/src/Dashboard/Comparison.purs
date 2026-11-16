-- | What the Simulations and Live pages share (docs/07_simulator.md §9): the run browser, three tabs over a list of
-- | runs. Compare: every run at a glance, one row each, a row's tick drawing it in the plots beneath and a click on it
-- | opening it. Agency: what each seat changes alone, when agency runs are among them. Run: the chosen run in full.
-- | Every view model comes from the read API, through `Dashboard.Client`.
module Dashboard.Comparison (mkRunBrowser) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (useLoad)
import Dashboard.BalanceSheet (mkLinks)
import Dashboard.Client as Client
import Dashboard.Components (Size(..), blocksOf, groupedBars, loaded, note, overlayChart, placeholderFrame, tableOf)
import Dashboard.DataTable (DataTable, defaults)
import Dashboard.Layout as Layout
import Dashboard.Palette (runColour)
import Dashboard.Results (mkRunResults)
import Dashboard.UI as UI
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM (css)
import React.Basic.DOM as D
import Data.Array (elem, filter, find, index, null, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React
import Schema.Run (RunStatus(..))
import Views.Compare (CompareView)
import Views.Overview (RunList)

-- | The run tables' id column: a run's id is its first cell.
runKeyColumn :: Int
runKeyColumn = 0

-- | A listed run: its id, its name, and whether it completed.
type Listed = { id :: String, name :: String, ok :: Boolean }

-- | The runs of a list, each with its name and whether it completed (the list's Name and Status cells).
listed :: RunList -> Array Listed
listed t = map (\(Tuple f row) -> { id: f.id, name: fromMaybe f.id (index row 1), ok: index row 6 == Just (show Completed) }) (zip t.runs t.rows)

-- | One run in full: its results and, where the page hands them out, each Actor's balance-sheet page.
mkRunPage :: DataTable -> Component { id :: String, version :: Int, title :: String, links :: Boolean }
mkRunPage grid = do
  results <- mkRunResults grid
  links <- mkLinks grid
  component "RunPage" \p -> React.do
    overview <- useLoad (p.id <> "@" <> show p.version) (Client.overview p.id)
    pure $ Layout.group
      { title: p.title
      , purpose: "The seats' scorecards, whether each country reproduces itself, and the circuit between them."
      , children:
          [ results { id: p.id, version: p.version, overview } ]
            <> (if p.links then [ Layout.subsection { title: "Balance sheets", purpose: "Each Actor's own page, to hand to whoever plays it.", children: [ links p.id ] } ] else [])
      }

-- | What both pages show of a list of runs, as tabs. `baseline` names the run that heads the comparison and is open
-- | in the Run tab when none is chosen; with none, the Run tab appears once a run is chosen. `openRun` opens the Run
-- | tab to start, `badge` marks a run's name, `links` shows a run's balance-sheet pages, and `version`, bumped,
-- | refetches the comparison and the open run.
mkRunBrowser
  :: DataTable
  -> Component
       { list :: RunList
       , selected :: Maybe String
       , select :: String -> Effect Unit
       , baseline :: Maybe String
       , empty :: String
       , badge :: String -> Maybe JSX
       , version :: Int
       , links :: Boolean
       , openRun :: Boolean
       }
mkRunBrowser grid = do
  runPage <- mkRunPage grid
  component "RunBrowser" \p -> React.do
    chosenTab /\ setTab <- useState' (Nothing :: Maybe String)
    ticked /\ setTicked <- useState' (Nothing :: Maybe (Array String))
    let
      runs = listed p.list
      baseline = maybe "" _.id (p.baseline >>= \b -> find (\r -> r.id == b) runs)
      current = p.selected <|> (if baseline == "" then Nothing else Just baseline)
      ordered = map _.id (filter (\r -> r.id == baseline) runs <> filter (\r -> r.id /= baseline) runs)
      nameOf id = maybe id (\r -> r.name <> (if r.ok then "" else " (failed)")) (find (\r -> r.id == id) runs)
      plotted = fromMaybe (if baseline == "" then [] else [ baseline ]) ticked
      toggle id on = setTicked (Just (if on then (if elem id plotted then plotted else plotted <> [ id ]) else filter (_ /= id) plotted))
      compared ids = if null ids then pure (Left "") else Client.compare ids
    everyRun <- useLoad (String.joinWith "," ordered <> "@" <> show p.version) (compared ordered)
    plots <- useLoad (String.joinWith "," plotted <> "@" <> show p.version) (compared plotted)
    let
      hasAgency = case everyRun.value of
        Just (Right c) -> not (null c.agency.rows)
        _ -> false
      -- the tab asked for, when it is on the page: a chosen run opens the Run tab
      wanted = fromMaybe (if isJust p.selected || p.openRun then "run" else "compare") chosenTab
      tab
        | wanted == "run" && isJust current = "run"
        | wanted == "agency" && hasAgency = "agency"
        | otherwise = "compare"
      open id = p.select id *> setTab (Just "run")
      glance c = grid
        ( (defaults c.summary)
            { onRow = Just \row -> open (fromMaybe "" (index row runKeyColumn))
            , activeRow = \row -> index row runKeyColumn == current
            , badge = \row i -> if i == 1 then index row runKeyColumn >>= p.badge else Nothing
            , tick = Just
                { control: \row ->
                    let
                      id = fromMaybe "" (index row runKeyColumn)
                    in
                      D.label
                        { className: "flex items-center gap-2"
                        , children:
                            [ element UI.checkbox { checked: elem id plotted, onCheckedChange: mkEffectFn1 (toggle id), "aria-label": "Plot " <> nameOf id }
                            , D.span { className: "size-3 shrink-0 rounded-full", style: css { backgroundColor: if elem id plotted then runColour plotted id else "transparent" } }
                            ]
                        }
                , label: "Plot"
                , hint: "Tick a run to draw it in the plots below. The first ticked is the baseline."
                }
            }
        )
      compare = Layout.group
        { title: "Compare the runs"
        , purpose: "Tick a run to plot it; click a row to open it."
        , children:
            [ Layout.subsection
                { title: "The runs at a glance"
                , purpose: "After the final round: who leads and with what, how even net worth is, what went short, and the Rules enacted. The first run ticked is the baseline."
                , children: [ loaded everyRun.value glance ]
                }
            , Layout.subsection
                { title: "Where each run leaves each country"
                , purpose: "After the final round: its widest substitute gap (above 0, an industry of its own stands idle), the least of its output its households absorb, and the most one of its sellers owes over what it sells."
                , children: [ loaded everyRun.value \c -> tableOf grid c.traps ]
                }
            , if null plotted then note "Tick a run above to plot it." else plotSections plotted nameOf plots.value
            ]
        }
      agency = loaded everyRun.value \c -> Layout.group
        { title: "Agency"
        , purpose: "What one seat changes on its own: each seat playing its alternative, against the run it departs from."
        , children:
            [ Layout.subsection { title: "What each seat moves", purpose: "The change in prices, in others' holdings, in institutional decisions and in the seat's own criteria.", children: [ tableOf grid c.agency ] }
            , Layout.subsection { title: "Each seat's criteria", purpose: "Each criterion under the baseline and under the alternative.", children: [ blocksOf grid c.agencyCriteria ] }
            ]
        }
      -- the run open, chosen from every run listed
      chooser id = D.label
        { className: "flex w-full max-w-md items-center gap-2 text-sm"
        , children:
            [ D.text "Run"
            , D.select
                { value: id
                , className: "app-select h-9 w-full rounded-md border border-input bg-transparent pl-3 pr-9 text-sm"
                , onChange: handler targetValue (maybe (pure unit) p.select)
                , children: ordered <#> \r -> D.option { key: r, value: r, children: [ D.text (nameOf r <> " (" <> r <> ")") ] }
                }
            ]
        }
      run id = React.fragment [ chooser id, runPage { id, version: p.version, title: "Run: " <> nameOf id, links: p.links } ]
    pure $
      if null ordered then note p.empty
      else Layout.tabbed
        { value: tab
        , onChange: setTab <<< Just
        , tabs:
            [ { key: "compare", label: "Compare", content: compare } ]
              <> (if hasAgency then [ { key: "agency", label: "Agency", content: agency } ] else [])
              <> maybe [] (\id -> [ { key: "run", label: "Run", content: run id } ]) current
        }

-- | The plots of the runs ticked, the first the baseline: the prices that move and each player's net worth as small
-- | plots read together, then the final standings.
plotSections :: Array String -> (String -> String) -> Maybe (Either String CompareView) -> JSX
plotSections plotted nameOf shown = React.fragment
  [ Layout.subsection
      { title: "Prices that move"
      , purpose: "Each Exchange price that changes in a plotted run, by round."
      , children: withPlots \c -> if null c.exchanges then [ note "No price moves in the plotted runs." ] else [ Layout.tiles (c.exchanges <#> \m -> Layout.captioned (m.title <> " (" <> m.unit <> ")") [ overlayChart Small plotted (label c) baseline m ]) ]
      }
  , Layout.subsection
      { title: "Net worth over time"
      , purpose: "Each player's net worth from the start (round 0), by round."
      , children: withPlots \c -> [ Layout.tiles (c.netWorth <#> \nw -> Layout.captioned (nw.player <> " (" <> nw.chart.unit <> ")") [ overlayChart Small plotted (label c) baseline nw.chart ]) ]
      }
  , Layout.subsection
      { title: "Final standings"
      , purpose: "Each player's net worth after the final round."
      , children: withPlots \c -> [ groupedBars plotted (label c) "Player" c.standings ]
      }
  ]
  where
  baseline = fromMaybe "" (index plotted 0)

  withPlots :: (CompareView -> Array JSX) -> Array JSX
  withPlots f = case shown of
    Just (Right c) -> f c
    Just (Left e) -> [ note e ]
    Nothing -> [ placeholderFrame "Loading the plots…" ]
  label c id = maybe (nameOf id) _.name (find (\r -> r.id == id) c.runs) <> (if id == baseline then " (baseline)" else "")
