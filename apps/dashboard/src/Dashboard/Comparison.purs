-- | What the Simulations and Live pages share (docs/07_simulator.md §9): a run list as a table, one run's results, and
-- | a comparison of many runs. The comparison reads in two parts: every run at a glance, one row each, and the plots
-- | of the few runs ticked, named in their legends. Every view model comes from the read API, through
-- | `Dashboard.Client`.
module Dashboard.Comparison (runTable, mkRunPage, mkComparison, plotLimit) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Components (groupedBars, loaded, note, overlayChart, placeholderFrame, selectInput, tableOf)
import Dashboard.DataTable (DataTable, defaults)
import Dashboard.Layout as Layout
import Dashboard.Palette (runSwatch)
import Dashboard.Results (mkRunResults)
import Dashboard.UI as UI
import Data.Array (elem, filter, find, index, length, null, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React
import Views.Compare (CompareView)
import Views.Overview (RunList)

-- | The most runs the plots draw at once: one per chart colour, so every line can be told apart.
plotLimit :: Int
plotLimit = 5

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

-- | The run page: the selected run's overview, fetched once, and its results, set apart from what is above it.
mkRunPage :: DataTable -> Component { id :: String }
mkRunPage grid = do
  results <- mkRunResults grid
  component "RunPage" \p -> React.do
    overview <- useLoad p.id (Client.overview p.id)
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-gutter border-t-2 border-border pt-8"
      , children: [ results { id: p.id, version: 0, overview } ]
      }

-- | A comparison of the runs given (in order, the first the baseline): every run at a glance, then the plots of the
-- | runs ticked, at most `plotLimit`, and the agency of each seat when agency runs are among them. `initial` names the
-- | runs ticked to start with.
mkComparison :: DataTable -> Component { ids :: Array String, names :: String -> String, initial :: Array String }
mkComparison grid = component "Comparison" \p -> React.do
  everyRun <- useLoad (String.joinWith "," p.ids) (Client.compare p.ids)
  ticked /\ setTicked <- useState' (Nothing :: Maybe (Array String))
  picked /\ setPicked <- useState' (Nothing :: Maybe String)
  let
    plotted = take plotLimit (fromMaybe p.initial ticked)
    toggle id on = setTicked (Just (if on then (if elem id plotted || length plotted >= plotLimit then plotted else plotted <> [ id ]) else filter (_ /= id) plotted))
  plots <- useLoad (String.joinWith "," plotted) (if null plotted then pure (Left "") else Client.compare plotted)
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children:
        [ Layout.section
            { title: "The runs at a glance"
            , purpose: "One row per run: who leads after the final round and with what, how even final net worth is, how many units of fixed requirements went short, and how many Rules were enacted."
            , children: [ loaded everyRun.value \c -> tableOf grid c.summary ]
            }
        , Layout.section
            { title: "Plotted runs"
            , purpose: "Tick up to " <> show plotLimit <> " runs to plot them on shared axes. The first ticked is the baseline."
            , children: [ chooser p.ids p.names plotted toggle ]
            }
        , if null plotted then note "Tick a run above to plot it." else plotSections plotted picked setPicked plots.value
        , loaded everyRun.value \c ->
            if null c.agency.rows then D.span_ []
            else Layout.section
              { title: "Agency"
              , purpose: "For each agency run: how far one seat playing its alternative preset moves the run, against the preset run it deviates from, and its objective criteria under each."
              , children: [ tableOf grid c.agency, tableOf grid c.agencyCriteria ]
              }
        ]
    }
  where
  chooser ids names plotted toggle = D.div
    { className: "grid grid-cols-1 gap-2 sm:grid-cols-2 lg:grid-cols-3"
    , children: ids <#> \id -> D.label
        { key: id
        , className: "flex items-center gap-2 text-sm"
        , children:
            [ element UI.checkbox { checked: elem id plotted, onCheckedChange: mkEffectFn1 (toggle id) }
            , D.span { className: "size-3 shrink-0 rounded-full " <> (if elem id plotted then runSwatch plotted id else "bg-transparent") }
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
      D.div
        { className: "flex min-w-0 flex-col gap-section"
        , children:
            [ Layout.section
                { title: "Prices that move"
                , purpose: "Each Exchange price that changes in any plotted run, after every round: one line per run, on the same axes."
                , children: withPlots \c -> if null c.exchanges then [ note "No price moves in the plotted runs." ] else [ Layout.grid (c.exchanges <#> \m -> D.div { className: "col-span-12 min-w-0 lg:col-span-6", children: [ Layout.captioned m.title [ overlayChart plotted (label c) baseline m ] ] }) ]
                }
            , Layout.section
                { title: "Net worth over time"
                , purpose: "One player's net worth from the start (round 0) to the final round, with one line per plotted run. Choose the player."
                , children: withPlots \c ->
                    [ D.label { className: "flex items-center gap-2 text-sm", children: [ D.text "Player", selectInput { value: chosen c, options: map _.player c.netWorth, onChange: setPicked } ] }
                    , maybe waiting (\nw -> overlayChart plotted (label c) baseline nw.chart) (find (\nw -> nw.player == chosen c) c.netWorth)
                    ]
                }
            , Layout.section
                { title: "Final standings"
                , purpose: "Each player's net worth after the final round: one group per player, with one bar per plotted run."
                , children: withPlots \c -> [ groupedBars plotted (label c) baseline "Player" c.standings ]
                }
            ]
        }
