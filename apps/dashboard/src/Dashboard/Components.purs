-- | The generic views (tables, the field list, metric charts) and the fetching helper. They render the
-- | harness view models as given; no game logic here. Pages place these in Layout panels, which carry
-- | each view's title and purpose.
module Dashboard.Components (stale, tableOf, linkedTable, diffTable, metricChart, stackedBars, fetched, status, definition, Render, as, card) where

import Prelude

import Dashboard.Api (useApi)
import Dashboard.Chart (chartRows)
import Dashboard.DataTable (DataTable)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Array (cons, index, mapWithIndex)
import Data.Int (toNumber)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Harness.Views.Actions (ActionsChart)
import Harness.Views.Common (Chart, Statistic, Table)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, element, useEffect, useState', (/\))
import React.Basic.Hooks as React

type Render = Json -> JSX

-- | A bordered card with a title, for the few places that are not a Layout panel.
card :: String -> Array JSX -> JSX
card title body = element UI.card
  { children: [ element UI.cardHeader { children: [ element UI.cardTitle { children: [ D.text title ] } ] }, element UI.cardContent { children: body } ] }

-- | A statistic's name and its one-line definition, shown above the view that holds it.
definition :: Statistic -> JSX
definition s = D.p
  { className: "text-sm text-muted-foreground"
  , children: [ D.span { className: "font-medium text-foreground", children: [ D.text (s.name <> ": ") ] }, D.text s.definition ]
  }

-- | A view-model Table as a sortable table. Its cells are plain text.
tableOf :: DataTable -> Table -> JSX
tableOf grid t = D.div
  { className: "max-h-[28rem] overflow-auto"
  , children: [ linkedTable grid (\_ _ -> Nothing) t ]
  }

-- | A view-model Table whose cells link where `link` gives a target (by column index and row).
linkedTable :: DataTable -> (Int -> Array String -> Maybe String) -> Table -> JSX
linkedTable grid link t = D.div
  { className: "overflow-x-auto"
  , children: [ grid { columns: t.columns, rows: t.rows, link, mark: \_ _ -> false, onRow: Nothing, activeRow: \_ -> false } ]
  }

-- | A fetch in progress, or its error.
status :: Maybe String -> JSX
status = case _ of
  Nothing -> D.p { className: "text-muted-foreground", children: [ D.text "Loading…" ] }
  Just e -> D.pre { className: "text-destructive", children: [ D.text e ] }

-- | A Table whose value cells are highlighted where they differ from the baseline column. Columns
-- | before `baseline` are labels (not compared); the baseline is the reference value for the row.
diffTable :: DataTable -> { baseline :: Int } -> Table -> JSX
diffTable grid b t = D.div
  { className: "overflow-x-auto"
  , children: [ grid { columns: t.columns, rows: t.rows, link: \_ _ -> Nothing, mark, onRow: Nothing, activeRow: \_ -> false } ]
  }
  where
  mark row i = i > b.baseline && index row i /= index row b.baseline

-- | A Chart as a line per series, by round. Values that do not parse are not plotted (a gap).
metricChart :: Chart -> JSX
metricChart c = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.p { className: "text-sm font-medium", children: [ D.text (c.title <> " (" <> c.unit <> ")") ] }
      , element UI.chartContainer
          { config: Object.fromFoldable (mapWithIndex (\i s -> Tuple s.name { label: s.name, color: "var(--chart-" <> show (i `mod` 5 + 1) <> ")" }) c.series)
          , className: "h-64 w-full"
          , children:
              [ element UI.lineChart
                  { data: chartRows c.series
                  , children:
                      [ element UI.cartesianGrid { vertical: false }
                      , element UI.xAxis { dataKey: "round" }
                      , element UI.yAxis { domain: [ "auto", "auto" ] }
                      , element UI.chartTooltip { content: element UI.chartTooltipContent {} }
                      , element UI.chartLegend { content: element UI.chartLegendContent {} }
                      ] <> map (\s -> element UI.line { key: s.name, dataKey: s.name, stroke: "var(--color-" <> s.name <> ")", dot: false, isAnimationActive: false }) c.series
                  }
              ]
          }
      ]
  }

-- | A stacked bar chart: one bar per player, stacked by kind, with a legend of the kinds' labels. The
-- | chart's purpose line is shown above it; the counts are the view model's, mapped onto the bars here.
stackedBars :: ActionsChart -> JSX
stackedBars c = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.p { className: "text-sm font-medium", children: [ D.text c.title ] }
      , D.p { className: "text-sm text-muted-foreground", children: [ D.text c.purpose ] }
      , element UI.chartContainer
          { config: Object.fromFoldable (mapWithIndex (\i k -> Tuple k.key { label: k.label, color: "var(--chart-" <> show (i `mod` 5 + 1) <> ")" }) c.kinds)
          , className: "h-80 w-full"
          , children:
              [ element UI.barChart
                  { data: map bar c.bars
                  , children:
                      [ element UI.cartesianGrid { vertical: false }
                      , element UI.xAxis { dataKey: "player" }
                      , element UI.yAxis { allowDecimals: false }
                      , element UI.chartTooltip { content: element UI.chartTooltipContent {} }
                      , element UI.chartLegend { content: element UI.chartLegendContent {} }
                      ] <> map (\k -> element UI.bar { key: k.key, dataKey: k.key, stackId: "actions", fill: "var(--color-" <> k.key <> ")", isAnimationActive: false }) c.kinds
                  }
              ]
          }
      ]
  }
  where
  bar b = Object.fromFoldable (cons (Tuple "player" (fromString b.player)) (map (\s -> Tuple s.key (fromNumber (toNumber s.count))) b.counts))

-- | Decode a view model and render it, or show the decoding error.
as :: forall a. DecodeJson a => (a -> JSX) -> Render
as f j = case decodeJson j of
  Right a -> f a
  Left e -> D.pre_ [ D.text (printJsonDecodeError e) ]

-- | Fetch a path and render it.
fetched :: Component { path :: String, render :: Render }
fetched = component "Fetched" \p -> React.do
  res <- useApi unit p.path
  -- the last loaded value stays on screen, dimmed, while a new one loads: nothing collapses or jumps
  shown /\ setShown <- useState' (Nothing :: Maybe (Either String Json))
  useEffect res do
    case res of
      Just r -> setShown (Just r)
      Nothing -> pure unit
    pure (pure unit)
  let busy = isNothing res
  pure $ stale busy case (if busy then shown else res) of
    Just (Right j) -> p.render j
    Just (Left e) -> status (Just e)
    Nothing -> status Nothing

-- | Dims content that is the previous selection while its replacement loads.
stale :: Boolean -> JSX -> JSX
stale busy child = if busy then D.div { className: "opacity-60 transition-opacity", children: [ child ] } else child
