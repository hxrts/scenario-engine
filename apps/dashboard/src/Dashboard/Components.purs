-- | The generic views (tables, the field list, metric charts) and the fetching helper. They render the
-- | analysis view models as given; no game logic and no formatting here. Pages place these in Layout panels,
-- | which carry each view's title and purpose.
module Dashboard.Components
  ( stale
  , loaded
  , selectInput
  , tableOf
  , diffTable
  , metricChart
  , stackedBars
  , overlayChart
  , groupedBars
  , placeholderFrame
  , fetched
  , status
  , note
  , errorBlock
  , labelled
  , textInput
  , statistic
  ) where

import Data.Argonaut.Core (Json, fromNumber, fromString)
import Prelude

import Core.Num (Kind, displayTick)
import Dashboard.Api (useLoad)
import Dashboard.Chart (barRows, chartRows, tooltipFormatter)
import Dashboard.DataTable (DataTable, defaults)
import Dashboard.Layout as Layout
import Dashboard.Palette (runColour, seriesColour)
import Dashboard.UI as UI
import Data.Array (cons, find, index, mapWithIndex, zip)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Views.Actions (ActionsChart)
import Views.Compare (Marked)
import Views.Common (Bars, Chart, Statistic, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, component, element)
import React.Basic.Hooks as React

-- | A key figure: its name, the question it answers, its definition and its formula.
statistic :: Statistic -> JSX
statistic s = D.div
  { className: "flex min-w-0 flex-col gap-1 text-sm"
  , children:
      [ D.p { className: "font-medium", children: [ D.text s.name ] }
      , D.p { className: "text-muted-foreground", children: [ D.text (s.question <> " " <> s.definition) ] }
      , D.p { className: "text-muted-foreground", children: [ D.text ("Formula: " <> s.formula) ] }
      ]
  }

-- | A view-model Table as a sortable table. Its cells are plain text.
tableOf :: DataTable -> Table -> JSX
tableOf grid t = D.div
  { className: "max-h-[28rem] overflow-auto"
  , children: [ D.div { className: "overflow-x-auto", children: [ grid (defaults t) ] } ]
  }

-- | A fetch in progress, or its error.
-- | A plain line of information.
note :: String -> JSX
note text = D.p { className: "text-muted-foreground", children: [ D.text text ] }

status :: Maybe String -> JSX
status = case _ of
  Nothing -> D.p { className: "text-muted-foreground", children: [ D.text "Loading…" ] }
  Just e -> D.pre { className: "text-destructive", children: [ D.text e ] }

-- | A loaded value's view; while it loads or when it failed, its status.
loaded :: forall a. Maybe (Either String a) -> (a -> JSX) -> JSX
loaded value f = case value of
  Just (Right a) -> f a
  Just (Left e) -> status (Just e)
  Nothing -> status Nothing

-- | A failure message, shown where a form or an action can fail; nothing when there is none.
errorBlock :: Maybe String -> JSX
errorBlock = maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] })

-- | A form field: its heading, then its control, in one label.
labelled :: JSX -> JSX -> JSX
labelled heading control = D.label { className: "flex flex-col gap-1 text-sm", children: [ heading, control ] }

-- | A text input bound to its value; `onChange` receives the new text.
textInput :: { value :: String, required :: Boolean, onChange :: String -> Effect Unit } -> JSX
textInput p = element UI.input
  { value: p.value
  , "aria-required": p.required
  , onChange: handler targetValue (\v -> p.onChange (fromMaybe "" v))
  }

-- | A select over its options; `onChange` receives the chosen value.
selectInput :: { value :: String, options :: Array String, onChange :: Maybe String -> Effect Unit } -> JSX
selectInput p = D.select
  { value: p.value
  , className: "app-select h-9 w-full rounded-md border border-input bg-transparent pl-3 pr-9 text-sm"
  , onChange: handler targetValue p.onChange
  , children: p.options <#> \o -> D.option { key: o, value: o, children: [ D.text o ] }
  }

-- | A Table with the cells the view marked highlighted. A row is found by its cells, so the marks hold
-- | after the table is sorted.
diffTable :: DataTable -> Marked -> JSX
diffTable grid m = D.div
  { className: "overflow-x-auto"
  , children: [ grid ((defaults m.table) { mark = mark }) ]
  }
  where
  marks = zip m.table.rows m.marked
  mark row i = fromMaybe false (find (\(Tuple r _) -> r == row) marks >>= \(Tuple _ flags) -> index flags i)

-- | A chart's pieces: the data, the axes and the tooltip, and one mark per series.
type ChartParts =
  { config :: Object.Object { label :: String, color :: String }
  , className :: String
  , rows :: Array (Object.Object Json)
  , xAxis :: JSX
  , yAxis :: JSX
  , tooltip :: JSX
  , marks :: Array JSX
  }

-- | A chart of one Recharts kind from its pieces: the frame every chart shares.
chartOf :: UI.C -> ChartParts -> JSX
chartOf kind p = element UI.chartContainer
  { config: p.config
  , className: p.className
  , children:
      [ element kind
          { data: p.rows
          , children:
              [ element UI.cartesianGrid { vertical: false }
              , p.xAxis
              , p.yAxis
              , p.tooltip
              , element UI.chartLegend { content: element UI.chartLegendContent {} }
              ] <> p.marks
          }
      ]
  }

-- | A line chart from its pieces.
lineChartOf :: ChartParts -> JSX
lineChartOf = chartOf UI.lineChart

-- | A bar chart from its pieces.
barChartOf :: ChartParts -> JSX
barChartOf = chartOf UI.barChart

-- | A chart's tooltip, showing each value as displayed.
valueTooltip :: JSX
valueTooltip = element UI.chartTooltip { content: element UI.chartTooltipContent { formatter: tooltipFormatter } }

-- | The y-axis under a display kind, with the unit named when there is one.
axisOf :: Kind -> Maybe String -> JSX
axisOf kind = case _ of
  Nothing -> element UI.yAxis { domain: [ "auto", "auto" ], tickFormatter: unsafeCoerce (displayTick kind) }
  Just u -> element UI.yAxis { domain: [ "auto", "auto" ], tickFormatter: unsafeCoerce (displayTick kind), label: { value: u, angle: -90, position: "insideLeft" } }

-- | A Chart as a line per series, by round. Values that do not parse are not plotted (a gap).
metricChart :: Chart -> JSX
metricChart c = Layout.captioned (c.title <> " (" <> c.unit <> ")")
  [ lineChartOf
      { config: Object.fromFoldable (mapWithIndex (\i s -> Tuple s.name { label: s.name, color: seriesColour i }) c.series)
      , className: "h-96 w-full"
      , rows: chartRows c.kind c.series
      , xAxis: element UI.xAxis { dataKey: "round" }
      , yAxis: axisOf c.kind Nothing
      , tooltip: valueTooltip
      , marks: mapWithIndex (\i s -> element UI.line { key: s.name, dataKey: s.name, stroke: seriesColour i, strokeWidth: 2, dot: false, isAnimationActive: false }) c.series
      }
  ]

-- | A stacked bar chart: one bar per player, stacked by kind, with a legend of the kinds' labels. The
-- | chart's purpose line is shown above it; the counts are the view model's, mapped onto the bars here.
stackedBars :: ActionsChart -> JSX
stackedBars c = Layout.captioned c.title
  [ Layout.purpose c.purpose
  , barChartOf
      { config: Object.fromFoldable (mapWithIndex (\i k -> Tuple k.key { label: k.label, color: seriesColour i }) c.kinds)
      , className: "h-96 w-full"
      , rows: map bar c.bars
      , xAxis: element UI.xAxis { dataKey: "player" }
      , yAxis: element UI.yAxis { allowDecimals: false }
      , tooltip: element UI.chartTooltip { content: element UI.chartTooltipContent {} }
      , marks: mapWithIndex (\i k -> element UI.bar { key: k.key, dataKey: k.key, stackId: "stacked", fill: seriesColour i, isAnimationActive: false }) c.kinds
      }
  ]
  where
  bar b = Object.fromFoldable (cons (Tuple "player" (fromString b.player)) (map (\s -> Tuple s.key (fromNumber (toNumber s.count))) b.counts))

-- | A plot's frame: a labelled figure of a fixed height, so the plot takes the same room whether or
-- | not its data has arrived, and the page does not move when it does.
plotFrame :: String -> JSX -> JSX
plotFrame title child = element UI.region
  { role: "figure"
  , "aria-label": title
  , className: "block h-[360px] w-full min-w-0"
  , children: [ child ]
  }

-- | The same frame, empty, with a message: what the plot will show once its data is loaded.
placeholderFrame :: String -> JSX
placeholderFrame message = element UI.region
  { role: "figure"
  , "aria-label": message
  , className: "flex h-[360px] w-full items-center justify-center text-sm text-muted-foreground"
  , children: [ D.text message ]
  }

-- | Runs on the same axes: one line per run, each in its run's colour and named in the legend by `label`. The
-- | baseline (`CompareView.baseline`) is drawn thicker. The axes name the round and the unit.
overlayChart :: Array String -> (String -> String) -> String -> Chart -> JSX
overlayChart ids label baseline c = plotFrame c.title $ lineChartOf
  { config: Object.fromFoldable (map (\s -> Tuple s.name { label: label s.name, color: runColour ids s.name }) c.series)
  , className: "h-full w-full"
  , rows: chartRows c.kind c.series
  , xAxis: element UI.xAxis { dataKey: "round", label: { value: "Round", position: "insideBottom", offset: -4 } }
  , yAxis: axisOf c.kind (Just c.unit)
  , tooltip: valueTooltip
  , marks: map (\s -> element UI.line { key: s.name, dataKey: s.name, stroke: runColour ids s.name, strokeWidth: if s.name == baseline then 3 else 2, dot: false, isAnimationActive: false }) c.series
  }

-- | Grouped bars: one group per category, one bar per run in the run's colour, named in the legend.
-- | `xLabel` names the categories (a player, or a measure) on the horizontal axis.
groupedBars :: Array String -> (String -> String) -> String -> Bars -> JSX
groupedBars ids label xLabel b = plotFrame b.title $ barChartOf
  { config: Object.fromFoldable (map (\s -> Tuple s.name { label: label s.name, color: runColour ids s.name }) b.series)
  , className: "h-full w-full"
  , rows: barRows b
  , xAxis: element UI.xAxis { dataKey: "category", label: { value: xLabel, position: "insideBottom", offset: -4 } }
  , yAxis: element UI.yAxis { tickFormatter: unsafeCoerce (displayTick b.kind), label: { value: b.unit, angle: -90, position: "insideLeft" } }
  , tooltip: valueTooltip
  , marks: map (\s -> element UI.bar { key: s.name, dataKey: s.name, fill: runColour ids s.name, isAnimationActive: false }) b.series
  }

-- | Fetch a view (`load`, keyed by `key`) and render it. The last loaded view stays on screen, dimmed,
-- | while a new one loads.
fetched :: Component { key :: String, load :: Aff (Either String JSX) }
fetched = component "Fetched" \p -> React.do
  view <- useLoad p.key p.load
  pure $ stale view.busy (loaded view.value identity)

-- | Dims content that is the previous selection while its replacement loads. The wrapper is always
-- | present, so toggling it never remounts the content (a remount would detach it for a frame).
stale :: Boolean -> JSX -> JSX
stale busy child = D.div { className: if busy then "opacity-60 transition-opacity" else "transition-opacity", children: [ child ] }
