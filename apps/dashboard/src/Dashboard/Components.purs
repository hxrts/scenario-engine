-- | The generic views (tables, the field list, metric charts) and the fetching helper. They render the
-- | analysis view models as given; no game logic and no formatting here. Pages place these in Layout panels,
-- | which carry each view's title and purpose.
module Dashboard.Components
  ( stale
  , tableOf
  , diffTable
  , metricChart
  , stackedBars
  , overlayChart
  , groupedBars
  , placeholderFrame
  , fetched
  , status
  , errorBlock
  , labelled
  , textInput
  , statistic
  , Render
  , as
  , StatVariant(..)
  ) where

import Prelude

import Core.Num (displayTick)
import Dashboard.Api (useLoad)
import Dashboard.Chart (barRows, chartRows, tooltipFormatter)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Palette (runColour, runLabel, seriesColour)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
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

type Render = Json -> JSX

-- | A statistic's name and its one-line definition, shown above the view that holds it.
-- | `Figure` is a key figure: its name, the question it answers, its definition and its formula.
data StatVariant = Figure | Definition

statistic :: StatVariant -> Statistic -> JSX
statistic variant s = case variant of
  Figure -> D.div
    { className: "flex min-w-0 flex-col gap-1 text-sm"
    , children:
        [ D.p { className: "font-medium", children: [ D.text s.name ] }
        , D.p { className: "text-muted-foreground", children: [ D.text (s.question <> " " <> s.definition) ] }
        , D.p { className: "text-muted-foreground", children: [ D.text ("Formula: " <> s.formula) ] }
        ]
    }
  Definition -> D.p
    { className: "text-sm text-muted-foreground"
    , children: [ D.span { className: "font-medium text-foreground", children: [ D.text (s.name <> ": ") ] }, D.text s.definition ]
    }

-- | A view-model Table as a sortable table. Its cells are plain text.
tableOf :: DataTable -> Table -> JSX
tableOf grid t = D.div
  { className: "max-h-[28rem] overflow-auto"
  , children: [ linkedTable grid (\_ _ -> Nothing) t ]
  }

-- | A view-model Table with no links.
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

-- | A Table with the cells the view marked highlighted. A row is found by its cells, so the marks hold
-- | after the table is sorted.
diffTable :: DataTable -> Marked -> JSX
diffTable grid m = D.div
  { className: "overflow-x-auto"
  , children: [ grid { columns: m.table.columns, rows: m.table.rows, link: \_ _ -> Nothing, mark, onRow: Nothing, activeRow: \_ -> false } ]
  }
  where
  marks = zip m.table.rows m.marked
  mark row i = fromMaybe false (find (\(Tuple r _) -> r == row) marks >>= \(Tuple _ flags) -> index flags i)

-- | A Chart as a line per series, by round. Values that do not parse are not plotted (a gap).
metricChart :: Chart -> JSX
metricChart c = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ Layout.caption (c.title <> " (" <> c.unit <> ")")
      , element UI.chartContainer
          { config: Object.fromFoldable (mapWithIndex (\i s -> Tuple s.name { label: s.name, color: seriesColour i }) c.series)
          , className: "h-96 w-full"
          , children:
              [ element UI.lineChart
                  { data: chartRows c.kind c.series
                  , children:
                      [ element UI.cartesianGrid { vertical: false }
                      , element UI.xAxis { dataKey: "round" }
                      , element UI.yAxis { domain: [ "auto", "auto" ], tickFormatter: unsafeCoerce (displayTick c.kind) }
                      , element UI.chartTooltip { content: element UI.chartTooltipContent { formatter: tooltipFormatter } }
                      , element UI.chartLegend { content: element UI.chartLegendContent {} }
                      ] <> mapWithIndex (\i s -> element UI.line { key: s.name, dataKey: s.name, stroke: seriesColour i, strokeWidth: 2, dot: false, isAnimationActive: false }) c.series
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
      [ Layout.caption c.title
      , Layout.purpose c.purpose
      , element UI.chartContainer
          { config: Object.fromFoldable (mapWithIndex (\i k -> Tuple k.key { label: k.label, color: seriesColour i }) c.kinds)
          , className: "h-96 w-full"
          , children:
              [ element UI.barChart
                  { data: map bar c.bars
                  , children:
                      [ element UI.cartesianGrid { vertical: false }
                      , element UI.xAxis { dataKey: "player" }
                      , element UI.yAxis { allowDecimals: false }
                      , element UI.chartTooltip { content: element UI.chartTooltipContent {} }
                      , element UI.chartLegend { content: element UI.chartLegendContent {} }
                      ] <> mapWithIndex (\i k -> element UI.bar { key: k.key, dataKey: k.key, stackId: "actions", fill: seriesColour i, isAnimationActive: false }) c.kinds
                  }
              ]
          }
      ]
  }
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

-- | Runs on the same axes: one line per run, each in its run's colour and named in the legend. The
-- | baseline (the first run) is drawn thicker. The axes name the round and the unit.
overlayChart :: Array String -> Chart -> JSX
overlayChart ids c = plotFrame c.title $ element UI.chartContainer
  { config: Object.fromFoldable (map (\s -> Tuple s.name { label: runLabel ids s.name, color: runColour ids s.name }) c.series)
  , className: "h-full w-full"
  , children:
      [ element UI.lineChart
          { data: chartRows c.kind c.series
          , children:
              [ element UI.cartesianGrid { vertical: false }
              , element UI.xAxis { dataKey: "round", label: { value: "Round", position: "insideBottom", offset: -4 } }
              , element UI.yAxis { domain: [ "auto", "auto" ], tickFormatter: unsafeCoerce (displayTick c.kind), label: { value: c.unit, angle: -90, position: "insideLeft" } }
              , element UI.chartTooltip { content: element UI.chartTooltipContent { formatter: tooltipFormatter } }
              , element UI.chartLegend { content: element UI.chartLegendContent {} }
              ]
                <> map (\s -> element UI.line { key: s.name, dataKey: s.name, stroke: runColour ids s.name, strokeWidth: if index ids 0 == Just s.name then 3 else 2, dot: false, isAnimationActive: false }) c.series
          }
      ]
  }

-- | Grouped bars: one group per category, one bar per run in the run's colour, named in the legend.
-- | `xLabel` names the categories (a player, or a measure) on the horizontal axis.
groupedBars :: Array String -> String -> Bars -> JSX
groupedBars ids xLabel b = plotFrame b.title $ element UI.chartContainer
  { config: Object.fromFoldable (map (\s -> Tuple s.name { label: runLabel ids s.name, color: runColour ids s.name }) b.series)
  , className: "h-full w-full"
  , children:
      [ element UI.groupedBarChart
          { data: barRows b
          , children:
              [ element UI.cartesianGrid { vertical: false }
              , element UI.xAxis { dataKey: "category", label: { value: xLabel, position: "insideBottom", offset: -4 } }
              , element UI.yAxis { tickFormatter: unsafeCoerce (displayTick b.kind), label: { value: b.unit, angle: -90, position: "insideLeft" } }
              , element UI.chartTooltip { content: element UI.chartTooltipContent { formatter: tooltipFormatter } }
              , element UI.chartLegend { content: element UI.chartLegendContent {} }
              ]
                <> map (\s -> element UI.groupedBar { key: s.name, dataKey: s.name, fill: runColour ids s.name, isAnimationActive: false }) b.series
          }
      ]
  }

-- | Decode a view model and render it, or show the decoding error.
as :: forall a. DecodeJson a => (a -> JSX) -> Render
as f j = case decodeJson j of
  Right a -> f a
  Left e -> D.pre_ [ D.text (printJsonDecodeError e) ]

-- | Fetch a view (`load`, keyed by `key`) and render it. The last loaded view stays on screen, dimmed,
-- | while a new one loads.
fetched :: Component { key :: String, load :: Aff (Either String Json), render :: Render }
fetched = component "Fetched" \p -> React.do
  loaded <- useLoad p.key p.load
  pure $ stale loaded.busy case loaded.value of
    Just (Right j) -> p.render j
    Just (Left e) -> status (Just e)
    Nothing -> status Nothing

-- | Dims content that is the previous selection while its replacement loads. The wrapper is always
-- | present, so toggling it never remounts the content (a remount would detach it for a frame).
stale :: Boolean -> JSX -> JSX
stale busy child = D.div { className: if busy then "opacity-60 transition-opacity" else "transition-opacity", children: [ child ] }
