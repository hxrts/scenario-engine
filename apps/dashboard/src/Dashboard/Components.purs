-- | The four generic components (RecordTable, MetricChart, RunPicker, DiffTable) and the
-- | fetching panel. They render the harness view models as given; no game logic here.
module Dashboard.Components (recordTable, plainTable, status, diffTable, metricChart, runPicker, panel, Render, as, card) where

import Prelude

import Dashboard.Api (useApi)
import Dashboard.DataTable (DataTable)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Array (cons, elem, filter, index, mapWithIndex, nub, sort, concatMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Foreign.Object as Object
import Harness.Views (Chart, Table)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, element)
import React.Basic.Hooks as React

type Render = Json -> JSX

card :: String -> Array JSX -> JSX
card title body = element UI.card
  { children: [ element UI.cardHeader { children: [ element UI.cardTitle { children: [ D.text title ] } ] }, element UI.cardContent { children: body } ] }

-- | A view-model Table; `link` turns a row into a link on its first cell.
recordTable :: DataTable -> (Array String -> Maybe String) -> Table -> JSX
recordTable grid link t = cells grid (\_ _ -> false) link t

-- | A Table with no row links.
plainTable :: DataTable -> Table -> JSX
plainTable grid = recordTable grid (const Nothing)

-- | A fetch in progress, or its error.
status :: Maybe String -> JSX
status = case _ of
  Nothing -> D.p { className: "text-muted-foreground", children: [ D.text "Loading…" ] }
  Just e -> D.pre { className: "text-destructive", children: [ D.text e ] }

-- | A Table whose value cells are highlighted where they differ from the first value column.
diffTable :: DataTable -> Table -> JSX
diffTable grid = cells grid differs (const Nothing)
  where
  differs row i = i > 1 && index row i /= index row 1

cells :: DataTable -> (Array String -> Int -> Boolean) -> (Array String -> Maybe String) -> Table -> JSX
cells grid mark link t = card t.title
  [ grid { columns: t.columns, rows: t.rows, link, mark } ]

-- | A Chart as a line per series, by round.
metricChart :: Chart -> JSX
metricChart c = card (c.title <> " (" <> c.unit <> ")")
  [ element UI.chartContainer
      { config: Object.fromFoldable (mapWithIndex (\i s -> Tuple s.name { label: s.name, color: "var(--chart-" <> show (i `mod` 5 + 1) <> ")" }) c.series)
      , className: "h-64 w-full"
      , children:
          [ element UI.lineChart
              { data: rows
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
  where
  rounds = sort (nub (concatMap (map _.round <<< _.points) c.series))
  rows = map (\r -> Object.fromFoldable (cons (Tuple "round" (show r)) (concatMap (\s -> map (\p -> Tuple s.name p.value) (filter (\p -> p.round == r) s.points)) c.series))) rounds
    <#> map (\v -> fromMaybe 0.0 (Number.fromString v))

-- | Select runs by id from the run list (first column).
runPicker :: { runs :: Table, selected :: Array String, onChange :: Array String -> Effect Unit } -> JSX
runPicker p = D.div
  { className: "flex flex-wrap gap-4"
  , children: p.runs.rows <#> \r ->
      let
        id = fromMaybe "" (index r 0)
      in
        D.label
          { key: id
          , className: "flex items-center gap-2 text-sm"
          , children:
              [ element UI.checkbox { checked: id `elem` p.selected, onCheckedChange: mkEffectFn1 \on -> p.onChange (if on then p.selected <> [ id ] else filter (_ /= id) p.selected) }
              , D.text id
              ]
          }
  }

-- | Decode a view model and render it, or show the decoding error.
as :: forall a. DecodeJson a => (a -> JSX) -> Render
as f j = case decodeJson j of
  Right a -> f a
  Left e -> D.pre_ [ D.text (printJsonDecodeError e) ]

-- | Fetch a path and render it.
panel :: Component { path :: String, render :: Render }
panel = component "Panel" \p -> React.do
  res <- useApi unit p.path
  pure case res of
    Just (Right j) -> p.render j
    Just (Left e) -> status (Just e)
    Nothing -> status Nothing
