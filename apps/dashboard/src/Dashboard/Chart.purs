-- | Pure helpers behind the charts, kept free of React so they can be tested directly. Each point
-- | is plotted at full precision (`Q.toDisplayNumber`); its axis ticks and tooltips show the value
-- | under its display kind (docs/02 §9.1), which the row carries beside the plotted number.
module Dashboard.Chart (Series, plotValue, chartRows, barRows, shownKey, tooltipFormatter, colourOf) where

import Prelude

import Core.Num (Kind)
import Core.Num as Q
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Core as J
import Data.Array (concatMap, cons, elemIndex, filter, index, mapWithIndex, nub, sort)
import Data.Function.Uncurried (Fn5, mkFn5)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Views.Common (Bars)

type Series = { name :: String, points :: Array { round :: Int, value :: String } }

-- | A value as a plottable number: parsed exactly by the one decimal parser, then converted at the
-- | display boundary (`Q.toDisplayNumber`). Anything unparseable is Nothing: the point is left out
-- | of the plot, so it never appears as a zero.
plotValue :: String -> Maybe Number
plotValue s = Q.toDisplayNumber <$> Q.parse s

-- | The row key that holds a series' value as displayed, beside its plotted number.
shownKey :: String -> String
shownKey name = name <> " (shown)"

-- | One row per round, keyed by series name, with `round` as a number. The plotted value is a
-- | number; the `shownKey` entry is the same value under the chart's display kind. A series with no
-- | plottable value for a round has no key in that row, which the chart draws as a gap.
chartRows :: Kind -> Array Series -> Array (Object Json)
chartRows kind series = map row rounds
  where
  rounds = sort (nub (concatMap (map _.round <<< _.points) series))
  row r = Object.fromFoldable (cons (Tuple "round" (fromNumber (toNumber r))) (concatMap (plotted r) series))
  plotted r s = concatMap (\p -> maybe [] (\q -> [ Tuple s.name (fromNumber (Q.toDisplayNumber q)), Tuple (shownKey s.name) (fromString (Q.display kind q)) ]) (Q.parse p.value)) (filter (\p -> p.round == r) s.points)

-- | One row per category, keyed by series name, with the category as its `category` label. The
-- | bar's number is plotted; its `shownKey` entry is the value under the bars' display kind. A value
-- | that does not parse has no key in its row, so its bar is a gap.
barRows :: Bars -> Array (Object Json)
barRows b = mapWithIndex row b.categories
  where
  row i cat = Object.fromFoldable (cons (Tuple "category" (fromString cat)) (concatMap (bar i) b.series))
  bar i s = maybe [] (\q -> [ Tuple s.name (fromNumber (Q.toDisplayNumber q)), Tuple (shownKey s.name) (fromString (Q.display b.kind q)) ]) (index s.values i >>= Q.parse)

-- | The tooltip's value for a series at a round, read from the row it hovers over.
tooltipText :: String -> Object Json -> String
tooltipText name row = fromMaybe "" (Object.lookup (shownKey name) row >>= J.toString)

-- | The Recharts tooltip formatter, called as (value, name, item, index, payload): the displayed
-- | value replaces the plotted number.
tooltipFormatter :: Fn5 Json String Json Int (Object Json) String
tooltipFormatter = mkFn5 \_ name _ _ payload -> tooltipText name payload

-- | The colour of a run: its position among the compared runs (the route's ids), so a run keeps its
-- | colour in every plot and in the choice list. The theme has five chart colours, which repeat after
-- | the fifth run. Every colour is a theme variable, so both themes use the same plots.
colourOf :: Array String -> String -> String
colourOf ids id = "var(--chart-" <> show ((fromMaybe 0 (elemIndex id ids) `mod` 5) + 1) <> ")"
