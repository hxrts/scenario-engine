-- | Pure helpers behind the metric charts, kept free of React so they can be tested directly.
-- | Each point is plotted at full precision (`Q.toDisplayNumber`); its axis ticks and tooltips show
-- | the value under its display kind (docs/02 §9.1), which the row carries beside the plotted number.
module Dashboard.Chart (Series, plotValue, chartRows, shownKey, tooltipFormatter) where

import Prelude

import Core.Num (Kind)
import Core.Num as Q
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Core as J
import Data.Array (concatMap, cons, filter, nub, sort)
import Data.Function.Uncurried (Fn5, mkFn5)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

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

-- | The tooltip's value for a series at a round, read from the row it hovers over.
tooltipText :: String -> Object Json -> String
tooltipText name row = fromMaybe "" (Object.lookup (shownKey name) row >>= J.toString)

-- | The Recharts tooltip formatter, called as (value, name, item, index, payload): the displayed
-- | value replaces the plotted number.
tooltipFormatter :: Fn5 Json String Json Int (Object Json) String
tooltipFormatter = mkFn5 \_ name _ _ payload -> tooltipText name payload
