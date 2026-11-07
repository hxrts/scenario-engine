-- | Pure helpers behind the charts, kept free of React so they can be tested directly. Each point
-- | is plotted at full precision (`Q.toDisplayNumber`); its axis ticks and tooltips show the value
-- | under its display kind (docs/02 §9.1), which the row carries beside the plotted number.
module Dashboard.Chart (Series, pair, chartRows, barRows, shownKey, tooltipFormatter) where

import Prelude

import Core.Num (Kind, Q)
import Core.Num as Q
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Core as J
import Data.Array (concatMap, cons, filter, index, mapWithIndex, nub, sort)
import Data.Function.Uncurried (Fn5, mkFn5)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Views.Common (Bars)

type Series = { name :: String, points :: Array { round :: Int, value :: String } }

-- | The row key that holds a series' value as displayed, beside its plotted number.
shownKey :: String -> String
shownKey name = name <> " (shown)"

-- | One plotted value as its row entries: the number under the series' name, and the value under its
-- | display kind under `shownKey`. Every chart's rows are built from this one pair.
pair :: Kind -> String -> Q -> Array (Tuple String Json)
pair kind name q =
  [ Tuple name (fromNumber (Q.toDisplayNumber q))
  , Tuple (shownKey name) (fromString (Q.display kind q))
  ]

-- | One row per round, keyed by series name, with `round` as a number. A series with no plottable
-- | value for a round has no key in that row, which the chart draws as a gap.
chartRows :: Kind -> Array Series -> Array (Object Json)
chartRows kind series = map row rounds
  where
  rounds = sort (nub (concatMap (map _.round <<< _.points) series))
  row r = Object.fromFoldable (cons (Tuple "round" (fromNumber (toNumber r))) (concatMap (plotted r) series))
  plotted r s = concatMap (\p -> maybe [] (pair kind s.name) (Q.parse p.value)) (filter (\p -> p.round == r) s.points)

-- | One row per category, keyed by series name, with the category as its `category` label. A value
-- | that does not parse has no key in its row, so its bar is a gap.
barRows :: Bars -> Array (Object Json)
barRows b = mapWithIndex row b.categories
  where
  row i cat = Object.fromFoldable (cons (Tuple "category" (fromString cat)) (concatMap (bar i) b.series))
  bar i s = maybe [] (pair b.kind s.name) (index s.values i >>= Q.parse)

-- | The tooltip's value for a series at a round, read from the row it hovers over.
tooltipText :: String -> Object Json -> String
tooltipText name row = fromMaybe "" (Object.lookup (shownKey name) row >>= J.toString)

-- | The Recharts tooltip formatter, called as (value, name, item, index, payload): the displayed
-- | value replaces the plotted number.
tooltipFormatter :: Fn5 Json String Json Int (Object Json) String
tooltipFormatter = mkFn5 \_ name _ _ payload -> tooltipText name payload
