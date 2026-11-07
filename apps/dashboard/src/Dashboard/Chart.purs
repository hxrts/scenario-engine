-- | Pure helpers behind the charts, kept free of React so they can be tested directly.
module Dashboard.Chart (Series, plotValue, chartRows, barRows, colourOf) where

import Prelude

import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Array (concatMap, cons, elemIndex, filter, index, mapWithIndex, nub, sort)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Core.Num as Q
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Views.Common (Bars)

type Series = { name :: String, points :: Array { round :: Int, value :: String } }

-- | A value as a plottable number: parsed exactly by the one decimal parser, then converted at the
-- | display boundary (`Q.toDisplayNumber`). Anything unparseable is Nothing: the point is left out
-- | of the plot, so it never appears as a zero.
plotValue :: String -> Maybe Number
plotValue s = Q.toDisplayNumber <$> Q.parse s

-- | One row per round, keyed by series name, with `round` as a number. A series with no
-- | plottable value for a round has no key in that row, which the chart draws as a gap.
chartRows :: Array Series -> Array (Object Number)
chartRows series = map row rounds
  where
  rounds = sort (nub (concatMap (map _.round <<< _.points) series))
  row r = Object.fromFoldable (cons (Tuple "round" (toNumber r)) (concatMap (plotted r) series))
  plotted r s = concatMap (\p -> maybe [] (\v -> [ Tuple s.name v ]) (plotValue p.value)) (filter (\p -> p.round == r) s.points)

-- | One row per category, keyed by series name, with the category as its `category` label. A value
-- | that does not parse has no key in its row, so its bar is a gap.
barRows :: Bars -> Array (Object Json)
barRows b = mapWithIndex row b.categories
  where
  row i cat = Object.fromFoldable (cons (Tuple "category" (fromString cat)) (concatMap (bar i) b.series))
  bar i s = maybe [] (\v -> [ Tuple s.name (fromNumber v) ]) (index s.values i >>= plotValue)

-- | The colour of a run: its position among the compared runs (the route's ids), so a run keeps its
-- | colour in every plot and in the choice list. The theme has five chart colours, which repeat after
-- | the fifth run. Every colour is a theme variable, so both themes use the same plots.
colourOf :: Array String -> String -> String
colourOf ids id = "var(--chart-" <> show ((fromMaybe 0 (elemIndex id ids) `mod` 5) + 1) <> ")"
