-- | Pure helpers behind the metric charts, kept free of React so they can be tested directly.
module Dashboard.Chart (Series, plotValue, chartRows) where

import Prelude

import Data.Array (concatMap, cons, filter, nub, sort)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber)
import Data.Number (isFinite)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Series = { name :: String, points :: Array { round :: Int, value :: String } }

-- | A value as a plottable number. Anything unparseable (or not finite) is Nothing: the point is
-- | left out of the plot, so it never appears as a zero.
plotValue :: String -> Maybe Number
plotValue s = case Number.fromString s of
  Just n | isFinite n -> Just n
  _ -> Nothing

-- | One row per round, keyed by series name, with `round` as a number. A series with no
-- | plottable value for a round has no key in that row, which the chart draws as a gap.
chartRows :: Array Series -> Array (Object Number)
chartRows series = map row rounds
  where
  rounds = sort (nub (concatMap (map _.round <<< _.points) series))
  row r = Object.fromFoldable (cons (Tuple "round" (toNumber r)) (concatMap (plotted r) series))
  plotted r s = concatMap (\p -> maybe [] (\v -> [ Tuple s.name v ]) (plotValue p.value)) (filter (\p -> p.round == r) s.points)
