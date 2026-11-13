-- | The colours and labels of the plots and their legends, in one place. A metric chart's series get a
-- | colour by position; the compared runs get a colour by their position among the ids, so a run keeps its colour in
-- | every plot and in the choice list: the theme's five chart colours first, then distinct hues, so any number of runs
-- | can be told apart.
module Dashboard.Palette (seriesColour, runColour, runLabel) where

import Prelude

import Data.Array (elemIndex)
import Data.Maybe (fromMaybe)

-- | The colour of the i-th metric series: a distinct hue, spaced by the golden angle so neighbours
-- | differ. Lightness and saturation are fixed so every hue reads on both themes.
seriesColour :: Int -> String
seriesColour i = "hsl(" <> show ((i * 137) `mod` 360) <> " 70% 58%)"

-- | The position of a run among the compared ids.
slot :: Array String -> String -> Int
slot ids id = fromMaybe 0 (elemIndex id ids)

-- | A run's plot colour: a theme chart variable (`--chart-1` to `--chart-5`) for the first five runs, then a
-- | distinct hue for each run after them.
runColour :: Array String -> String -> String
runColour ids id = let i = slot ids id in if i < 5 then "var(--chart-" <> show (i + 1) <> ")" else seriesColour (i + 3)

-- | A run's legend label: its id, and the word `baseline` for the comparison's baseline run.
runLabel :: String -> String -> String
runLabel baseline id = if id == baseline then id <> " (baseline)" else id
