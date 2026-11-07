-- | The colours and labels of the plots and their legends, in one place. A metric chart's series get a
-- | colour by position; the compared runs get a theme colour by their position among the ids, so a run
-- | keeps its colour in every plot and in the choice list. The theme has five chart colours, which repeat
-- | after the fifth run; each is a theme variable, so both themes use the same plots.
module Dashboard.Palette (seriesColour, runColour, runSwatch, runLabel) where

import Prelude

import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), fromMaybe)

-- | The colour of the i-th metric series: a distinct hue, spaced by the golden angle so neighbours
-- | differ. Lightness and saturation are fixed so every hue reads on both themes.
seriesColour :: Int -> String
seriesColour i = "hsl(" <> show ((i * 137) `mod` 360) <> " 70% 58%)"

-- | The position of a run among the compared ids, folded onto the five theme colours.
slot :: Array String -> String -> Int
slot ids id = fromMaybe 0 (elemIndex id ids) `mod` 5

-- | A run's plot colour: a theme chart variable (`--chart-1` to `--chart-5`).
runColour :: Array String -> String -> String
runColour ids id = "var(--chart-" <> show (slot ids id + 1) <> ")"

-- | A run's swatch class: the same theme colour, named in full so Tailwind finds each class.
runSwatch :: Array String -> String -> String
runSwatch ids id = case slot ids id of
  0 -> "bg-[var(--chart-1)]"
  1 -> "bg-[var(--chart-2)]"
  2 -> "bg-[var(--chart-3)]"
  3 -> "bg-[var(--chart-4)]"
  _ -> "bg-[var(--chart-5)]"

-- | A run's legend label: its id, and the word `baseline` for the first run (the baseline).
runLabel :: Array String -> String -> String
runLabel ids id = if elemIndex id ids == Just 0 then id <> " (baseline)" else id
