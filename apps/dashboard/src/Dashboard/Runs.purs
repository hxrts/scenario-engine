-- | Reads of the run list's Table (`/api/runs`) by column name: an archived flag, the source run
-- | a run was derived from, and the runs derived from a given run. Pure; the server decides the
-- | values, and this only reads the columns it returns.
module Dashboard.Runs (RunInfo, runInfos, runInfo, mergeRuns, derivedFrom, sourceRow) where

import Prelude

import Data.Array (elem, filter, findIndex, find, index)
import Data.Maybe (Maybe(..), fromMaybe)
import Harness.Views (Table)

type RunInfo = { id :: String, archived :: Boolean, source :: String }

-- | The value of a named column in a row, or "" when the table has no such column.
cell :: Table -> String -> Array String -> String
cell t name row = fromMaybe "" (findIndex (_ == name) t.columns >>= \i -> index row i)

infoOf :: Table -> Array String -> RunInfo
infoOf t row = { id: cell t "Run" row, archived: cell t "Archived" row == "yes", source: cell t "Source" row }

runInfos :: Table -> Array RunInfo
runInfos t = map (infoOf t) t.rows

runInfo :: Table -> String -> Maybe RunInfo
runInfo t id = find (\r -> r.id == id) (runInfos t)

-- | The rows of the first table, then the rows of the second whose run is not already listed. Both
-- | are the run list, so they share columns. The first table's title and columns are kept.
mergeRuns :: Table -> Table -> Table
mergeRuns a b = a { rows = a.rows <> filter (\r -> not (elem (cell b "Run" r) listed)) b.rows }
  where
  listed = map (cell a "Run") a.rows

-- | The Source cell of a row: the id of the run it was derived from, or Nothing for an original run.
sourceRow :: Table -> Array String -> Maybe String
sourceRow t row = case cell t "Source" row of
  "" -> Nothing
  s -> Just s

-- | The ids of the runs whose Source is this run, in list order.
derivedFrom :: Table -> String -> Array String
derivedFrom t id = map _.id (filter (\r -> r.source == id) (runInfos t))
