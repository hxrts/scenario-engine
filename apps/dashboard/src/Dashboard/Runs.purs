-- | Reads of the run list (`GET /api/runs`), pure. The list's rows are its table; its `runs` are the typed
-- | flags of the same runs (archived, readable), read here and nowhere else. The server decides the values.
module Dashboard.Runs (mergeRuns, shownRuns, readableIds, latestReadable, flagsOf) where

import Prelude

import Analysis.Views.Overview (RunFlags, RunList)
import Data.Array (elem, filter, find, index, reverse)
import Data.Maybe (Maybe(..))

-- | The run id of a row: its first column.
rowId :: Array String -> String
rowId row = case index row 0 of
  Just id -> id
  Nothing -> ""

-- | The rows and flags of the first list, then those of the second whose run is not already listed. Both
-- | are the run list, so they share columns. The first list's title and columns are kept.
mergeRuns :: RunList -> RunList -> RunList
mergeRuns a b =
  a
    { rows = a.rows <> filter (\r -> not (elem (rowId r) listed)) b.rows
    , runs = a.runs <> filter (\f -> not (elem f.id listed)) b.runs
    }
  where
  listed = map (\r -> rowId r) a.rows

-- | The list without its archived runs, unless `archived` asks for them too.
shownRuns :: Boolean -> RunList -> RunList
shownRuns archived t
  | archived = t
  | otherwise = t { rows = filter (\r -> elem (rowId r) kept) t.rows, runs = filter (\f -> not f.archived) t.runs }
      where
      kept = map _.id (filter (\f -> not f.archived) t.runs)

-- | The flags of a run in the list.
flagsOf :: RunList -> String -> Maybe RunFlags
flagsOf t id = find (\f -> f.id == id) t.runs

-- | The ids of the runs the server can read, in list order: the choices the Compare page offers.
readableIds :: RunList -> Array String
readableIds t = map _.id (filter _.readable t.runs)

-- | The most recent run (the list is in creation order) that can be read.
latestReadable :: RunList -> Maybe String
latestReadable t = map _.id (find _.readable (reverse t.runs))
