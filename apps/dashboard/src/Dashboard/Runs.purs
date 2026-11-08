-- | Reads of the run list (`GET /api/runs`), pure. The list's rows are its table; its `runs` are the typed
-- | flags of the same runs (archived, readable), read here and nowhere else. The server decides the values.
module Dashboard.Runs (shownRuns, readableIds, latestReadable, flagsOf) where

import Prelude

import Views.Overview (RunFlags, RunList)
import Data.Array (filter, find, reverse, zip)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))

-- | The list without its archived runs, unless `archived` asks for them too.
shownRuns :: Boolean -> RunList -> RunList
shownRuns archived t
  | archived = t
  | otherwise = t { rows = map snd kept, runs = map fst kept }
      where
      kept = filter (not _.archived <<< fst) (zip t.runs t.rows)

-- | The flags of a run in the list.
flagsOf :: RunList -> String -> Maybe RunFlags
flagsOf t id = find (\f -> f.id == id) t.runs

-- | The ids of the runs the server can read, in list order: the choices the Compare page offers.
readableIds :: RunList -> Array String
readableIds t = map _.id (filter _.readable t.runs)

-- | The most recent run (the list is in creation order) that can be read.
latestReadable :: RunList -> Maybe String
latestReadable t = map _.id (find _.readable (reverse t.runs))
