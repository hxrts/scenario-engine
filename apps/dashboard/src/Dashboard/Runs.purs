-- | Reads of the run list (`GET /api/runs`), pure. The list's rows are its table; its `runs` are the typed
-- | flags of the same runs (class, readable), read here and nowhere else. The server decides the values.
module Dashboard.Runs (ofClass, readableIds, latestReadable, flagsOf) where

import Prelude

import Views.Overview (RunFlags, RunList)
import Schema.Run (RunClass)
import Data.Array (filter, find, reverse, zip)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe)

-- | The runs of one class, simulated or interactive.
ofClass :: RunClass -> RunList -> RunList
ofClass c t = t { rows = map snd kept, runs = map fst kept }
  where
  kept = filter (\x -> (fst x).runClass == c) (zip t.runs t.rows)

-- | The flags of a run in the list.
flagsOf :: RunList -> String -> Maybe RunFlags
flagsOf t id = find (\f -> f.id == id) t.runs

-- | The ids of the runs the server can read, in list order: the choices the Compare page offers.
readableIds :: RunList -> Array String
readableIds t = map _.id (filter _.readable t.runs)

-- | The most recent run (the list is in creation order) that can be read.
latestReadable :: RunList -> Maybe String
latestReadable t = map _.id (find _.readable (reverse t.runs))
