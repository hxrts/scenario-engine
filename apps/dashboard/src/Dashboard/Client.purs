-- | The typed client of the read and write API (docs/06_simulator.md §8): one function per request
-- | the dashboard makes, and the only place the endpoint paths are written (with `Dashboard.Api`, which
-- | holds the request functions). Views the page renders by kind come back as JSON, decoded where
-- | they are shown.
module Dashboard.Client
  ( CompareView
  , History
  , runs
  , launch
  , scenario
  , glossary
  , overview
  , entities
  , entity
  , runView
  , compare
  , agency
  , archive
  , resume
  , delete
  ) where

import Prelude

import Dashboard.Api (command, encode, get, post, remove)
import Dashboard.Launch (LaunchRequest, Scenario)
import Dashboard.RunForms (resumeBody)
import Analysis.Views.Common (Bars, Chart, Table)
import Analysis.Views.Entity (EntityList)
import Analysis.Views.Glossary (Glossary)
import Analysis.Views.Overview (RunList, RunOverview)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.String (joinWith)
import Effect.Aff (Aff)

-- | The comparison view model (`GET /api/compare`). `baseline` names the first run's column, which the
-- | difference tables mark their changes against.
type CompareView =
  { markets :: Array Chart
  , netWorth :: Array { player :: String, chart :: Chart }
  , standings :: Bars
  , balance :: { spread :: Bars, gini :: Bars }
  , keyValues :: Table
  , final :: Table
  , diff :: Table
  , config :: Table
  , compatibility :: Table
  , seeds :: Table
  , baseline :: String
  }

-- | An Entity's history (`GET /api/runs/:id/entity/:entity`): its holdings and net worth as charts.
type History = { chart :: Chart, netWorth :: Chart }

-- | The run path: the run's id as one encoded segment.
runPath :: String -> String
runPath id = "/api/runs/" <> encode id

-- | The run list. `archived` adds the archived runs (every run, the default list without them).
runs :: Boolean -> Aff (Either String RunList)
runs archived = get (if archived then "/api/runs?archived=true" else "/api/runs")

-- | Launch a run (`POST /api/runs`); the reply names the new run.
launch :: LaunchRequest -> Aff (Either String { id :: String })
launch req = post "/api/runs" req

-- | The scenario the launch form runs, with its rounds, seed, default run name and presets.
scenario :: Aff (Either String Scenario)
scenario = get "/api/scenario"

-- | The glossary: the definitions and empty-value reasons the tables and the header show.
glossary :: Aff (Either String Glossary)
glossary = get "/api/glossary"

-- | A run's overview: its rounds, target and header facts.
overview :: String -> Aff (Either String RunOverview)
overview id = get (runPath id <> "/overview")

-- | A run's Entities, each flagged as a seat, and the default one.
entities :: String -> Aff (Either String EntityList)
entities id = get (runPath id <> "/entities")

-- | One Entity's history.
entity :: String -> String -> Aff (Either String History)
entity id e = get (runPath id <> "/entity/" <> encode e)

-- | One view of a run by its name (for example `markets`, or `balances?round=3`), as JSON.
runView :: String -> String -> Aff (Either String Json)
runView id view = get (runPath id <> "/" <> view)

-- | The comparison of the runs, the first being the baseline.
compare :: Array String -> Aff (Either String CompareView)
compare ids = get ("/api/compare?runs=" <> joinWith "," (map encode ids))

-- | The agency analysis against the fixed baseline, as JSON.
agency :: Aff (Either String Json)
agency = get "/api/agency"

-- | Archive a run, or unarchive it.
archive :: String -> Boolean -> Aff (Either String Json)
archive id archived = command (runPath id <> if archived then "/unarchive" else "/archive")

-- | Resume a run to a round count.
resume :: String -> Int -> Aff (Either String Json)
resume id rounds = post (runPath id <> "/resume") (resumeBody rounds)

-- | Delete a run; the request names the run it deletes.
delete :: String -> Aff (Either String Json)
delete id = remove (runPath id <> "?confirm=" <> encode id)
