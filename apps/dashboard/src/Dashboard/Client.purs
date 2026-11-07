-- | The dashboard's operations (docs/06_simulator.md §8): one function per request the dashboard makes.
-- | Each sends an `API` operation through `Handlers.Api.Client` over the dashboard's fetch device and
-- | decodes the reply's JSON as the view it names. The paths are `Handlers.Api.Route`'s.
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

import Dashboard.Api (fetch)
import Dashboard.Launch (LaunchRequest, Scenario)
import Effects.Api (ApiRequest(..), call)
import Handlers.Api.Client (replyText, runApi)
import Schema.Decode.Common (readJson)
import Views.Common (Bars, Chart, Table)
import Views.Entity (EntityList)
import Views.Glossary (Glossary)
import Views.Overview (RunList, RunOverview)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

-- | The comparison view model (`GET /api/compare`). `baseline` names the first run's column, which the
-- | difference tables mark their changes against.
type CompareView =
  { markets :: Array Chart
  , netWorth :: Array { player :: String, chart :: Chart }
  , standings :: Bars
  , balance :: { spread :: Bars, gini :: Bars, unit :: String }
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

-- | One operation: its reply's body, decoded as the typed value it names.
typed :: forall a. DecodeJson a => ApiRequest -> Aff (Either String a)
typed req = (\reply -> replyText reply >>= readJson) <$> runApi fetch (call req)

-- | The run list. `archived` adds the archived runs (every run, the default list without them).
runs :: Boolean -> Aff (Either String RunList)
runs archived = typed (ListRuns { archived })

-- | Launch a run (`POST /api/runs`); the reply names the new run.
launch :: LaunchRequest -> Aff (Either String { id :: String })
launch = typed <<< LaunchRun

-- | The scenario the launch form runs, with its rounds, seed, default run name and presets.
scenario :: Aff (Either String Scenario)
scenario = typed Scenario

-- | The glossary: the definitions and empty-value reasons the tables and the header show.
glossary :: Aff (Either String Glossary)
glossary = typed Glossary

-- | A run's overview: its rounds, target and header facts.
overview :: String -> Aff (Either String RunOverview)
overview id = typed (RunView { id, view: "overview", round: Nothing })

-- | A run's Entities, each flagged as a seat, and the default one.
entities :: String -> Aff (Either String EntityList)
entities = typed <<< Entities

-- | One Entity's history.
entity :: String -> String -> Aff (Either String History)
entity id e = typed (Entity id e)

-- | One view of a run by its name (for example `markets`), as JSON; `balances` at a round takes the
-- | round as well.
runView :: String -> String -> Maybe Int -> Aff (Either String Json)
runView id view round = typed (RunView { id, view, round: show <$> round })

-- | The comparison of the runs, the first being the baseline.
compare :: Array String -> Aff (Either String CompareView)
compare ids = typed (Compare { runs: ids, market: Nothing })

-- | The agency analysis against the fixed baseline, as JSON.
agency :: Aff (Either String Json)
agency = typed (Agency { playerSeed: Nothing, preset: Nothing, alternative: Nothing })

-- | Archive a run, or unarchive it.
archive :: String -> Boolean -> Aff (Either String Json)
archive id archived = typed (if archived then Unarchive id else Archive id)

-- | Resume a run to a round count.
resume :: String -> Int -> Aff (Either String Json)
resume id rounds = typed (Resume { id, rounds })

-- | Delete a run; the request names the run it deletes.
delete :: String -> Aff (Either String Json)
delete id = typed (Delete { id, confirm: id })
