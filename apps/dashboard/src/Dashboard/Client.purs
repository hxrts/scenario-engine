-- | The dashboard's operations (docs/06_simulator.md §8): one function per request the dashboard makes.
-- | Each sends an `API` operation through `Handlers.Api.Client` over the dashboard's fetch device and
-- | decodes the reply through the shape it names (`Views.Keys`). The paths are `Handlers.Api.Route`'s.
module Dashboard.Client
  ( runs
  , launch
  , premise
  , glossary
  , overview
  , actors
  , actor
  , runView
  , compare
  , agency
  , agencyDetail
  , archive
  , resume
  , deriveRun
  , sets
  , saveSet
  , setSummary
  , renameSet
  , deleteSet
  , step
  , delete
  ) where

import Prelude

import Dashboard.Api (fetch)
import Core.Shape (Format(Snapshot), Shape, parse, prim)
import Effects.Api (ApiRequest(..), DeriveRequest, Launch, call)
import Schema.Run (DeriveKind)
import Effects.Signatures (RunSet)
import Handlers.Api.Client (replyText, runApi)
import Schema.Decode (decodeError)
import Views.Common (Table)
import Views.Compare (CompareView)
import Views.Actor (ActorList, History)
import Views.Glossary (Glossary)
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Overview (RunList, RunOverview)
import Views.Premise (Premise)
import Data.Argonaut.Core (jsonNull)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

-- | One operation: its reply's body, decoded through the shape it names.
typed :: forall a. Shape a -> ApiRequest -> Aff (Either String a)
typed shape req = (\reply -> replyText reply >>= decodeError <<< parse Snapshot shape) <$> runApi fetch (call req)

-- | A reply whose body the dashboard does not read: only whether it succeeded.
unread :: Shape Unit
unread = prim "unread" (const jsonNull) (const (Right unit))

-- | The run list: every run, archived or not, in creation order (`?archived=true`).
runs :: Aff (Either String RunList)
runs = typed Keys.runList (ListRuns { archived: true })

-- | Derive a run (`POST /api/runs/:id/derive`); the reply names the new run and, for a rerun, whether its records match.
deriveRun :: String -> DeriveRequest -> Aff (Either String { id :: String, source :: String, kind :: DeriveKind, recordsMatch :: Maybe Boolean })
deriveRun id body = typed Keys.derived (Derive { id, body })

-- | The named Run sets.
sets :: Aff (Either String { sets :: Array RunSet })
sets = typed Keys.sets ListRunSets

setSummary :: String -> Aff (Either String Table)
setSummary = typed Keys.table <<< SetSummary

saveSet :: RunSet -> Aff (Either String RunSet)
saveSet = typed Keys.runSet <<< SaveRunSet

renameSet :: String -> String -> Aff (Either String Unit)
renameSet name to = typed unread (RenameRunSet { name, to })

deleteSet :: String -> Aff (Either String Unit)
deleteSet = typed unread <<< DeleteRunSet

-- | Launch a run (`POST /api/runs`); the reply names the new run.
launch :: Launch -> Aff (Either String { id :: String, playerSeed :: Int })
launch = typed Keys.launched <<< LaunchRun

-- | The premises the launch form offers, each with its rounds, seed, default run name and presets.
premise :: Aff (Either String (Array Premise))
premise = typed Keys.premises Premise

-- | The glossary: the definitions and empty-value reasons the tables and the header show.
glossary :: Aff (Either String Glossary)
glossary = typed Keys.glossary Glossary

-- | A run's overview: its rounds, target and header facts.
overview :: String -> Aff (Either String RunOverview)
overview id = runView Keys.overview id Nothing

-- | A run's Actors, each flagged as a seat, and the default one.
actors :: String -> Aff (Either String ActorList)
actors = typed Keys.actorList <<< Actors

-- | One Actor's history.
actor :: String -> String -> Aff (Either String History)
actor id e = typed Keys.history (Actor id e)

-- | One view of a run, decoded through its key; `balances` and `events` at a round take the round as well.
runView :: forall a. ViewKey a -> String -> Maybe Int -> Aff (Either String a)
runView key id round = typed key.shape (RunView { id, view: key.view, round })

-- | The comparison of the runs, the first being the baseline.
compare :: Array String -> Aff (Either String CompareView)
compare ids = typed Keys.compare (Compare { runs: ids, exchange: Nothing })

-- | The agency analysis beyond its table: each criterion under the baseline and the alternative.
agencyDetail :: Aff (Either String { criteria :: Table, profile :: Table })
agencyDetail = typed Keys.agencyDetail (AgencyDetail { premise: Nothing, playerSeed: Nothing, preset: Nothing, alternative: Nothing })

-- | The agency analysis against the fixed baseline.
agency :: Aff (Either String Table)
agency = typed Keys.table (Agency { premise: Nothing, playerSeed: Nothing, preset: Nothing, alternative: Nothing })

-- | Archive a run, or unarchive it.
archive :: String -> Boolean -> Aff (Either String Unit)
archive id archived = typed unread (if archived then Unarchive id else Archive id)

-- | Resume a run to a round count.
resume :: String -> Int -> Aff (Either String Unit)
resume id rounds = typed unread (Resume { id, rounds })

-- | Play one more round of a run short of its target.
step :: String -> Aff (Either String Unit)
step id = typed unread (Step id)

-- | Delete a run; the request names the run it deletes.
delete :: String -> Aff (Either String Unit)
delete id = typed unread (Delete { id, confirm: id })
