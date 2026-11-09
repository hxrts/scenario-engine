-- | The server's handler of the `API` row (docs/06_simulator.md §8–§10): what each operation does, as a program
-- | over the Run Store and the system rows. The paths, methods, and body decoding are `Handlers.Api.Route`'s;
-- | this module supplies only each operation's semantics, from `Server.Read`, `Server.Launch`, and
-- | `Server.Derive`. Reads of a run go through the cache of decoded runs; every write clears it first.
module Server.Implementation (answer) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Core.Shape (listOf)
import Effects.Api (ApiRequest(..), Reply)
import Effects.Cache (forgetRuns)
import Handlers.Api.Route (wire)
import Run (Run)
import Server.Derive (deleteOne, deriveRun, resumeRun, setArchive, stepRun)
import Server.Errors (Answer, apiReply, badRequest, ok)
import Effects.Launch (runExperiment)
import Server.Launch (launch, launchBatch, serverRole, premiseInfo)
import Server.Sessions (sessionView, startSession, submitTranscript)
import Server.Read (agencyDetailView, agencyView, compareView, actorsView, actorView, getView, roundReportView, runSummaries)
import Server.Rows (SERVER)
import Server.Sets (deleteRunSet, renameRunSet, saveSet, setSummary, setsView)
import Server.Premises (Premises, chosen)
import Views.Glossary (glossary)
import Views.Keys as Keys
import Views.Overview as Overview

-- | The reply to one request over the loaded premises.
answer :: forall r. Premises -> ApiRequest -> Run (SERVER r) Reply
answer premises request = do
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  when ((wire request).method /= "GET") forgetRuns
  apiReply <$> operation request
  where
  operation :: ApiRequest -> Run (SERVER r) Answer
  operation = case _ of
    ListRuns { archived } -> runSummaries archived >>= ok Keys.runList <<< Overview.runList
    Premise -> premiseInfo premises >>= ok Keys.premises
    Glossary -> ok Keys.glossary (glossary (mapMaybe _.possibleWorld.world.focus premises))
    LaunchRun l -> either (pure <<< Left) (\p -> launch p l) (chosen premises l.premise) >>= either badRequest (ok Keys.launched)
    LaunchBatch b -> either (pure <<< Left) (\ps -> launchBatch ps b) (maybe (Right premises) (traverse (chosen premises <<< Just)) b.premises) >>= either badRequest (ok (listOf Keys.batched))
    Actors id -> actorsView id
    Actor id actor -> actorView id actor
    RunView r -> getView premises r
    RoundReport r -> roundReportView r.id r.round
    Compare r -> compareView premises r
    Agency r -> agencyView premises r
    AgencyDetail r -> agencyDetailView premises r
    Derive { id, body } -> deriveRun premises id body
    Resume { id, rounds } -> resumeRun premises id rounds
    Step id -> stepRun premises id
    RunExperiment r -> either (pure <<< Left) (\p -> runExperiment serverRole p r) (chosen premises r.premise) >>= either badRequest (ok Keys.experiment)
    ListRunSets -> setsView
    SetSummary name -> setSummary premises name
    SaveRunSet set -> saveSet set
    RenameRunSet r -> renameRunSet r
    DeleteRunSet name -> deleteRunSet name
    StartSession s -> startSession premises s
    GetSession id -> sessionView id
    SubmitTranscript r -> submitTranscript r.id r.submission
    Archive id -> setArchive id true
    Unarchive id -> setArchive id false
    Delete { id, confirm } -> deleteOne id confirm
