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
import Data.Tuple (Tuple(..))
import Effects.Api (ApiRequest, Reply)
import Effects.Cache (forgetRuns)
import Handlers.Api.Contract (dispatch)
import Handlers.Api.Route (wire)
import Run (Run)
import Server.Derive (deleteOne, deriveRun, resumeRun, setArchive, stepRun)
import Server.Errors (badRequest, ok, notFound)
import Effects.Launch (runExperiment)
import Server.Launch (launch, launchBatch, serverRole, premiseInfo)
import Server.OpenApi (document, serviceOf)
import Server.Sessions (sessionView, startSession, submitTranscript)
import Server.Read (agencyDetailView, agencyView, compareView, actorsView, actorView, getView, balanceSheetLinksView, balanceSheetView, roundReportView, runSummaries)
import Server.Rows (SERVER)
import Server.Sets (deleteRunSet, renameRunSet, saveSet, setSummary, setsView)
import Server.Premises (Premises, chosen)
import Views.Glossary (glossary)
import Views.Overview as Overview

-- | The reply to one request over the loaded premises. The record has one field per operation of the contract, and
-- | each returns its reply value or a refusal its entry declares; `dispatch` encodes the result as the entry states.
answer :: forall r. Premises -> ApiRequest -> Run (SERVER r) Reply
answer premises request = do
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  when ((wire request).method /= "GET") forgetRuns
  dispatch
    { "ListRuns": \{ archived } -> runSummaries archived >>= ok <<< Overview.runList
    , "Premise": \_ -> premiseInfo premises >>= ok
    , "Glossary": \_ -> ok (glossary (mapMaybe _.possibleWorld.world.focus premises))
    , "LaunchRun": \l -> either (pure <<< Left) (\p -> launch p l) (chosen premises l.premise) >>= either badRequest ok
    , "LaunchBatch": \b -> either (pure <<< Left) (\ps -> launchBatch ps b) (maybe (Right premises) (traverse (chosen premises <<< Just)) b.premises) >>= either badRequest ok
    , "Actors": actorsView
    , "Actor": \(Tuple id actor) -> actorView id actor
    , "RunView": getView premises
    , "RoundReport": \r -> roundReportView r.id r.round
    , "OpenApi": \name -> maybe (notFound ("no service " <> name)) (ok <<< document) (serviceOf name)
    , "BalanceSheetLinks": balanceSheetLinksView
    , "BalanceSheet": \r -> balanceSheetView premises r.id r.token
    , "Compare": compareView premises
    , "Agency": agencyView premises
    , "AgencyDetail": agencyDetailView premises
    , "Derive": \{ id, body } -> deriveRun premises id body
    , "Resume": \{ id, rounds } -> resumeRun premises id rounds
    , "Step": stepRun premises
    , "RunExperiment": \r -> either (pure <<< Left) (\p -> runExperiment serverRole p r) (chosen premises r.premise) >>= either badRequest ok
    , "ListRunSets": \_ -> setsView
    , "SetSummary": setSummary premises
    , "SaveRunSet": saveSet
    , "RenameRunSet": renameRunSet
    , "DeleteRunSet": deleteRunSet
    , "StartSession": startSession premises
    , "GetSession": sessionView
    , "SubmitTranscript": \r -> submitTranscript r.id r.submission
    , "Archive": \id -> setArchive id true
    , "Unarchive": \id -> setArchive id false
    , "Delete": \{ id, confirm } -> deleteOne id confirm
    }
    request
