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
import Server.Launch (launch, launchBatch, serverRole, compositionInfo)
import Server.OpenApi (document, serviceOf)
import Server.Sessions (sessionView, startSession, submitTranscript)
import Server.Read (agencyDetailView, agencyView, compareView, actorsView, actorView, getView, balanceSheetLinksView, balanceSheetView, roundReportView, runSummaries)
import Server.Rows (SERVER)
import Server.Sets (deleteRunSet, renameRunSet, saveSet, setSummary, setsView)
import Server.Compositions (Compositions, chosen)
import Views.Glossary (glossary)
import Views.Overview as Overview

-- | The reply to one request over the loaded compositions. The record has one field per operation of the contract, and
-- | each returns its reply value or a refusal its entry declares; `dispatch` encodes the result as the entry states.
answer :: forall r. Compositions -> ApiRequest -> Run (SERVER r) Reply
answer compositions request = do
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  when ((wire request).method /= "GET") forgetRuns
  dispatch
    { "ListRuns": \{ archived } -> runSummaries archived >>= ok <<< Overview.runList
    , "Premise": \_ -> compositionInfo compositions >>= ok
    , "Glossary": \_ -> ok (glossary (mapMaybe _.composition.world.focus compositions))
    , "LaunchRun": \l -> either (pure <<< Left) (\p -> launch p l) (chosen compositions l.composition) >>= either badRequest ok
    , "LaunchBatch": \b -> either (pure <<< Left) (\ps -> launchBatch ps b) (maybe (Right compositions) (traverse (chosen compositions <<< Just)) b.compositions) >>= either badRequest ok
    , "Actors": actorsView
    , "Actor": \(Tuple id actor) -> actorView id actor
    , "RunView": getView compositions
    , "RoundReport": \r -> roundReportView r.id r.round
    , "OpenApi": \name -> maybe (notFound ("no service " <> name)) (ok <<< document) (serviceOf name)
    , "BalanceSheetLinks": balanceSheetLinksView
    , "BalanceSheet": \r -> balanceSheetView compositions r.id r.token
    , "Compare": compareView compositions
    , "Agency": agencyView compositions
    , "AgencyDetail": agencyDetailView compositions
    , "Derive": \{ id, body } -> deriveRun compositions id body
    , "Resume": \{ id, rounds } -> resumeRun compositions id rounds
    , "Step": stepRun compositions
    , "RunExperiment": \r -> either (pure <<< Left) (\p -> runExperiment serverRole p r) (chosen compositions r.composition) >>= either badRequest ok
    , "ListRunSets": \_ -> setsView
    , "SetSummary": setSummary compositions
    , "SaveRunSet": saveSet
    , "RenameRunSet": renameRunSet
    , "DeleteRunSet": deleteRunSet
    , "StartSession": startSession compositions
    , "GetSession": sessionView
    , "SubmitTranscript": \r -> submitTranscript r.id r.submission
    , "Archive": \id -> setArchive id true
    , "Unarchive": \id -> setArchive id false
    , "Delete": \{ id, confirm } -> deleteOne id confirm
    }
    request
