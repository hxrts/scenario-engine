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
import Server.Launch (launch, launchBatch, serverRole, settingInfo)
import Server.Read (agencyDetailView, agencyView, compareView, entitiesView, entityView, getView, runSummaries)
import Server.Rows (SERVER)
import Server.Sets (deleteScenarioSet, renameScenarioSet, saveSet, setSummary, setsView)
import Server.Settings (Settings, chosen)
import Views.Glossary (glossary)
import Views.Keys as Keys
import Views.Overview as Overview

-- | The reply to one request over the loaded settings.
answer :: forall r. Settings -> ApiRequest -> Run (SERVER r) Reply
answer settings request = do
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  when ((wire request).method /= "GET") forgetRuns
  apiReply <$> operation request
  where
  operation :: ApiRequest -> Run (SERVER r) Answer
  operation = case _ of
    ListRuns { archived } -> runSummaries archived >>= ok Keys.runList <<< Overview.runList
    Setting -> settingInfo settings >>= ok Keys.settings
    Glossary -> ok Keys.glossary (glossary (mapMaybe _.gameboard.world.focus settings))
    LaunchRun l -> either (pure <<< Left) (\p -> launch p l) (chosen settings l.setting) >>= either badRequest (ok Keys.launched)
    LaunchBatch b -> either (pure <<< Left) (\ps -> launchBatch ps b) (maybe (Right settings) (traverse (chosen settings <<< Just)) b.settings) >>= either badRequest (ok (listOf Keys.batched))
    Entities id -> entitiesView id
    Entity id entity -> entityView id entity
    RunView r -> getView settings r
    Compare r -> compareView settings r
    Agency r -> agencyView settings r
    AgencyDetail r -> agencyDetailView settings r
    Derive { id, body } -> deriveRun settings id body
    Resume { id, rounds } -> resumeRun settings id rounds
    Step id -> stepRun settings id
    RunExperiment r -> either (pure <<< Left) (\p -> runExperiment serverRole p r) (chosen settings r.setting) >>= either badRequest (ok Keys.experiment)
    ListScenarioSets -> setsView
    SetSummary name -> setSummary settings name
    SaveScenarioSet set -> saveSet set
    RenameScenarioSet r -> renameScenarioSet r
    DeleteScenarioSet name -> deleteScenarioSet name
    Archive id -> setArchive id true
    Unarchive id -> setArchive id false
    Delete { id, confirm } -> deleteOne id confirm
