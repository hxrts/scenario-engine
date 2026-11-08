-- | The server's implementation of the `API` row (docs/06_simulator.md §8–§10): what each
-- | operation does. The paths, methods, and body decoding are `Handlers.Api.Route`'s; this module supplies
-- | only each operation's semantics, from `Server.Read`, `Server.Launch`, and `Server.Derive`. Reads
-- | of a run go through the cache of decoded runs; every write clears it first.
module Server.Implementation (implement) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Effects.Api (ApiRequest(..))
import Effect (Effect)
import Handlers.Api.Route (Implementation)
import Handlers.Store (StoreBackend)
import Views.Glossary (glossary)
import Views.Keys as Keys
import Views.Overview as Overview
import Server.Cache (Cache, clear)
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (Answer, apiReply, badRequest, ok)
import Core.Shape (listOf)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Server.Launch (batchedShape, launch, launchBatch, launchedShape, settingInfo)
import Server.Settings (Settings, chosen)
import Server.Read (agencyView, compareView, entitiesView, entityView, getView, runSummaries)

-- | The implementation of every operation over the loaded settings, one store, and the store's run cache.
implement :: Settings -> StoreBackend -> Cache -> Implementation
implement settings store cache = map apiReply <<< answer
  where
  answer :: ApiRequest -> Effect Answer
  answer = case _ of
    ListRuns { archived } -> runSummaries archived store >>= ok Keys.runList <<< Overview.runList
    Setting -> settingInfo settings store >>= ok Keys.settings
    Glossary -> ok Keys.glossary (glossary (mapMaybe _.gameboard.world.focus settings))
    LaunchRun l -> writing (either (pure <<< Left) (\p -> launch p store l) (chosen settings l.setting) >>= either (badRequest) (ok launchedShape))
    LaunchBatch b -> writing (either (pure <<< Left) (\ps -> launchBatch ps store b) (maybe (Right settings) (traverse (chosen settings <<< Just)) b.settings) >>= either (badRequest) (ok (listOf batchedShape)))
    Entities id -> entitiesView cache store id
    Entity id entity -> entityView cache store id entity
    RunView r -> getView cache settings store r
    Compare r -> compareView cache settings store r
    Agency r -> agencyView cache settings r
    Derive { id, body } -> writing (deriveRun cache settings store id body)
    Resume { id, rounds } -> writing (resumeRun settings store id rounds)
    Archive id -> writing (setArchive store id true)
    Unarchive id -> writing (setArchive store id false)
    Delete { id, confirm } -> writing (deleteOne store id confirm)
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  writing op = clear cache *> op
