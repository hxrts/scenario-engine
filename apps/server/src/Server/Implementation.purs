-- | The server's implementation of the `API` row (docs/06_simulator.md §8–§10): what each
-- | operation does. The paths, methods, and body decoding are `Handlers.Api.Route`'s; this module supplies
-- | only each operation's semantics, from `Server.Read`, `Server.Launch`, and `Server.Derive`. Reads
-- | of a run go through the cache of decoded runs; every write clears it first.
module Server.Implementation (implement) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Effects.Api (ApiRequest(..))
import Handlers.Api.Route (Implementation)
import Handlers.Store (StoreBackend)
import Views.Glossary (glossary)
import Views.Keys as Keys
import Views.Overview as Overview
import Server.Cache (Cache, clear)
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (failWith, ok)
import Core.Shape (listOf)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Server.Launch (batchedShape, launch, launchBatch, launchedShape, premiseInfo)
import Server.Premises (Premises, chosen)
import Server.Read (agencyView, compareView, entitiesView, entityView, getView, runSummaries)

-- | The implementation of every operation over the loaded premises, one store, and the store's run cache.
implement :: Premises -> StoreBackend -> Cache -> Implementation
implement premises store cache = case _ of
  ListRuns { archived } -> runSummaries archived store >>= ok Keys.runList <<< Overview.runList
  Premise -> premiseInfo premises store >>= ok Keys.premises
  Glossary -> ok Keys.glossary (glossary (mapMaybe _.gameboard.world.focus premises))
  LaunchRun l -> writing (either (pure <<< Left) (\p -> launch p store l) (chosen premises l.premise) >>= either (failWith 400) (ok launchedShape))
  LaunchBatch b -> writing (either (pure <<< Left) (\ps -> launchBatch ps store b) (maybe (Right premises) (traverse (chosen premises <<< Just)) b.premises) >>= either (failWith 400) (ok (listOf batchedShape)))
  Entities id -> entitiesView cache store id
  Entity id entity -> entityView cache store id entity
  RunView r -> getView cache premises store r
  Compare r -> compareView cache premises store r
  Agency r -> agencyView cache premises r
  Derive { id, body } -> writing (deriveRun cache premises store id body)
  Resume { id, rounds } -> writing (resumeRun premises store id rounds)
  Archive id -> writing (setArchive store id true)
  Unarchive id -> writing (setArchive store id false)
  Delete { id, confirm } -> writing (deleteOne store id confirm)
  where
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  writing op = clear cache *> op
