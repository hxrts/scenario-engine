-- | The server's implementation of the `API` row (docs/06_simulator.md §8–§10): what each
-- | operation does. The paths, methods, and body decoding are `Handlers.Api.Route`'s; this module supplies
-- | only each operation's semantics, from `Server.Read`, `Server.Launch`, and `Server.Derive`. Reads
-- | of a run go through the cache of decoded runs; every write clears it first.
module Server.Implementation (implement) where

import Prelude

import Data.Either (either)
import Game.Premise (GameboardParts)
import Effects.Api (ApiRequest(..))
import Handlers.Api.Route (Implementation)
import Handlers.Store (StoreBackend)
import Views.Glossary (glossary)
import Views.Overview as Overview
import Server.Cache (Cache, clear)
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (failWith, ok)
import Server.Launch (launch, premiseInfo)
import Server.Read (agencyView, compareView, entitiesView, entityView, getView, runSummaries)

-- | The implementation of every operation over one premise, one store, and the store's run cache.
implement :: GameboardParts -> StoreBackend -> Cache -> Implementation
implement ctx store cache = case _ of
  ListRuns { archived } -> runSummaries archived store >>= ok <<< Overview.runList
  Premise -> premiseInfo ctx store >>= ok
  Glossary -> ok glossary
  LaunchRun l -> writing (launch ctx store l >>= either (failWith 400) ok)
  Entities id -> entitiesView cache store id
  Entity id entity -> entityView cache store id entity
  RunView r -> getView cache ctx store r
  Compare r -> compareView cache ctx store r
  Agency r -> agencyView ctx r
  Derive { id, body } -> writing (deriveRun cache ctx store id body)
  Resume { id, rounds } -> writing (resumeRun ctx store id rounds)
  Archive id -> writing (setArchive store id true)
  Unarchive id -> writing (setArchive store id false)
  Delete { id, confirm } -> writing (deleteOne store id confirm)
  where
  -- a write may change any run (a delete changes which sources exist), so the whole cache goes
  writing op = clear cache *> op
