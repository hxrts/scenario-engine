-- | The server's implementation of the `API` row (docs/06_simulator.md §8–§10, work/4.md §7.4): what each
-- | operation does. The paths, methods, and body decoding are `Handlers.Api.Route`'s; this module supplies
-- | only each operation's semantics, from `Server.Read`, `Server.Launch`, and `Server.Derive`.
module Server.Implementation (implement) where

import Prelude

import Data.Either (either)
import Game.Premise (GameboardParts)
import Effects.Api (ApiRequest(..))
import Handlers.Api.Route (Implementation)
import Handlers.Store (StoreBackend)
import Views.Glossary (glossary)
import Views.Overview as Overview
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (failWith, ok)
import Server.Launch (launch, scenarioInfo)
import Server.Read (agencyView, compareView, entitiesView, entityView, getView, runSummaries)

-- | The implementation of every operation over one premise and one store.
implement :: GameboardParts -> StoreBackend -> Implementation
implement ctx store = case _ of
  ListRuns { archived } -> runSummaries archived store >>= ok <<< Overview.runList
  Scenario -> scenarioInfo ctx store >>= ok
  Glossary -> ok glossary
  LaunchRun l -> launch ctx store l >>= either (failWith 400) ok
  Entities id -> entitiesView store id
  Entity id entity -> entityView store id entity
  RunView r -> getView ctx store r
  Compare r -> compareView ctx store r
  Agency r -> agencyView ctx r
  Derive { id, body } -> deriveRun ctx store id body
  Resume { id, rounds } -> resumeRun ctx store id rounds
  Archive id -> setArchive store id true
  Unarchive id -> setArchive store id false
  Delete { id, confirm } -> deleteOne store id confirm
