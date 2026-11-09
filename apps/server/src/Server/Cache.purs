-- | The server's cache of decoded runs (docs/06_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the kept runs, so a reply never shows a run older than the store.
-- | The agency analysis plays several whole games on the event loop and depends only on the composition
-- | and its query, so its successful result is kept for the server's lifetime.
module Server.Cache (Profiles, cached, cachedAgency) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effects.Cache (keepReply, keepRun, lookupReply, lookupRun)
import Effects.Signatures (CACHE, RunError)
import Run (Run)
import Schema.Run (RunStatus(..))
import Type.Row (type (+))
import Views.Agency (AgencyProfile)
import Views.Common (StoredRun)

-- | The result of an agency analysis, which is what the cache keeps.
type Profiles = Array AgencyProfile

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: forall r. String -> Run (CACHE StoredRun Profiles + r) (Either RunError StoredRun) -> Run (CACHE StoredRun Profiles + r) (Either RunError StoredRun)
cached id load = lookupRun id >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= Running -> keepRun id run
      _ -> pure unit
    pure r

-- | The agency analysis for a query, computed once. A refusal is not kept.
cachedAgency :: forall r e. String -> Run (CACHE StoredRun Profiles + r) (Either e Profiles) -> Run (CACHE StoredRun Profiles + r) (Either e Profiles)
cachedAgency key load = lookupReply key >>= case _ of
  Just profiles -> pure (Right profiles)
  Nothing -> do
    result <- load
    either (const (pure unit)) (keepReply key) result
    pure result
