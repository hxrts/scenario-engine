-- | The server's cache of decoded runs (docs/07_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the kept runs, so a reply never shows a run older than the store.
module Server.Cache (cached) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effects.Cache (keepRun, lookupRun)
import Effects.Signatures (CACHE, RunError)
import Run (Run)
import Schema.Run (RunStatus(..))
import Type.Row (type (+))
import Views.Common (StoredRun)

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: forall r. String -> Run (CACHE StoredRun Unit + r) (Either RunError StoredRun) -> Run (CACHE StoredRun Unit + r) (Either RunError StoredRun)
cached id load = lookupRun id >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= Running -> keepRun id run
      _ -> pure unit
    pure r
