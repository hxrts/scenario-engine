-- | The server's cache of decoded runs (docs/07_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the kept runs, so a reply never shows a run older than the store.
module Server.Cache (cached, cachedCompare) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effects.Cache (keepReply, keepRun, lookupReply, lookupRun)
import Views.Compare (CompareView)
import Effects.Signatures (CACHE, RunError)
import Run (Run)
import Schema.Run (RunStatus(..))
import Type.Row (type (+))
import Views.Common (StoredRun)

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: forall r. String -> Run (CACHE StoredRun CompareView + r) (Either RunError StoredRun) -> Run (CACHE StoredRun CompareView + r) (Either RunError StoredRun)
cached id load = lookupRun id >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= Running -> keepRun id run
      _ -> pure unit
    pure r

-- | A comparison from the cache, or computed and then kept when every run it reads is finished and none was skipped: a
-- | comparison of the whole suite takes seconds, and a page asks for it on every visit. A write forgets it.
cachedCompare :: forall r e. String -> Run (CACHE StoredRun CompareView + r) { answer :: Either e CompareView, final :: Boolean } -> Run (CACHE StoredRun CompareView + r) (Either e CompareView)
cachedCompare key compute = lookupReply key >>= case _ of
  Just v -> pure (Right v)
  Nothing -> do
    r <- compute
    case r.answer of
      Right v | r.final -> keepReply key v
      _ -> pure unit
    pure r.answer
