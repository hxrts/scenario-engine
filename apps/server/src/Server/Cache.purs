-- | The server's cache of decoded runs (docs/06_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the kept runs, so a reply never shows a run older than the store.
-- | The agency analysis plays several whole games on the event loop and depends only on the setting
-- | and its query, so its successful reply is kept for the server's lifetime.
module Server.Cache (cached, cachedAgency) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effects.Cache (keepReply, keepRun, lookupReply, lookupRun)
import Effects.Signatures (CACHE, RunError)
import Run (Run)
import Schema.Run (RunStatus(..))
import Server.Errors (Answer)
import Type.Row (type (+))
import Views.Common (StoredRun)

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: forall r. String -> Run (CACHE StoredRun Answer + r) (Either RunError StoredRun) -> Run (CACHE StoredRun Answer + r) (Either RunError StoredRun)
cached id load = lookupRun id >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= Running -> keepRun id run
      _ -> pure unit
    pure r

-- | The agency reply for a query, computed once. A reply that is an error is not kept.
cachedAgency :: forall r. String -> Run (CACHE StoredRun Answer + r) Answer -> Run (CACHE StoredRun Answer + r) Answer
cachedAgency key load = lookupReply key >>= case _ of
  Just reply -> pure reply
  Nothing -> do
    reply <- load
    when (either (const false) (\r -> r.status == 200) reply) (keepReply key reply)
    pure reply
