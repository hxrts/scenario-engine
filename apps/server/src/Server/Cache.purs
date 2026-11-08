-- | The server's cache of decoded runs (docs/06_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the kept runs, so a reply never shows a run older than the store.
-- | The agency analysis plays several whole games on the event loop and depends only on the setting
-- | and its query, so its successful reply is kept for the server's lifetime.
module Server.Cache (Cache, newCache, cached, cachedAgency, clear) where

import Prelude

import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Schema.Run (RunStatus(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Server.Errors (Answer)
import Effects.Signatures (RunError)
import Views.Common (StoredRun)

type Cache = { runs :: Ref (Map String StoredRun), agency :: Ref (Map String Answer) }

newCache :: Effect Cache
newCache = { runs: _, agency: _ } <$> Ref.new Map.empty <*> Ref.new Map.empty

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: Cache -> String -> Effect (Either RunError StoredRun) -> Effect (Either RunError StoredRun)
cached c id load = Map.lookup id <$> Ref.read c.runs >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= Running -> Ref.modify_ (Map.insert id run) c.runs
      _ -> pure unit
    pure r

-- | The agency reply for a query, computed once. A reply that is an error is not kept.
cachedAgency :: Cache -> String -> Effect Answer -> Effect Answer
cachedAgency c key load = Map.lookup key <$> Ref.read c.agency >>= case _ of
  Just reply -> pure reply
  Nothing -> do
    reply <- load
    when (either (const false) (\r -> r.status == 200) reply) (Ref.modify_ (Map.insert key reply) c.agency)
    pure reply

-- | Forget every run: called before each write operation. The agency replies stay: no write changes them.
clear :: Cache -> Effect Unit
clear c = Ref.write Map.empty c.runs
