-- | The server's cache of decoded runs (docs/06_simulator.md §8.1). Every view of a run reads and
-- | decodes the whole run; a page of the dashboard asks for ten views at once, so the decoded run is
-- | kept after its first read. A running run is never kept (its rounds are still arriving), and
-- | every write operation clears the cache, so a reply never shows a run older than the store.
module Server.Cache (Cache, newCache, cached, clear) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Server.Errors (LoadError)
import Views.Common (StoredRun)

type Cache = Ref (Map String StoredRun)

newCache :: Effect Cache
newCache = Ref.new Map.empty

-- | The run from the cache, or loaded and then kept when it is finished.
cached :: Cache -> String -> Effect (Either LoadError StoredRun) -> Effect (Either LoadError StoredRun)
cached c id load = Map.lookup id <$> Ref.read c >>= case _ of
  Just r -> pure (Right r)
  Nothing -> do
    r <- load
    case r of
      Right run | run.status /= "running" -> Ref.modify_ (Map.insert id run) c
      _ -> pure unit
    pure r

-- | Forget every run: called before each write operation.
clear :: Cache -> Effect Unit
clear c = Ref.write Map.empty c
