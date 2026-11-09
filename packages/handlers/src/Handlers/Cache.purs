-- | The `CACHE` interpreter: maps kept in references, private to the handler.
module Handlers.Cache (Cache, newCache, interpretCache) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (CacheF(..))

type Cache run reply = { runs :: Ref (Map String run), replies :: Ref (Map String reply) }

newCache :: forall run reply. Effect (Cache run reply)
newCache = { runs: _, replies: _ } <$> Ref.new Map.empty <*> Ref.new Map.empty

interpretCache :: forall run reply. Cache run reply -> CacheF run reply ~> Effect
interpretCache c = case _ of
  LookupRun key k -> k <<< Map.lookup key <$> Ref.read c.runs
  KeepRun key value next -> next <$ Ref.modify_ (Map.insert key value) c.runs
  LookupReply key k -> k <<< Map.lookup key <$> Ref.read c.replies
  KeepReply key value next -> next <$ Ref.modify_ (Map.insert key value) c.replies
  ForgetRuns next -> next <$ Ref.write Map.empty c.runs
