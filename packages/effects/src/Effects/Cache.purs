-- | Smart constructors for `CACHE` (`Effects.Signatures`).
module Effects.Cache (lookupRun, keepRun, lookupReply, keepReply, forgetRuns) where

import Prelude

import Data.Maybe (Maybe)
import Effects.Signatures (CACHE, CacheF(..), _cache)
import Run (Run, lift)
import Type.Row (type (+))

lookupRun :: forall run reply r. String -> Run (CACHE run reply + r) (Maybe run)
lookupRun key = lift _cache (LookupRun key identity)

keepRun :: forall run reply r. String -> run -> Run (CACHE run reply + r) Unit
keepRun key value = lift _cache (KeepRun key value unit)

lookupReply :: forall run reply r. String -> Run (CACHE run reply + r) (Maybe reply)
lookupReply key = lift _cache (LookupReply key identity)

keepReply :: forall run reply r. String -> reply -> Run (CACHE run reply + r) Unit
keepReply key value = lift _cache (KeepReply key value unit)

forgetRuns :: forall run reply r. Run (CACHE run reply + r) Unit
forgetRuns = lift _cache (ForgetRuns unit)
