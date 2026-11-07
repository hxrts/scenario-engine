-- | The wall clock (docs/07_architecture.md): the one raw time primitive. Simulated time is the
-- | round index; this is the only reading of the real clock, and the only timer.
module Adapters.Clock (nowIso, delayMs) where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)

foreign import nowIsoImpl :: Effect String

-- | The current time, for a run's `created` field.
nowIso :: Effect String
nowIso = nowIsoImpl

-- | Wait the given number of milliseconds of wall-clock time.
delayMs :: Int -> Aff Unit
delayMs ms = delay (Milliseconds (toNumber ms))
