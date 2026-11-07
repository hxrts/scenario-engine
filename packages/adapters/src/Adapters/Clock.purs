-- | The wall clock (docs/07_architecture.md): the one raw time primitive. Simulated time is the
-- | round index; this is the only reading of the real clock.
module Adapters.Clock (nowIso) where

import Effect (Effect)

foreign import nowIsoImpl :: Effect String

-- | The current time, for a run's `created` field.
nowIso :: Effect String
nowIso = nowIsoImpl
