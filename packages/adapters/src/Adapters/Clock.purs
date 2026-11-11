-- | The wall clock (docs/08_architecture.md): the one raw time primitive. Simulated time is the
-- | round index; this is the only reading of the real clock.
module Adapters.Clock (nowIso) where

import Effect (Effect)

-- | The current time, for a run's `created` field.
foreign import nowIso :: Effect String
