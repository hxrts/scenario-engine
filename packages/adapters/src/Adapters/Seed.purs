-- | The one place randomness enters the system (docs/06_simulator.md §5). A run's player seed is
-- | resolved here, at initialization, before the pure simulation starts: an injected seed is used
-- | as given, and only an absent one is drawn from the OS. Everything after this point is a keyed
-- | draw on the resolved seed, so the caller records the returned seed and the run replays from it.
module Adapters.Seed (resolveSeed) where

import Prelude

import Core.Draw (Seed(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import drawOsSeed :: Effect Int

-- | An injected seed is returned unchanged; `Nothing` draws one from the OS (node:crypto).
resolveSeed :: Maybe Seed -> Effect Seed
resolveSeed = case _ of
  Just s -> pure s
  Nothing -> Seed <$> drawOsSeed
