-- | The run driver: a run is a sequence of rounds, each prepared (Phase 1) before its step sees
-- | it. Fixture checking and invariant checking both fold over a run this way, so the fold is here.
module Core.Engine.Run
  ( runRounds
  ) where

import Core.Engine.Prepare (prepareRound)
import Core.World (WorldState)
import Data.Array (snoc)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))

-- | Step a run one round at a time from `initial`. `step` receives the round's World State, already
-- | prepared, and the round's input, and returns the World State the next round starts from with
-- | the round's report. The reports come back in round order.
runRounds :: forall i o. (WorldState -> i -> Tuple WorldState o) -> WorldState -> Array i -> Array o
runRounds step initial rs = (foldl one { world: initial, reports: [] } rs).reports
  where
  one acc i = case step (prepareRound acc.world) i of
    Tuple world report -> { world, reports: snoc acc.reports report }
