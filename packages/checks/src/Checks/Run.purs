-- | The run checker: the invariants of every round of a run, each checked against the world it
-- | started from (prepared, as the engine prepares it). Pure. The per-round expectations are
-- | `Checks.Round.checkRound`; a caller that has both runs both.
module Checks.Run (checkRun) where

import Prelude

import Checks.Round (Failure(..))
import Core.Engine.Prepare (prepareRound)
import Core.Invariants as Inv
import Core.World (RoundResult, WorldState)
import Data.Foldable (foldl)

-- | Every invariant failure of every round, in round order.
checkRun :: WorldState -> Array RoundResult -> Array Failure
checkRun initial rs = (foldl step { world: initial, out: [] } rs).out
  where
  step acc r =
    { world: r.world
    , out: acc.out <> map (\f -> Violated r.record.round f.invariant f.detail) (Inv.check (prepareRound acc.world) r)
    }
