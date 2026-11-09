-- | The versioned metric definitions (docs/06_simulator.md §8, docs/07_architecture.md §5). A metric is
-- | a projection of a round's World State: it is recomputed from the records, never edited, and carries
-- | the version of the definition it was computed under. The composition root passes `roundMetrics` to
-- | the Run Store, which saves the rows it is given. Pure.
module Views.Metrics
  ( metricsVersion
  , roundMetrics
  ) where

import Prelude

import Core.Market (marketPrice)
import Laws.World (RoundResult)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Views.Common (exact)

-- | The version of the metric definitions; recomputing a projection never edits records.
metricsVersion :: String
metricsVersion = "v1"

-- | The metrics of one round, each tagged with `metricsVersion`: each Market's price after the round.
roundMetrics :: RoundResult -> Array { metric :: String, version :: String, value :: String }
roundMetrics r = map (\(Tuple id m) -> { metric: "price:" <> show id, version: metricsVersion, value: exact (marketPrice m) })
  (Map.toUnfoldable r.world.markets)
