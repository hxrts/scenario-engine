-- | Round-record equality (docs/06_simulator.md §14, test 2): the one comparison that says two
-- | histories are the same. Replay equality and the rerun check (§8) both use it.
module Simulator.Records (recordsEqual) where

import Prelude

import Core.World (RoundRecord)
import Data.Array (length, zipWith)
import Data.Foldable (and)

-- | Two record sequences are equal when they have the same length and every round record is equal
-- | in every field (round, inputs, events). A difference in any one round, or a different number of
-- | rounds, is not equal.
recordsEqual :: Array RoundRecord -> Array RoundRecord -> Boolean
recordsEqual a b = length a == length b && and (zipWith (==) a b)
