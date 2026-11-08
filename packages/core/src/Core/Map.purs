-- | Small helpers over `Map` that the engine, the checks, and the views share.
module Core.Map
  ( values
  , entries
  , sumInto
  , sumMaps
  , groupBy
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Array (fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

-- | A Map's values, in key order.
values :: forall k v. Map k v -> Array v
values = fromFoldable <<< Map.values

-- | A Map's entries, in key order.
entries :: forall k v. Map k v -> Array (Tuple k v)
entries = Map.toUnfoldable

-- | Add a quantity to a key's total.
sumInto :: forall k. Ord k => k -> Q -> Map k Q -> Map k Q
sumInto = Map.insertWith Q.add

-- | Two sets of totals, added key by key.
sumMaps :: forall k. Ord k => Map k Q -> Map k Q -> Map k Q
sumMaps = Map.unionWith Q.add

-- | Group items into arrays by key, keeping their order.
groupBy :: forall k a. Ord k => (a -> k) -> Array a -> Map k (Array a)
groupBy key xs = Map.fromFoldableWith (flip (<>)) (map (\x -> Tuple (key x) [ x ]) xs)
