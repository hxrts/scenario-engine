-- | Resolution consistency (docs/04_instantiated_game.md §4.3, INV-14): two instantiations
-- | that differ only in explicitness, the Germany comparison, must describe the same economy.
module Board.Gameboard.Reconcile
  ( reconcile
  ) where

import Prelude

import Schema.Gameboard (InitialWorld)
import Board.Error (Difference(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Query (aggregateOf, lookupQ)
import Core.Types (Capability(..), EntityId)
import Core.World (WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable, nub, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | Two instantiations differing only in explicitness must describe the same initial
-- | economy: stocks, capacities, institutional authority, and procedure weights.
reconcile :: InitialWorld -> InitialWorld -> Either (Array Difference) Unit
reconcile a b =
  let
    diffs = compare' StockDiffers (stocks a.world) (stocks b.world)
      <> compare' CapacityDiffers (capacities a.world) (capacities b.world)
      <> compare' WeightDiffers (weights a.world) (weights b.world)
      <> (if authority a.world == authority b.world then [] else [ AuthorityDiffers ])
  in
    if null diffs then Right unit else Left diffs
  where
  compare' :: forall k. Ord k => (k -> Q -> Q -> Difference) -> Map k Q -> Map k Q -> Array Difference
  compare' mk x y = concatMap (diff mk x y) (nub (fromFoldable (Map.keys x) <> fromFoldable (Map.keys y)))

  diff :: forall k. Ord k => (k -> Q -> Q -> Difference) -> Map k Q -> Map k Q -> k -> Array Difference
  diff mk x y k =
    let
      l = lookupQ k x
      r = lookupQ k y
    in
      if l == r then [] else [ mk k l r ]
  stocks w = sumMaps (map _.accounts (fromFoldable (Map.values w.entities)) <> map _.reserves (fromFoldable (Map.values w.markets)))
  capacities w = sumMaps (map _.capacities (fromFoldable (Map.values w.entities)))

  -- Data.Map here has no unionsWith
  sumMaps :: forall k. Ord k => Array (Map k Q) -> Map k Q
  sumMaps = foldl (Map.unionWith Q.add) Map.empty
  weights w = sumMaps
    ( concatMap
        ( \e -> case aggregateOf e of
            Just x -> map (\(Tuple pid p) -> Map.singleton pid (Q.add p.residualWeight (Q.sumQ (fromFoldable (Map.values p.weights))))) (Map.toUnfoldable x.procedures)
            Nothing -> []
        )
        (fromFoldable (Map.values w.entities))
    )

  -- the enactment capabilities each Entity holds, as a set of (Entity, capability)
  authority :: WorldState -> Set (Tuple EntityId Capability)
  authority w = Set.fromFoldable (concatMap (\e -> map (Tuple e.id) (filter isEnact e.capabilities)) (fromFoldable (Map.values w.entities)))
  isEnact = case _ of
    MayEnact _ -> true
    _ -> false
