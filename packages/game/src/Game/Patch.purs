-- | Named patches of a base premise (docs/03): the only way a premise is changed from outside after it
-- | is authored. Each fails on a name the premise does not have, so a patch never silently does nothing.
module Game.Patch
  ( setCapacity
  , setCurve
  ) where

import Prelude

import Board.Error (ValidationError(..))
import Core.Num (Q)
import Core.Types (CapacityRef, CostFunction, EntityId, MarketId, RefKind(..), idText)
import Data.Either (Either(..))
import Data.Map as Map
import Game.Error (GameError(..), Stage(..))
import Schema.Gameboard (Gameboard)

-- | Set what an Entity holds of a capacity.
setCapacity :: EntityId -> CapacityRef -> Q -> Gameboard -> Either GameError Gameboard
setCapacity e ref q gb
  | Map.member e gb.world.entities = Right gb { world { entities = Map.update (\x -> pure x { capacities = Map.insert ref q x.capacities }) e gb.world.entities } }
  | otherwise = Left (Invalid AfterOverrides [ UnknownReference EntityRef (idText e) ])

-- | Set a Market's curve.
setCurve :: MarketId -> CostFunction -> Gameboard -> Either GameError Gameboard
setCurve m cost gb
  | Map.member m gb.world.markets = Right gb { world { markets = Map.update (\x -> pure x { cost = cost }) m gb.world.markets } }
  | otherwise = Left (Invalid AfterOverrides [ UnknownReference Market (idText m) ])
