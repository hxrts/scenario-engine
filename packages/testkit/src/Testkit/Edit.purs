-- | Small edits of a premise or World in a test: change the named item of a section and leave the rest.
module Testkit.Edit
  ( onEntity
  , onMarket
  , onTransformation
  , onActor
  ) where

import Prelude

import Core.Types (Entity, EntityId, Market, MarketId, Transformation, TransformationId)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Schema.Gameboard (ActorDefinition)

onEntity :: forall r. EntityId -> (Entity -> Entity) -> { entities :: Map EntityId Entity | r } -> { entities :: Map EntityId Entity | r }
onEntity k f w = w { entities = Map.update (Just <<< f) k w.entities }

onMarket :: forall r. MarketId -> (Market -> Market) -> { markets :: Map MarketId Market | r } -> { markets :: Map MarketId Market | r }
onMarket k f w = w { markets = Map.update (Just <<< f) k w.markets }

onTransformation :: forall r. TransformationId -> (Transformation -> Transformation) -> { transformations :: Map TransformationId Transformation | r } -> { transformations :: Map TransformationId Transformation | r }
onTransformation k f w = w { transformations = Map.update (Just <<< f) k w.transformations }

onActor :: forall r. EntityId -> (ActorDefinition -> ActorDefinition) -> { actors :: Map EntityId ActorDefinition | r } -> { actors :: Map EntityId ActorDefinition | r }
onActor k f w = w { actors = Map.update (Just <<< f) k w.actors }
