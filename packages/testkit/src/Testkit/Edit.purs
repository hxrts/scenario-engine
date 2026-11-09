-- | Small edits of a premise or World in a test: change the named item of a section and leave the rest.
module Testkit.Edit
  ( onActor
  , onExchange
  , onTransformation
  , onExplicitActor
  ) where

import Prelude

import Laws.Types (Actor, ActorId, Exchange, ExchangeId, Transformation, TransformationId)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Schema.PossibleWorld (ActorDefinition)

onActor :: forall r. ActorId -> (Actor -> Actor) -> { actors :: Map ActorId Actor | r } -> { actors :: Map ActorId Actor | r }
onActor k f w = w { actors = Map.update (Just <<< f) k w.actors }

onExchange :: forall r. ExchangeId -> (Exchange -> Exchange) -> { exchanges :: Map ExchangeId Exchange | r } -> { exchanges :: Map ExchangeId Exchange | r }
onExchange k f w = w { exchanges = Map.update (Just <<< f) k w.exchanges }

onTransformation :: forall r. TransformationId -> (Transformation -> Transformation) -> { transformations :: Map TransformationId Transformation | r } -> { transformations :: Map TransformationId Transformation | r }
onTransformation k f w = w { transformations = Map.update (Just <<< f) k w.transformations }

onExplicitActor :: forall r. ActorId -> (ActorDefinition -> ActorDefinition) -> { explicitActors :: Map ActorId ActorDefinition | r } -> { explicitActors :: Map ActorId ActorDefinition | r }
onExplicitActor k f w = w { explicitActors = Map.update (Just <<< f) k w.explicitActors }
