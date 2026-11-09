-- | Meridian's Entities: Actors with their objectives, and Aggregates with their
-- | procedures. `just _settings` compiles them to `packages/settings/meridian/v0/entities.json`.
-- |
-- | This module is the catalog. The seats are in `Entities.Seats`, the Aggregates in
-- | `Entities.Aggregates`, and the capability sets they are built from in `Entities.Capabilities`.
module Settings.Meridian.Entities (entities, actors) where

import Prelude (map, (<>))
import Settings.Meridian.Names (n)
import Data.Tuple (Tuple(..), fst, snd)
import Laws.Types (Entity, EntityId)
import Data.Map (Map)
import Schema.Build.Base (keyed)
import Schema.Build.World (entitiesOf)
import Schema.Gameboard (ActorDefinition)
import Settings.Meridian.Entities.Aggregates (cnPrivate, energySuppliers, eu, euPool, globalSmes, usWorkers, worldHouseholds)
import Settings.Meridian.Entities.Seats (cn, seats)

-- | The Entities with an Actor definition: the seats in catalog order, then CN.
seated :: Array (Tuple EntityId (Tuple Entity ActorDefinition))
seated = seats <> [ cn ]

-- | Every Actor's definition.
actors :: Map EntityId ActorDefinition
actors = keyed (map (map snd) seated)

-- | Every Entity, in catalog order.
entities :: Map EntityId Entity
entities = entitiesOf (map (map fst) seated <> aggregates)
  where
  aggregates =
    [ Tuple n.eu eu
    , Tuple n.euPool euPool
    , Tuple n.cnPrivate cnPrivate
    , Tuple n.usWorkers usWorkers
    , Tuple n.globalSmes globalSmes
    , Tuple n.energySuppliers energySuppliers
    , Tuple n.worldHouseholds worldHouseholds
    ]
