-- | Meridian's Entities: Actors with their objectives, and Aggregates with their
-- | procedures. `just _premises` compiles them to `packages/premises/meridian/v0/entities.json`.
-- |
-- | This module is the catalog. The seats are in `Entities.Seats`, the Aggregates in
-- | `Entities.Aggregates`, and the capability sets they are built from in `Entities.Capabilities`.
module Premises.Meridian.Entities (entities, actors) where

import Prelude (map, (<>))
import Premises.Names as N
import Data.Array (zip)
import Data.Tuple (Tuple(..), fst, snd)
import Core.Types (Entity, EntityId)
import Data.Map (Map)
import Schema.Build.Base (keyed)
import Schema.Build.World (entitiesOf)
import Schema.Gameboard (ActorDefinition)
import Premises.Meridian.Entities.Aggregates (cnPrivate, energySuppliers, eu, euPool, globalSmes, usWorkers, worldHouseholds)
import Premises.Meridian.Entities.Seats (aid, br, cn, chip, def, de, enco, india, jp, labor, ng, sa, ship, sme, uk, us)

-- | The Entities with an Actor definition, in the order of `N.seatActors`, then CN.
seated :: Array (Tuple String (Tuple Entity ActorDefinition))
seated = zip N.seatActors [ us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid ] <> [ Tuple N.cn cn ]

-- | Every Actor's definition.
actors :: Map EntityId ActorDefinition
actors = keyed (map (map snd) seated)

-- | Every Entity, in catalog order.
entities :: Map EntityId Entity
entities = entitiesOf (map (map fst) seated <> aggregates)
  where
  aggregates =
    [ Tuple N.eu eu
    , Tuple N.euPool euPool
    , Tuple N.cnPrivate cnPrivate
    , Tuple N.usWorkers usWorkers
    , Tuple N.globalSmes globalSmes
    , Tuple N.energySuppliers energySuppliers
    , Tuple N.worldHouseholds worldHouseholds
    ]
