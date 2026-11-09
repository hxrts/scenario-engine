-- | Meridian's Actors: Actors with their objectives, and Aggregates with their
-- | procedures. `just _premises` compiles them to `packages/premises/meridian/v0/Actors.json`.
-- |
-- | This module is the catalog. The seats are in `Actors.Seats`, the Aggregates in
-- | `Actors.Aggregates`, and the capability sets they are built from in `Actors.Capabilities`.
module Premises.Meridian.Actors (actors, explicitActors) where

import Prelude (map, (<>))
import Premises.Meridian.Names (n)
import Data.Tuple (Tuple(..), fst, snd)
import Laws.Types (Actor, ActorId)
import Data.Map (Map)
import Schema.Build.Base (keyed)
import Schema.Build.World (actorsOf)
import Schema.PossibleWorld (ActorDefinition)
import Premises.Meridian.Actors.Aggregates (cnPrivate, energySuppliers, eu, euPool, globalSmes, usWorkers, worldHouseholds)
import Premises.Meridian.Actors.Seats (cn, seats)

-- | The Actors with an Actor definition: the seats in catalog order, then CN.
seated :: Array (Tuple ActorId (Tuple Actor ActorDefinition))
seated = seats <> [ cn ]

-- | Every Actor's definition.
explicitActors :: Map ActorId ActorDefinition
explicitActors = keyed (map (map snd) seated)

-- | Every Actor, in catalog order.
actors :: Map ActorId Actor
actors = actorsOf (map (map fst) seated <> aggregates)
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
