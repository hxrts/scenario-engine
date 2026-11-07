-- | Meridian's Entities: Actors with their objectives, and Aggregates with their
-- | procedures. `just scenarios` compiles them to `packages/scenarios/meridian/v0/entities.json`.
-- |
-- | This module is the catalog. The seats are in `Entities.Seats`, the Aggregates in
-- | `Entities.Aggregates`, and the capability sets they are built from in `Entities.Capabilities`.
module Scenarios.Meridian.Entities (entities) where

import Data.Tuple (Tuple(..))
import DSL.Base (Built, Keyed, keyed)
import Scenarios.Meridian.Entities.Aggregates (cnPrivate, energySuppliers, eu, euPool, globalSmes, usWorkers, worldHouseholds)
import Scenarios.Meridian.Entities.Seats (aid, br, cn, chip, def, de, enco, india, jp, labor, ng, sa, ship, sme, uk, us)

-- | Every Entity, in catalog order.
entities :: Keyed (Built "entity")
entities = keyed
  [ Tuple "US" us
  , Tuple "DE" de
  , Tuple "IN" india
  , Tuple "JP" jp
  , Tuple "UK" uk
  , Tuple "BR" br
  , Tuple "SA" sa
  , Tuple "NG" ng
  , Tuple "ENCO" enco
  , Tuple "CHIP" chip
  , Tuple "SHIP" ship
  , Tuple "DEF" def
  , Tuple "LABOR" labor
  , Tuple "SME" sme
  , Tuple "AID" aid
  , Tuple "CN" cn
  , Tuple "EU" eu
  , Tuple "EU_POOL" euPool
  , Tuple "CN_PRIVATE" cnPrivate
  , Tuple "US_WORKERS" usWorkers
  , Tuple "GLOBAL_SMES" globalSmes
  , Tuple "ENERGY_SUPPLIERS" energySuppliers
  , Tuple "WORLD_HOUSEHOLDS" worldHouseholds
  ]
