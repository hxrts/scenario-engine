-- | Meridian's Entities: Actors with their objectives, and Aggregates with their
-- | procedures. `just premises` compiles them to `packages/premises/meridian/v0/entities.json`.
-- |
-- | This module is the catalog. The seats are in `Entities.Seats`, the Aggregates in
-- | `Entities.Aggregates`, and the capability sets they are built from in `Entities.Capabilities`.
module Premises.Meridian.Entities (entities) where

import Premises.Names as N
import Data.Tuple (Tuple(..))
import Schema.Build.Base (Keyed, keyed)
import Schema.Premise.World (EntityDoc)
import Premises.Meridian.Entities.Aggregates (cnPrivate, energySuppliers, eu, euPool, globalSmes, usWorkers, worldHouseholds)
import Premises.Meridian.Entities.Seats (aid, br, cn, chip, def, de, enco, india, jp, labor, ng, sa, ship, sme, uk, us)

-- | Every Entity, in catalog order.
entities :: Keyed EntityDoc
entities = keyed
  [ Tuple N.us us
  , Tuple N.de de
  , Tuple N.india india
  , Tuple N.jp jp
  , Tuple N.uk uk
  , Tuple N.br br
  , Tuple N.sa sa
  , Tuple N.ng ng
  , Tuple N.enco enco
  , Tuple N.chip chip
  , Tuple N.ship ship
  , Tuple N.def def
  , Tuple N.labor labor
  , Tuple N.sme sme
  , Tuple N.aid aid
  , Tuple N.cn cn
  , Tuple N.eu eu
  , Tuple N.euPool euPool
  , Tuple N.cnPrivate cnPrivate
  , Tuple N.usWorkers usWorkers
  , Tuple N.globalSmes globalSmes
  , Tuple N.energySuppliers energySuppliers
  , Tuple N.worldHouseholds worldHouseholds
  ]
