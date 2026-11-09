-- | Meridian's Resources.
module Premises.Meridian.Resources
  ( resources
  , reservable
  , decays
  , coreResources
  ) where

import Premises.Meridian.Names (n)

import Data.Tuple (Tuple(..))
import Data.Map as Map

import Schema.Build.Quantity (q)
import Schema.Build.World (noResource, resourcesOf)
import Premises.Kit.Resources (capacity)

import Laws.Types (ResourceDefinition, ResourceId, Transferability(..))
import Data.Map (Map)
import Schema.Recurring (Decay)

resources :: Map ResourceId ResourceDefinition
resources = Map.union (coreResources)
  ( resourcesOf
      [ Tuple n.softPower (noResource { transferability = NonTransferable })
      , Tuple n.energyProduction capacity
      , Tuple n.semiconductors capacity
      , Tuple n.transport capacity
      , Tuple n.attention capacity
      ]
  )

-- | What a transformation may reserve, and each Resource's decay: the premise document holds them.
reservable :: Array ResourceId
reservable = [ n.mil ]

decays :: Array Decay
decays = [ { resource: n.softPower, rate: q "0.1" } ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Map ResourceId ResourceDefinition
coreResources = resourcesOf
  [ Tuple n.money (noResource { transferability = Free })
  , Tuple n.energy (noResource { transferability = Free })
  , Tuple n.mil (noResource { transferability = Restricted })
  ]
