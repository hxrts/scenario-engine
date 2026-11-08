-- | Meridian's Resources.
module Settings.Meridian.Resources
  ( resources
  , reservable
  , decays
  , coreResources
  ) where

import Prelude (identity, map, negate, not, show, (+), (<>))
import Core.World (FlowGate)
import Settings.Meridian.Names (n, seatActors)
import Core.Num (Q)
import Data.Array (elem, mapWithIndex)
import Data.Either (Either(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Schema.Build.Base (keyed, (:=), payee, sales)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, noFacility, noResource, resourcesOf)
import Settings.Compose (Setting)
import Settings.Kit.Markets (MarketTerms, market)
import Settings.Kit.Compose (Premise, assemble, emptyParts, premise)
import Settings.Kit.Seats (flowsOf, requirementsOf, (~>))
import Settings.Kit.Markets as Kit
import Settings.Kit.Presets (PresetBinding, standardThresholds)
import Schema.Build.Decision (lit)
import Schema.Gameboard (Gameboard, InstantiationConfig, Scheme)
import Core.Draw (Seed(..))
import Core.Types (CostFunction, EntityId, Estimator, Holdings, Id(..), Market, MarketId, Payment, ResourceDefinition, ResourceId, Transferability(..), Transformation, TransformationId)
import Data.Map (Map)
import Schema.Recurring (Circulation, Decay, Flow, Production, Requirement, Share, compileIn)

resources :: Map ResourceId ResourceDefinition
resources = Map.union (coreResources) (resourcesOf [ Tuple n.softPower (noResource { transferability = NonTransferable }) ])

-- | What a facility may reserve, and each Resource's decay: the setting document holds them.
reservable :: Array ResourceId
reservable = [ Id n.mil ]

decays :: Array Decay
decays = [ { resource: Id n.softPower, rate: q "0.1" } ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Map ResourceId ResourceDefinition
coreResources = resourcesOf
  [ Tuple n.money (noResource { transferability = Free })
  , Tuple n.energy (noResource { transferability = Free })
  , Tuple n.mil (noResource { transferability = Restricted })
  ]
