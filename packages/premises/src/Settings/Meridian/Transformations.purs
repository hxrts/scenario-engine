-- | Meridian's Transformations.
module Settings.Meridian.Transformations
  ( transformations
  , defenseProcurementFacility
  , procurement
  , paid
  , energyShipping
  , deployment
  , majorMilitaryAction
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

transformations :: Map TransformationId Transformation
transformations = byKey
  [ Tuple n.defenseProcurement defenseProcurementFacility
  , Tuple n.defenseProcurementCn (procurement n.cnPrivate)
  , Tuple n.publicAttention
      (noFacility { outputs = keyed [ n.softPower := 1 ], payment = paid { intercept: q 3, slope: q "0.2", domain: { lo: q 0, hi: q 50 } } (Id n.worldHouseholds), capacity = Just (Id n.attention), maximum = Just (q 10) })
  , Tuple n.energyShipping energyShipping
  , Tuple n.humanitarianAid
      (noFacility { inputs = keyed [ n.energy := 5 ], outputs = keyed [ n.softPower := 4 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.worldHouseholds), maximum = Just (q 2) })
  , Tuple n.militaryDeployment deployment
  , Tuple n.majorMilitaryAction majorMilitaryAction
  , Tuple n.logisticsInvestment
      (noFacility { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.globalSmes), delay = 1, effects = [ { capacity: Id n.transport, holder: Nothing, perUnit: q 10, rounds: Nothing } ] })
  , Tuple n.energyCapacityInvestment
      (noFacility { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 12, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.globalSmes), delay = 1, capacity = Just (Id n.semiconductors), capacityUse = (q 1), ownerSigns = true, effects = [ { capacity: Id n.energyProduction, holder: Nothing, perUnit: q 2, rounds: Nothing } ] })
  , Tuple n.strike
      (noFacility { inputs = keyed [ n.softPower := 8 ], payment = paid { intercept: q 5, slope: q 0, domain: { lo: q 0, hi: q 1 } } (Id n.usWorkers), delay = 1, maximum = Just (q 1), effects = [ { capacity: Id n.energyProduction, holder: Just (Id n.enco), perUnit: q "-6.25", rounds: Just 1 }, { capacity: Id n.semiconductors, holder: Just (Id n.chip), perUnit: q "-3.75", rounds: Just 1 } ] })
  ]

-- | The US defence procurement facility, which DEF operates.
defenseProcurementFacility :: Transformation
defenseProcurementFacility = procurement n.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> Transformation
procurement payeeEntity =
  noFacility { inputs = keyed [ n.energy := 2 ], outputs = keyed [ n.mil := 1 ], payment = paid { intercept: q 8, slope: q "0.5", domain: { lo: q 0, hi: q 30 } } (Id payeeEntity), capacity = Just (Id n.semiconductors), capacityUse = (q "0.5"), delay = 1 }

-- | A facility's price, paid in the numeraire to the Entity.
paid :: CostFunction -> EntityId -> Maybe Payment
paid = Kit.paid (Id n.money)

energyShipping :: Transformation
energyShipping = noFacility { payment = paid { intercept: q "0.3", slope: q 0, domain: { lo: q 0, hi: q 60 } } (Id n.ship), capacity = Just (Id n.transport), ownerSigns = true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: Transformation
deployment = noFacility { escrowPerUnit = keyed [ n.mil := 1 ] }

majorMilitaryAction :: Transformation
majorMilitaryAction = noFacility { inputs = keyed [ n.mil := 1 ], outputs = keyed [ n.mil := "0.75" ], minimum = (q 12) }
