-- | Meridian's Transformations.
module Premises.Meridian.Transformations
  ( transformations
  , defenseProcurementTransformation
  , procurement
  , paid
  , energyShipping
  , deployment
  , majorMilitaryAction
  ) where

import Premises.Meridian.Names (n)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Schema.Build.Base (keyed, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, fromExecutor, fromHolder, noTransformation)

import Premises.Kit.Exchanges as Kit

import Laws.Types (CostFunction, ActorId, Payment, Transformation, TransformationId)
import Data.Map (Map)

transformations :: Map TransformationId Transformation
transformations = byKey
  [ Tuple n.defenseProcurement defenseProcurementTransformation
  , Tuple n.defenseProcurementCn (procurement n.cnPrivate)
  , Tuple n.publicAttention
      (noTransformation { outputs = keyed [ n.softPower := 1 ], payment = paid { intercept: q 3, slope: q "0.2", domain: { lo: q 0, hi: q 50 } } n.worldHouseholds, draws = [ fromHolder n.attention (q 1) ], maximum = Just (q 10) })
  , Tuple n.energyShipping energyShipping
  , Tuple n.humanitarianAid
      (noTransformation { inputs = keyed [ n.energy := 5 ], outputs = keyed [ n.softPower := 4 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } n.worldHouseholds, maximum = Just (q 2) })
  , Tuple n.militaryDeployment deployment
  , Tuple n.majorMilitaryAction majorMilitaryAction
  , Tuple n.logisticsInvestment
      (noTransformation { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } n.globalSmes, delay = 1, effects = [ { resource: n.transport, holder: Nothing, perUnit: q 10, rounds: Nothing } ] })
  , Tuple n.energyCapacityInvestment
      (noTransformation { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 12, slope: q 0, domain: { lo: q 0, hi: q 1000 } } n.globalSmes, delay = 1, draws = [ fromHolder n.semiconductors (q 1) ], ownerSigns = true, effects = [ { resource: n.energyProduction, holder: Nothing, perUnit: q 2, rounds: Nothing } ] })
  , Tuple n.strike
      (noTransformation { inputs = keyed [ n.softPower := 8 ], payment = paid { intercept: q 5, slope: q 0, domain: { lo: q 0, hi: q 1 } } n.usWorkers, delay = 1, maximum = Just (q 1), effects = [ { resource: n.energyProduction, holder: Just n.enco, perUnit: q "-6.25", rounds: Just 1 }, { resource: n.semiconductors, holder: Just n.chip, perUnit: q "-3.75", rounds: Just 1 } ] })
  ]

-- | The US defence procurement transformation, which DEF operates.
defenseProcurementTransformation :: Transformation
defenseProcurementTransformation = procurement n.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: ActorId -> Transformation
procurement payeeActor =
  noTransformation { inputs = keyed [ n.energy := 2 ], outputs = keyed [ n.mil := 1 ], payment = paid { intercept: q 8, slope: q "0.5", domain: { lo: q 0, hi: q 30 } } payeeActor, draws = [ fromHolder n.semiconductors (q "0.5") ], delay = 1 }

-- | A transformation's price, paid in the numeraire to the Actor.
paid :: CostFunction -> ActorId -> Maybe Payment
paid = Kit.paid n.money

energyShipping :: Transformation
energyShipping = noTransformation { payment = paid { intercept: q "0.3", slope: q 0, domain: { lo: q 0, hi: q 60 } } n.ship, draws = [ fromHolder n.transport (q 1) ], ownerSigns = true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_meridian.md §13.3).
deployment :: Transformation
deployment = noTransformation { draws = [ fromExecutor n.mil (q 1) ] }

majorMilitaryAction :: Transformation
majorMilitaryAction = noTransformation { inputs = keyed [ n.mil := 1 ], outputs = keyed [ n.mil := "0.75" ], minimum = (q 12) }
