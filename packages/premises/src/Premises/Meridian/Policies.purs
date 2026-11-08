-- | Meridian's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just _premises` compiles it to
-- | `packages/premises/meridian/v0/policies.json`.
module Premises.Meridian.Policies (policies) where

import Prelude (negate, (<>))

import Data.Maybe (Maybe(..))

import Core.Strategy (Clause, Strategy)
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (asTemplate, orderAt, selfTransform, each, eqQ, factor, ge, gt, holding, le, lit, max', mulQ, named, not', param, priceAt, procedure, reciprocate, restrictedLastRound, restrictionInForce, roundQ, rule, signFacility, subQ, tradeAt, transformedLastRound)
import Schema.Build.Intent (transfer)
import Premises.Meridian.Parameters (disruptionRound, euMatchingTariff, majorActionThreshold, supplierAsia, supplierAtlantic, supplierHighPrice)
import Premises.Meridian.Names as N
import Premises.Kit (banded, supplierOffers)
import Premises.Meridian.Rules (tariff)

-- | The in-world Policy catalog (NPCs, Aggregates, institutional fallbacks).
policies :: Array (Tuple String Strategy)
policies =
  [ Tuple N.policyChinaStrategic chinaStrategic
  , Tuple N.policyEuCoordination euCoordination
  , Tuple N.policyGlobalSmes globalSmes
  , Tuple N.policyCnPrivate cnPrivate
  , Tuple N.policyUsWorkers usWorkers
  , Tuple N.policyWorldHouseholds worldHouseholds
  , Tuple N.policyEnergySuppliers energySuppliers
  , Tuple N.policyGovernmentDefault governmentDefault
  , Tuple N.policyFirmDefault firmDefault
  , Tuple N.policySolidarityPool solidarityPool
  ]

chinaStrategic :: Strategy
chinaStrategic =
  { rules:
      [ rule [ transformedLastRound { by: N.us, transformation: N.majorMilitaryAction, atLeast: param majorActionThreshold } ]
          [ asTemplate (selfTransform { transformation: N.defenseProcurementCn, quantity: lit 4 }) ]
      , rule [ gt (priceAt (named N.energyAsia)) (lit "3.2") ]
          [ orderAt (named N.energyAsia) (lit (-12)) Nothing ]
      , rule [ restrictedLastRound { by: N.us, target: N.cn } ]
          [ reciprocate { target: N.us } ]
      ]
  , sign: []
  }

euCoordination :: Strategy
euCoordination =
  { rules:
      banded N.energyAtlantic 3 8 4
        <>
          [ rule [ gt (priceAt (named N.energyAtlantic)) (lit 3), ge (holding N.energy) (lit 8) ]
              [ procedure { id: N.euSolidarity, signers: [ N.de ], intents: [ transfer { from: N.euPool, to: N.eu, resource: N.energy, quantity: lit 8 }, tradeAt { market: named N.energyAtlantic, quantity: lit (-8), limit: Nothing } ] } ]
          , tariffMatch N.us
          , tariffMatch N.cn
          ]
  , sign: []
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: String -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: N.eu }) ]
  [ procedure { id: N.euTrade, signers: [ N.de ], intents: [ tariff.self { origin, rate: param euMatchingTariff } ] } ]

globalSmes :: Strategy
globalSmes =
  { rules:
      [ rule [ le (priceAt (named N.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named N.energyAtlantic) (mulQ (lit 40) (factor N.energy)) Nothing ]
      , rule [ gt (priceAt (named N.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named N.energyAtlantic) (mulQ (lit 32) (factor N.energy)) Nothing ]
      ]
  , sign: []
  }

cnPrivate :: Strategy
cnPrivate = { rules: banded N.energyAsia 3 12 9, sign: [] }

usWorkers :: Strategy
usWorkers = { rules: banded N.energyAtlantic 3 8 6, sign: [] }

worldHouseholds :: Strategy
worldHouseholds = { rules: banded N.energyAsia 3 12 9, sign: [] }

energySuppliers :: Strategy
energySuppliers = { rules: offers N.energyAtlantic supplierAtlantic.names <> offers N.energyAsia supplierAsia.names, sign: [] }
  where
  offers = supplierOffers { disruptionRound, highPrice: supplierHighPrice }

governmentDefault :: Strategy
governmentDefault = { rules: [], sign: [] }

-- | The solidarity pool has no policy: it only gives Energy when the EU's procedure draws on it.
solidarityPool :: Strategy
solidarityPool = { rules: [], sign: [] }

-- | A firm sells its Energy surplus (above 30) on every Energy Market it has access to.
firmDefault :: Strategy
firmDefault =
  { rules:
      [ rule [ gt (holding N.energy) (lit 30) ]
          [ orderAt (each N.energy) (subQ (lit 30) (holding N.energy)) Nothing ]
      ]
  , sign: [ signFacility N.energyShipping ]
  }
