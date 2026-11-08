-- | Meridian's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just _premises` compiles it to
-- | `packages/premises/meridian/v0/policies.json`.
module Settings.Meridian.Policies (policies) where

import Prelude (negate, (<>))

import Data.Maybe (Maybe(..))

import Core.Strategy (Clause, Strategy)
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (passive, asTemplate, each, factor, ge, gt, holding, le, lit, mulQ, named, not', orderAt, param, priceAt, procedure, reciprocate, restrictedLastRound, restrictionInForce, rule, selfTransform, signFacility, subQ, tradeAt, transformedLastRound)
import Schema.Build.Intent (transfer)
import Settings.Meridian.Parameters (disruptionRound, euMatchingTariff, majorActionThreshold, supplierAsia, supplierAtlantic, supplierHighPrice)
import Settings.Meridian.Names (n)
import Settings.Kit (banded, supplierOffers)
import Settings.Meridian.Rules (tariff)

-- | The in-world Policy catalog (NPCs, Aggregates, institutional fallbacks).
policies :: Array (Tuple String Strategy)
policies =
  [ Tuple n.policyChinaStrategic chinaStrategic
  , Tuple n.policyEuCoordination euCoordination
  , Tuple n.policyGlobalSmes globalSmes
  , Tuple n.policyCnPrivate cnPrivate
  , Tuple n.policyUsWorkers usWorkers
  , Tuple n.policyWorldHouseholds worldHouseholds
  , Tuple n.policyEnergySuppliers energySuppliers
  , Tuple n.policyGovernmentDefault governmentDefault
  , Tuple n.policyFirmDefault firmDefault
  , Tuple n.policySolidarityPool solidarityPool
  ]

chinaStrategic :: Strategy
chinaStrategic =
  { rules:
      [ rule [ transformedLastRound { by: n.us, transformation: n.majorMilitaryAction, atLeast: param majorActionThreshold } ]
          [ asTemplate (selfTransform { transformation: n.defenseProcurementCn, quantity: lit 4 }) ]
      , rule [ gt (priceAt (named n.energyAsia)) (lit "3.2") ]
          [ orderAt (named n.energyAsia) (lit (-12)) Nothing ]
      , rule [ restrictedLastRound { by: n.us, target: n.cn } ]
          [ reciprocate { target: n.us } ]
      ]
  , sign: []
  }

euCoordination :: Strategy
euCoordination =
  { rules:
      banded n.energyAtlantic 3 8 4
        <>
          [ rule [ gt (priceAt (named n.energyAtlantic)) (lit 3), ge (holding n.energy) (lit 8) ]
              [ procedure { id: n.euSolidarity, signers: [ n.de ], intents: [ transfer { from: n.euPool, to: n.eu, resource: n.energy, quantity: lit 8 }, tradeAt { market: named n.energyAtlantic, quantity: lit (-8), limit: Nothing } ] } ]
          , tariffMatch n.us
          , tariffMatch n.cn
          ]
  , sign: []
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: String -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: n.eu }) ]
  [ procedure { id: n.euTrade, signers: [ n.de ], intents: [ tariff.self { origin, rate: param euMatchingTariff } ] } ]

globalSmes :: Strategy
globalSmes =
  { rules:
      [ rule [ le (priceAt (named n.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named n.energyAtlantic) (mulQ (lit 40) (factor n.energy)) Nothing ]
      , rule [ gt (priceAt (named n.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named n.energyAtlantic) (mulQ (lit 32) (factor n.energy)) Nothing ]
      ]
  , sign: []
  }

cnPrivate :: Strategy
cnPrivate = { rules: banded n.energyAsia 3 12 9, sign: [] }

usWorkers :: Strategy
usWorkers = { rules: banded n.energyAtlantic 3 8 6, sign: [] }

worldHouseholds :: Strategy
worldHouseholds = { rules: banded n.energyAsia 3 12 9, sign: [] }

energySuppliers :: Strategy
energySuppliers = { rules: offers n.energyAtlantic supplierAtlantic.names <> offers n.energyAsia supplierAsia.names, sign: [] }
  where
  offers = supplierOffers { disruptionRound, highPrice: supplierHighPrice }

governmentDefault :: Strategy
governmentDefault = passive

-- | The solidarity pool has no policy: it only gives Energy when the EU's procedure draws on it.
solidarityPool :: Strategy
solidarityPool = passive

-- | A firm sells its Energy surplus (above 30) on every Energy Market it has access to.
firmDefault :: Strategy
firmDefault =
  { rules:
      [ rule [ gt (holding n.energy) (lit 30) ]
          [ orderAt (each n.energy) (subQ (lit 30) (holding n.energy)) Nothing ]
      ]
  , sign: [ signFacility n.energyShipping ]
  }
