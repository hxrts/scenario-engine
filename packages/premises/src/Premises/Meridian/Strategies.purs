-- | Meridian's in-world Strategy catalog: the China non-playable Actor, Aggregate baselines, and
-- | institutional fallbacks (docs/06_meridian.md §11). `just _premises` compiles it to
-- | `packages/premises/meridian/v0/strategies.json`.
module Premises.Meridian.Strategies (strategies) where

import Laws.Types (ActorId, StrategyId)
import Prelude (negate, (<>))

import Data.Maybe (Maybe(..))

import Laws.StrategyRules (Clause, StrategyRules)
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (passive, asTemplate, each, factor, ge, gt, holding, le, lit, mulQ, named, not', orderAt, param, priceAt, procedure, reciprocate, restrictedLastRound, restrictionInForce, rule, selfTransform, signTransformation, subQ, tradeAt, transformedLastRound)
import Schema.Build.Intent (transfer)
import Premises.Meridian.Parameters (disruptionRound, euMatchingTariff, majorActionThreshold, supplierAsia, supplierAtlantic, supplierHighPrice)
import Premises.Meridian.Names (n)
import Premises.Kit.StrategyRulesRules (banded, supplierOffers)
import Premises.Meridian.Rules (tariff)

-- | The in-world Strategy catalog (non-playable Actors, Aggregates, institutional fallbacks).
strategies :: Array (Tuple StrategyId StrategyRules)
strategies =
  [ Tuple n.strategyChinaStrategic chinaStrategic
  , Tuple n.strategyEuCoordination euCoordination
  , Tuple n.strategyGlobalSmes globalSmes
  , Tuple n.strategyCnPrivate cnPrivate
  , Tuple n.strategyUsWorkers usWorkers
  , Tuple n.strategyWorldHouseholds worldHouseholds
  , Tuple n.strategyEnergySuppliers energySuppliers
  , Tuple n.strategyGovernmentDefault governmentDefault
  , Tuple n.strategyFirmDefault firmDefault
  , Tuple n.strategySolidarityPool solidarityPool
  ]

chinaStrategic :: StrategyRules
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

euCoordination :: StrategyRules
euCoordination =
  { rules:
      banded n.energyAtlantic 3 8 4
        <>
          [ rule [ gt (priceAt (named n.energyAtlantic)) (lit 3), ge (holding n.energy) (lit 8) ]
              [ procedure { id: n.euSolidarity, signers: [ n.de ], intents: [ transfer { from: n.euPool, to: n.eu, resource: n.energy, quantity: lit 8 }, tradeAt { exchange: named n.energyAtlantic, quantity: lit (-8), limit: Nothing } ] } ]
          , tariffMatch n.us
          , tariffMatch n.cn
          ]
  , sign: []
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: ActorId -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: n.eu }) ]
  [ procedure { id: n.euTrade, signers: [ n.de ], intents: [ tariff.self { origin, rate: param euMatchingTariff } ] } ]

globalSmes :: StrategyRules
globalSmes =
  { rules:
      [ rule [ le (priceAt (named n.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named n.energyAtlantic) (mulQ (lit 40) (factor n.energy)) Nothing ]
      , rule [ gt (priceAt (named n.energyAtlantic)) (lit "3.5") ]
          [ orderAt (named n.energyAtlantic) (mulQ (lit 32) (factor n.energy)) Nothing ]
      ]
  , sign: []
  }

cnPrivate :: StrategyRules
cnPrivate = { rules: banded n.energyAsia 3 12 9, sign: [] }

usWorkers :: StrategyRules
usWorkers = { rules: banded n.energyAtlantic 3 8 6, sign: [] }

worldHouseholds :: StrategyRules
worldHouseholds = { rules: banded n.energyAsia 3 12 9, sign: [] }

energySuppliers :: StrategyRules
energySuppliers = { rules: offers n.energyAtlantic supplierAtlantic.names <> offers n.energyAsia supplierAsia.names, sign: [] }
  where
  offers = supplierOffers { disruptionRound, highPrice: supplierHighPrice }

governmentDefault :: StrategyRules
governmentDefault = passive

-- | The solidarity pool has no strategy: it only gives Energy when the EU's procedure draws on it.
solidarityPool :: StrategyRules
solidarityPool = passive

-- | A firm sells its Energy surplus (above 30) on every Energy Exchange it has access to.
firmDefault :: StrategyRules
firmDefault =
  { rules:
      [ rule [ gt (holding n.energy) (lit 30) ]
          [ orderAt (each n.energy) (subQ (lit 30) (holding n.energy)) Nothing ]
      ]
  , sign: [ signTransformation n.energyShipping ]
  }
