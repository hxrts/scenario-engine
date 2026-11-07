-- | Meridian's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just premises` compiles it to
-- | `packages/premises/meridian/v0/policies.json`.
module Premises.Meridian.Policies (policies) where

import Prelude (negate, (<>))

import Data.Maybe (Maybe(..))

import Core.Strategy (Clause, Expr, Strategy, Template)
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (asTemplate, eq, factor, ge, gt, holding, homeMarket, le, lit, max', mul, not', param, price, procedure, reciprocate, restrictedLastRound, restrictionInForce, round, rule, selfEntity, signFacility, strategy, sub, transformedLastRound)
import Schema.Build.Intent (enactTariff, trade, transfer, transform)
import Premises.Meridian.Parameters (disruptionRound, euMatchingTariff, majorActionThreshold, supplierAsia, supplierAtlantic, supplierHighPrice)
import Premises.Names as N

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

-- | A market order with no limit price.
order :: String -> Expr -> Template
order m quantity = asTemplate (trade { market: m, quantity, limit: Nothing })

chinaStrategic :: Strategy
chinaStrategic = strategy
  { rules:
      [ rule [ transformedLastRound { by: N.us, transformation: N.majorMilitaryAction, atLeast: param majorActionThreshold } ]
          [ asTemplate (transform { by: selfEntity, transformation: N.defenseProcurementCn, quantity: lit 4 }) ]
      , rule [ gt (price N.energyAsia) (lit "3.2") ]
          [ order N.energyAsia (lit (-12)) ]
      , rule [ restrictedLastRound { by: N.us, target: N.cn } ]
          [ reciprocate { target: N.us } ]
      ]
  , sign: []
  }

euCoordination :: Strategy
euCoordination = strategy
  { rules:
      banded N.energyAtlantic 3 8 4
        <>
          [ rule [ gt (price N.energyAtlantic) (lit 3), ge (holding N.energy) (lit 8) ]
              [ procedure { id: N.euSolidarity, signers: [ N.de ], intents: [ transfer { from: N.euPool, to: N.eu, resource: N.energy, quantity: lit 8 }, trade { market: N.energyAtlantic, quantity: lit (-8), limit: Nothing } ] } ]
          , tariffMatch N.us
          , tariffMatch N.cn
          ]
  , sign: []
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: String -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: N.eu }) ]
  [ procedure { id: N.euTrade, signers: [ N.de ], intents: [ enactTariff { jurisdiction: selfEntity, origin, rate: param euMatchingTariff } ] } ]

-- | A price band: trade `below` while the Market's price is at or below `threshold`, and `above`
-- | while it is above. Every band in the catalog is this shape.
banded :: String -> Int -> Int -> Int -> Array Clause
banded m threshold below above =
  [ rule [ le (price m) (lit threshold) ] [ order m (lit below) ]
  , rule [ gt (price m) (lit threshold) ] [ order m (lit above) ]
  ]

globalSmes :: Strategy
globalSmes = strategy
  { rules:
      [ rule [ le (price N.energyAtlantic) (lit "3.5") ]
          [ order N.energyAtlantic (mul (lit 40) (factor N.energy)) ]
      , rule [ gt (price N.energyAtlantic) (lit "3.5") ]
          [ order N.energyAtlantic (mul (lit 32) (factor N.energy)) ]
      ]
  , sign: []
  }

cnPrivate :: Strategy
cnPrivate = strategy { rules: banded N.energyAsia 3 12 9, sign: [] }

usWorkers :: Strategy
usWorkers = strategy { rules: banded N.energyAtlantic 3 8 6, sign: [] }

worldHouseholds :: Strategy
worldHouseholds = strategy { rules: banded N.energyAsia 3 12 9, sign: [] }

-- | The Energy suppliers' offers in one Market, from the Market and the supplier parameters that
-- | the Market's suffix names (`Parameters.supplierAtlantic` and `supplierAsia`). Below the high
-- | price they offer the base volume; at or above it, the high volume. In the disruption round the
-- | offer is cut by the disruption amount, and never below zero: a cut larger than the offer
-- | removes it, and never turns it into a purchase.
supplierOffers :: String -> { offer :: String, high :: String, cut :: String } -> Array Clause
supplierOffers m names =
  [ rule [ notDisruption, low ] [ offer (sub (lit 0) (param names.offer)) ]
  , rule [ notDisruption, high ] [ offer (sub (lit 0) (param names.high)) ]
  , rule [ disruption, low ] [ offer (sub (lit 0) (max' (lit 0) (sub (param names.offer) (param names.cut)))) ]
  , rule [ disruption, high ] [ offer (sub (lit 0) (max' (lit 0) (sub (param names.high) (param names.cut)))) ]
  ]
  where
  disruption = eq round (param disruptionRound)
  notDisruption = not' disruption
  low = not' (ge (price m) (param supplierHighPrice))
  high = ge (price m) (param supplierHighPrice)
  offer quantity = order m quantity

energySuppliers :: Strategy
energySuppliers = strategy
  { rules: supplierOffers N.energyAtlantic supplierAtlantic.names <> supplierOffers N.energyAsia supplierAsia.names, sign: [] }

governmentDefault :: Strategy
governmentDefault = strategy { rules: [], sign: [] }

-- | The solidarity pool has no policy: it only gives Energy when the EU's procedure draws on it.
solidarityPool :: Strategy
solidarityPool = strategy { rules: [], sign: [] }

firmDefault :: Strategy
firmDefault = strategy
  { rules:
      [ rule [ gt (holding N.energy) (lit 30) ]
          [ order homeMarket (sub (lit 30) (holding N.energy)) ]
      ]
  , sign: [ signFacility N.energyShipping ]
  }
