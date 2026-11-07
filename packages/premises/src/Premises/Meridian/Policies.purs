-- | Meridian's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just premises` compiles it to
-- | `packages/premises/meridian/v0/policies.json`.
module Premises.Meridian.Policies (policies) where

import Prelude (negate, (<>))

import Data.Tuple (Tuple(..))
import Schema.Build.Base (Built, Keyed, keyed)
import Schema.Build.Decision (Clause, enactTariffT, eq, factor, ge, gt, holding, le, lit, max', mul, not', param, price, procedureT, strategy, reciprocate, restrictedLastRound, restrictionInForce, round, rule, signFacility, sub, tradeT, transferT, transformT, transformedLastRound)
import Schema.Tags as T
import Premises.Meridian.Parameters (disruptionRound, euMatchingTariff, majorActionThreshold, supplierAsia, supplierAtlantic, supplierHighPrice)
import Premises.Names as N

-- | The in-world Policy catalog (NPCs, Aggregates, institutional fallbacks).
policies :: Keyed (Built "strategy")
policies = keyed
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

chinaStrategic :: Built "strategy"
chinaStrategic = strategy
  { rules:
      [ rule [ transformedLastRound { by: N.us, transformation: N.majorMilitaryAction, atLeast: param majorActionThreshold } ]
          [ transformT { transformation: N.defenseProcurementCn, quantity: (lit 4) } ]
      , rule [ gt (price N.energyAsia) (lit "3.2") ]
          [ tradeT { market: N.energyAsia, quantity: (lit (-12)) } ]
      , rule [ restrictedLastRound { by: N.us, target: N.cn } ]
          [ reciprocate { target: N.us } ]
      ]
  }

euCoordination :: Built "strategy"
euCoordination = strategy
  { rules:
      banded N.energyAtlantic 3 8 4
        <>
          [ rule [ gt (price N.energyAtlantic) (lit 3), ge (holding N.energy) (lit 8) ]
              [ procedureT { id: N.euSolidarity, signers: [ N.de ], intents: [ transferT { from: N.euPool, to: N.eu, resource: N.energy, quantity: (lit 8) }, tradeT { market: N.energyAtlantic, quantity: (lit (-8)) } ] } ]
          , tariffMatch N.us
          , tariffMatch N.cn
          ]
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: String -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: N.eu }) ]
  [ procedureT { id: N.euTrade, signers: [ N.de ], intents: [ enactTariffT { origin: origin, rate: (param euMatchingTariff) } ] } ]

-- | A price band: trade `below` while the Market's price is at or below `threshold`, and `above`
-- | while it is above. Every band in the catalog is this shape.
banded :: String -> Int -> Int -> Int -> Array Clause
banded m threshold below above =
  [ rule [ le (price m) (lit threshold) ] [ tradeT { market: m, quantity: lit below } ]
  , rule [ gt (price m) (lit threshold) ] [ tradeT { market: m, quantity: lit above } ]
  ]

globalSmes :: Built "strategy"
globalSmes = strategy
  { rules:
      [ rule [ le (price N.energyAtlantic) (lit "3.5") ]
          [ tradeT { market: N.energyAtlantic, quantity: (mul (lit 40) (factor N.energy)) } ]
      , rule [ gt (price N.energyAtlantic) (lit "3.5") ]
          [ tradeT { market: N.energyAtlantic, quantity: (mul (lit 32) (factor N.energy)) } ]
      ]
  }

cnPrivate :: Built "strategy"
cnPrivate = strategy { rules: banded N.energyAsia 3 12 9 }

usWorkers :: Built "strategy"
usWorkers = strategy { rules: banded N.energyAtlantic 3 8 6 }

worldHouseholds :: Built "strategy"
worldHouseholds = strategy { rules: banded N.energyAsia 3 12 9 }

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
  offer quantity = tradeT { market: m, quantity: quantity }

energySuppliers :: Built "strategy"
energySuppliers = strategy
  { rules: supplierOffers N.energyAtlantic supplierAtlantic.names <> supplierOffers N.energyAsia supplierAsia.names }

governmentDefault :: Built "strategy"
governmentDefault = strategy {}

-- | The solidarity pool has no policy: it only gives Energy when the EU's procedure draws on it.
solidarityPool :: Built "strategy"
solidarityPool = strategy {}

firmDefault :: Built "strategy"
firmDefault = strategy
  { rules:
      [ rule [ gt (holding N.energy) (lit 30) ]
          [ tradeT { market: T.homeMarket, quantity: (sub (lit 30) (holding N.energy)) } ]
      ]
  , sign: [ signFacility N.energyShipping ]
  }
