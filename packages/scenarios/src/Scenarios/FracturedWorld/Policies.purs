-- | The Fractured World's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just scenarios` compiles it to
-- | `packages/scenarios/fractured-world/v0/policies.json`.
module Scenarios.FracturedWorld.Policies (policies) where

import Prelude (negate, (<>))

import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, Built)
import DSL.Decision (Clause, enactTariffT, eq, factor, ge, gt, holding, le, lit, mul, not', param, price, procedureT, program, reciprocate, restrictedLastRound, restrictionInForce, round, rule, signFacility, sub, tradeT, transformT, transformedLastRound)

-- | The in-world Policy catalog (NPCs, Aggregates, institutional fallbacks).
policies :: Keyed (Built "program")
policies = keyed
  [ Tuple "china-strategic-v0" chinaStrategic
  , Tuple "eu-coordination-v0" euCoordination
  , Tuple "global-smes-v0" globalSmes
  , Tuple "cn-private-v0" cnPrivate
  , Tuple "us-workers-v0" usWorkers
  , Tuple "world-households-v0" worldHouseholds
  , Tuple "energy-suppliers-v0" energySuppliers
  , Tuple "government-default-v0" governmentDefault
  , Tuple "firm-default-v0" firmDefault
  ]

chinaStrategic :: Built "program"
chinaStrategic = program
  { rules:
      [ rule [ transformedLastRound { by: "US", transformation: "major-military-action", atLeast: q 12 } ]
          [ transformT { transformation: "defense-procurement-cn", quantity: (lit 4) } ]
      , rule [ gt (price "energy-asia") (lit "3.2") ]
          [ tradeT { market: "energy-asia", quantity: (lit (-12)) } ]
      , rule [ restrictedLastRound { by: "US", target: "CN" } ]
          [ reciprocate { target: "US" } ]
      ]
  }

euCoordination :: Built "program"
euCoordination = program
  { rules:
      [ rule [ le (price "energy-atlantic") (lit 3) ]
          [ tradeT { market: "energy-atlantic", quantity: (lit 8) } ]
      , rule [ gt (price "energy-atlantic") (lit 3) ]
          [ tradeT { market: "energy-atlantic", quantity: (lit 4) } ]
      , rule [ gt (price "energy-atlantic") (lit 3) ]
          [ procedureT { id: "eu-solidarity", signers: [ "DE" ], intents: [ tradeT { market: "energy-atlantic", quantity: (lit (-8)) } ] } ]
      , tariffMatch "US"
      , tariffMatch "CN"
      ]
  }

-- | The EU matches a rival's restriction with a tariff of its own, unless it already has one in force.
tariffMatch :: String -> Clause
tariffMatch origin = rule [ restrictionInForce { by: origin }, not' (restrictionInForce { by: "EU" }) ]
  [ procedureT { id: "eu-trade", signers: [ "DE" ], intents: [ enactTariffT { origin: origin, rate: (param "eu-matching-tariff") } ] } ]

globalSmes :: Built "program"
globalSmes = program
  { rules:
      [ rule [ le (price "energy-atlantic") (lit "3.5") ]
          [ tradeT { market: "energy-atlantic", quantity: (mul (lit 40) (factor "E")) } ]
      , rule [ gt (price "energy-atlantic") (lit "3.5") ]
          [ tradeT { market: "energy-atlantic", quantity: (mul (lit 32) (factor "E")) } ]
      ]
  }

cnPrivate :: Built "program"
cnPrivate = program
  { rules:
      [ rule [ le (price "energy-asia") (lit 3) ]
          [ tradeT { market: "energy-asia", quantity: (lit 12) } ]
      , rule [ gt (price "energy-asia") (lit 3) ]
          [ tradeT { market: "energy-asia", quantity: (lit 9) } ]
      ]
  }

usWorkers :: Built "program"
usWorkers = program
  { rules:
      [ rule [ le (price "energy-atlantic") (lit 3) ]
          [ tradeT { market: "energy-atlantic", quantity: (lit 8) } ]
      , rule [ gt (price "energy-atlantic") (lit 3) ]
          [ tradeT { market: "energy-atlantic", quantity: (lit 6) } ]
      ]
  }

worldHouseholds :: Built "program"
worldHouseholds = program
  { rules:
      [ rule [ le (price "energy-asia") (lit 3) ]
          [ tradeT { market: "energy-asia", quantity: (lit 12) } ]
      , rule [ gt (price "energy-asia") (lit 3) ]
          [ tradeT { market: "energy-asia", quantity: (lit 9) } ]
      ]
  }

-- | The Energy suppliers' offers in one Market, parameterized by the market and the
-- | suffix its scenario parameters carry (`supplier-offer-<suffix>`, `disruption-cut-<suffix>`).
-- | Below the high price they offer the base volume; at or above it, the high volume. In the
-- | disruption round the offer is cut by the disruption amount.
supplierOffers :: String -> String -> Array Clause
supplierOffers m suffix =
  [ rule [ notDisruption, low ] [ offer (sub (lit 0) (param base)) ]
  , rule [ notDisruption, high ] [ offer (sub (lit 0) (param baseHigh)) ]
  , rule [ disruption, low ] [ offer (sub (sub (lit 0) (param base)) (sub (lit 0) (param cut))) ]
  , rule [ disruption, high ] [ offer (sub (sub (lit 0) (param baseHigh)) (sub (lit 0) (param cut))) ]
  ]
  where
  base = "supplier-offer-" <> suffix
  baseHigh = base <> "-high"
  cut = "disruption-cut-" <> suffix
  disruption = eq round (param "disruption-round")
  notDisruption = not' disruption
  low = not' (ge (price m) (param "supplier-high-price"))
  high = ge (price m) (param "supplier-high-price")
  offer quantity = tradeT { market: m, quantity: quantity }

energySuppliers :: Built "program"
energySuppliers = program
  { rules: supplierOffers "energy-atlantic" "atlantic" <> supplierOffers "energy-asia" "asia" }

governmentDefault :: Built "program"
governmentDefault = program {}

firmDefault :: Built "program"
firmDefault = program
  { rules:
      [ rule [ gt (holding "E") (lit 30) ]
          [ tradeT { market: "$home", quantity: (sub (lit 30) (holding "E")) } ]
      ]
  , sign: [ signFacility "energy-shipping" ]
  }
