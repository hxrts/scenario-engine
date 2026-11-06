-- | The Fractured World's in-world Policy catalog: the China NPC, Aggregate baselines, and
-- | institutional fallbacks (docs/05_test_instance.md §11). `just scenarios` compiles it to `packages/scenarios/fractured-world/v0/policies.json`.
module Scenarios.FracturedWorld.Policies where

import Prelude (negate)

import Data.Tuple (Tuple(..))
import Scenario.DSL (Built, Keyed, enactTariffT, eq, factor, ge, gt, holding, keyed, le, lit, mul, not', param, price, procedureT, program, q, reciprocate, restrictedLastRound, restrictionInForce, round, rule, signFacility, sub, tradeT, transformT, transformedLastRound)

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
      , rule [ restrictionInForce { by: "US" }, not' (restrictionInForce { by: "EU" }) ]
          [ procedureT { id: "eu-trade", signers: [ "DE" ], intents: [ enactTariffT { origin: "US", rate: (param "eu-matching-tariff") } ] } ]
      , rule [ restrictionInForce { by: "CN" }, not' (restrictionInForce { by: "EU" }) ]
          [ procedureT { id: "eu-trade", signers: [ "DE" ], intents: [ enactTariffT { origin: "CN", rate: (param "eu-matching-tariff") } ] } ]
      ]
  }

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

energySuppliers :: Built "program"
energySuppliers = program
  { rules:
      [ rule [ not' (eq round (param "disruption-round")), not' (ge (price "energy-atlantic") (param "supplier-high-price")) ]
          [ tradeT { market: "energy-atlantic", quantity: (sub (lit 0) (param "supplier-offer-atlantic")) } ]
      , rule [ not' (eq round (param "disruption-round")), ge (price "energy-atlantic") (param "supplier-high-price") ]
          [ tradeT { market: "energy-atlantic", quantity: (sub (lit 0) (param "supplier-offer-atlantic-high")) } ]
      , rule [ eq round (param "disruption-round"), not' (ge (price "energy-atlantic") (param "supplier-high-price")) ]
          [ tradeT { market: "energy-atlantic", quantity: (sub (sub (lit 0) (param "supplier-offer-atlantic")) (sub (lit 0) (param "disruption-cut-atlantic"))) } ]
      , rule [ eq round (param "disruption-round"), ge (price "energy-atlantic") (param "supplier-high-price") ]
          [ tradeT { market: "energy-atlantic", quantity: (sub (sub (lit 0) (param "supplier-offer-atlantic-high")) (sub (lit 0) (param "disruption-cut-atlantic"))) } ]
      , rule [ not' (eq round (param "disruption-round")), not' (ge (price "energy-asia") (param "supplier-high-price")) ]
          [ tradeT { market: "energy-asia", quantity: (sub (lit 0) (param "supplier-offer-asia")) } ]
      , rule [ not' (eq round (param "disruption-round")), ge (price "energy-asia") (param "supplier-high-price") ]
          [ tradeT { market: "energy-asia", quantity: (sub (lit 0) (param "supplier-offer-asia-high")) } ]
      , rule [ eq round (param "disruption-round"), not' (ge (price "energy-asia") (param "supplier-high-price")) ]
          [ tradeT { market: "energy-asia", quantity: (sub (sub (lit 0) (param "supplier-offer-asia")) (sub (lit 0) (param "disruption-cut-asia"))) } ]
      , rule [ eq round (param "disruption-round"), ge (price "energy-asia") (param "supplier-high-price") ]
          [ tradeT { market: "energy-asia", quantity: (sub (sub (lit 0) (param "supplier-offer-asia-high")) (sub (lit 0) (param "disruption-cut-asia"))) } ]
      ]
  }

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
