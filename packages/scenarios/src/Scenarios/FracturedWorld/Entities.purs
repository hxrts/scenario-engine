-- | The Fractured World's Entities: Actors with their objectives, and Aggregates with their
-- | procedures. `just scenarios` compiles them to `packages/scenarios/fractured-world/v0/entities.json`.
module Scenarios.FracturedWorld.Entities where

import Prelude (negate)

import Data.Tuple (Tuple(..))
import Scenario.DSL (Built, Keyed, aggregate, atLeast, capacityOf, collective, coverage, criterion, cumulative, definition, enactAccessBan, enactTariff, enactWithholding, entity, final, finishes, holdingOf, institutional, keyed, matching, metric, neverShort, objective, own, payeeReceipts, q, shortfallOf, shortfallRounds, trade, transfer, transform, volumeOf, (:=))

-- | Every Entity, in catalog order.
entities :: Keyed (Built "entity")
entities = keyed
  [ Tuple "US" us
  , Tuple "DE" de
  , Tuple "IN" india
  , Tuple "JP" jp
  , Tuple "UK" uk
  , Tuple "BR" br
  , Tuple "SA" sa
  , Tuple "NG" ng
  , Tuple "ENCO" enco
  , Tuple "CHIP" chip
  , Tuple "SHIP" ship
  , Tuple "DEF" def
  , Tuple "LABOR" labor
  , Tuple "SME" sme
  , Tuple "AID" aid
  , Tuple "CN" cn
  , Tuple "EU" eu
  , Tuple "CN_PRIVATE" cnPrivate
  , Tuple "US_WORKERS" usWorkers
  , Tuple "GLOBAL_SMES" globalSmes
  , Tuple "ENERGY_SUPPLIERS" energySuppliers
  , Tuple "WORLD_HOUSEHOLDS" worldHouseholds
  ]

us :: Built "entity"
us = entity
  { accounts: keyed [ "M" := 260, "E" := 70, "Mil" := 95, "S" := 60 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: true
      , seat: true
      , npc: []
      , objective: objective
          { description: "Military leadership, solvent"
          , criteria:
              [ finishes "US" (holdingOf "Mil") (matching (own "CN" (holdingOf "Mil")))
              , finishes "US" (holdingOf "M") (atLeast 200)
              ]
          }
      , capability: "Tariffs and shipment bans; deployment; largest procurement budget"
      , dependency: "Energy above its holdings; procurement capacity"
      }
  }

de :: Built "entity"
de = entity
  { accounts: keyed [ "M" := 125, "E" := 38, "Mil" := 24, "S" := 45 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "public-attention", transform "humanitarian-aid", transform "military-deployment", transfer "Mil", transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: []
      , objective: objective
          { description: "Energy security"
          , criteria:
              [ neverShort "DE" "E"
              , finishes "DE" (holdingOf "M") (atLeast 110)
              ]
          }
      , capability: "Signs EU procedure Proposals; optional solidarity contributions"
      , dependency: "Atlantic Energy; tax revenue from the EU residual"
      , fallback: coverage "EU"
      }
  }

india :: Built "entity"
india = entity
  { accounts: keyed [ "M" := 180, "E" := 65, "Mil" := 65, "S" := 35 ]
  , capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", transform "energy-capacity-investment", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed [ "energy-production" := 5 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "Industrial autonomy"
          , criteria:
              [ finishes "IN" (capacityOf "energy-production") (atLeast 7)
              , neverShort "IN" "E"
              ]
          }
      , capability: "Energy production (5); tariffs"
      , dependency: "Asia Energy prices"
      , fallback: institutional "government-default-v0"
      }
  }

jp :: Built "entity"
jp = entity
  { accounts: keyed [ "M" := 150, "E" := 40, "Mil" := 36, "S" := 48 ]
  , capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "Energy security, credible defense"
          , criteria:
              [ neverShort "JP" "E"
              , finishes "JP" (holdingOf "Mil") (atLeast 36)
              ]
          }
      , capability: "Tariffs; procurement; large Asia purchasing"
      , dependency: "Imports and transport; no production"
      , fallback: institutional "government-default-v0"
      }
  }

uk :: Built "entity"
uk = entity
  { accounts: keyed [ "M" := 130, "E" := 30, "Mil" := 38, "S" := 47 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "Diplomatic influence"
          , criteria:
              [ finishes "UK" (holdingOf "S") (atLeast 47)
              , finishes "UK" (holdingOf "M") (atLeast 110)
              ]
          }
      , capability: "Tariffs; procurement; campaigns"
      , dependency: "Atlantic Energy"
      , fallback: institutional "government-default-v0"
      }
  }

br :: Built "entity"
br = entity
  { accounts: keyed [ "M" := 105, "E" := 85, "Mil" := 25, "S" := 32 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", transform "energy-capacity-investment", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed [ "energy-production" := 13 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "Stronger finances"
          , criteria:
              [ finishes "BR" (holdingOf "M") (atLeast 150)
              ]
          }
      , capability: "Energy production (13); capacity investment"
      , dependency: "Semiconductor units for investment"
      , fallback: institutional "government-default-v0"
      }
  }

sa :: Built "entity"
sa = entity
  { accounts: keyed [ "M" := 155, "E" := 110, "Mil" := 32, "S" := 28 ]
  , capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", transform "energy-capacity-investment", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed [ "energy-production" := 25 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "Energy wealth"
          , criteria:
              [ finishes "SA" (holdingOf "M") (atLeast 250)
              , finishes "SA" (holdingOf "E") (atLeast 60)
              ]
          }
      , capability: "Largest government production (25)"
      , dependency: "Transport for Atlantic sales"
      , fallback: institutional "government-default-v0"
      }
  }

ng :: Built "entity"
ng = entity
  { accounts: keyed [ "M" := 85, "E" := 60, "Mil" := 18, "S" := 24 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement", transform "energy-capacity-investment", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed [ "energy-production" := 12 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "government-default-v0" ]
      , objective: objective
          { description: "National capacity"
          , criteria:
              [ finishes "NG" (capacityOf "energy-production") (atLeast 14)
              , finishes "NG" (holdingOf "M") (atLeast 90)
              ]
          }
      , capability: "Energy production (12); capacity investment"
      , dependency: "Semiconductor units; investment"
      , fallback: institutional "government-default-v0"
      }
  }

enco :: Built "entity"
enco = entity
  { accounts: keyed [ "M" := 140, "E" := 140, "S" := 20 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "energy-capacity-investment", transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed [ "energy-production" := 25 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "firm-default-v0" ]
      , objective: objective
          { description: "Profit and supply growth"
          , criteria:
              [ finishes "ENCO" (holdingOf "M") (atLeast 220)
              , finishes "ENCO" (capacityOf "energy-production") (atLeast 27)
              ]
          }
      , capability: "Energy production (25); capacity investment"
      , dependency: "Semiconductor units; strikes"
      , fallback: institutional "firm-default-v0"
      }
  }

chip :: Built "entity"
chip = entity
  { accounts: keyed [ "M" := 135, "E" := 55, "S" := 30 ]
  , capabilities: [ trade "energy-asia", transfer "M", transfer "E", enactWithholding, transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed [ "semiconductors" := 15 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "firm-default-v0" ]
      , objective: objective
          { description: "Secure Energy inputs"
          , criteria:
              [ neverShort "CHIP" "E"
              , finishes "CHIP" (holdingOf "M") (atLeast 150)
              ]
          }
      , capability: "Owns 15 units/round of semiconductor capacity backing procurement; may withhold or sell units"
      , dependency: "6 E/round operating Energy"
      , fallback: institutional "firm-default-v0"
      }
  }

ship :: Built "entity"
ship = entity
  { accounts: keyed [ "M" := 110, "E" := 65, "S" := 25 ]
  , capabilities: [ trade "energy-atlantic", trade "energy-asia", transfer "M", transfer "E", transform "logistics-investment", transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed [ "transport" := 60 ]
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "firm-default-v0" ]
      , objective: objective
          { description: "Open routes, high throughput"
          , criteria:
              [ criterion { metric: metric { entities: [ "SHIP" ], measure: volumeOf "energy-shipping", negated: false }, over: cumulative, atLeast: atLeast 150 }
              , finishes "SHIP" (holdingOf "M") (atLeast 130)
              ]
          }
      , capability: "Owns the energy-shipping facility and chooses which shipments to sign; arbitrage"
      , dependency: "4 E/round fuel; shipment bans"
      , fallback: institutional "firm-default-v0"
      }
  }

def :: Built "entity"
def = entity
  { accounts: keyed [ "M" := 110, "E" := 30, "Mil" := 5, "S" := 18 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: [ "firm-default-v0" ]
      , objective: objective
          { description: "Procurement revenue"
          , criteria:
              [ criterion { metric: metric { entities: [ "DEF" ], measure: payeeReceipts, negated: false }, over: cumulative, atLeast: atLeast 300 }
              ]
          }
      , capability: "Operates procurement and receives non-Chinese revenue"
      , dependency: "CHIP's semiconductor capacity; strikes"
      , fallback: institutional "firm-default-v0"
      }
  }

labor :: Built "entity"
labor = entity
  { accounts: keyed [ "M" := 30, "E" := 4, "S" := 42 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "strike", transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: []
      , objective: objective
          { description: "Workers' position and influence"
          , criteria:
              [ finishes "US_WORKERS" (holdingOf "M") (atLeast 260)
              , finishes "LABOR" (holdingOf "S") (atLeast 30)
              ]
          }
      , capability: "Strike, declared or withheld each round"
      , dependency: "Dues; Soft Power"
      , fallback: coverage "US_WORKERS"
      }
  }

sme :: Built "entity"
sme = entity
  { accounts: keyed [ "M" := 15, "E" := 5, "S" := 12 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: []
      , objective: objective
          { description: "Business viability"
          , criteria:
              [ finishes "SME" (holdingOf "M") (atLeast 15)
              , criterion { metric: metric { entities: [ "GLOBAL_SMES" ], measure: shortfallRounds "E", negated: true }, over: final, atLeast: atLeast (-1) }
              ]
          }
      , capability: "Representation of Global SME demand"
      , dependency: "Atlantic prices; small cash"
      , fallback: coverage "GLOBAL_SMES"
      }
  }

aid :: Built "entity"
aid = entity
  { accounts: keyed [ "M" := 45, "E" := 12, "S" := 55 ]
  , capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "humanitarian-aid", transform "public-attention", transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: false
      , seat: true
      , npc: []
      , objective: objective
          { description: "Limit crisis damage"
          , criteria:
              [ criterion { metric: metric { entities: [ "EU", "CN_PRIVATE", "US_WORKERS", "GLOBAL_SMES", "ENERGY_SUPPLIERS", "WORLD_HOUSEHOLDS" ], measure: shortfallOf "E", negated: true }, over: cumulative, atLeast: atLeast (-20) }
              , finishes "AID" (holdingOf "S") (atLeast 55)
              ]
          }
      , capability: "Aid; Energy relief; Soft Power"
      , dependency: "EU donations; Energy purchases"
      , fallback: coverage "WORLD_HOUSEHOLDS"
      }
  }

cn :: Built "entity"
cn = entity
  { accounts: keyed [ "M" := 255, "E" := 95, "Mil" := 88, "S" := 57 ]
  , capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform "defense-procurement-cn", enactTariff, enactAccessBan, transform "energy-shipping" ]
  , capacities: keyed []
  , definition: definition
      { required: true
      , seat: true
      , npc: [ "china-strategic-v0" ]
      , objective: objective
          { description: "Military parity, solvent"
          , criteria:
              [ finishes "CN" (holdingOf "Mil") (matching (own "US" (holdingOf "Mil")))
              , finishes "CN" (holdingOf "M") (atLeast 200)
              ]
          }
      , capability: "Tariffs and shipment bans; deployment"
      , dependency: "Asia Energy prices; CN_PRIVATE tax base"
      }
  }

eu :: Built "entity"
eu = entity
  { accounts: keyed [ "M" := 320, "E" := 125, "Mil" := 50, "S" := 75 ], capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", enactTariff, transform "energy-shipping" ], capacities: keyed [], aggregate: aggregate { policy: "eu-coordination-v0", members: [ "DE" ], procedures: keyed [ Tuple "eu-solidarity" (collective { weights: keyed [ "DE" := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ trade "energy-atlantic" ] }), Tuple "eu-trade" (collective { weights: keyed [ "DE" := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ enactTariff ] }) ] } }

cnPrivate :: Built "entity"
cnPrivate = entity
  { accounts: keyed [ "M" := 210, "E" := 85, "S" := 25 ], capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [], aggregate: aggregate { policy: "cn-private-v0", members: [] } }

usWorkers :: Built "entity"
usWorkers = entity
  { accounts: keyed [ "M" := 260, "E" := 60, "S" := 45 ], capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [], aggregate: aggregate { policy: "us-workers-v0", members: [ "LABOR" ] } }

globalSmes :: Built "entity"
globalSmes = entity
  { accounts: keyed [ "M" := 300, "E" := 80, "S" := 25 ], capabilities: [ trade "energy-atlantic", transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [], aggregate: aggregate { policy: "global-smes-v0", members: [ "SME" ] } }

energySuppliers :: Built "entity"
energySuppliers = entity
  { accounts: keyed [ "M" := 200, "E" := 240, "S" := 22 ], capabilities: [ trade "energy-atlantic", trade "energy-asia", transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [ "energy-production" := 60 ], aggregate: aggregate { policy: "energy-suppliers-v0", members: [] } }

worldHouseholds :: Built "entity"
worldHouseholds = entity
  { accounts: keyed [ "M" := 360, "E" := 90, "S" := 35 ], capabilities: [ trade "energy-asia", transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [ "attention" := 50 ], aggregate: aggregate { policy: "world-households-v0", members: [ "AID" ] } }
