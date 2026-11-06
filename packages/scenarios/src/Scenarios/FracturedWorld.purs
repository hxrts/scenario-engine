-- | *The Fractured World* (docs/05_test_instance.md), authored in the scenario DSL.
-- | `just scenarios` compiles it to `packages/scenarios/fractured-world/v0/*.json`.
module Scenarios.FracturedWorld
  ( files
  , gameSeed
  , years
  , resources
  , coreResources
  , atlanticTerms
  , asiaTerms
  , transport
  , productionCapacity
  , flows
  , requirements
  , procurement
  , energyShipping
  , deployment
  , majorMilitaryAction
  , defenceTerms
  , defenceAccess
  , smeSample
  ) where

import Prelude (negate, (<>))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Tuple (Tuple(..))
import Record (union)
import DSL.Base (Qty, q, Keyed, keyed, (:=), Built, free, restricted, nonTransferable, sales, payee)
import DSL.World (assignment, cost, effect, estimator, facility, flow, market, production, requirement, resource, scheme, share)
import Scenarios.FracturedWorld.Entities (entities)
import Scenarios.FracturedWorld.Policies (policies)

-- | The game seed of the reference instance (docs/05_test_instance.md §14). It is the only
-- | in-world seed; fixtures and golden tests use it too.
gameSeed :: Int
gameSeed = 26021

-- | The reference horizon in years, which is also its round count: one round per year.
years :: Int
years = 5

-- | The test-instance label of the reference game: its game id is `fractured-world/<label>`
-- | (docs/05_test_instance.md §14).
testLabel :: String
testLabel = "fractured-world-test-01"

-- | One JSON file per section, as the loader reads them.
files :: Array (Tuple String Json)
files =
  [ Tuple "scenario.json" (encodeJson { id: "fractured-world", version: "v0", numeraire: "M", horizon: years, roundRange: [ 4, 8 ], label: testLabel })
  , Tuple "resources.json" (encodeJson { resources })
  , Tuple "markets.json" (encodeJson { markets, transport, jurisdiction })
  , Tuple "transformations.json" (encodeJson { transformations })
  , Tuple "circulation.json" (encodeJson { circulation: { production: productionCapacity, flows, shares, requirements } })
  , Tuple "entities.json" (encodeJson { entities })
  , Tuple "representations.json" (encodeJson { schemes })
  , Tuple "game-rules.json" (encodeJson { parameters, rules: ([] :: Array Json) })
  , Tuple "policies.json" (encodeJson { policies })
  , Tuple "instantiation.json" (encodeJson instantiation)
  ]

resources :: Keyed (Built "resource")
resources = coreResources <> keyed [ Tuple "S" (resource { transferability: nonTransferable, decay: q "0.1" }) ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Keyed (Built "resource")
coreResources = keyed
  [ Tuple "M" (resource { transferability: free })
  , Tuple "E" (resource { transferability: free })
  , Tuple "Mil" (resource { transferability: restricted, reservable: true })
  ]

markets :: Keyed (Built "market")
markets = keyed
  [ Tuple "energy-atlantic" (market (union atlanticTerms { access: [ "US", "DE", "UK", "BR", "NG", "ENCO", "DEF", "LABOR", "SME", "AID", "EU", "US_WORKERS", "GLOBAL_SMES", "SHIP", "ENERGY_SUPPLIERS" ] }))
  , Tuple "energy-asia" (market (union asiaTerms { access: [ "CN", "IN", "JP", "SA", "CHIP", "CN_PRIVATE", "WORLD_HOUSEHOLDS", "SHIP", "ENERGY_SUPPLIERS" ] }))
  , Tuple "defence-goods" (market (union defenceTerms { access: defenceAccess }))
  ]

-- | The Entities that may trade Military: the US and its allies, Germany, the UK, and Japan.
-- | Each holds the `trade "defence-goods"` capability (Entities/Capabilities.purs), so the Market's access
-- | list and the capabilities agree.
defenceAccess :: Array String
defenceAccess = [ "US", "DE", "UK", "JP" ]

-- | The defence-goods Market (docs/05_test_instance.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Markets. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Markets sit 50 below theirs); the lower bound is the Energy
-- | Markets' −80, and the Money reserve of 300 pays sellers. Access is `defenceAccess`.
defenceTerms :: { base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty }
defenceTerms = { base: "Mil", quote: "M", cost: cost { base: q 3, slope: q "0.01", lo: q (-80), hi: q 20 }, reserves: keyed [ "M" := 300, "Mil" := 60 ] }

-- | Each regional Energy Market's curve and reserves; access is set per use.
atlanticTerms :: { base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty }
atlanticTerms = { base: "E", quote: "M", cost: cost { base: q 2, slope: q "0.02", lo: q (-80), hi: q 300 }, reserves: keyed [ "M" := 700, "E" := 350 ] }

asiaTerms :: { base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty }
asiaTerms = { base: "E", quote: "M", cost: cost { base: q 2, slope: q "0.02", lo: q (-80), hi: q 200 }, reserves: keyed [ "M" := 500, "E" := 250 ] }

-- | The facility that carries each Resource between regions.
transport :: Keyed String
transport = keyed [ Tuple "E" "energy-shipping" ]

-- | Each Entity's governing jurisdiction, where it is not its own.
jurisdiction :: Keyed String
jurisdiction = keyed
  [ Tuple "ENCO" "US"
  , Tuple "DEF" "US"
  , Tuple "LABOR" "US"
  , Tuple "SME" "US"
  , Tuple "US_WORKERS" "US"
  , Tuple "CN_PRIVATE" "CN"
  , Tuple "DE" "EU"
  ]

transformations :: Keyed (Built "facility")
transformations = keyed
  [ Tuple "defense-procurement" (procurement "DEF")
  , Tuple "defense-procurement-cn" (procurement "CN_PRIVATE")
  , Tuple "public-attention"
      (facility { outputs: keyed [ "S" := 1 ], cost: cost { base: q 3, slope: q "0.2", lo: q 0, hi: q 50 }, capacity: "attention", payee: "WORLD_HOUSEHOLDS", maximum: q 10 })
  , Tuple "energy-shipping" energyShipping
  , Tuple "humanitarian-aid"
      (facility { inputs: keyed [ "E" := 5 ], outputs: keyed [ "S" := 4 ], cost: cost { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee: "WORLD_HOUSEHOLDS", maximum: q 2 })
  , Tuple "military-deployment" deployment
  , Tuple "major-military-action" majorMilitaryAction
  , Tuple "logistics-investment"
      (facility { inputs: keyed [ "E" := 3 ], cost: cost { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee: "GLOBAL_SMES", delay: 1, effects: [ effect { capacity: "transport", perUnit: q 10 } ] })
  , Tuple "energy-capacity-investment"
      (facility { inputs: keyed [ "E" := 3 ], cost: cost { base: q 12, slope: q 0, lo: q 0, hi: q 1000 }, payee: "GLOBAL_SMES", delay: 1, capacity: "semiconductors", capacityUse: q 1, ownerSigns: true, effects: [ effect { capacity: "energy-production", perUnit: q 2 } ] })
  , Tuple "strike"
      (facility { inputs: keyed [ "S" := 8 ], cost: cost { base: q 5, slope: q 0, lo: q 0, hi: q 1 }, payee: "US_WORKERS", delay: 1, maximum: q 1, effects: [ effect { capacity: "energy-production", holder: "ENCO", perUnit: q "-6.25", rounds: 1 }, effect { capacity: "semiconductors", holder: "CHIP", perUnit: q "-3.75", rounds: 1 } ] })
  ]

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> Built "facility"
procurement payeeEntity = facility { inputs: keyed [ "E" := 2 ], outputs: keyed [ "Mil" := 1 ], cost: cost { base: q 8, slope: q "0.5", lo: q 0, hi: q 30 }, capacity: "semiconductors", capacityUse: q "0.5", delay: 1, payee: payeeEntity }

energyShipping :: Built "facility"
energyShipping = facility { cost: cost { base: q "0.3", slope: q 0, lo: q 0, hi: q 60 }, capacity: "transport", payee: "SHIP", ownerSigns: true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: Built "facility"
deployment = facility { reserves: keyed [ "Mil" := 1 ] }

majorMilitaryAction :: Built "facility"
majorMilitaryAction = facility { inputs: keyed [ "Mil" := 1 ], outputs: keyed [ "Mil" := "0.75" ], minimum: q 12 }

-- | The Entities that produce Energy (their capacities are on the Entities; this list must
-- | agree with them, which validation checks).
productionCapacity :: Array (Built "production")
productionCapacity =
  [ production { entity: "ENERGY_SUPPLIERS", resource: "E", capacity: "energy-production" }
  , production { entity: "SA", resource: "E", capacity: "energy-production" }
  , production { entity: "ENCO", resource: "E", capacity: "energy-production" }
  , production { entity: "BR", resource: "E", capacity: "energy-production" }
  , production { entity: "NG", resource: "E", capacity: "energy-production" }
  , production { entity: "IN", resource: "E", capacity: "energy-production" }
  ]

flows :: Array (Built "flow")
flows =
  [ flow { from: "US_WORKERS", to: "US", resource: "M", amount: q 15 }
  , flow { from: "GLOBAL_SMES", to: "US", resource: "M", amount: q 5 }
  , flow { from: "CN_PRIVATE", to: "CN", resource: "M", amount: q 20 }
  , flow { from: "EU", to: "DE", resource: "M", amount: q 10 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "IN", resource: "M", amount: q 10 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "JP", resource: "M", amount: q 10 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "UK", resource: "M", amount: q 8 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "BR", resource: "M", amount: q 6 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "NG", resource: "M", amount: q 4 }
  , flow { from: "US", to: "US_WORKERS", resource: "M", amount: q 12 }
  , flow { from: "CN", to: "CN_PRIVATE", resource: "M", amount: q 18 }
  , flow { from: "DE", to: "EU", resource: "M", amount: q 8 }
  , flow { from: "IN", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 8 }
  , flow { from: "JP", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 8 }
  , flow { from: "UK", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 6 }
  , flow { from: "BR", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 5 }
  , flow { from: "NG", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 3 }
  , flow { from: "ENCO", to: "US_WORKERS", resource: "M", amount: q 6 }
  , flow { from: "DEF", to: "US_WORKERS", resource: "M", amount: q 6 }
  , flow { from: "CHIP", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 6 }
  , flow { from: "SHIP", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 5 }
  , flow { from: "GLOBAL_SMES", to: "WORLD_HOUSEHOLDS", resource: "M", amount: q 10 }
  , flow { from: "GLOBAL_SMES", to: "US_WORKERS", resource: "M", amount: q 6 }
  , flow { from: "US_WORKERS", to: "GLOBAL_SMES", resource: "M", amount: q 12 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "GLOBAL_SMES", resource: "M", amount: q 12 }
  , flow { from: "US_WORKERS", to: "SME", resource: "M", amount: q 3 }
  , flow { from: "WORLD_HOUSEHOLDS", to: "SHIP", resource: "M", amount: q 4 }
  , flow { from: "US_WORKERS", to: "LABOR", resource: "M", amount: q 3 }
  , flow { from: "EU", to: "AID", resource: "M", amount: q 3 }
  , flow { from: "DEF", to: "CHIP", resource: "M", amount: q 6 }
  ]

shares :: Array (Built "share")
shares =
  [ share { from: "ENERGY_SUPPLIERS", to: "WORLD_HOUSEHOLDS", share: q "0.5", source: sales "E" }
  , share { from: "DEF", to: "US_WORKERS", share: q "0.25", source: payee }
  ]

requirements :: Array (Built "requirement")
requirements =
  [ requirement { entity: "US", resource: "E", amount: q 10, penalty: q 2 }
  , requirement { entity: "CN", resource: "E", amount: q 10, penalty: q 2 }
  , requirement { entity: "DE", resource: "E", amount: q 8, penalty: q 2 }
  , requirement { entity: "IN", resource: "E", amount: q 8, penalty: q 2 }
  , requirement { entity: "JP", resource: "E", amount: q 8, penalty: q 2 }
  , requirement { entity: "EU", resource: "E", amount: q 8, penalty: q 2 }
  , requirement { entity: "UK", resource: "E", amount: q 6, penalty: q 2 }
  , requirement { entity: "CHIP", resource: "E", amount: q 6, penalty: q 2 }
  , requirement { entity: "SHIP", resource: "E", amount: q 4, penalty: q 2 }
  , requirement { entity: "CN_PRIVATE", resource: "E", penalty: q 2 }
  , requirement { entity: "US_WORKERS", resource: "E", penalty: q 2 }
  , requirement { entity: "WORLD_HOUSEHOLDS", resource: "E", penalty: q 2 }
  , requirement { entity: "GLOBAL_SMES", resource: "E", penalty: q 2 }
  ]

-- | The SME Representation (docs/05 §12): Global SMEs' Energy demand, estimated from SME's
-- | sample. Its scheme and the fixtures' Representations share these values.
smeSample
  :: { id :: String
     , actor :: String
     , aggregate :: String
     , domain :: String
     , reference :: Qty
     , shrinkage :: Qty
     , bound :: Qty
     }
smeSample =
  { id: "sme-energy-sample"
  , actor: "SME"
  , aggregate: "GLOBAL_SMES"
  , domain: "E"
  , reference: q 5
  , shrinkage: q "0.25"
  , bound: q "0.5"
  }

schemes :: Array (Built "scheme")
schemes =
  [ scheme { id: smeSample.id, eligibleActors: [ smeSample.actor ], target: smeSample.aggregate, domain: smeSample.domain, estimator: estimator { reference: smeSample.reference, shrinkage: smeSample.shrinkage, bound: smeSample.bound, noiseSpread: q "0.1" } }
  ]

parameters :: Keyed Qty
parameters = keyed
  [ "eu-matching-tariff" := "0.5"
  , "major-action-threshold" := 12
  , "supplier-offer-atlantic" := 24
  , "supplier-offer-asia" := 16
  , "supplier-offer-atlantic-high" := 30
  , "supplier-offer-asia-high" := 20
  , "supplier-high-price" := "2.6"
  , "disruption-round" := 3
  , "disruption-cut-atlantic" := 12
  , "disruption-cut-asia" := 8
  , "maximum-import-tariff" := 1
  ]

instantiation :: { seed :: Int, rounds :: Int, seats :: Keyed String, npcs :: Keyed String, representations :: Array (Built "assignment") }
instantiation =
  { seed: gameSeed
  , rounds: years
  , seats: keyed
      [ Tuple "US" "seat-1"
      , Tuple "DE" "seat-2"
      , Tuple "IN" "seat-3"
      , Tuple "JP" "seat-4"
      , Tuple "UK" "seat-5"
      , Tuple "BR" "seat-6"
      , Tuple "SA" "seat-7"
      , Tuple "NG" "seat-8"
      , Tuple "ENCO" "seat-9"
      , Tuple "CHIP" "seat-10"
      , Tuple "SHIP" "seat-11"
      , Tuple "DEF" "seat-12"
      , Tuple "LABOR" "seat-13"
      , Tuple "SME" "seat-14"
      , Tuple "AID" "seat-15"
      ]
  , npcs: keyed [ Tuple "CN" "china-strategic-v0" ]
  , representations: [ assignment { scheme: smeSample.id, actor: smeSample.actor, weight: q 1 } ]
  }
