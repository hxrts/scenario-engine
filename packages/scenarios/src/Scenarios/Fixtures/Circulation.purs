-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Scenarios.Fixtures.Circulation (fixtureCirculation) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (keyed, (:=))
import DSL.World (entity)
import DSL.Fixture (expect, roundOf)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.FracturedWorld (coreResources, flows, productionCapacity, requirements)

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [] }
          )
      , Tuple "DE"
          ( entity
              { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [] }
          )
      , Tuple "IN"
          ( entity
              { accounts: keyed [ "M" := 180, "E" := 65 ], capabilities: [], capacities: keyed [ "energy-production" := 5 ] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [] }
          )
      , Tuple "UK"
          ( entity
              { accounts: keyed [ "M" := 130, "E" := 30 ], capabilities: [] }
          )
      , Tuple "BR"
          ( entity
              { accounts: keyed [ "M" := 105, "E" := 85 ], capabilities: [], capacities: keyed [ "energy-production" := 13 ] }
          )
      , Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [], capacities: keyed [ "energy-production" := 25 ] }
          )
      , Tuple "NG"
          ( entity
              { accounts: keyed [ "M" := 85, "E" := 60 ], capabilities: [], capacities: keyed [ "energy-production" := 12 ] }
          )
      , Tuple "ENCO"
          ( entity
              { accounts: keyed [ "M" := 140, "E" := 140 ], capabilities: [], capacities: keyed [ "energy-production" := 25 ] }
          )
      , Tuple "CHIP"
          ( entity
              { accounts: keyed [ "M" := 135, "E" := 55 ], capabilities: [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30 ], capabilities: [] }
          )
      , Tuple "LABOR"
          ( entity
              { accounts: keyed [ "M" := 30, "E" := 4 ], capabilities: [] }
          )
      , Tuple "SME"
          ( entity
              { accounts: keyed [ "M" := 15, "E" := 5 ], capabilities: [] }
          )
      , Tuple "AID"
          ( entity
              { accounts: keyed [ "M" := 45, "E" := 12 ], capabilities: [] }
          )
      , Tuple "CN"
          ( entity
              { accounts: keyed [ "M" := 255, "E" := 95 ], capabilities: [] }
          )
      , Tuple "EU"
          ( entity
              { accounts: keyed [ "M" := 320, "E" := 125 ], capabilities: [] }
          )
      , Tuple "CN_PRIVATE"
          ( entity
              { accounts: keyed [ "M" := 210, "E" := 85 ], capabilities: [] }
          )
      , Tuple "US_WORKERS"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 60 ], capabilities: [] }
          )
      , Tuple "GLOBAL_SMES"
          ( entity
              { accounts: keyed [ "M" := 300, "E" := 80 ], capabilities: [] }
          )
      , Tuple "ENERGY_SUPPLIERS"
          ( entity
              { accounts: keyed [ "M" := 200, "E" := 240 ], capabilities: [], capacities: keyed [ "energy-production" := 60 ] }
          )
      , Tuple "WORLD_HOUSEHOLDS"
          ( entity
              { accounts: keyed [ "M" := 360, "E" := 90 ], capabilities: [] }
          )
      ]
  , circulation: { production: productionCapacity, flows, requirements }
  , rounds:
      [ roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf { expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 300 ]), Tuple "DE" (keyed [ "M" := 131, "E" := 0 ]) ] } }
      ]
  }
