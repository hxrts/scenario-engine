-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Scenarios.Fixtures.Circulation (fixtureCirculation) where

import Scenarios.Names as N
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (keyed, (:=))
import DSL.World (entity)
import DSL.Fixture (expect, roundOf)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.Meridian (coreResources, flows, productionCapacity, requirements)

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.us
          ( entity
              { accounts: keyed [ N.money := 260, N.energy := 70 ], capabilities: [] }
          )
      , Tuple N.de
          ( entity
              { accounts: keyed [ N.money := 125, N.energy := 38 ], capabilities: [] }
          )
      , Tuple N.india
          ( entity
              { accounts: keyed [ N.money := 180, N.energy := 65 ], capabilities: [], capacities: keyed [ N.energyProduction := 5 ] }
          )
      , Tuple N.jp
          ( entity
              { accounts: keyed [ N.money := 150, N.energy := 40 ], capabilities: [] }
          )
      , Tuple N.uk
          ( entity
              { accounts: keyed [ N.money := 130, N.energy := 30 ], capabilities: [] }
          )
      , Tuple N.br
          ( entity
              { accounts: keyed [ N.money := 105, N.energy := 85 ], capabilities: [], capacities: keyed [ N.energyProduction := 13 ] }
          )
      , Tuple N.sa
          ( entity
              { accounts: keyed [ N.money := 155, N.energy := 110 ], capabilities: [], capacities: keyed [ N.energyProduction := 25 ] }
          )
      , Tuple N.ng
          ( entity
              { accounts: keyed [ N.money := 85, N.energy := 60 ], capabilities: [], capacities: keyed [ N.energyProduction := 12 ] }
          )
      , Tuple N.enco
          ( entity
              { accounts: keyed [ N.money := 140, N.energy := 140 ], capabilities: [], capacities: keyed [ N.energyProduction := 25 ] }
          )
      , Tuple N.chip
          ( entity
              { accounts: keyed [ N.money := 135, N.energy := 55 ], capabilities: [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: keyed [ N.money := 110, N.energy := 65 ], capabilities: [] }
          )
      , Tuple N.def
          ( entity
              { accounts: keyed [ N.money := 110, N.energy := 30 ], capabilities: [] }
          )
      , Tuple N.labor
          ( entity
              { accounts: keyed [ N.money := 30, N.energy := 4 ], capabilities: [] }
          )
      , Tuple N.sme
          ( entity
              { accounts: keyed [ N.money := 15, N.energy := 5 ], capabilities: [] }
          )
      , Tuple N.aid
          ( entity
              { accounts: keyed [ N.money := 45, N.energy := 12 ], capabilities: [] }
          )
      , Tuple N.cn
          ( entity
              { accounts: keyed [ N.money := 255, N.energy := 95 ], capabilities: [] }
          )
      , Tuple N.eu
          ( entity
              { accounts: keyed [ N.money := 320, N.energy := 125 ], capabilities: [] }
          )
      , Tuple N.cnPrivate
          ( entity
              { accounts: keyed [ N.money := 210, N.energy := 85 ], capabilities: [] }
          )
      , Tuple N.usWorkers
          ( entity
              { accounts: keyed [ N.money := 260, N.energy := 60 ], capabilities: [] }
          )
      , Tuple N.globalSmes
          ( entity
              { accounts: keyed [ N.money := 300, N.energy := 80 ], capabilities: [] }
          )
      , Tuple N.energySuppliers
          ( entity
              { accounts: keyed [ N.money := 200, N.energy := 240 ], capabilities: [], capacities: keyed [ N.energyProduction := 60 ] }
          )
      , Tuple N.worldHouseholds
          ( entity
              { accounts: keyed [ N.money := 360, N.energy := 90 ], capabilities: [] }
          )
      ]
  , circulation: { production: productionCapacity, flows, requirements }
  , rounds:
      [ roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf { expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.money := 300 ]), Tuple N.de (keyed [ N.money := 131, N.energy := 0 ]) ] } }
      ]
  }
