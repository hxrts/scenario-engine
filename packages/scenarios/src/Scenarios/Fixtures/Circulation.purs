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
import Scenarios.Meridian.Holdings (pickAccounts)
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
              { accounts: pickAccounts N.us [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.de
          ( entity
              { accounts: pickAccounts N.de [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.india
          ( entity
              { accounts: pickAccounts N.india [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 5 ] }
          )
      , Tuple N.jp
          ( entity
              { accounts: pickAccounts N.jp [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.uk
          ( entity
              { accounts: pickAccounts N.uk [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.br
          ( entity
              { accounts: pickAccounts N.br [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 13 ] }
          )
      , Tuple N.sa
          ( entity
              { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 25 ] }
          )
      , Tuple N.ng
          ( entity
              { accounts: pickAccounts N.ng [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 12 ] }
          )
      , Tuple N.enco
          ( entity
              { accounts: pickAccounts N.enco [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 25 ] }
          )
      , Tuple N.chip
          ( entity
              { accounts: pickAccounts N.chip [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: pickAccounts N.ship [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.def
          ( entity
              { accounts: pickAccounts N.def [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.labor
          ( entity
              { accounts: pickAccounts N.labor [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.sme
          ( entity
              { accounts: pickAccounts N.sme [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.aid
          ( entity
              { accounts: pickAccounts N.aid [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.cn
          ( entity
              { accounts: pickAccounts N.cn [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.eu
          ( entity
              { accounts: pickAccounts N.eu [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.cnPrivate
          ( entity
              { accounts: pickAccounts N.cnPrivate [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.usWorkers
          ( entity
              { accounts: pickAccounts N.usWorkers [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.globalSmes
          ( entity
              { accounts: pickAccounts N.globalSmes [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.energySuppliers
          ( entity
              { accounts: pickAccounts N.energySuppliers [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.energyProduction := 60 ] }
          )
      , Tuple N.worldHouseholds
          ( entity
              { accounts: pickAccounts N.worldHouseholds [ N.money, N.energy ], capabilities: [] }
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
