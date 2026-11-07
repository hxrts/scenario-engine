-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Premises.Fixtures.Circulation (fixtureCirculation) where

import Prelude (negate, (<>))
import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (defaultFixture, energy, fixture)
import Premises.Meridian (coreResources, flows, productionCapacity, requirements)
import Premises.Meridian.Holdings (pickAccounts)
import Schema.Build.Base (keyed, (:=))
import Schema.Build.World (noEntity)
import Schema.Build.Fixture (noExpect, noRound)
import Data.Maybe (Maybe(..))

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  ( defaultFixture
      { resources = coreResources
      , markets = energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
      , entities = keyed
          [ Tuple N.us
              ( (noEntity { accounts = pickAccounts N.us [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.de
              ( (noEntity { accounts = pickAccounts N.de [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.india
              ( (noEntity { accounts = pickAccounts N.india [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 5 ]) })
              )
          , Tuple N.jp
              ( (noEntity { accounts = pickAccounts N.jp [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.uk
              ( (noEntity { accounts = pickAccounts N.uk [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.br
              ( (noEntity { accounts = pickAccounts N.br [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 13 ]) })
              )
          , Tuple N.sa
              ( (noEntity { accounts = pickAccounts N.sa [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 25 ]) })
              )
          , Tuple N.ng
              ( (noEntity { accounts = pickAccounts N.ng [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 12 ]) })
              )
          , Tuple N.enco
              ( (noEntity { accounts = pickAccounts N.enco [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 25 ]) })
              )
          , Tuple N.chip
              ( (noEntity { accounts = pickAccounts N.chip [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.ship
              ( (noEntity { accounts = pickAccounts N.ship [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.def
              ( (noEntity { accounts = pickAccounts N.def [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.labor
              ( (noEntity { accounts = pickAccounts N.labor [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.sme
              ( (noEntity { accounts = pickAccounts N.sme [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.aid
              ( (noEntity { accounts = pickAccounts N.aid [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.cn
              ( (noEntity { accounts = pickAccounts N.cn [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.eu
              ( (noEntity { accounts = pickAccounts N.eu [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.cnPrivate
              ( (noEntity { accounts = pickAccounts N.cnPrivate [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.usWorkers
              ( (noEntity { accounts = pickAccounts N.usWorkers [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.globalSmes
              ( (noEntity { accounts = pickAccounts N.globalSmes [ N.money, N.energy ], capabilities = [] })
              )
          , Tuple N.energySuppliers
              ( (noEntity { accounts = pickAccounts N.energySuppliers [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.energyProduction := 60 ]) })
              )
          , Tuple N.worldHouseholds
              ( (noEntity { accounts = pickAccounts N.worldHouseholds [ N.money, N.energy ], capabilities = [] })
              )
          ]
      , circulation = Just { production: Just productionCapacity, flows: Just flows, shares: Nothing, requirements: Just requirements }
      , rounds = Just
          ( [ noRound
            , noRound
            , noRound
            , noRound
            , (noRound { expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.us (keyed [ N.money := 300 ]), Tuple N.de (keyed [ N.money := 131, N.energy := 0 ]) ]) })) })
            ]
          )
      }
  )
