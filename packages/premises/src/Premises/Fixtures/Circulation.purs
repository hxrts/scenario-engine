-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Premises.Fixtures.Circulation (fixtureCirculation) where

import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (holding, defaultFixture, fixture)
import Premises.Meridian (flows, productionCapacity, requirements)
import Premises.Meridian.Holdings (pickAccounts)
import Schema.Build.Base (keyed, (:=))
import Schema.Build.World (entitiesOf, noEntity)
import Schema.Build.Fixture (balances, noExpect, noRound)
import Data.Maybe (Maybe(..))

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple N.us
              ( (holding N.us [ N.money, N.energy ] [])
              )
          , Tuple N.de
              ( (holding N.de [ N.money, N.energy ] [])
              )
          , Tuple N.india
              ( (noEntity { accounts = pickAccounts N.india [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 5 ] })
              )
          , Tuple N.jp
              ( (holding N.jp [ N.money, N.energy ] [])
              )
          , Tuple N.uk
              ( (holding N.uk [ N.money, N.energy ] [])
              )
          , Tuple N.br
              ( (noEntity { accounts = pickAccounts N.br [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 13 ] })
              )
          , Tuple N.sa
              ( (noEntity { accounts = pickAccounts N.sa [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 25 ] })
              )
          , Tuple N.ng
              ( (noEntity { accounts = pickAccounts N.ng [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 12 ] })
              )
          , Tuple N.enco
              ( (noEntity { accounts = pickAccounts N.enco [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 25 ] })
              )
          , Tuple N.chip
              ( (holding N.chip [ N.money, N.energy ] [])
              )
          , Tuple N.ship
              ( (holding N.ship [ N.money, N.energy ] [])
              )
          , Tuple N.def
              ( (holding N.def [ N.money, N.energy ] [])
              )
          , Tuple N.labor
              ( (holding N.labor [ N.money, N.energy ] [])
              )
          , Tuple N.sme
              ( (holding N.sme [ N.money, N.energy ] [])
              )
          , Tuple N.aid
              ( (holding N.aid [ N.money, N.energy ] [])
              )
          , Tuple N.cn
              ( (holding N.cn [ N.money, N.energy ] [])
              )
          , Tuple N.eu
              ( (holding N.eu [ N.money, N.energy ] [])
              )
          , Tuple N.cnPrivate
              ( (holding N.cnPrivate [ N.money, N.energy ] [])
              )
          , Tuple N.usWorkers
              ( (holding N.usWorkers [ N.money, N.energy ] [])
              )
          , Tuple N.globalSmes
              ( (holding N.globalSmes [ N.money, N.energy ] [])
              )
          , Tuple N.energySuppliers
              ( (noEntity { accounts = pickAccounts N.energySuppliers [ N.money, N.energy ], capabilities = [], capacities = keyed [ N.energyProduction := 60 ] })
              )
          , Tuple N.worldHouseholds
              ( (holding N.worldHouseholds [ N.money, N.energy ] [])
              )
          ]
      , circulation = Just { production: productionCapacity, flows: flows, shares: [], requirements: requirements }
      , rounds = Just
          ( [ noRound
            , noRound
            , noRound
            , noRound
            , (noRound { expect = Just ((noExpect { accounts = balances ([ Tuple N.us (keyed [ N.money := 300 ]), Tuple N.de (keyed [ N.money := 131, N.energy := 0 ]) ]) })) })
            ]
          )
      }
  )
