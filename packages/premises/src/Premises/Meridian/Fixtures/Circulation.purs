-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Premises.Meridian.Fixtures.Circulation (fixtureCirculation) where

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)

import Premises.Meridian.Fixtures.Common (holding, defaultFixture, fixture)
import Premises.Meridian (flows, productionCapacity, requirements)
import Premises.Meridian.Holdings (pickAccounts)

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple n.us
              ( (holding n.us [ n.money, n.energy ] [])
              )
          , Tuple n.de
              ( (holding n.de [ n.money, n.energy ] [])
              )
          , Tuple n.india
              ( (noEntity { accounts = pickAccounts n.india [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 5 ] })
              )
          , Tuple n.jp
              ( (holding n.jp [ n.money, n.energy ] [])
              )
          , Tuple n.uk
              ( (holding n.uk [ n.money, n.energy ] [])
              )
          , Tuple n.br
              ( (noEntity { accounts = pickAccounts n.br [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 13 ] })
              )
          , Tuple n.sa
              ( (noEntity { accounts = pickAccounts n.sa [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 25 ] })
              )
          , Tuple n.ng
              ( (noEntity { accounts = pickAccounts n.ng [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 12 ] })
              )
          , Tuple n.enco
              ( (noEntity { accounts = pickAccounts n.enco [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 25 ] })
              )
          , Tuple n.chip
              ( (holding n.chip [ n.money, n.energy ] [])
              )
          , Tuple n.ship
              ( (holding n.ship [ n.money, n.energy ] [])
              )
          , Tuple n.def
              ( (holding n.def [ n.money, n.energy ] [])
              )
          , Tuple n.labor
              ( (holding n.labor [ n.money, n.energy ] [])
              )
          , Tuple n.sme
              ( (holding n.sme [ n.money, n.energy ] [])
              )
          , Tuple n.aid
              ( (holding n.aid [ n.money, n.energy ] [])
              )
          , Tuple n.cn
              ( (holding n.cn [ n.money, n.energy ] [])
              )
          , Tuple n.eu
              ( (holding n.eu [ n.money, n.energy ] [])
              )
          , Tuple n.cnPrivate
              ( (holding n.cnPrivate [ n.money, n.energy ] [])
              )
          , Tuple n.usWorkers
              ( (holding n.usWorkers [ n.money, n.energy ] [])
              )
          , Tuple n.globalSmes
              ( (holding n.globalSmes [ n.money, n.energy ] [])
              )
          , Tuple n.energySuppliers
              ( (noEntity { accounts = pickAccounts n.energySuppliers [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.energyProduction := 60 ] })
              )
          , Tuple n.worldHouseholds
              ( (holding n.worldHouseholds [ n.money, n.energy ] [])
              )
          ]
      , circulation = Just { production: productionCapacity, flows: flows, shares: [], requirements: requirements }
      , rounds = Just
          ( [ noRound
            , noRound
            , noRound
            , noRound
            , (noRound { expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := 300 ]), Tuple n.de (keyed [ n.money := 131, n.energy := 0 ]) ]) })) })
            ]
          )
      }
  )
