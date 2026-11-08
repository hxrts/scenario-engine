-- | The circulation golden fixture (docs/05_test_instance.md §8, Economic Circulation): every
-- | Entity of the reference instance, with its production (§8.1) and flows (§8.4), over five
-- | rounds of nothing but circulation. Fixture `circulation`.
module Settings.Meridian.Fixtures.Circulation (fixtureCirculation) where

import Prelude (map, (<>))

import Settings.Kit.Prelude

import Settings.Meridian.Names (n)

import Settings.Meridian (flows, productionCapacity, requirements)
import Settings.Meridian.Fixtures.Common (cut, defaultFixture, fixture, holding, keeping, money)

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  ( defaultFixture
      { cut = cut
          { circulation = { production: productionCapacity, flows, shares: [], requirements }
          , entities =
              map (\e -> holding e money [])
                [ n.us, n.de, n.india, n.jp, n.uk, n.br, n.sa, n.ng, n.enco, n.chip, n.ship, n.def, n.labor, n.sme, n.aid, n.cn, n.cnPrivate, n.usWorkers, n.globalSmes, n.energySuppliers, n.worldHouseholds ]
                <> [ keeping n.eu money, keeping n.euPool [ n.energy ] ]
          }
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
