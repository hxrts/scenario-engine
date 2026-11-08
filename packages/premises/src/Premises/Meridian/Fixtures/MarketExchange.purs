-- | Golden fixtures for direct market exchange (docs/05_test_instance.md §21, Fixture A): two
-- | Energy traders on the Atlantic Market, with and without a limit price. Shares the Energy
-- | Markets of §3.1 with the other families through `Premises.Meridian.Fixtures.Common`.
module Premises.Meridian.Fixtures.MarketExchange (fixtureA, fixtureA2) where

import Prelude (negate)

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)

import Premises.Meridian.Fixtures.Common (holding, defaultFixture, fixture)

-- | Two Energy traders in the Atlantic market: the US and Germany (fixtures A and A2).
atlanticTraders :: Map EntityId Entity
atlanticTraders = entitiesOf
  [ Tuple n.us (holding n.us [ n.money, n.energy ] [ mayTrade n.energyAtlantic ])
  , Tuple n.de (holding n.de [ n.money, n.energy ] [ mayTrade n.energyAtlantic ])
  ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A" (defaultFixture { entities = atlanticTraders, proposals = Just ([ proposal { author: n.us, intents: [ trade { market: n.energyAtlantic, quantity: q 10, limit: Nothing } ] }, proposal { author: n.de, intents: [ trade { market: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := "239.5", n.energy := 80 ]), Tuple n.de (keyed [ n.money := "135.25", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "710.25", n.energy := 345 ]) ] })) })

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2" (defaultFixture { entities = atlanticTraders, proposals = Just ([ proposal { author: n.us, intents: [ trade { market: n.energyAtlantic, quantity: q 10, limit: Just (q "2.04") } ] }, proposal { author: n.de, intents: [ trade { market: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := 260, n.energy := 70 ]), Tuple n.de (keyed [ n.money := "134.75", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "690.25", n.energy := 355 ]) ], rejected = Just ([ ref n.us ]) })) })

