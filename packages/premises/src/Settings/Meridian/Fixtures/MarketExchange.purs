-- | Golden fixtures for direct market exchange (docs/05_test_instance.md §21, Fixture A): two
-- | Energy traders on the Atlantic Market, with and without a limit price. Shares the Energy
-- | Markets of §3.1 with the other families through `Settings.Meridian.Fixtures.Common`.
module Settings.Meridian.Fixtures.MarketExchange (fixtureA, fixtureA2) where

import Prelude (map, negate)

import Settings.Kit.Prelude

import Settings.Meridian.Names (n)

import Settings.Kit.Fixtures (KeptEntity)
import Settings.Meridian.Fixtures.Common (cut, holding, defaultFixture, fixture, money)

-- | Two Energy traders in the Atlantic market: the US and Germany (fixtures A and A2).
atlanticTraders :: Array KeptEntity
atlanticTraders = map (\e -> holding e money [ mayTrade n.energyAtlantic ]) [ n.us, n.de ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A" (defaultFixture { cut = cut { entities = atlanticTraders }, proposals = Just ([ proposal { author: n.us, intents: [ trade { market: n.energyAtlantic, quantity: q 10, limit: Nothing } ] }, proposal { author: n.de, intents: [ trade { market: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := "239.5", n.energy := 80 ]), Tuple n.de (keyed [ n.money := "135.25", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "710.25", n.energy := 345 ]) ] })) })

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2" (defaultFixture { cut = cut { entities = atlanticTraders }, proposals = Just ([ proposal { author: n.us, intents: [ trade { market: n.energyAtlantic, quantity: q 10, limit: Just (q "2.04") } ] }, proposal { author: n.de, intents: [ trade { market: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := 260, n.energy := 70 ]), Tuple n.de (keyed [ n.money := "134.75", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "690.25", n.energy := 355 ]) ], rejected = Just ([ ref n.us ]) })) })

