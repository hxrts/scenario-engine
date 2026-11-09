-- | Golden fixtures for direct exchange exchange (docs/05_meridian.md §21, Fixture A): two
-- | Energy traders on the Atlantic Exchange, with and without a limit price. Shares the Energy
-- | Exchanges of §3.1 with the other families through `Premises.Meridian.Fixtures.Common`.
module Premises.Meridian.Fixtures.ExchangeFill (fixtureA, fixtureA2) where

import Prelude (map, negate)

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)

import Premises.Kit.Fixtures (KeptActor)
import Premises.Meridian.Fixtures.Common (cut, holding, defaultFixture, fixture, money)

-- | Two Energy traders in the Atlantic exchange: the US and Germany (fixtures A and A2).
atlanticTraders :: Array KeptActor
atlanticTraders = map (\e -> holding e money [ mayTrade n.energyAtlantic ]) [ n.us, n.de ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A" (defaultFixture { cut = cut { actors = atlanticTraders }, candidateTransactions = Just ([ candidateTransaction { author: n.us, intents: [ trade { exchange: n.energyAtlantic, quantity: q 10, limit: Nothing } ] }, candidateTransaction { author: n.de, intents: [ trade { exchange: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := "239.5", n.energy := 80 ]), Tuple n.de (keyed [ n.money := "135.25", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "710.25", n.energy := 345 ]) ] })) })

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2" (defaultFixture { cut = cut { actors = atlanticTraders }, candidateTransactions = Just ([ candidateTransaction { author: n.us, intents: [ trade { exchange: n.energyAtlantic, quantity: q 10, limit: Just (q "2.04") } ] }, candidateTransaction { author: n.de, intents: [ trade { exchange: n.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := 260, n.energy := 70 ]), Tuple n.de (keyed [ n.money := "134.75", n.energy := 33 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "690.25", n.energy := 355 ]) ], rejected = Just ([ ref n.us ]) })) })

