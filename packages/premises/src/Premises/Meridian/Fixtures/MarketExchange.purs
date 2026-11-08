-- | Golden fixtures for direct market exchange (docs/05_test_instance.md §21, Fixture A): two
-- | Energy traders on the Atlantic Market, with and without a limit price. Shares the Energy
-- | Markets of §3.1 with the other families through `Premises.Meridian.Fixtures.Common`.
module Premises.Meridian.Fixtures.MarketExchange (fixtureA, fixtureA2) where

import Data.Map (Map)
import Prelude (negate)

import Premises.Meridian.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Meridian.Fixtures.Common (holding, defaultFixture, fixture)

import Schema.Build.World (entitiesOf)
import Schema.Build.Base (keyed, (:=), mayTrade)
import Schema.Build.Quantity (q)

import Schema.Build.Fixture (balances, noExpect, proposal, ref)
import Schema.Build.Intent (trade)
import Core.Types (EntityId, Entity)
import Data.Maybe (Maybe(..))

-- | Two Energy traders in the Atlantic market: the US and Germany (fixtures A and A2).
atlanticTraders :: Map EntityId Entity
atlanticTraders = entitiesOf
  [ Tuple N.us (holding N.us [ N.money, N.energy ] [ mayTrade N.energyAtlantic ])
  , Tuple N.de (holding N.de [ N.money, N.energy ] [ mayTrade N.energyAtlantic ])
  ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A" (defaultFixture { entities = atlanticTraders, proposals = Just ([ proposal { author: N.us, intents: [ trade { market: N.energyAtlantic, quantity: q 10, limit: Nothing } ] }, proposal { author: N.de, intents: [ trade { market: N.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.us (keyed [ N.money := "239.5", N.energy := 80 ]), Tuple N.de (keyed [ N.money := "135.25", N.energy := 33 ]) ]), reserves = balances [ Tuple N.energyAtlantic (keyed [ N.money := "710.25", N.energy := 345 ]) ] })) })

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2" (defaultFixture { entities = atlanticTraders, proposals = Just ([ proposal { author: N.us, intents: [ trade { market: N.energyAtlantic, quantity: q 10, limit: Just (q "2.04") } ] }, proposal { author: N.de, intents: [ trade { market: N.energyAtlantic, quantity: q (-5), limit: Nothing } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.us (keyed [ N.money := 260, N.energy := 70 ]), Tuple N.de (keyed [ N.money := "134.75", N.energy := 33 ]) ]), reserves = balances [ Tuple N.energyAtlantic (keyed [ N.money := "690.25", N.energy := 355 ]) ], rejected = Just ([ ref N.us ]) })) })

