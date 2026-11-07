-- | Golden fixtures for direct market exchange (docs/05_test_instance.md §21, Fixture A): two
-- | Energy traders on the Atlantic Market, with and without a limit price. Shares the Energy
-- | Markets of §3.1 with the other families through `Scenarios.Fixtures.Common`.
module Scenarios.Fixtures.MarketExchange (fixtureA, fixtureA2) where

import Prelude (negate)
import Scenarios.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, (:=), Built, trade)
import DSL.World (entity)
import DSL.Fixture (expect, proposal, ref, tradeI)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.Meridian (coreResources)

-- | Two Energy traders in the Atlantic market: the US and Germany (fixtures A and A2).
atlanticTraders :: Keyed (Built "entity")
atlanticTraders = keyed
  [ Tuple N.us (entity { accounts: keyed [ N.money := 260, N.energy := 70 ], capabilities: [ trade N.energyAtlantic ], capacities: keyed [] })
  , Tuple N.de (entity { accounts: keyed [ N.money := 125, N.energy := 38 ], capabilities: [ trade N.energyAtlantic ], capacities: keyed [] })
  ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: N.us, intents: [ tradeI { market: N.energyAtlantic, quantity: q 10 } ] }, proposal { author: N.de, intents: [ tradeI { market: N.energyAtlantic, quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.money := "239.5", N.energy := 80 ]), Tuple N.de (keyed [ N.money := "135.25", N.energy := 33 ]) ], reserves: keyed [ Tuple N.energyAtlantic (keyed [ N.money := "710.25", N.energy := 345 ]) ] }
  }

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: N.us, intents: [ tradeI { market: N.energyAtlantic, quantity: q 10, limit: q "2.04" } ] }, proposal { author: N.de, intents: [ tradeI { market: N.energyAtlantic, quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.money := 260, N.energy := 70 ]), Tuple N.de (keyed [ N.money := "134.75", N.energy := 33 ]) ], reserves: keyed [ Tuple N.energyAtlantic (keyed [ N.money := "690.25", N.energy := 355 ]) ], rejected: [ ref N.us ] }
  }

