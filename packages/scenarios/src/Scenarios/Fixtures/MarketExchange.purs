-- | Golden fixtures for direct market exchange (docs/05_test_instance.md §21, Fixture A): two
-- | Energy traders on the Atlantic Market, with and without a limit price. Shares the Energy
-- | Markets of §3.1 with the other families through `Scenarios.Fixtures.Common`.
module Scenarios.Fixtures.MarketExchange (fixtureA, fixtureA2) where

import Prelude (negate)

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
  [ Tuple "US" (entity { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [ trade "energy-atlantic" ], capacities: keyed [] })
  , Tuple "DE" (entity { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ trade "energy-atlantic" ], capacities: keyed [] })
  ]

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: "US", intents: [ tradeI { market: "energy-atlantic", quantity: q 10 } ] }, proposal { author: "DE", intents: [ tradeI { market: "energy-atlantic", quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := "239.5", "E" := 80 ]), Tuple "DE" (keyed [ "M" := "135.25", "E" := 33 ]) ], reserves: keyed [ Tuple "energy-atlantic" (keyed [ "M" := "710.25", "E" := 345 ]) ] }
  }

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: "US", intents: [ tradeI { market: "energy-atlantic", quantity: q 10, limit: q "2.04" } ] }, proposal { author: "DE", intents: [ tradeI { market: "energy-atlantic", quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 260, "E" := 70 ]), Tuple "DE" (keyed [ "M" := "134.75", "E" := 33 ]) ], reserves: keyed [ Tuple "energy-atlantic" (keyed [ "M" := "690.25", "E" := 355 ]) ], rejected: [ ref "US" ] }
  }

