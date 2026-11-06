-- | Golden fixtures for simultaneous conflicting orders (docs/05_test_instance.md §26, Fixture F):
-- | three governments order defence procurement in the same round, and the procurement facility
-- | and the semiconductor capacity allocate what they can. Fixture F.
module Scenarios.Fixtures.ConflictingActions (fixtureF) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, keyed, (:=), transform)
import DSL.World (entity)
import DSL.Fixture (delivery, expect, proposal, transformI)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.FracturedWorld (coreResources)

fixtureF :: Tuple String Json
fixtureF = fixture "F"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "UK"
          ( entity
              { accounts: keyed [ "M" := 130, "E" := 30 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30 ], capabilities: [], capacities: keyed [] }
          )
      , Tuple "CHIP"
          ( entity
              { accounts: keyed [], capabilities: [], capacities: keyed [ "semiconductors" := 15 ] }
          )
      ]
  , proposals:
      [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "defense-procurement", quantity: q 15 } ] }, proposal { author: "UK", intents: [ transformI { by: "UK", transformation: "defense-procurement", quantity: q 8 } ] }, proposal { author: "JP", intents: [ transformI { by: "JP", transformation: "defense-procurement", quantity: q 9 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 43, "E" := 42 ]), Tuple "UK" (keyed [ "M" := 6, "E" := 14 ]), Tuple "JP" (keyed [ "M" := 26, "E" := 24 ]), Tuple "DEF" (keyed [ "M" := 575, "E" := 30 ]) ], deliveries: [ delivery { entity: "JP", resource: "Mil", quantity: q 8 }, delivery { entity: "UK", resource: "Mil", quantity: q 8 }, delivery { entity: "US", resource: "Mil", quantity: q 14 } ] }
  }

