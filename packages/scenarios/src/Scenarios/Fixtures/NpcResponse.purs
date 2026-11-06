-- | Golden fixtures for the NPC response (docs/05_test_instance.md §24, Fixture D): the US's
-- | major military action, and China answering as its NPC (§11.1) with defence procurement. The
-- | shorter D-minor carries the major action alone. Fixtures D and D-minor.
module Scenarios.Fixtures.NpcResponse (fixtureD, fixtureDMinor) where

import Prelude ((<>))

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, keyed, (:=), transform)
import DSL.World (entity)
import DSL.Fixture (delivery, expect, proposal, ref, roundOf, transformI)
import Scenarios.Fixtures.Common (energy, facilities, fixture)
import Scenarios.FracturedWorld (majorMilitaryAction, procurement)

fixtureDMinor :: Tuple String Json
fixtureDMinor = fixture "D-minor"
  { markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , transformations: facilities <> keyed [ Tuple "major-military-action" majorMilitaryAction ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70, "Mil" := 95 ], capabilities: [ transform "major-military-action" ] }
          )
      ]
  , proposals:
      [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "major-military-action", quantity: q 8 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "Mil" := 95 ]) ], rejected: [ ref "US" ] }
  }

fixtureD :: Tuple String Json
fixtureD = fixture "D"
  { markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , transformations: facilities <> keyed [ Tuple "defense-procurement-cn" (procurement "CN_PRIVATE"), Tuple "major-military-action" majorMilitaryAction ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70, "Mil" := 95 ], capabilities: [ transform "major-military-action" ] }
          )
      , Tuple "CN"
          ( entity
              { accounts: keyed [ "M" := 255, "E" := 95, "Mil" := 88 ], capabilities: [ transform "defense-procurement-cn" ] }
          )
      , Tuple "CN_PRIVATE"
          ( entity
              { accounts: keyed [ "M" := 210, "E" := 85 ], capabilities: [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30, "Mil" := 5 ], capabilities: [], capacities: keyed [] }
          )
      , Tuple "CHIP"
          ( entity
              { accounts: keyed [], capabilities: [], capacities: keyed [ "semiconductors" := 15 ] }
          )
      ]
  , rounds:
      [ roundOf
          { proposals:
              [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "major-military-action", quantity: q 12 } ] } ]
          , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "Mil" := 92, "M" := 260 ]) ] }
          }
      , roundOf
          { proposals:
              [ proposal { author: "CN", intents: [ transformI { by: "CN", transformation: "defense-procurement-cn", quantity: q 4 } ] } ]
          , expect: expect { accounts: keyed [ Tuple "CN" (keyed [ "M" := 219, "E" := 87, "Mil" := 88 ]), Tuple "CN_PRIVATE" (keyed [ "M" := 246 ]) ], deliveries: [ delivery { entity: "CN", resource: "Mil", quantity: q 4 } ] }
          }
      , roundOf { expect: expect { accounts: keyed [ Tuple "CN" (keyed [ "Mil" := 92 ]) ] } }
      ]
  }

