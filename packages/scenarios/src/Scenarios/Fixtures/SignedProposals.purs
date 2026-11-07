-- | Golden fixtures for signed proposals (docs/05_test_instance.md §27, Fixture G): transfers
-- | between Saudi Arabia, Japan, Germany, the shipping facility and the other Actors, settled
-- | atomically when fully signed and lapsed otherwise; G3-ban runs under an access ban (§3.2).
-- | Fixtures G1, G2, G3-ban, G3, G4, G5-unsigned and G5.
module Scenarios.Fixtures.SignedProposals (fixtureG1, fixtureG2, fixtureG3Ban, fixtureG3, fixtureG4, fixtureG5Unsigned, fixtureG5) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, (:=), Built, transfer, transform)
import DSL.World (entity)
import DSL.Fixture (accessBanRule, expect, proposal, ref, transferI, transformI)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.Meridian (coreResources)

-- | Fixtures G1 to G5: transfers between Saudi Arabia and Japan, with and without signatures.
saAndJp :: Keyed (Built "entity")
saAndJp = keyed
  [ Tuple "SA" (entity { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] })
  , Tuple "JP" (entity { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] })
  ]

fixtureG1 :: Tuple String Json
fixtureG1 = fixture "G1"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: saAndJp
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "JP", resource: "E", quantity: q 20 }, transferI { from: "JP", to: "SA", resource: "M", quantity: q 48 } ], signers: [ "JP" ], signatures: [ "JP" ] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 203, "E" := 90 ]), Tuple "JP" (keyed [ "M" := 102, "E" := 60 ]) ] }
  }

fixtureG2 :: Tuple String Json
fixtureG2 = fixture "G2"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: saAndJp
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "JP", resource: "E", quantity: q 20 }, transferI { from: "JP", to: "SA", resource: "M", quantity: q 48 } ], signers: [ "JP" ], signatures: [] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 155, "E" := 110 ]), Tuple "JP" (keyed [ "M" := 150, "E" := 40 ]) ], lapsed: [ ref "SA" ] }
  }

fixtureG3Ban :: Tuple String Json
fixtureG3Ban = fixture "G3-ban"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , rules: [ accessBanRule { jurisdiction: "SA", target: "DE" } ]
  , entities: keyed
      [ Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [ transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [] }
          )
      , Tuple "DE"
          ( entity
              { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [], capacities: keyed [ "transport" := 60 ] }
          )
      ]
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "DE", resource: "E", quantity: q 20 }, transferI { from: "DE", to: "SA", resource: "M", quantity: q 48 }, transformI { by: "SA", transformation: "energy-shipping", quantity: q 20 } ], signers: [ "DE", "SHIP" ], signatures: [ "DE", "SHIP" ] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 155, "E" := 110 ]), Tuple "DE" (keyed [ "M" := 125, "E" := 38 ]), Tuple "SHIP" (keyed [ "M" := 110, "E" := 65 ]) ], rejected: [ ref "SA" ] }
  }

fixtureG3 :: Tuple String Json
fixtureG3 = fixture "G3"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [ transfer "M", transfer "E", transform "energy-shipping" ], capacities: keyed [] }
          )
      , Tuple "DE"
          ( entity
              { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [], capacities: keyed [ "transport" := 60 ] }
          )
      ]
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "DE", resource: "E", quantity: q 20 }, transferI { from: "DE", to: "SA", resource: "M", quantity: q 48 }, transformI { by: "SA", transformation: "energy-shipping", quantity: q 20 } ], signers: [ "DE", "SHIP" ], signatures: [ "DE", "SHIP" ] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 197, "E" := 90 ]), Tuple "DE" (keyed [ "M" := 77, "E" := 58 ]), Tuple "SHIP" (keyed [ "M" := 116, "E" := 65 ]) ] }
  }

fixtureG4 :: Tuple String Json
fixtureG4 = fixture "G4"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "LABOR"
          ( entity
              { accounts: keyed [ "M" := 30, "E" := 4 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "ENCO"
          ( entity
              { accounts: keyed [ "M" := 140, "E" := 140 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: "ENCO", intents: [ transferI { from: "ENCO", to: "LABOR", resource: "M", quantity: q 10 } ] }, proposal { author: "DEF", intents: [ transferI { from: "DEF", to: "LABOR", resource: "M", quantity: q 10 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "LABOR" (keyed [ "M" := 50, "E" := 4 ]), Tuple "ENCO" (keyed [ "M" := 130, "E" := 140 ]), Tuple "DEF" (keyed [ "M" := 100, "E" := 30 ]) ] }
  }

fixtureG5Unsigned :: Tuple String Json
fixtureG5Unsigned = fixture "G5-unsigned"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "JP", resource: "E", quantity: q 20 }, transferI { from: "JP", to: "SA", resource: "M", quantity: q 48 }, transferI { from: "JP", to: "SHIP", resource: "M", quantity: q 10 } ], signers: [ "JP", "SHIP" ], signatures: [ "JP" ] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 155, "E" := 110 ]), Tuple "JP" (keyed [ "M" := 150, "E" := 40 ]), Tuple "SHIP" (keyed [ "M" := 110, "E" := 65 ]) ], lapsed: [ ref "SA" ] }
  }

fixtureG5 :: Tuple String Json
fixtureG5 = fixture "G5"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [ transfer "M", transfer "E" ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: "SA", intents: [ transferI { from: "SA", to: "JP", resource: "E", quantity: q 20 }, transferI { from: "JP", to: "SA", resource: "M", quantity: q 48 }, transferI { from: "JP", to: "SHIP", resource: "M", quantity: q 10 } ], signers: [ "JP", "SHIP" ], signatures: [ "JP", "SHIP" ] } ]
  , expect: expect { accounts: keyed [ Tuple "SA" (keyed [ "M" := 203, "E" := 90 ]), Tuple "JP" (keyed [ "M" := 92, "E" := 60 ]), Tuple "SHIP" (keyed [ "M" := 120, "E" := 65 ]) ] }
  }

