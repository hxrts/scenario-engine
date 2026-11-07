-- | Golden fixtures for SME representation (docs/05_test_instance.md §23, Fixture C): the
-- | Small-Business Owner's Representation of Global SMEs' Energy demand, its stored factor applied
-- | a round later (§12.2). Fixtures C1, C2, C3 and C5; C5 checks only the seeded noise.
module Scenarios.Fixtures.SmeRepresentation (fixtureC1, fixtureC2, fixtureC3, fixtureC5) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (Qty, q, Keyed, keyed, (:=), Built, trade)
import DSL.World (entity)
import DSL.Fixture (expect, proposal, ref, representation, tradeI)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.Meridian (smeSample)
import Scenarios.Meridian.Entities.Aggregates (globalSmesAggregate)

-- | The SME and its Representation's Aggregate (fixtures C1 to C5).
smeAndGlobalSmes :: Keyed (Built "entity")
smeAndGlobalSmes = keyed
  [ Tuple "SME" (entity { accounts: keyed [ "M" := 15, "E" := 5 ], capabilities: [ trade "energy-atlantic" ] })
  , Tuple "GLOBAL_SMES" (entity { accounts: keyed [ "M" := 300, "E" := 80 ], capabilities: [ trade "energy-atlantic" ], aggregate: globalSmesAggregate })
  ]

-- | The SME Representation of the reference instance, with the fixture's noise.
smeRepresentation :: Qty -> Built "representation"
smeRepresentation noise = representation
  { id: smeSample.id
  , actor: smeSample.actor
  , aggregate: smeSample.aggregate
  , domain: smeSample.domain
  , weight: q 1
  , reference: smeSample.reference
  , shrinkage: smeSample.shrinkage
  , bound: smeSample.bound
  , noiseSpread: noise
  }

-- | Fixtures C1 to C5: the SME's Representation of Global SMEs' Energy demand.
fixtureC1 :: Tuple String Json
fixtureC1 = fixture "C1"
  { markets: energy [ "SME", "GLOBAL_SMES" ] [ "SA", "JP" ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: "SME", intents: [ tradeI { market: "energy-atlantic", quantity: q 4, limit: q "2.1" } ] } ]
  , expect: expect { accounts: keyed [ Tuple "SME" (keyed [ "M" := "6.84", "E" := 9 ]) ], factors: keyed [ Tuple "GLOBAL_SMES" (keyed [ "E" := "0.95" ]) ] }
  }

fixtureC2 :: Tuple String Json
fixtureC2 = fixture "C2"
  { markets: energy [ "SME", "GLOBAL_SMES" ] [ "SA", "JP" ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: "SME", intents: [ tradeI { market: "energy-atlantic", quantity: q 0, limit: q "2.1" } ] } ]
  , expect: expect { accounts: keyed [ Tuple "SME" (keyed [ "M" := 15, "E" := 5 ]) ], factors: keyed [ Tuple "GLOBAL_SMES" (keyed [ "E" := "0.875" ]) ] }
  }

fixtureC3 :: Tuple String Json
fixtureC3 = fixture "C3"
  { markets: energy [ "SME", "GLOBAL_SMES" ] [ "SA", "JP" ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: "SME", intents: [ tradeI { market: "energy-atlantic", quantity: q 4, limit: q 1 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "SME" (keyed [ "M" := 15, "E" := 5 ]) ], factors: keyed [ Tuple "GLOBAL_SMES" (keyed [ "E" := "0.95" ]) ], rejected: [ ref "SME" ] }
  }

-- | C5 checks only the seeded noise and the invariants: the golden test's C5 case compares
-- | the sampled factor against the closed form and reruns it under another seed.
fixtureC5 :: Tuple String Json
fixtureC5 = fixture "C5"
  { markets: energy [ "SME", "GLOBAL_SMES" ] [ "SA", "JP" ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q "0.1") ]
  , proposals:
      [ proposal { author: "SME", intents: [ tradeI { market: "energy-atlantic", quantity: q 4, limit: q "2.1" } ] } ]
  }

