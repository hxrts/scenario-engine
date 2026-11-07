-- | Golden fixtures for SME representation (docs/05_test_instance.md §23, Fixture C): the
-- | Small-Business Owner's Representation of Global SMEs' Energy demand, its stored factor applied
-- | a round later (§12.2). Fixtures C1, C2, C3 and C5; C5 checks only the seeded noise.
module Premises.Fixtures.SmeRepresentation (fixtureC1, fixtureC2, fixtureC3, fixtureC5) where

import Premises.Names as N
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (Qty, q, Keyed, keyed, (:=), Built, trade)
import Schema.Build.World (entity)
import Schema.Build.Fixture (expect, proposal, ref, representation, tradeI)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Fixtures.Common (energy, fixture)
import Premises.Meridian (smeSample)
import Premises.Meridian.Entities.Aggregates (globalSmesAggregate)

-- | The SME and its Representation's Aggregate (fixtures C1 to C5).
smeAndGlobalSmes :: Keyed (Built "entity")
smeAndGlobalSmes = keyed
  [ Tuple N.sme (entity { accounts: pickAccounts N.sme [ N.money, N.energy ], capabilities: [ trade N.energyAtlantic ] })
  , Tuple N.globalSmes (entity { accounts: pickAccounts N.globalSmes [ N.money, N.energy ], capabilities: [ trade N.energyAtlantic ], aggregate: globalSmesAggregate })
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
  { markets: energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: N.sme, intents: [ tradeI { market: N.energyAtlantic, quantity: q 4, limit: q "2.1" } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sme (keyed [ N.money := "6.84", N.energy := 9 ]) ], factors: keyed [ Tuple N.globalSmes (keyed [ N.energy := "0.95" ]) ] }
  }

fixtureC2 :: Tuple String Json
fixtureC2 = fixture "C2"
  { markets: energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: N.sme, intents: [ tradeI { market: N.energyAtlantic, quantity: q 0, limit: q "2.1" } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sme (keyed [ N.money := 15, N.energy := 5 ]) ], factors: keyed [ Tuple N.globalSmes (keyed [ N.energy := "0.875" ]) ] }
  }

fixtureC3 :: Tuple String Json
fixtureC3 = fixture "C3"
  { markets: energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q 0) ]
  , proposals:
      [ proposal { author: N.sme, intents: [ tradeI { market: N.energyAtlantic, quantity: q 4, limit: q 1 } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sme (keyed [ N.money := 15, N.energy := 5 ]) ], factors: keyed [ Tuple N.globalSmes (keyed [ N.energy := "0.95" ]) ], rejected: [ ref N.sme ] }
  }

-- | C5 checks only the seeded noise and the invariants: the golden test's C5 case compares
-- | the sampled factor against the closed form and reruns it under another seed.
fixtureC5 :: Tuple String Json
fixtureC5 = fixture "C5"
  { markets: energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ]
  , entities: smeAndGlobalSmes
  , representations: [ smeRepresentation (q "0.1") ]
  , proposals:
      [ proposal { author: N.sme, intents: [ tradeI { market: N.energyAtlantic, quantity: q 4, limit: q "2.1" } ] } ]
  }

