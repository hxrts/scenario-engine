-- | Golden fixtures for SME representation (docs/05_test_instance.md §23, Fixture C): the
-- | Small-Business Owner's Representation of Global SMEs' Energy demand, its stored factor applied
-- | a round later (§12.2). Fixtures C1, C2, C3 and C5; C5 checks only the seeded noise.
module Settings.Meridian.Fixtures.SmeRepresentation (fixtureC1, fixtureC2, fixtureC3, fixtureC5) where

import Rules.Types (idText)
import Settings.Kit.Prelude

import Settings.Meridian.Names (n)

import Game.Patch (setRepresentations)
import Settings.Kit.Fixtures (Cut)
import Settings.Meridian.Parameters (smeSample)
import Settings.Meridian.Fixtures.Common (allResources, cut, defaultFixture, fixture, holding, money)

-- | The SME and its Representation's Aggregate, with the Representation bound at the fixture's noise
-- | (fixtures C1 to C5).
smeAndGlobalSmes :: Q -> Cut
smeAndGlobalSmes noise = cut
  { resources = allResources
  , entities =
      [ holding n.sme money [ mayTrade n.energyAtlantic ]
      , holding n.globalSmes money [ mayTrade n.energyAtlantic ]
      ]
  , patches = [ setRepresentations [ smeRepresentation noise ] ]
  }

-- | The SME Representation of the reference instance, with the fixture's noise.
smeRepresentation :: Q -> Representation
smeRepresentation noise =
  { id: Id smeSample.id
  , actor: smeSample.actor
  , aggregate: smeSample.aggregate
  , domain: Id (idText smeSample.domain)
  , observes: smeSample.domain
  , weight: q 1
  , estimator: smeSample.estimator noise
  }

-- | Fixtures C1 to C5: the SME's Representation of Global SMEs' Energy demand.
fixtureC1 :: Tuple String Json
fixtureC1 = fixture "C1" (defaultFixture { cut = smeAndGlobalSmes (q 0), proposals = Just ([ proposal { author: n.sme, intents: [ trade { market: n.energyAtlantic, quantity: q 4, limit: Just (q "2.1") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.sme (keyed [ n.money := "6.84", n.energy := 9 ]) ]), factors = balances ([ Tuple n.globalSmes (keyed [ idText n.energy := "0.95" ]) ]) })) })

fixtureC2 :: Tuple String Json
fixtureC2 = fixture "C2" (defaultFixture { cut = smeAndGlobalSmes (q 0), proposals = Just ([ proposal { author: n.sme, intents: [ trade { market: n.energyAtlantic, quantity: q 0, limit: Just (q "2.1") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.sme (keyed [ n.money := 15, n.energy := 5 ]) ]), factors = balances ([ Tuple n.globalSmes (keyed [ idText n.energy := "0.875" ]) ]) })) })

fixtureC3 :: Tuple String Json
fixtureC3 = fixture "C3" (defaultFixture { cut = smeAndGlobalSmes (q 0), proposals = Just ([ proposal { author: n.sme, intents: [ trade { market: n.energyAtlantic, quantity: q 4, limit: Just (q 1) } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.sme (keyed [ n.money := 15, n.energy := 5 ]) ]), factors = balances ([ Tuple n.globalSmes (keyed [ idText n.energy := "0.95" ]) ]), rejected = Just ([ ref n.sme ]) })) })

-- | C5 pins the seeded noise (docs/05 §23): the draw for game seed 26021 gives a factor of 0.9358. The
-- | golden test also reruns it under another seed.
fixtureC5 :: Tuple String Json
fixtureC5 = fixture "C5" (defaultFixture { cut = smeAndGlobalSmes (q "0.1"), proposals = Just ([ proposal { author: n.sme, intents: [ trade { market: n.energyAtlantic, quantity: q 4, limit: Just (q "2.1") } ] } ]), expect = Just (noExpect { factors = balances ([ Tuple n.globalSmes (keyed [ idText n.energy := "0.9358" ]) ]) }) })

