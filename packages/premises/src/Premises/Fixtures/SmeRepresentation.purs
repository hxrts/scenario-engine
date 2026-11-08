-- | Golden fixtures for SME representation (docs/05_test_instance.md §23, Fixture C): the
-- | Small-Business Owner's Representation of Global SMEs' Energy demand, its stored factor applied
-- | a round later (§12.2). Fixtures C1, C2, C3 and C5; C5 checks only the seeded noise.
module Premises.Fixtures.SmeRepresentation (fixtureC1, fixtureC2, fixtureC3, fixtureC5) where

import Data.Map (Map)
import Premises.Names as N
import Core.Types (EntityKind(Aggregate), EntityId, Id(..))

import Core.Num (Q)
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (holding, defaultFixture, energy, fixture)
import Premises.Meridian (smeSample, resources)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Meridian.Entities.Aggregates (globalSmesAggregate)
import Schema.Build.Base (keyed, (:=), mayTrade)
import Schema.Build.Quantity (q)
import Schema.Build.World (entitiesOf, noEntity)
import Schema.Build.Fixture (balances, noExpect, proposal, ref)
import Schema.Build.Intent (trade)
import Core.Types (Entity, Representation)
import Data.Maybe (Maybe(..))

-- | The SME and its Representation's Aggregate (fixtures C1 to C5).
smeAndGlobalSmes :: Map EntityId Entity
smeAndGlobalSmes = entitiesOf
  [ Tuple N.sme (holding N.sme [ N.money, N.energy ] [ mayTrade N.energyAtlantic ])
  , Tuple N.globalSmes ((noEntity { accounts = pickAccounts N.globalSmes [ N.money, N.energy ], capabilities = [ mayTrade N.energyAtlantic ], kind = Aggregate (globalSmesAggregate) }))
  ]

-- | The SME Representation of the reference instance, with the fixture's noise.
smeRepresentation :: Q -> Representation
smeRepresentation noise =
  { id: Id smeSample.id
  , actor: Id smeSample.actor
  , aggregate: Id smeSample.aggregate
  , domain: Id smeSample.domain
  , observes: Id smeSample.domain
  , weight: q 1
  , estimator: smeSample.estimator noise
  }

-- | Fixtures C1 to C5: the SME's Representation of Global SMEs' Energy demand.
fixtureC1 :: Tuple String Json
fixtureC1 = fixture "C1" (defaultFixture { resources = resources, markets = energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ], entities = smeAndGlobalSmes, representations = Just ([ smeRepresentation (q 0) ]), proposals = Just ([ proposal { author: N.sme, intents: [ trade { market: N.energyAtlantic, quantity: q 4, limit: Just (q "2.1") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.sme (keyed [ N.money := "6.84", N.energy := 9 ]) ]), factors = balances ([ Tuple N.globalSmes (keyed [ N.energy := "0.95" ]) ]) })) })

fixtureC2 :: Tuple String Json
fixtureC2 = fixture "C2" (defaultFixture { resources = resources, markets = energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ], entities = smeAndGlobalSmes, representations = Just ([ smeRepresentation (q 0) ]), proposals = Just ([ proposal { author: N.sme, intents: [ trade { market: N.energyAtlantic, quantity: q 0, limit: Just (q "2.1") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.sme (keyed [ N.money := 15, N.energy := 5 ]) ]), factors = balances ([ Tuple N.globalSmes (keyed [ N.energy := "0.875" ]) ]) })) })

fixtureC3 :: Tuple String Json
fixtureC3 = fixture "C3" (defaultFixture { resources = resources, markets = energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ], entities = smeAndGlobalSmes, representations = Just ([ smeRepresentation (q 0) ]), proposals = Just ([ proposal { author: N.sme, intents: [ trade { market: N.energyAtlantic, quantity: q 4, limit: Just (q 1) } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.sme (keyed [ N.money := 15, N.energy := 5 ]) ]), factors = balances ([ Tuple N.globalSmes (keyed [ N.energy := "0.95" ]) ]), rejected = Just ([ ref N.sme ]) })) })

-- | C5 pins the seeded noise (docs/05 §23): the draw for game seed 26021 gives a factor of 0.9358. The
-- | golden test also reruns it under another seed.
fixtureC5 :: Tuple String Json
fixtureC5 = fixture "C5" (defaultFixture { resources = resources, markets = energy [ N.sme, N.globalSmes ] [ N.sa, N.jp ], entities = smeAndGlobalSmes, representations = Just ([ smeRepresentation (q "0.1") ]), proposals = Just ([ proposal { author: N.sme, intents: [ trade { market: N.energyAtlantic, quantity: q 4, limit: Just (q "2.1") } ] } ]), expect = Just (noExpect { factors = balances ([ Tuple N.globalSmes (keyed [ N.energy := "0.9358" ]) ]) }) })

