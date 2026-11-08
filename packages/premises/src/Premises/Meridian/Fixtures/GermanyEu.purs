-- | Golden fixtures for Germany and the EU (docs/05_test_instance.md §22, Fixture B): Germany
-- | transfers Energy to the EU, and the EU's collective procedures (§12.1) enact or lapse. Fixtures
-- | B1 to B4 and B4-unsigned.
module Premises.Meridian.Fixtures.GermanyEu (fixtureB1, fixtureB2, fixtureB2Pool, fixtureB3, fixtureB4Unsigned, fixtureB4) where

import Prelude (map, negate)

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)
import Premises.Meridian.Rules (tariff)

import Premises.Meridian (resources)
import Premises.Meridian.Fixtures.Common (holding, defaultFixture, energyAt, fixture)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Meridian.Entities.Aggregates (euAggregate, euPoolAggregate)

-- | Germany, the EU, and the EU's solidarity pool holding `pool` E (fixtures B1 to B4). The pool
-- | is the EU's agent in `eu-solidarity` (docs/05 §12.1); it is a separate account with no policy.
germanyAndEu :: Q -> Map EntityId Entity
germanyAndEu pool = keyed
  [ Tuple n.de (holding n.de [ n.money, n.energy ] [ mayTransfer n.energy, mayTrade n.energyAtlantic ])
  , Tuple n.eu ((noEntity { accounts = pickAccounts n.eu [ n.money, n.energy ], capabilities = [ mayTrade n.energyAtlantic, tariff.may ], kind = Aggregate (euAggregate) }))
  , Tuple n.euPool ((noEntity { accounts = keyed [ n.energy := pool ], capabilities = [], kind = Aggregate euPoolAggregate }))
  ]

-- | The Atlantic Market's access list in the Germany and EU fixtures: the pool trades within the region.
atlanticAccess :: Array String
atlanticAccess = [ n.de, n.eu, n.euPool ]

-- | Fixture B1 (docs/05 §22): Germany transfers 10 E into the pool; the Atlantic price is at or below 3.0.
fixtureB1 :: Tuple String Json
fixtureB1 = fixture "B1" (defaultFixture { resources = resources, markets = energyAt (q 0) atlanticAccess [ n.sa, n.jp ], entities = germanyAndEu (q 0), proposals = Just ([ proposal { author: n.de, intents: [ transfer { from: n.de, to: n.euPool, resource: n.energy, quantity: q 10 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.de (keyed [ n.energy := 28 ]), Tuple n.eu (keyed [ n.energy := 125 ]), Tuple n.euPool (keyed [ n.energy := 10 ]) ]) })) })

-- | Fixture B2 (docs/05 §22): high prices, Germany signs, and the pool is empty: the pool's transfer
-- | gives nothing, and the EU sells its 8 E from its own holdings.
fixtureB2 :: Tuple String Json
fixtureB2 = fixture "B2" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ n.sa, n.jp ], entities = germanyAndEu (q 0), proposals = Just ([ (proposal { author: n.eu, intents: [ transfer { from: n.euPool, to: n.eu, resource: n.energy, quantity: q 8 }, trade { market: n.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id n.euSolidarity), signers = map Id [ n.de ] }, signatures = map Id [ n.de ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.eu (keyed [ n.money := "344.96", n.energy := 117 ]), Tuple n.de (keyed [ n.money := 125, n.energy := 38 ]), Tuple n.euPool (keyed [ n.energy := 0 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "675.04", n.energy := 358 ]) ], procedures = Just ([ procedureOutcome (ref n.eu) (q "0.6") true ]) })) })

-- | Fixture B2-pool (docs/05 §22): as B2, with 10 E in the pool. The pool gives 8 E first, so the
-- | EU's own holdings end where they began (125), and the pool keeps 2 E.
fixtureB2Pool :: Tuple String Json
fixtureB2Pool = fixture "B2-pool" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ n.sa, n.jp ], entities = germanyAndEu (q 10), proposals = Just ([ (proposal { author: n.eu, intents: [ transfer { from: n.euPool, to: n.eu, resource: n.energy, quantity: q 8 }, trade { market: n.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id n.euSolidarity), signers = map Id [ n.de ] }, signatures = map Id [ n.de ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.eu (keyed [ n.money := "344.96", n.energy := 125 ]), Tuple n.euPool (keyed [ n.energy := 2 ]), Tuple n.de (keyed [ n.money := 125, n.energy := 38 ]) ]), reserves = balances [ Tuple n.energyAtlantic (keyed [ n.money := "675.04", n.energy := 358 ]) ], procedures = Just ([ procedureOutcome (ref n.eu) (q "0.6") true ]) })) })

fixtureB3 :: Tuple String Json
fixtureB3 = fixture "B3" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ n.sa, n.jp ], entities = germanyAndEu (q 0), proposals = Just ([ (proposal { author: n.eu, intents: [ trade { market: n.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id n.euSolidarity), signers = map Id [ n.de ] }, signatures = map Id [] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.eu (keyed [ n.money := 320, n.energy := 125 ]) ]), lapsed = Just ([ ref n.eu ]), procedures = Just ([ procedureOutcome (ref n.eu) (q "0.4") false ]) })) })

-- | The EU's `eu-trade` procedure enacts a tariff on the US. The variants differ only in
-- | who signed and so in the support the procedure gathers.
euTariffProcedure :: String -> Array String -> String -> Tuple String Json
euTariffProcedure name signatures support = fixture name
  ( defaultFixture
      { resources = resources
      , markets = energyAt (q 0) atlanticAccess [ n.sa, n.jp ]
      , entities = germanyAndEu (q 0)
      , proposals = Just
          [ (proposal { author: n.eu, intents: [ tariff.enact { jurisdiction: n.eu, origin: n.us, rate: q "0.5" } ] }) { proposal { procedure = Just (Id n.euTrade), signers = map Id [ n.de ] }, signatures = map Id signatures } ]
      , expect = Just (noExpect { scheduled = Just 1, procedures = Just [ procedureOutcome (ref n.eu) (q support) true ] })
      }
  )

fixtureB4Unsigned :: Tuple String Json
fixtureB4Unsigned = euTariffProcedure "B4-unsigned" [] "0.48"

fixtureB4 :: Tuple String Json
fixtureB4 = euTariffProcedure "B4" [ n.de ] "0.68"

