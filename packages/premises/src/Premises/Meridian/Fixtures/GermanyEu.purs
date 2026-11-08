-- | Golden fixtures for Germany and the EU (docs/05_test_instance.md §22, Fixture B): Germany
-- | transfers Energy to the EU, and the EU's collective procedures (§12.1) enact or lapse. Fixtures
-- | B1 to B4 and B4-unsigned.
module Premises.Meridian.Fixtures.GermanyEu (fixtureB1, fixtureB2, fixtureB2Pool, fixtureB3, fixtureB4Unsigned, fixtureB4) where

import Data.Map (Map)
import Prelude (map, negate)

import Premises.Meridian.Names as N
import Premises.Meridian.Rules (tariff)
import Core.Types (EntityKind(Aggregate), EntityId, Id(..))

import Core.Num (Q)
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Meridian (resources)
import Premises.Meridian.Fixtures.Common (holding, defaultFixture, energyAt, fixture)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Meridian.Entities.Aggregates (euAggregate, euPoolAggregate)
import Schema.Build.Base (keyed, (:=), mayTrade, mayTransfer)
import Schema.Build.Quantity (q)
import Schema.Build.World (noEntity)
import Schema.Build.Fixture (balances, noExpect, procedureOutcome, proposal, ref)
import Schema.Build.Intent (trade, transfer)
import Core.Types (Entity)
import Data.Maybe (Maybe(..))

-- | Germany, the EU, and the EU's solidarity pool holding `pool` E (fixtures B1 to B4). The pool
-- | is the EU's agent in `eu-solidarity` (docs/05 §12.1); it is a separate account with no policy.
germanyAndEu :: Q -> Map EntityId Entity
germanyAndEu pool = keyed
  [ Tuple N.de (holding N.de [ N.money, N.energy ] [ mayTransfer N.energy, mayTrade N.energyAtlantic ])
  , Tuple N.eu ((noEntity { accounts = pickAccounts N.eu [ N.money, N.energy ], capabilities = [ mayTrade N.energyAtlantic, tariff.may ], kind = Aggregate (euAggregate) }))
  , Tuple N.euPool ((noEntity { accounts = keyed [ N.energy := pool ], capabilities = [], kind = Aggregate euPoolAggregate }))
  ]

-- | The Atlantic Market's access list in the Germany and EU fixtures: the pool trades within the region.
atlanticAccess :: Array String
atlanticAccess = [ N.de, N.eu, N.euPool ]

-- | Fixture B1 (docs/05 §22): Germany transfers 10 E into the pool; the Atlantic price is at or below 3.0.
fixtureB1 :: Tuple String Json
fixtureB1 = fixture "B1" (defaultFixture { resources = resources, markets = energyAt (q 0) atlanticAccess [ N.sa, N.jp ], entities = germanyAndEu (q 0), proposals = Just ([ proposal { author: N.de, intents: [ transfer { from: N.de, to: N.euPool, resource: N.energy, quantity: q 10 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.de (keyed [ N.energy := 28 ]), Tuple N.eu (keyed [ N.energy := 125 ]), Tuple N.euPool (keyed [ N.energy := 10 ]) ]) })) })

-- | Fixture B2 (docs/05 §22): high prices, Germany signs, and the pool is empty: the pool's transfer
-- | gives nothing, and the EU sells its 8 E from its own holdings.
fixtureB2 :: Tuple String Json
fixtureB2 = fixture "B2" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ N.sa, N.jp ], entities = germanyAndEu (q 0), proposals = Just ([ (proposal { author: N.eu, intents: [ transfer { from: N.euPool, to: N.eu, resource: N.energy, quantity: q 8 }, trade { market: N.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id N.euSolidarity), signers = map Id [ N.de ] }, signatures = map Id [ N.de ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.eu (keyed [ N.money := "344.96", N.energy := 117 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]), Tuple N.euPool (keyed [ N.energy := 0 ]) ]), reserves = balances [ Tuple N.energyAtlantic (keyed [ N.money := "675.04", N.energy := 358 ]) ], procedures = Just ([ procedureOutcome (ref N.eu) (q "0.6") true ]) })) })

-- | Fixture B2-pool (docs/05 §22): as B2, with 10 E in the pool. The pool gives 8 E first, so the
-- | EU's own holdings end where they began (125), and the pool keeps 2 E.
fixtureB2Pool :: Tuple String Json
fixtureB2Pool = fixture "B2-pool" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ N.sa, N.jp ], entities = germanyAndEu (q 10), proposals = Just ([ (proposal { author: N.eu, intents: [ transfer { from: N.euPool, to: N.eu, resource: N.energy, quantity: q 8 }, trade { market: N.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id N.euSolidarity), signers = map Id [ N.de ] }, signatures = map Id [ N.de ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.eu (keyed [ N.money := "344.96", N.energy := 125 ]), Tuple N.euPool (keyed [ N.energy := 2 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]) ]), reserves = balances [ Tuple N.energyAtlantic (keyed [ N.money := "675.04", N.energy := 358 ]) ], procedures = Just ([ procedureOutcome (ref N.eu) (q "0.6") true ]) })) })

fixtureB3 :: Tuple String Json
fixtureB3 = fixture "B3" (defaultFixture { resources = resources, markets = energyAt (q 60) atlanticAccess [ N.sa, N.jp ], entities = germanyAndEu (q 0), proposals = Just ([ (proposal { author: N.eu, intents: [ trade { market: N.energyAtlantic, quantity: q (-8), limit: Nothing } ] }) { proposal { procedure = Just (Id N.euSolidarity), signers = map Id [ N.de ] }, signatures = map Id [] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.eu (keyed [ N.money := 320, N.energy := 125 ]) ]), lapsed = Just ([ ref N.eu ]), procedures = Just ([ procedureOutcome (ref N.eu) (q "0.4") false ]) })) })

-- | The EU's `eu-trade` procedure enacts a tariff on the US. The variants differ only in
-- | who signed and so in the support the procedure gathers.
euTariffProcedure :: String -> Array String -> String -> Tuple String Json
euTariffProcedure name signatures support = fixture name
  ( defaultFixture
      { resources = resources
      , markets = energyAt (q 0) atlanticAccess [ N.sa, N.jp ]
      , entities = germanyAndEu (q 0)
      , proposals = Just
          [ (proposal { author: N.eu, intents: [ tariff.enact { jurisdiction: N.eu, origin: N.us, rate: q "0.5" } ] }) { proposal { procedure = Just (Id N.euTrade), signers = map Id [ N.de ] }, signatures = map Id signatures } ]
      , expect = Just (noExpect { scheduled = Just 1, procedures = Just [ procedureOutcome (ref N.eu) (q support) true ] })
      }
  )

fixtureB4Unsigned :: Tuple String Json
fixtureB4Unsigned = euTariffProcedure "B4-unsigned" [] "0.48"

fixtureB4 :: Tuple String Json
fixtureB4 = euTariffProcedure "B4" [ N.de ] "0.68"

