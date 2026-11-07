-- | Golden fixtures for Germany and the EU (docs/05_test_instance.md §22, Fixture B): Germany
-- | transfers Energy to the EU, and the EU's collective procedures (§12.1) enact or lapse. Fixtures
-- | B1 to B4 and B4-unsigned.
module Scenarios.Fixtures.GermanyEu (fixtureB1, fixtureB2, fixtureB2Pool, fixtureB3, fixtureB4Unsigned, fixtureB4) where

import Prelude (negate)
import Scenarios.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Qty, Keyed, keyed, (:=), Built, transfer, trade, enactTariff)
import DSL.World (aggregate, entity)
import DSL.Fixture (enactTariffI, expect, procedureOutcome, proposal, ref, transferI, tradeI)
import Scenarios.Fixtures.Common (energyAt, fixture)
import Scenarios.Meridian.Entities.Aggregates (euAggregate)

-- | Germany, the EU, and the EU's solidarity pool holding `pool` E (fixtures B1 to B4). The pool
-- | is the EU's agent in `eu-solidarity` (docs/05 §12.1); it is a separate account with no policy.
germanyAndEu :: Qty -> Keyed (Built "entity")
germanyAndEu pool = keyed
  [ Tuple N.de (entity { accounts: keyed [ N.money := 125, N.energy := 38 ], capabilities: [ transfer N.energy, trade N.energyAtlantic ] })
  , Tuple N.eu (entity { accounts: keyed [ N.money := 320, N.energy := 125 ], capabilities: [ trade N.energyAtlantic, enactTariff ], aggregate: euAggregate })
  , Tuple N.euPool (entity { accounts: keyed [ N.energy := pool ], capabilities: [], aggregate: aggregate { policy: N.policySolidarityPool, members: [] } })
  ]

-- | The Atlantic Market's access list in the Germany and EU fixtures: the pool trades within the region.
atlanticAccess :: Array String
atlanticAccess = [ N.de, N.eu, N.euPool ]

-- | Fixture B1 (docs/05 §22): Germany transfers 10 E into the pool; the Atlantic price is at or below 3.0.
fixtureB1 :: Tuple String Json
fixtureB1 = fixture "B1"
  { markets: energyAt (q 0) atlanticAccess [ N.sa, N.jp ]
  , entities: germanyAndEu (q 0)
  , proposals:
      [ proposal { author: N.de, intents: [ transferI { from: N.de, to: N.euPool, resource: N.energy, quantity: q 10 } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.de (keyed [ N.energy := 28 ]), Tuple N.eu (keyed [ N.energy := 125 ]), Tuple N.euPool (keyed [ N.energy := 10 ]) ] }
  }

-- | Fixture B2 (docs/05 §22): high prices, Germany signs, and the pool is empty: the pool's transfer
-- | gives nothing, and the EU sells its 8 E from its own holdings.
fixtureB2 :: Tuple String Json
fixtureB2 = fixture "B2"
  { markets: energyAt (q 60) atlanticAccess [ N.sa, N.jp ]
  , entities: germanyAndEu (q 0)
  , proposals:
      [ proposal { author: N.eu, procedure: "eu-solidarity", intents: [ transferI { from: N.euPool, to: N.eu, resource: N.energy, quantity: q 8 }, tradeI { market: N.energyAtlantic, quantity: q (-8) } ], signers: [ N.de ], signatures: [ N.de ] } ]
  , expect: expect { accounts: keyed [ Tuple N.eu (keyed [ N.money := "344.96", N.energy := 117 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]), Tuple N.euPool (keyed [ N.energy := 0 ]) ], reserves: keyed [ Tuple N.energyAtlantic (keyed [ N.money := "675.04", N.energy := 358 ]) ], procedures: [ procedureOutcome { author: N.eu, support: q "0.6", executed: true } ] }
  }

-- | Fixture B2-pool (docs/05 §22): as B2, with 10 E in the pool. The pool gives 8 E first, so the
-- | EU's own holdings end where they began (125), and the pool keeps 2 E.
fixtureB2Pool :: Tuple String Json
fixtureB2Pool = fixture "B2-pool"
  { markets: energyAt (q 60) atlanticAccess [ N.sa, N.jp ]
  , entities: germanyAndEu (q 10)
  , proposals:
      [ proposal { author: N.eu, procedure: "eu-solidarity", intents: [ transferI { from: N.euPool, to: N.eu, resource: N.energy, quantity: q 8 }, tradeI { market: N.energyAtlantic, quantity: q (-8) } ], signers: [ N.de ], signatures: [ N.de ] } ]
  , expect: expect { accounts: keyed [ Tuple N.eu (keyed [ N.money := "344.96", N.energy := 125 ]), Tuple N.euPool (keyed [ N.energy := 2 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]) ], reserves: keyed [ Tuple N.energyAtlantic (keyed [ N.money := "675.04", N.energy := 358 ]) ], procedures: [ procedureOutcome { author: N.eu, support: q "0.6", executed: true } ] }
  }

fixtureB3 :: Tuple String Json
fixtureB3 = fixture "B3"
  { markets: energyAt (q 60) atlanticAccess [ N.sa, N.jp ]
  , entities: germanyAndEu (q 0)
  , proposals:
      [ proposal { author: N.eu, procedure: "eu-solidarity", intents: [ tradeI { market: N.energyAtlantic, quantity: q (-8) } ], signers: [ N.de ], signatures: [] } ]
  , expect: expect { accounts: keyed [ Tuple N.eu (keyed [ N.money := 320, N.energy := 125 ]) ], lapsed: [ ref N.eu ], procedures: [ procedureOutcome { author: N.eu, support: q "0.4", executed: false } ] }
  }

-- | The EU's `eu-trade` procedure enacts a tariff on the US. The variants differ only in
-- | who signed and so in the support the procedure gathers.
euTradeTariff :: String -> Array String -> String -> Tuple String Json
euTradeTariff name signatures support = fixture name
  { markets: energyAt (q 0) atlanticAccess [ N.sa, N.jp ]
  , entities: germanyAndEu (q 0)
  , proposals:
      [ proposal { author: N.eu, procedure: "eu-trade", intents: [ enactTariffI { jurisdiction: N.eu, origin: N.us, rate: q "0.5" } ], signers: [ N.de ], signatures: signatures } ]
  , expect: expect { scheduled: 1, procedures: [ procedureOutcome { author: N.eu, support: q support, executed: true } ] }
  }

fixtureB4Unsigned :: Tuple String Json
fixtureB4Unsigned = euTradeTariff "B4-unsigned" [] "0.48"

fixtureB4 :: Tuple String Json
fixtureB4 = euTradeTariff "B4" [ N.de ] "0.68"

