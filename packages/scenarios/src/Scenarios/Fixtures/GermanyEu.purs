-- | Golden fixtures for Germany and the EU (docs/05_test_instance.md §22, Fixture B): Germany
-- | transfers Energy to the EU, and the EU's collective procedures (§12.1) enact or lapse. Fixtures
-- | B1 to B4 and B4-unsigned.
module Scenarios.Fixtures.GermanyEu (fixtureB1, fixtureB2, fixtureB3, fixtureB4Unsigned, fixtureB4) where

import Prelude (negate)

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, (:=), Built, transfer, trade, enactTariff)
import DSL.World (entity)
import DSL.Fixture (enactTariffI, expect, procedureOutcome, proposal, ref, transferI, tradeI)
import Scenarios.Fixtures.Common (energyAt, fixture)
import Scenarios.FracturedWorld.Entities.Aggregates (euAggregate)

-- | Germany and the EU's aggregate (fixtures B1 to B4): Germany transfers Energy to the EU.
germanyAndEu :: Keyed (Built "entity")
germanyAndEu = keyed
  [ Tuple "DE" (entity { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ transfer "E", trade "energy-atlantic" ] })
  , Tuple "EU" (entity { accounts: keyed [ "M" := 320, "E" := 125 ], capabilities: [ trade "energy-atlantic", enactTariff ], aggregate: euAggregate })
  ]

-- | Fixtures B1 to B4: Germany transfers Energy to the EU, and the EU's procedures.
fixtureB1 :: Tuple String Json
fixtureB1 = fixture "B1"
  { markets: energyAt (q 0) [ "DE", "EU" ] [ "SA", "JP" ]
  , entities: germanyAndEu
  , proposals:
      [ proposal { author: "DE", intents: [ transferI { from: "DE", to: "EU", resource: "E", quantity: q 10 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "DE" (keyed [ "E" := 28 ]), Tuple "EU" (keyed [ "E" := 135 ]) ] }
  }

fixtureB2 :: Tuple String Json
fixtureB2 = fixture "B2"
  { markets: energyAt (q 60) [ "DE", "EU" ] [ "SA", "JP" ]
  , entities: germanyAndEu
  , proposals:
      [ proposal { author: "EU", procedure: "eu-solidarity", intents: [ tradeI { market: "energy-atlantic", quantity: q (-8) } ], signers: [ "DE" ], signatures: [ "DE" ] } ]
  , expect: expect { accounts: keyed [ Tuple "EU" (keyed [ "M" := "344.96", "E" := 117 ]), Tuple "DE" (keyed [ "M" := 125, "E" := 38 ]) ], reserves: keyed [ Tuple "energy-atlantic" (keyed [ "M" := "675.04", "E" := 358 ]) ], procedures: [ procedureOutcome { author: "EU", support: q "0.6", executed: true } ] }
  }

fixtureB3 :: Tuple String Json
fixtureB3 = fixture "B3"
  { markets: energyAt (q 60) [ "DE", "EU" ] [ "SA", "JP" ]
  , entities: germanyAndEu
  , proposals:
      [ proposal { author: "EU", procedure: "eu-solidarity", intents: [ tradeI { market: "energy-atlantic", quantity: q (-8) } ], signers: [ "DE" ], signatures: [] } ]
  , expect: expect { accounts: keyed [ Tuple "EU" (keyed [ "M" := 320, "E" := 125 ]) ], lapsed: [ ref "EU" ], procedures: [ procedureOutcome { author: "EU", support: q "0.4", executed: false } ] }
  }

-- | The EU's `eu-trade` procedure enacts a tariff on the US. The variants differ only in
-- | who signed and so in the support the procedure gathers.
euTradeTariff :: String -> Array String -> String -> Tuple String Json
euTradeTariff name signatures support = fixture name
  { markets: energyAt (q 0) [ "DE", "EU" ] [ "SA", "JP" ]
  , entities: germanyAndEu
  , proposals:
      [ proposal { author: "EU", procedure: "eu-trade", intents: [ enactTariffI { jurisdiction: "EU", origin: "US", rate: q "0.5" } ], signers: [ "DE" ], signatures: signatures } ]
  , expect: expect { scheduled: 1, procedures: [ procedureOutcome { author: "EU", support: q support, executed: true } ] }
  }

fixtureB4Unsigned :: Tuple String Json
fixtureB4Unsigned = euTradeTariff "B4-unsigned" [] "0.48"

fixtureB4 :: Tuple String Json
fixtureB4 = euTradeTariff "B4" [ "DE" ] "0.68"

