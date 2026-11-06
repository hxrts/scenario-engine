-- | The engine-level golden fixtures (docs/05_test_instance.md §21–§27). Every expected
-- | value is a literal from the worked examples: the DSL shares the world pieces, never
-- | the answers. `just scenarios` compiles them to `packages/scenarios/fixtures/*.json`.
-- |
-- | Fixture C5 has no expectation: it checks the seeded sampling noise (the golden test's
-- | C5 case), and its round is checked only for invariants. Every other fixture states its
-- | balances, outcomes, or both.
module Scenarios.Fixtures (fixtures) where

import Prelude (negate, (<<<), (<>))

import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prim.Row (class Union)
import Record (union)
import Scenario.DSL
import Scenarios.FracturedWorld (asiaTerms, atlanticTerms, coreResources, deployment, gameSeed, majorMilitaryAction, procurement, energyShipping, productionCapacity, flows, requirements, resources, smeSample, transport)
import Scenarios.FracturedWorld.Entities (euAggregate, globalSmesAggregate)

-- | Every fixture by name (the golden tests read these names and their JSON).
fixtures :: Array (Tuple String Json)
fixtures =
  [ fixtureA
  , fixtureA2
  , fixtureB1
  , fixtureB2
  , fixtureB3
  , fixtureB4Unsigned
  , fixtureB4
  , fixtureC1
  , fixtureC2
  , fixtureC3
  , fixtureC5
  , fixtureDMinor
  , fixtureD
  , fixtureR
  , fixtureRAlone
  , fixtureF
  , fixtureG1
  , fixtureG2
  , fixtureG3Ban
  , fixtureG3
  , fixtureG4
  , fixtureG5Unsigned
  , fixtureG5
  , fixtureCirculation
  ]

-- | What every fixture shares; each fixture gives the rest and may override these.
type FixtureFields =
  ( name :: String
  , seed :: Int
  , round :: Int
  , numeraire :: String
  , resources :: Keyed (Built "resource")
  , markets :: Keyed (Built "market")
  , transformations :: Keyed (Built "facility")
  , transport :: Keyed String
  , rules :: Array (Built "rule")
  , entities :: Keyed (Built "entity")
  , representations :: Array (Built "representation")
  , circulation :: { production :: Array (Built "production"), flows :: Array (Built "flow"), requirements :: Array (Built "requirement") }
  , proposals :: Array (Built "proposal")
  , expect :: Built "expect"
  , rounds :: Array (Built "round")
  )

fixture
  :: forall r rest
   . Union r rest FixtureFields
  => EncodeJson (Record r)
  => String
  -> Record r
  -> Tuple String Json
fixture name r = Tuple name (fromObject (Object.union (fields (encodeJson r)) (fields defaults)))
  where
  defaults = encodeJson { name, seed: gameSeed, round: 1, numeraire: "M", resources, transformations: facilities, transport, rules: ([] :: Array (Built "rule")) }
  fields = fromMaybe Object.empty <<< toObject

-- | Both regional Energy Markets with the given access lists.
energy :: Array String -> Array String -> Keyed (Built "market")
energy atlantic asia = keyed [ Tuple "energy-atlantic" (market (union atlanticTerms { access: atlantic })), Tuple "energy-asia" (market (union asiaTerms { access: asia })) ]

-- | As `energy`, with the Atlantic Market starting at a position.
energyAt :: Qty -> Array String -> Array String -> Keyed (Built "market")
energyAt position atlantic asia = keyed [ Tuple "energy-atlantic" (market (union atlanticTerms { access: atlantic, position })), Tuple "energy-asia" (market (union asiaTerms { access: asia })) ]

-- | Procurement and shipping, as the scenario defines them; fixtures add others as needed.
facilities :: Keyed (Built "facility")
facilities = keyed [ Tuple "defense-procurement" (procurement "DEF"), Tuple "energy-shipping" energyShipping ]

-- Shared entity sets -------------------------------------------------------------------

-- | Two Energy traders in the Atlantic market: the US and Germany (fixtures A and A2).
atlanticTraders :: Keyed (Built "entity")
atlanticTraders = keyed
  [ Tuple "US" (entity { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [ trade "energy-atlantic" ], capacities: keyed [] })
  , Tuple "DE" (entity { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ trade "energy-atlantic" ], capacities: keyed [] })
  ]

-- | Germany and the EU's aggregate (fixtures B1 to B4): Germany transfers Energy to the EU.
germanyAndEu :: Keyed (Built "entity")
germanyAndEu = keyed
  [ Tuple "DE" (entity { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [ transfer "E", trade "energy-atlantic" ] })
  , Tuple "EU" (entity { accounts: keyed [ "M" := 320, "E" := 125 ], capabilities: [ trade "energy-atlantic", enactTariff ], aggregate: euAggregate })
  ]

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

-- | Fixtures A and A2: two Atlantic traders, with and without a limit price.
fixtureA :: Tuple String Json
fixtureA = fixture "A"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: "US", intents: [ tradeI { market: "energy-atlantic", quantity: q 10 } ] }, proposal { author: "DE", intents: [ tradeI { market: "energy-atlantic", quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := "239.5", "E" := 80 ]), Tuple "DE" (keyed [ "M" := "135.25", "E" := 33 ]) ], reserves: keyed [ Tuple "energy-atlantic" (keyed [ "M" := "710.25", "E" := 345 ]) ] }
  }

fixtureA2 :: Tuple String Json
fixtureA2 = fixture "A2"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: atlanticTraders
  , proposals:
      [ proposal { author: "US", intents: [ tradeI { market: "energy-atlantic", quantity: q 10, limit: q "2.04" } ] }, proposal { author: "DE", intents: [ tradeI { market: "energy-atlantic", quantity: q (-5) } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 260, "E" := 70 ]), Tuple "DE" (keyed [ "M" := "134.75", "E" := 33 ]) ], reserves: keyed [ Tuple "energy-atlantic" (keyed [ "M" := "690.25", "E" := 355 ]) ], rejected: [ ref "US" ] }
  }

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

-- | Fixture R (docs/05_test_instance.md §13.3): the US orders a 60 Mil deployment, which reserves
-- | 60 Mil, and a 40-unit major action, which consumes 1 Mil per unit. Their claims on 95 Mil
-- | total 100, so both scale by 95/100: the deployment reserves 57 and the action orders 38.
-- | The action consumes 38 Mil and returns 28.5 at commit, so US Military is 95 − 38 + 28.5 =
-- | 85.5. Round 2 shows the reservation released: the 50 Mil transferred are free.
fixtureR :: Tuple String Json
fixtureR = fixture "R"
  { markets: energy [ "US" ] [ "JP" ]
  , transformations: facilities <> keyed [ Tuple "military-deployment" deployment, Tuple "major-military-action" majorMilitaryAction ]
  , entities: usAndJapan
  , rounds:
      [ roundOf
          { proposals:
              [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "military-deployment", quantity: q 60 }, transformI { by: "US", transformation: "major-military-action", quantity: q 40 } ] } ]
          , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "Mil" := "85.5", "M" := 260 ]) ] }
          }
      , roundOf
          { proposals:
              [ proposal { author: "US", intents: [ transferI { from: "US", to: "JP", resource: "Mil", quantity: q 50 } ] } ]
          , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "Mil" := "35.5" ]), Tuple "JP" (keyed [ "Mil" := 86 ]) ] }
          }
      ]
  }

-- | Fixture R-alone: the same major action with no deployment. Nothing competes with it, so its
-- | 40 units run in full: 95 − 40 + 30 = 85 Mil, against R's 85.5.
fixtureRAlone :: Tuple String Json
fixtureRAlone = fixture "R-alone"
  { markets: energy [ "US" ] [ "JP" ]
  , transformations: facilities <> keyed [ Tuple "military-deployment" deployment, Tuple "major-military-action" majorMilitaryAction ]
  , entities: usAndJapan
  , proposals:
      [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "major-military-action", quantity: q 40 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "Mil" := 85 ]) ] }
  }

-- | The two Mil holders of fixtures R and R-alone: the US seat, which may deploy and act, and Japan.
usAndJapan :: Keyed (Built "entity")
usAndJapan = keyed
  [ Tuple "US" (entity { accounts: keyed [ "M" := 260, "E" := 70, "Mil" := 95 ], capabilities: [ transform "military-deployment", transform "major-military-action", transfer "Mil" ] })
  , Tuple "JP" (entity { accounts: keyed [ "M" := 150, "E" := 40, "Mil" := 36 ], capabilities: [] })
  ]

fixtureF :: Tuple String Json
fixtureF = fixture "F"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "UK"
          ( entity
              { accounts: keyed [ "M" := 130, "E" := 30 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [ transform "defense-procurement" ], capacities: keyed [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30 ], capabilities: [], capacities: keyed [] }
          )
      , Tuple "CHIP"
          ( entity
              { accounts: keyed [], capabilities: [], capacities: keyed [ "semiconductors" := 15 ] }
          )
      ]
  , proposals:
      [ proposal { author: "US", intents: [ transformI { by: "US", transformation: "defense-procurement", quantity: q 15 } ] }, proposal { author: "UK", intents: [ transformI { by: "UK", transformation: "defense-procurement", quantity: q 8 } ] }, proposal { author: "JP", intents: [ transformI { by: "JP", transformation: "defense-procurement", quantity: q 9 } ] } ]
  , expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 43, "E" := 42 ]), Tuple "UK" (keyed [ "M" := 6, "E" := 14 ]), Tuple "JP" (keyed [ "M" := 26, "E" := 24 ]), Tuple "DEF" (keyed [ "M" := 575, "E" := 30 ]) ], deliveries: [ delivery { entity: "JP", resource: "Mil", quantity: q 8 }, delivery { entity: "UK", resource: "Mil", quantity: q 8 }, delivery { entity: "US", resource: "Mil", quantity: q 14 } ] }
  }

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

-- | The circulation fixture: every Entity of the reference instance, with its production and
-- | flows, over five rounds of nothing but circulation.
fixtureCirculation :: Tuple String Json
fixtureCirculation = fixture "circulation"
  { resources: coreResources
  , markets: energy [ "US", "DE", "UK" ] [ "SA", "JP" ]
  , entities: keyed
      [ Tuple "US"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 70 ], capabilities: [] }
          )
      , Tuple "DE"
          ( entity
              { accounts: keyed [ "M" := 125, "E" := 38 ], capabilities: [] }
          )
      , Tuple "IN"
          ( entity
              { accounts: keyed [ "M" := 180, "E" := 65 ], capabilities: [], capacities: keyed [ "energy-production" := 5 ] }
          )
      , Tuple "JP"
          ( entity
              { accounts: keyed [ "M" := 150, "E" := 40 ], capabilities: [] }
          )
      , Tuple "UK"
          ( entity
              { accounts: keyed [ "M" := 130, "E" := 30 ], capabilities: [] }
          )
      , Tuple "BR"
          ( entity
              { accounts: keyed [ "M" := 105, "E" := 85 ], capabilities: [], capacities: keyed [ "energy-production" := 13 ] }
          )
      , Tuple "SA"
          ( entity
              { accounts: keyed [ "M" := 155, "E" := 110 ], capabilities: [], capacities: keyed [ "energy-production" := 25 ] }
          )
      , Tuple "NG"
          ( entity
              { accounts: keyed [ "M" := 85, "E" := 60 ], capabilities: [], capacities: keyed [ "energy-production" := 12 ] }
          )
      , Tuple "ENCO"
          ( entity
              { accounts: keyed [ "M" := 140, "E" := 140 ], capabilities: [], capacities: keyed [ "energy-production" := 25 ] }
          )
      , Tuple "CHIP"
          ( entity
              { accounts: keyed [ "M" := 135, "E" := 55 ], capabilities: [] }
          )
      , Tuple "SHIP"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 65 ], capabilities: [] }
          )
      , Tuple "DEF"
          ( entity
              { accounts: keyed [ "M" := 110, "E" := 30 ], capabilities: [] }
          )
      , Tuple "LABOR"
          ( entity
              { accounts: keyed [ "M" := 30, "E" := 4 ], capabilities: [] }
          )
      , Tuple "SME"
          ( entity
              { accounts: keyed [ "M" := 15, "E" := 5 ], capabilities: [] }
          )
      , Tuple "AID"
          ( entity
              { accounts: keyed [ "M" := 45, "E" := 12 ], capabilities: [] }
          )
      , Tuple "CN"
          ( entity
              { accounts: keyed [ "M" := 255, "E" := 95 ], capabilities: [] }
          )
      , Tuple "EU"
          ( entity
              { accounts: keyed [ "M" := 320, "E" := 125 ], capabilities: [] }
          )
      , Tuple "CN_PRIVATE"
          ( entity
              { accounts: keyed [ "M" := 210, "E" := 85 ], capabilities: [] }
          )
      , Tuple "US_WORKERS"
          ( entity
              { accounts: keyed [ "M" := 260, "E" := 60 ], capabilities: [] }
          )
      , Tuple "GLOBAL_SMES"
          ( entity
              { accounts: keyed [ "M" := 300, "E" := 80 ], capabilities: [] }
          )
      , Tuple "ENERGY_SUPPLIERS"
          ( entity
              { accounts: keyed [ "M" := 200, "E" := 240 ], capabilities: [], capacities: keyed [ "energy-production" := 60 ] }
          )
      , Tuple "WORLD_HOUSEHOLDS"
          ( entity
              { accounts: keyed [ "M" := 360, "E" := 90 ], capabilities: [] }
          )
      ]
  , circulation: { production: productionCapacity, flows, requirements }
  , rounds:
      [ roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf {}
      , roundOf { expect: expect { accounts: keyed [ Tuple "US" (keyed [ "M" := 300 ]), Tuple "DE" (keyed [ "M" := 131, "E" := 0 ]) ] } }
      ]
  }
