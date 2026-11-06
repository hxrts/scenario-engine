-- | The capstone run (docs/08_coverage.md): a test-only game on the shipped Fractured World, eight
-- | rounds, with a seat program for each seat that exercises the features the audit found
-- | untested together. `just scenarios` compiles it to `packages/scenarios/capstone/run.json`,
-- | which the harness decodes through the normal Policy and Board paths. The world and the
-- | Entity Policies are the shipped ones; only the seat programs and the expectations are here.
module Scenarios.Capstone (file, rounds, playerSeed) where

import Prelude ((==), map)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Tuple (Tuple(..))
import DSL.Base (Built, Keyed, (:=), keyed)
import DSL.Base (q)
import DSL.Decision (deal, enactAccessBanT, enactWithholdingT, eq, ge, gt, lit, price, program, repealT, round, rule, signAll, signFacility, signIf, signProcedures, tradeT, transformT)
import DSL.Fixture (expect, procedureOutcome, refAt)

-- | Eight rounds, one per year, as the reference horizon is five (docs/05 §14).
rounds :: Int
rounds = 8

-- | The player seed: seat randomness only. The game seed is the shipped one.
playerSeed :: Int
playerSeed = 7

-- | The US: bans shipments into India in round 2 (in force from round 3), and repeals the ban in
-- | round 6. Its first Proposal in round 2 is the ban, so the Rule's id is `r2/US/0/rule-0`.
-- | Whenever the Atlantic price is above 3 it reserves 60 Mil for a deployment and orders a
-- | 40-unit major action (the shipped Deployer clause).
us :: Built "program"
us = program
  { rules:
      [ rule [ eq round (lit 2) ] [ enactAccessBanT { target: "IN" } ]
      , rule [ eq round (lit 6) ] [ repealT { rule: "r2/US/0/rule-0" } ]
      , rule [ gt (price "energy-atlantic") (lit 3) ]
          [ transformT { transformation: "military-deployment", quantity: lit 60 }
          , transformT { transformation: "major-military-action", quantity: lit 40 }
          ]
      ]
  }

-- | The US without the deployment: the counterfactual's US seat keeps its ban and repeal only.
usWithoutDeployment :: Built "program"
usWithoutDeployment = program
  { rules:
      [ rule [ eq round (lit 2) ] [ enactAccessBanT { target: "IN" } ]
      , rule [ eq round (lit 6) ] [ repealT { rule: "r2/US/0/rule-0" } ]
      ]
  }

-- | Germany signs the EU's solidarity Proposal from round 5 only. The EU's own policy proposes that
-- | procedure when the Atlantic price is above 3: round 4 starts above 3, Germany does not sign,
-- | and the procedure lapses; round 5 starts above 3 again, Germany signs, and it executes.
de :: Built "program"
de = program { sign: [ signIf [ ge round (lit 5) ] (signProcedures [ "eu-solidarity" ]), signProcedures [ "eu-trade" ] ] }

-- | Japan proposes a two-party deal in round 3: 20 Energy to India, which signs it too. Both are
-- | in the Asian region, so the Transfer needs no transport order. JP holds 24 Energy by then
-- | (its requirement takes 8 a round), so the deal is funded.
jp :: Built "program"
jp = program
  { rules:
      [ rule [ eq round (lit 3) ]
          [ deal { signers: [ "JP", "IN" ], transfers: [ { from: "self", to: "IN", resource: "E", quantity: lit 20 } ] } ]
      ]
  }

-- | Enco ships Energy into India across regions in rounds 4 and 7: a transport order (its own
-- | energy-shipping facility, which SHIP signs) and a deal that India signs. The US ban on
-- | shipments into India blocks round 4; the repeal in round 6 lets round 7 through.
enco :: Built "program"
enco = program
  { rules:
      [ rule [ eq round (lit 4) ] (shipment)
      , rule [ eq round (lit 7) ] (shipment)
      ]
  }
  where
  shipment =
    [ deal
        { signers: [ "ENCO", "IN", "SHIP" ]
        , transfers: [ { from: "self", to: "IN", resource: "E", quantity: lit 5 } ]
        , transforms: [ { transformation: "energy-shipping", quantity: lit 5 } ]
        }
    ]

-- | SME buys 4 Energy in the Atlantic market every round. Its orders are the observation its
-- | Representation (sme-energy-sample) estimates from, so GLOBAL_SMES' factor moves: the sample
-- | reference is 5, so the deviation is negative and the factor falls below 1.
sme :: Built "program"
sme = program { rules: [ rule [] [ tradeT { market: "energy-atlantic", quantity: lit 4 } ] ] }

-- | The shipping facility's owner signs every shipment that orders it.
shipping :: Built "program"
shipping = program { sign: [ signFacility "energy-shipping" ] }

-- | India signs every Proposal addressed to it.
india :: Built "program"
india = program { sign: [ signAll ] }

-- | Chip owner: withholds 5 units of semiconductor capacity from round 4, and repeals the
-- | withholding in round 6 (its first Proposal in round 4, `r4/CHIP/0/rule-0`).
chip :: Built "program"
chip = program
  { rules:
      [ rule [ eq round (lit 4) ] [ enactWithholdingT { capacity: "semiconductors", units: lit 5 } ]
      , rule [ eq round (lit 6) ] [ repealT { rule: "r4/CHIP/0/rule-0" } ]
      ]
  }

passive :: Built "program"
passive = program {}

-- | Every seat, in catalog order. Unlisted seats submit nothing, so every seat is named.
seats :: Built "program" -> Keyed (Built "program")
seats usProgram = keyed
  [ Tuple "US" usProgram
  , Tuple "DE" de
  , Tuple "IN" india
  , Tuple "JP" jp
  , Tuple "UK" passive
  , Tuple "BR" passive
  , Tuple "SA" passive
  , Tuple "NG" passive
  , Tuple "ENCO" enco
  , Tuple "CHIP" chip
  , Tuple "SHIP" shipping
  , Tuple "DEF" passive
  , Tuple "LABOR" passive
  , Tuple "SME" sme
  , Tuple "AID" passive
  ]

-- | Round expectations, checked by the harness with the fixture checker. The values are the
-- | capstone run's own (the seat programs above on the shipped economics): each one is
-- | explained by its mechanism in the comment, and the harness fails if any drifts.
expectations :: Array Json
expectations =
  [ roundExp 2 (expect { scheduled: 1 }) -- the US ban is enacted, in force from round 3
  , roundExp 3 (expect { procedures: [ procedureOutcome { author: "EU", index: 1, support: q "0.68", executed: true } ] }) -- EU tariff passes: residual 0.48 plus Germany's 0.2
  , roundExp 4
      ( expect
          { rejected: [ refAt "ENCO" 0 ] -- the shipment into India is banned
          , lapsed: [ refAt "EU" 1 ] -- Germany does not sign the solidarity procedure: 0.4 < 0.45
          , procedures: [ procedureOutcome { author: "EU", index: 1, support: q "0.4", executed: false } ]
          , accounts: keyed [ Tuple "US" (keyed [ "Mil" := "85.5" ]) ] -- 60 reserved, 38 scaled action
          , factors: keyed [ Tuple "GLOBAL_SMES" (keyed [ "E" := "0.9437" ]) ]
          }
      )
  , roundExp 5
      ( expect
          { procedures: [ procedureOutcome { author: "EU", index: 1, support: q "0.6", executed: true } ] -- Germany signs from round 5
          , accounts: keyed [ Tuple "US" (keyed [ "Mil" := "76.95" ]) ]
          }
      )
  , roundExp 6 (expect { scheduled: 2 }) -- both repeals are scheduled
  , roundExp 7 (expect { rejected: [] }) -- the round-7 shipment clears after the ban's repeal
  , roundExp 8 (expect { factors: keyed [ Tuple "GLOBAL_SMES" (keyed [ "E" := "0.9406" ]) ] })
  ]
  where
  roundExp :: Int -> Built "expect" -> Json
  roundExp r e = encodeJson { round: r, expect: e }

-- | An objective's expected outcome: "met" or "notMet".
outcome :: String -> String -> Json
outcome entity o = encodeJson { entity, outcome: o }

-- | Every Actor's objective, as the Entities that hold one (the shipped objectives).
allActors :: Array String
allActors = [ "AID", "BR", "CHIP", "CN", "DE", "DEF", "ENCO", "IN", "JP", "LABOR", "NG", "SA", "SHIP", "SME", "UK", "US" ]

-- | The base run meets no objective. The US misses its own: its Mil ends at 76.95, below China's
-- | holding. SME misses its Money target of 15, since it spends on its own Energy purchases.
baseObjectives :: Array Json
baseObjectives = map (\e -> outcome e "notMet") allActors

-- | Without the deployment the US keeps its 95 Mil and meets its objective; nothing else changes
-- | the outcomes.
counterfactualObjectives :: Array Json
counterfactualObjectives = map (\e -> outcome e (if e == "US" then "met" else "notMet")) allActors

-- | The run file: the seat programs, the counterfactual's replacement seats, the expectations,
-- | and every objective's expected outcome in each case.
file :: Json
file = encodeJson
  { label: "capstone"
  , rounds
  , playerSeed
  , seats: seats us
  , counterfactual: keyed [ Tuple "US" usWithoutDeployment ]
  , expectations
  , objectives: baseObjectives
  , counterfactualObjectives
  }
