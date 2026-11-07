-- | The capstone run (docs/08_coverage.md): a test-only game on the shipped Meridian, eight
-- | rounds, with a seat strategy for each seat that exercises the features the audit found
-- | untested together. It is a Gameboard premise with Strategy bindings (`seatBindings`); the
-- | counterfactual replaces one seat (`counterfactualBindings`). `just premises` also compiles the
-- | same seats, expectations and objectives to `capstone/run.json`, a record of them for readers; no
-- | code decodes that file. The world and the Entity Policies are the shipped ones; only the seat
-- | strategies and the expectations are here.
module Premises.Capstone
  ( file
  , rounds
  , playerSeed
  , seatBindings
  , counterfactualBindings
  , expectations
  , baseObjectives
  , counterfactualObjectives
  , ObjectiveExpect
  , JointLine
  , jointLines
  , jointLineFor
  ) where

import Prelude ((==), map, ($), (<<<), (<>))
import Premises.Names as N
import Game.Types (Bindings, SeatBinding(..))
import Data.Map as Map

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, jsonNull)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Core.Strategy (Strategy)
import Core.Types (Id(..))
import Data.Maybe (Maybe(..))
import Schema.Build.Base (Built, (:=), keyed, q)
import Schema.Build.Decision (deal, eq, ge, gt, asTemplate, lit, price, rule, round, selfEntity, signAll, signFacility, signIf, signProcedures, strategy)
import Schema.Build.Intent (enactAccessBan, enactWithholding, repeal, trade, transfer, transform)
import Schema.Premise.Strategy (presetsShape)
import Core.Shape (Format(..), encodeWith)
import Schema.Build.Fixture (expect, procedureOutcome, refAt)

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
us :: Strategy
us = strategy
  { rules:
      [ rule [ eq round (lit 2) ] [ asTemplate (enactAccessBan { jurisdiction: selfEntity, target: N.india }) ]
      , rule [ eq round (lit 6) ] [ asTemplate (repeal { rule: "r2/US/0/rule-0" }) ]
      , rule [ gt (price N.energyAtlantic) (lit 3) ]
          [ asTemplate (transform { by: selfEntity, transformation: N.militaryDeployment, quantity: lit 60 })
          , asTemplate (transform { by: selfEntity, transformation: N.majorMilitaryAction, quantity: lit 40 })
          ]
      ]
  , sign: []
  }

-- | The US without the deployment: the counterfactual's US seat keeps its ban and repeal only.
usWithoutDeployment :: Strategy
usWithoutDeployment = strategy
  { rules:
      [ rule [ eq round (lit 2) ] [ asTemplate (enactAccessBan { jurisdiction: selfEntity, target: N.india }) ]
      , rule [ eq round (lit 6) ] [ asTemplate (repeal { rule: "r2/US/0/rule-0" }) ]
      ]
  , sign: []
  }

-- | Germany signs the EU's solidarity Proposal from round 5 only. The EU's own policy proposes that
-- | procedure when the Atlantic price is above 3: round 4 starts above 3, Germany does not sign,
-- | and the procedure lapses; round 5 starts above 3 again, Germany signs, and it executes.
de :: Strategy
de = strategy { rules: [], sign: [ signIf [ ge round (lit 5) ] (signProcedures [ N.euSolidarity ]), signProcedures [ N.euTrade ] ] }

-- | Japan proposes a two-party deal in round 3: 20 Energy to India, which signs it too. Both are
-- | in the Asian region, so the Transfer needs no transport order. JP holds 24 Energy by then
-- | (its requirement takes 8 a round), so the deal is funded.
jp :: Strategy
jp = strategy
  { rules:
      [ rule [ eq round (lit 3) ]
          [ deal { signers: [ N.jp, N.india ], intents: [ transfer { from: selfEntity, to: N.india, resource: N.energy, quantity: lit 20 } ] } ]
      ]
  , sign: []
  }

-- | Enco ships Energy into India across regions in rounds 4 and 7: a transport order (its own
-- | energy-shipping facility, which SHIP signs) and a deal that India signs. The US ban on
-- | shipments into India blocks round 4; the repeal in round 6 lets round 7 through.
enco :: Strategy
enco = strategy
  { rules:
      [ rule [ eq round (lit 4) ] shipment
      , rule [ eq round (lit 7) ] shipment
      ]
  , sign: []
  }
  where
  shipment =
    [ deal
        { signers: [ N.enco, N.india, N.ship ]
        , intents:
            [ transfer { from: selfEntity, to: N.india, resource: N.energy, quantity: lit 5 }
            , transform { by: selfEntity, transformation: N.energyShipping, quantity: lit 5 }
            ]
        }
    ]

-- | SME buys 4 Energy in the Atlantic market every round. Its orders are the observation its
-- | Representation (sme-energy-sample) estimates from, so GLOBAL_SMES' factor moves: the sample
-- | reference is 5, so the deviation is negative and the factor falls below 1.
sme :: Strategy
sme = strategy { rules: [ rule [] [ asTemplate (trade { market: N.energyAtlantic, quantity: lit 4, limit: Nothing }) ] ], sign: [] }

-- | The shipping facility's owner signs every shipment that orders it.
shipping :: Strategy
shipping = strategy { rules: [], sign: [ signFacility N.energyShipping ] }

-- | India signs every Proposal addressed to it.
india :: Strategy
india = strategy { rules: [], sign: [ signAll ] }

-- | Chip owner: withholds 5 units of semiconductor capacity from round 4, and repeals the
-- | withholding in round 6 (its first Proposal in round 4, `r4/CHIP/0/rule-0`).
chip :: Strategy
chip = strategy
  { rules:
      [ rule [ eq round (lit 4) ] [ asTemplate (enactWithholding { capacity: N.semiconductors, units: lit 5 }) ]
      , rule [ eq round (lit 6) ] [ asTemplate (repeal { rule: "r4/CHIP/0/rule-0" }) ]
      ]
  , sign: []
  }

passive :: Strategy
passive = strategy { rules: [], sign: [] }

-- | Every seat, in catalog order. Unlisted seats submit nothing, so every seat is named.
seats :: Strategy -> Array (Tuple String Strategy)
seats usStrategy =
  [ Tuple N.us usStrategy
  , Tuple N.de de
  , Tuple N.india india
  , Tuple N.jp jp
  , Tuple N.uk passive
  , Tuple N.br passive
  , Tuple N.sa passive
  , Tuple N.ng passive
  , Tuple N.enco enco
  , Tuple N.chip chip
  , Tuple N.ship shipping
  , Tuple N.def passive
  , Tuple N.labor passive
  , Tuple N.sme sme
  , Tuple N.aid passive
  ]

-- | Round expectations, checked by the simulator with the fixture checker. The values are the
-- | capstone run's own (the seat strategies above on the shipped economics): each one is
-- | explained by its mechanism in the comment, and the simulator fails if any drifts.
expectations :: Array Json
expectations =
  [ roundExp 2 (expect { scheduled: 1 }) -- the US ban is enacted, in force from round 3
  , roundExp 3 (expect { procedures: [ procedureOutcome { author: N.eu, index: 1, support: q "0.68", executed: true } ] }) -- EU tariff passes: residual 0.48 plus Germany's 0.2
  , roundExp 4
      ( expect
          { rejected: [ refAt N.enco 0 ] -- the shipment into India is banned
          , lapsed: [ refAt N.eu 1 ] -- Germany does not sign the solidarity procedure: 0.4 < 0.45
          , procedures: [ procedureOutcome { author: N.eu, index: 1, support: q "0.4", executed: false } ]
          , accounts: keyed [ Tuple N.us (keyed [ N.mil := "85.5" ]) ] -- 60 reserved, 38 scaled action
          , factors: keyed [ Tuple N.globalSmes (keyed [ N.energy := "0.9437" ]) ]
          }
      )
  , roundExp 5
      ( expect
          { procedures: [ procedureOutcome { author: N.eu, index: 1, support: q "0.6", executed: true } ] -- Germany signs from round 5
          , accounts: keyed [ Tuple N.us (keyed [ N.mil := "76.95" ]) ]
          }
      )
  , roundExp 6 (expect { scheduled: 2 }) -- both repeals are scheduled
  , roundExp 7 (expect { rejected: [] }) -- the round-7 shipment clears after the ban's repeal
  , roundExp 8 (expect { factors: keyed [ Tuple N.globalSmes (keyed [ N.energy := "0.9406" ]) ] })
  ]
  where
  roundExp :: Int -> Built "expect" -> Json
  roundExp r e = encodeJson { round: r, expect: e }

-- | An objective's expected outcome, by its Entity: "met", "notMet" or "notEvaluated" (the premise spelling).
type ObjectiveExpect = { entity :: String, outcome :: String }

outcome :: String -> String -> ObjectiveExpect
outcome entity o = { entity, outcome: o }

-- | Every Actor's objective, as the Entities that hold one (the shipped objectives).
allActors :: Array String
allActors = [ N.aid, N.br, N.chip, N.cn, N.de, N.def, N.enco, N.india, N.jp, N.labor, N.ng, N.sa, N.ship, N.sme, N.uk, N.us ]

-- | The base run meets no objective. The US misses its own: its Mil ends at 76.95, below China's
-- | holding. SME misses its Money target of 15, since it spends on its own Energy purchases.
baseObjectives :: Array ObjectiveExpect
baseObjectives = map (\e -> outcome e "notMet") allActors

-- | Without the deployment the US keeps its 95 Mil and meets its objective; nothing else changes
-- | the outcomes.
counterfactualObjectives :: Array ObjectiveExpect
counterfactualObjectives = map (\e -> outcome e (if e == N.us then "met" else "notMet")) allActors

-- | The seats of the capstone: each named seat bound to its Strategy.
seatBindings :: Bindings
seatBindings = strategyBindings (seats us)

-- | The counterfactual's seats: the capstone's, with the US's Strategy replaced (the left operand wins).
counterfactualBindings :: Bindings
counterfactualBindings = Map.union (strategyBindings [ Tuple N.us usWithoutDeployment ]) seatBindings

strategyBindings :: Array (Tuple String Strategy) -> Bindings
strategyBindings xs = Map.fromFoldable (map (\(Tuple e s) -> Tuple (Id e) (Strategy s)) xs)

-- | The run file: the seat strategies, the counterfactual's replacement seats, the expectations,
-- | and every objective's expected outcome in each case.
file :: Json
file = encodeJson
  { label: "capstone"
  , rounds
  , playerSeed
  , seats: encodeWith Premise presetsShape (seats us)
  , counterfactual: encodeWith Premise presetsShape [ Tuple N.us usWithoutDeployment ]
  , expectations
  , objectives: baseObjectives
  , counterfactualObjectives
  }

-- Joint pass ------------------------------------------------------------------------------

-- | One exchange of the capstone's joint pass (docs/08_coverage.md): the seat's transcript, the
-- | model's reply as the wire carries it, and the Intents the joint pass must make of it, in the
-- | engine's JSON encoding. The generator (`apps/cli`, module `Transcript`) runs each exchange through
-- | the recording port and writes `capstone/transcript.json`; the handler suite replays that file.
type JointLine = { text :: String, reply :: Json, expect :: Array Json }

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

-- | An Intent in the engine's encoding: a tagged object with its `values` and its `tag`.
intent :: String -> Array Json -> Json
intent t vs = obj [ Tuple "values" (fromArray vs), Tuple "tag" (fromString t) ]

-- | The model's reply to one transcript: one Proposal by `author`, agreed by its own seat.
proposalReply :: String -> Json -> Json
proposalReply author i = obj
  [ Tuple "proposals" $ fromArray
      [ obj
          [ Tuple "author" (fromString author)
          , Tuple "intents" (fromArray [ i ])
          , Tuple "signers" (fromArray [])
          , Tuple "agreed" (fromArray [ fromString author ])
          ]
      ]
  ]

-- | The US's ban on shipments into India, as the joint pass reads it: a rule change that enacts an
-- | access ban on IN.
banIntent :: Json
banIntent = intent "ChangeRule"
  [ intent "Enact" [ intent "AccessBan" [ obj [ Tuple "target" (fromString N.india), Tuple "jurisdiction" (fromString N.us) ] ] ] ]

-- | The US buying ten Energy in the Atlantic market at no limit price.
tradeIntent :: Json
tradeIntent = intent "Trade"
  [ obj [ Tuple "quantity" (fromString "10.0000"), Tuple "market" (fromString N.energyAtlantic), Tuple "limitPrice" jsonNull ] ]

-- | The capstone's joint-pass exchanges, in the order the recording holds them.
jointLines :: Array JointLine
jointLines =
  [ { text: "ban shipments into India", reply: proposalReply N.us banIntent, expect: [ banIntent ] }
  , { text: "buy ten energy", reply: proposalReply N.us tradeIntent, expect: [ tradeIntent ] }
  ]

-- | The line whose transcript a joint call's payload names: the model's reply to that call.
jointLineFor :: String -> Maybe JointLine
jointLineFor input = find (\l -> contains (Pattern l.text) input) jointLines
