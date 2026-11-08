-- | The capstone run: a test-only game on the shipped Meridian, eight
-- | rounds, with a seat strategy for each seat that exercises the features the audit found
-- | untested together. It is a Gameboard premise with Strategy bindings (`seatBindings`); the
-- | counterfactual replaces one seat (`counterfactualBindings`). `just _premises` also compiles the
-- | same seats, expectations and objectives to `capstone/run.json`, a record of them for readers; no
-- | code decodes that file. The world and the Entity Policies are the shipped ones; only the seat
-- | strategies and the expectations are here.
module Premises.Capstone
  ( file
  , rounds
  , playerSeed
  , seatBindings
  , game
  , counterfactualBindings
  , expectations
  , baseObjectives
  , counterfactualObjectives
  , RoundExpect
  , ObjectiveExpect
  , JointLine
  , jointLines
  , jointLineFor
  , matchesExpect
  ) where

import Prelude (map, otherwise, ($), (<<<), (<>), (==))
import Premises.Names as N
import Premises.Rules (enactWithholding, selfAccessBan)
import Game.Assemble (assemble)
import Game.Bindings (strategyBindings)
import Game.Error (GameError)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Bindings, Game)
import Data.Bifunctor (lmap)
import Data.Map as Map

import Schema.Joint (JointProposal)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, jsonNull, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, mapMaybe, sort)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Core.Num (Q)
import Core.Strategy (Strategy)
import Core.Types (BehaviorDomain, EntityId, Id(..), MarketId, ResourceId, idText, proposalId)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Schema.Gameboard (Outcome(..))
import Schema.Build.Quantity (q)
import Schema.Build.Decision (passive, orderAt, selfTransform, asTemplate, joint, eqQ, ge, gt, lit, named, priceAt, rule, roundQ, selfEntity, signAll, signFacility, signIf, signProcedures)
import Schema.Build.Intent (repeal)
import Schema.Premise.Strategy (presetsShape)
import Schema.Round (Authored, Expect, ProposalRef, atRound)
import Core.Shape (encodeWith, Format(..))
import Schema.Build.Fixture (noExpect)
import Schema.Premise.Fixture (expectShape)

-- | Eight rounds, one per year, as the reference horizon is five (docs/05 §14).
rounds :: Int
rounds = 8

-- | The player seed: seat randomness only. The game seed is the shipped one.
playerSeed :: Int
playerSeed = 7

-- | The US: bans shipments into India in round 2 (in force from round 3), and repeals the ban in
-- | round 6. Its first Proposal in round 2 is the ban, so the Rule's id is `r2/US/0/rule-0`.
-- | Whenever the Atlantic price is above 3 it reserves 60 Mil for a deployment and orders a
-- | 40-unit Mil sale (the shipped Deployer clause).
us :: Strategy
us = usWithoutDeployment
  { rules = usWithoutDeployment.rules <>
      [ rule [ gt (priceAt (named N.energyAtlantic)) (lit 3) ]
          [ asTemplate (selfTransform { transformation: N.militaryDeployment, quantity: lit 60 })
          , asTemplate (selfTransform { transformation: N.majorMilitaryAction, quantity: lit 40 })
          ]
      ]
  }

-- | The US without the deployment: the counterfactual's US seat keeps its ban and repeal only.
usWithoutDeployment :: Strategy
usWithoutDeployment =
  { rules:
      [ rule [ eqQ roundQ (lit 2) ] [ asTemplate (selfAccessBan { target: N.india }) ]
      , rule [ eqQ roundQ (lit 6) ] [ asTemplate (repeal { rule: "r2/US/0/rule-0" }) ]
      ]
  , sign: []
  }

-- | Germany signs the EU's solidarity Proposal from round 5 only. The EU's own policy proposes that
-- | procedure when the Atlantic price is above 3: round 4 starts above 3, Germany does not sign,
-- | and the procedure lapses; round 5 starts above 3 again, Germany signs, and it executes.
de :: Strategy
de = { rules: [], sign: [ signIf [ ge roundQ (lit 5) ] (signProcedures [ N.euSolidarity ]), signProcedures [ N.euTrade ] ] }

-- | Japan proposes a two-party joint in round 3: 20 Energy to India, which signs it too. Both are
-- | in the Asian region, so the Transfer needs no transport order. JP holds 24 Energy by then
-- | (its requirement takes 8 a round), so the joint Proposal is funded.
jp :: Strategy
jp =
  { rules:
      [ rule [ eqQ roundQ (lit 3) ]
          [ joint { signers: [ N.jp, N.india ], transfers: [ { from: selfEntity, to: N.india, resource: N.energy, quantity: lit 20 } ], transforms: [] } ]
      ]
  , sign: []
  }

-- | Enco ships Energy into India across regions in rounds 4 and 7: a transport order (its own
-- | energy-shipping facility, which SHIP signs) and a joint Proposal that India signs. The US ban on
-- | shipments into India blocks round 4; the repeal in round 6 lets round 7 through.
enco :: Strategy
enco =
  { rules:
      [ rule [ eqQ roundQ (lit 4) ] shipment
      , rule [ eqQ roundQ (lit 7) ] shipment
      ]
  , sign: []
  }
  where
  shipment =
    [ joint
        { signers: [ N.enco, N.india, N.ship ]
        , transfers: [ { from: selfEntity, to: N.india, resource: N.energy, quantity: lit 5 } ]
        , transforms: [ { transformation: N.energyShipping, quantity: lit 5 } ]
        }
    ]

-- | SME buys 4 Energy in the Atlantic market every round. Its orders are the observation its
-- | Representation (sme-energy-sample) estimates from, so GLOBAL_SMES' factor moves: the sample
-- | reference is 5, so the deviation is negative and the factor falls below 1.
sme :: Strategy
sme = { rules: [ rule [] [ orderAt (named N.energyAtlantic) (lit 4) Nothing ] ], sign: [] }

-- | The shipping facility's owner signs every shipment that orders it.
shipping :: Strategy
shipping = { rules: [], sign: [ signFacility N.energyShipping ] }

-- | India signs every Proposal addressed to it.
india :: Strategy
india = { rules: [], sign: [ signAll ] }

-- | Chip owner: withholds 5 units of semiconductor capacity from round 4, and repeals the
-- | withholding in round 6 (its first Proposal in round 4, `r4/CHIP/0/rule-0`).
chip :: Strategy
chip =
  { rules:
      [ rule [ eqQ roundQ (lit 4) ] [ asTemplate (enactWithholding { capacity: N.semiconductors, units: lit 5 }) ]
      , rule [ eqQ roundQ (lit 6) ] [ asTemplate (repeal { rule: "r4/CHIP/0/rule-0" }) ]
      ]
  , sign: []
  }

-- | Every seat, in catalog order. Unlisted seats submit nothing, so every seat is named.
seats :: Strategy -> Array (Tuple String Strategy)
seats usStrategy = map (\e -> Tuple e (fromMaybe passive (Object.lookup e assigned))) N.seatActors
  where
  assigned = Object.fromFoldable
    [ Tuple N.us usStrategy
    , Tuple N.de de
    , Tuple N.india india
    , Tuple N.jp jp
    , Tuple N.enco enco
    , Tuple N.chip chip
    , Tuple N.ship shipping
    , Tuple N.sme sme
    ]

-- | One round's expectation: the round, and the checks it makes.
type RoundExpect = { round :: Int, expect :: Expect }

-- | Round expectations, checked by the simulator: the capstone run's own values (the seat strategies
-- | above on the shipped economics). Each one is explained by its mechanism in the comment, and the
-- | simulator fails if any drifts.
expectations :: Array RoundExpect
expectations = map (\r -> { round: r.round, expect: atRound r.round r.expect }) authoredExpectations

authoredExpectations :: Array { round :: Int, expect :: Authored }
authoredExpectations =
  [ roundExp 2 (noExpect { scheduled = Just 1 }) -- the US ban is enacted, in force from round 3
  , roundExp 3 (noExpect { procedures = Just [ { proposal: at N.eu 1, support: q "0.68", executed: true } ] }) -- EU tariff passes: residual 0.48 plus Germany's 0.2
  , roundExp 4
      ( noExpect
          { rejected = Just [ at N.enco 0 ] -- the shipment into India is banned
          , lapsed = Just [ at N.eu 1 ] -- Germany does not sign the solidarity procedure: 0.4 < 0.45
          , procedures = Just [ { proposal: at N.eu 1, support: q "0.4", executed: false } ]
          , accounts = single N.us N.mil "85.5" -- 60 reserved, 38 scaled action
          , factors = single N.globalSmes N.energy "0.9437"
          }
      )
  , roundExp 5
      ( noExpect
          { procedures = Just [ { proposal: at N.eu 1, support: q "0.6", executed: true } ] -- Germany signs from round 5
          , accounts = single N.us N.mil "76.95"
          }
      )
  , roundExp 6 (noExpect { scheduled = Just 2 }) -- both repeals are scheduled
  , roundExp 7 (noExpect { rejected = Just [] }) -- the round-7 shipment clears after the ban's repeal
  , roundExp 8 (noExpect { factors = single N.globalSmes N.energy "0.9406" })
  ]
  where
  roundExp :: Int -> Authored -> { round :: Int, expect :: Authored }
  roundExp r e = { round: r, expect: e }

-- | A Proposal by its author and its position among the author's Proposals in the round.
at :: String -> Int -> ProposalRef
at author i = { author: Id author, index: i }

-- | One balance (or factor) of one Entity: the leaf the expectation checks.
single :: forall a b. String -> String -> String -> Map.Map (Id a) (Map.Map (Id b) (Maybe Q))
single e k v = Map.singleton (Id e) (Map.singleton (Id k) (Just (q v)))

-- | An objective's expected outcome, by its Entity.
type ObjectiveExpect = { entity :: EntityId, outcome :: Outcome }

objective :: String -> Outcome -> ObjectiveExpect
objective e o = { entity: Id e, outcome: o }

-- | Every Actor's objective, as the Entities that hold one (the shipped objectives).
allActors :: Array String
allActors = sort (N.seatActors <> [ N.cn ])

-- | The base run meets no objective. The US misses its own: its Mil ends at 76.95, below China's
-- | holding. SME misses its Money target of 15, since it spends on its own Energy purchases.
baseObjectives :: Array ObjectiveExpect
baseObjectives = map (\e -> objective e NotMet) allActors

-- | Without the deployment the US keeps its 95 Mil and meets its objective; nothing else changes
-- | the outcomes.
counterfactualObjectives :: Array ObjectiveExpect
counterfactualObjectives = map (\e -> objective e (if e == N.us then Met else NotMet)) allActors

-- | The capstone's game: the shipped Meridian at its default configuration, with the capstone's seats.
game :: GameboardParts -> Either GameError Game
game p = assemble (GameboardPremise p) seatBindings []

-- | The seats of the capstone: each named seat bound to its Strategy.
seatBindings :: Bindings
seatBindings = strategyBindings (map (lmap Id) (seats us))

-- | The counterfactual's seats: the capstone's, with the US's Strategy replaced (the left operand wins).
counterfactualBindings :: Bindings
counterfactualBindings = Map.union (strategyBindings [ Tuple (Id N.us) usWithoutDeployment ]) seatBindings

-- | The run file: the seat strategies, the counterfactual's replacement seats, the expectations,
-- | and every objective's expected outcome in each case, in the premise's spelling.
file :: Json
file = encodeJson
  { label: "capstone"
  , rounds
  , playerSeed
  , seats: encodeWith Premise presetsShape (seats us)
  , counterfactual: encodeWith Premise presetsShape [ Tuple N.us usWithoutDeployment ]
  , expectations: map roundJson authoredExpectations
  , objectives: map objectiveJson baseObjectives
  , counterfactualObjectives: map objectiveJson counterfactualObjectives
  }
  where
  roundJson r = encodeJson { round: r.round, expect: encodeWith Premise expectShape r.expect }
  objectiveJson o = { entity: idText o.entity, outcome: word o.outcome }
  word = case _ of
    Met -> "met"
    NotMet -> "notMet"
    NotEvaluated -> "notEvaluated"

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
  [ intent "Enact"
      [ intent "FlowRule"
          [ obj
              [ Tuple "scope" (obj [ Tuple "over" (fromArray [ fromString N.energy ]), Tuple "governor" (fromString N.us), Tuple "direction" (intent "EitherWay" []), Tuple "counterparty" (fromString N.india) ])
              , Tuple "kind" (fromString "accessBan")
              , Tuple "effect" (intent "Block" [])
              ]
          ]
      ]
  ]

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

-- | Whether a joint pass gave exactly the Intents a line expects, compared in the engine's JSON encoding.
matchesExpect :: forall r. { expect :: Array Json | r } -> Array JointProposal -> Boolean
matchesExpect l js = map (\j -> stringify (encodeJson j.draft.intents)) js == [ stringify (encodeJson l.expect) ]

-- | The line whose transcript a joint call's payload names: the model's reply to that call.
jointLineFor :: String -> Maybe JointLine
jointLineFor input = find (\l -> contains (Pattern l.text) input) jointLines
