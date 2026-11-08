-- | The capstone run: a test-only game on the shipped Meridian, eight
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
  , RoundExpect
  , ObjectiveExpect
  , JointLine
  , jointLines
  , jointLineFor
  ) where

import Prelude (map, otherwise, ($), (<<<), (==))
import Premises.Names as N
import Game.Bindings (strategyBindings)
import Game.Types (Bindings)
import Data.Bifunctor (lmap)
import Data.Map as Map

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, jsonNull)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, mapMaybe)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Core.Num (Q)
import Core.Strategy (Strategy)
import Core.Types (BehaviorDomain, EntityId, Id(..), MarketId, ResourceId, proposalId)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Schema.Build.Base (Keyed)
import Schema.Gameboard (Outcome(..))
import Schema.Build.Quantity (q)
import Schema.Build.Decision (orderAt, selfAccessBan, selfTransform, asTemplate, deal, eqQ, ge, gt, lit, named, price, rule, roundQ, selfEntity, signAll, signFacility, signIf, signProcedures, strategy)
import Schema.Build.Intent (enactWithholding, repeal, transfer)
import Schema.Premise.Strategy (presetsShape)
import Schema.Round (Delivery, Expect)
import Core.Shape (encodeWith, Format(..))
import Schema.Premise.Fixture (DeliveryDoc, ExpectDoc, ProcedureDoc, ProposalRefDoc, expectShape)

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
      [ rule [ eqQ roundQ (lit 2) ] [ asTemplate (selfAccessBan { target: N.india }) ]
      , rule [ eqQ roundQ (lit 6) ] [ asTemplate (repeal { rule: "r2/US/0/rule-0" }) ]
      , rule [ gt (price N.energyAtlantic) (lit 3) ]
          [ asTemplate (selfTransform { transformation: N.militaryDeployment, quantity: lit 60 })
          , asTemplate (selfTransform { transformation: N.majorMilitaryAction, quantity: lit 40 })
          ]
      ]
  , sign: []
  }

-- | The US without the deployment: the counterfactual's US seat keeps its ban and repeal only.
usWithoutDeployment :: Strategy
usWithoutDeployment = strategy
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
de = strategy { rules: [], sign: [ signIf [ ge roundQ (lit 5) ] (signProcedures [ N.euSolidarity ]), signProcedures [ N.euTrade ] ] }

-- | Japan proposes a two-party deal in round 3: 20 Energy to India, which signs it too. Both are
-- | in the Asian region, so the Transfer needs no transport order. JP holds 24 Energy by then
-- | (its requirement takes 8 a round), so the deal is funded.
jp :: Strategy
jp = strategy
  { rules:
      [ rule [ eqQ roundQ (lit 3) ]
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
      [ rule [ eqQ roundQ (lit 4) ] shipment
      , rule [ eqQ roundQ (lit 7) ] shipment
      ]
  , sign: []
  }
  where
  shipment =
    [ deal
        { signers: [ N.enco, N.india, N.ship ]
        , intents:
            [ transfer { from: selfEntity, to: N.india, resource: N.energy, quantity: lit 5 }
            , selfTransform { transformation: N.energyShipping, quantity: lit 5 }
            ]
        }
    ]

-- | SME buys 4 Energy in the Atlantic market every round. Its orders are the observation its
-- | Representation (sme-energy-sample) estimates from, so GLOBAL_SMES' factor moves: the sample
-- | reference is 5, so the deviation is negative and the factor falls below 1.
sme :: Strategy
sme = strategy { rules: [ rule [] [ orderAt (named N.energyAtlantic) (lit 4) Nothing ] ], sign: [] }

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
      [ rule [ eqQ roundQ (lit 4) ] [ asTemplate (enactWithholding { capacity: N.semiconductors, units: lit 5 }) ]
      , rule [ eqQ roundQ (lit 6) ] [ asTemplate (repeal { rule: "r4/CHIP/0/rule-0" }) ]
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

-- | One round's expectation: the round, and the checks it makes.
type RoundExpect = { round :: Int, expect :: Expect }

-- | A Proposal as the capstone authors it: its round, its author, and its position among the author's
-- | Proposals that round. The run file writes the author and position; the checks use the id.
type AuthoredRef = { round :: Int, author :: String, position :: Int }

-- | An `Expect` with its Proposal references left as `p`.
type ExpectOf p =
  { accounts :: Map.Map EntityId (Map.Map ResourceId (Maybe Q))
  , reserves :: Map.Map MarketId (Map.Map ResourceId (Maybe Q))
  , factors :: Map.Map EntityId (Map.Map BehaviorDomain (Maybe Q))
  , deliveries :: Maybe (Array Delivery)
  , rejected :: Maybe (Array p)
  , lapsed :: Maybe (Array p)
  , procedures :: Maybe (Array { proposal :: p, support :: Q, executed :: Boolean })
  , scheduled :: Maybe Int
  }

-- | The expectation the checks read: each authored reference becomes the Proposal's id.
toExpect :: ExpectOf AuthoredRef -> Expect
toExpect e = e
  { rejected = map (map idOf) e.rejected
  , lapsed = map (map idOf) e.lapsed
  , procedures = map (map (\p -> p { proposal = idOf p.proposal })) e.procedures
  }
  where
  idOf r = proposalId r.round (Id r.author) r.position

-- | Round expectations, checked by the simulator: the capstone run's own values (the seat strategies
-- | above on the shipped economics). Each one is explained by its mechanism in the comment, and the
-- | simulator fails if any drifts.
expectations :: Array RoundExpect
expectations = map (\r -> { round: r.round, expect: toExpect r.expect }) authoredExpectations

authoredExpectations :: Array { round :: Int, expect :: ExpectOf AuthoredRef }
authoredExpectations =
  [ roundExp 2 (none { scheduled = Just 1 }) -- the US ban is enacted, in force from round 3
  , roundExp 3 (none { procedures = Just [ { proposal: at 3 N.eu 1, support: q "0.68", executed: true } ] }) -- EU tariff passes: residual 0.48 plus Germany's 0.2
  , roundExp 4
      ( none
          { rejected = Just [ at 4 N.enco 0 ] -- the shipment into India is banned
          , lapsed = Just [ at 4 N.eu 1 ] -- Germany does not sign the solidarity procedure: 0.4 < 0.45
          , procedures = Just [ { proposal: at 4 N.eu 1, support: q "0.4", executed: false } ]
          , accounts = single N.us N.mil "85.5" -- 60 reserved, 38 scaled action
          , factors = single N.globalSmes N.energy "0.9437"
          }
      )
  , roundExp 5
      ( none
          { procedures = Just [ { proposal: at 5 N.eu 1, support: q "0.6", executed: true } ] -- Germany signs from round 5
          , accounts = single N.us N.mil "76.95"
          }
      )
  , roundExp 6 (none { scheduled = Just 2 }) -- both repeals are scheduled
  , roundExp 7 (none { rejected = Just [] }) -- the round-7 shipment clears after the ban's repeal
  , roundExp 8 (none { factors = single N.globalSmes N.energy "0.9406" })
  ]
  where
  roundExp :: Int -> ExpectOf AuthoredRef -> { round :: Int, expect :: ExpectOf AuthoredRef }
  roundExp r e = { round: r, expect: e }

-- | The expectation that checks nothing: every field is absent.
none :: ExpectOf AuthoredRef
none = { accounts: Map.empty, reserves: Map.empty, factors: Map.empty, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, procedures: Nothing, scheduled: Nothing }

-- | A Proposal by its author and its position among the author's Proposals in `round`.
at :: Int -> String -> Int -> AuthoredRef
at r author i = { round: r, author, position: i }

-- | One balance (or factor) of one Entity: the leaf the expectation checks.
single :: forall a b. String -> String -> String -> Map.Map (Id a) (Map.Map (Id b) (Maybe Q))
single e k v = Map.singleton (Id e) (Map.singleton (Id k) (Just (q v)))

-- | An objective's expected outcome, by its Entity.
type ObjectiveExpect = { entity :: EntityId, outcome :: Outcome }

objective :: String -> Outcome -> ObjectiveExpect
objective e o = { entity: Id e, outcome: o }

-- | Every Actor's objective, as the Entities that hold one (the shipped objectives).
allActors :: Array String
allActors = [ N.aid, N.br, N.chip, N.cn, N.de, N.def, N.enco, N.india, N.jp, N.labor, N.ng, N.sa, N.ship, N.sme, N.uk, N.us ]

-- | The base run meets no objective. The US misses its own: its Mil ends at 76.95, below China's
-- | holding. SME misses its Money target of 15, since it spends on its own Energy purchases.
baseObjectives :: Array ObjectiveExpect
baseObjectives = map (\e -> objective e NotMet) allActors

-- | Without the deployment the US keeps its 95 Mil and meets its objective; nothing else changes
-- | the outcomes.
counterfactualObjectives :: Array ObjectiveExpect
counterfactualObjectives = map (\e -> objective e (if e == N.us then Met else NotMet)) allActors

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
  roundJson r = encodeJson { round: r.round, expect: encodeWith Premise expectShape (expectDoc r.expect) }
  objectiveJson o = { entity: unId o.entity, outcome: word o.outcome }
  word = case _ of
    Met -> "met"
    NotMet -> "notMet"
    NotEvaluated -> "notEvaluated"

-- | An expectation in the premise's spelling, so `capstone/run.json` keeps its written form.
expectDoc :: ExpectOf AuthoredRef -> ExpectDoc
expectDoc e =
  { accounts: keyedOf e.accounts
  , reserves: keyedOf e.reserves
  , deliveries: map (map deliveryDoc) e.deliveries
  , rejected: map (map refOf) e.rejected
  , lapsed: map (map refOf) e.lapsed
  , factors: keyedOf e.factors
  , scheduled: e.scheduled
  , procedures: map (map procedureDoc) e.procedures
  }

-- | A two-level map written as keyed objects; an empty map is an absent key.
keyedOf :: forall a b. Map.Map (Id a) (Map.Map (Id b) (Maybe Q)) -> Maybe (Keyed (Keyed Q))
keyedOf m
  | Map.isEmpty m = Nothing
  | otherwise = Just (map (\(Tuple (Id k) inner) -> Tuple k (mapMaybe leaf (Map.toUnfoldable inner))) (Map.toUnfoldable m))
      where
      leaf (Tuple (Id k) v) = map (Tuple k) v

refOf :: AuthoredRef -> ProposalRefDoc
refOf r = Right { author: r.author, index: Just r.position }

procedureDoc :: { proposal :: AuthoredRef, support :: Q, executed :: Boolean } -> ProcedureDoc
procedureDoc p = { author: p.proposal.author, index: Just p.proposal.position, support: p.support, executed: p.executed }

deliveryDoc :: Delivery -> DeliveryDoc
deliveryDoc d = { entity: unId d.entity, resource: unId d.resource, quantity: d.quantity }

unId :: forall a. Id a -> String
unId (Id s) = s

-- Joint pass ------------------------------------------------------------------------------

-- | One exchange of the capstone's joint pass: the seat's transcript, the
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
