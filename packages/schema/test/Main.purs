-- | Pins: setting and strategy JSON of docs/03 (each case marked `Pins`), and the shape formats of docs/02 §8.4.
module Test.Schema.Main where

import Testkit.Meridian (orThrow)
import Schema.Build.Quantity (q)
import Prelude

import Data.Map as Map

import Rules.World (RecurringAmount(..))
import Rules.Types (ResourceId, LevyBasis(..), LevyRecipient(..), Direction(..), FlowEffect(..), RuleF(..), EntityTag, proposalId, Capability(..), Id(..), Intent, IntentF(..), RuleChangeF(..))

import Core.Num as Q
import Rules.Strategy (Strategy)
import Core.Shape (Format(..), decodeJsonWith, encodeWith, prose)
import Data.Foldable (for_)
import Schema.Build.Decision (selfEnact, selfTransform, asTemplate, choose, joint, eqQ, factor, ge, gt, holding, le, lit, max', mulQ, noAction, not', param, priceAt, named, procedure, reciprocate, restrictedLastRound, restrictionInForce, roundQ, signAll, signFacility, signIf, signProcedures, subQ, transformedLastRound)
import Schema.Build.Decision as D
import Schema.Build.Intent (enactRule, repeal, route, transfer)
import Schema.Setting.Intent (capabilityShape, intentShape, ruleShape)
import Schema.Setting.Strategy (decodeStrategy, encodeStrategy)
import Core.Num (Q)
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Setting.Fixture (roundShape, toRound)
import Schema.Joint (jointProposals)
import Schema.Round (Expect)
import Schema.Setting.Gameboard (decodeGameboard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Testkit.Json (decodeText, json)
import Testkit.Gameboard (minimalGameboard)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The expectations of a one-round document.
expectOf :: String -> Expect
expectOf text = case decodeJsonWith Authored roundShape (json text) of
  Left e -> unsafeCrashWith ("round does not decode: " <> show e)
  Right r -> (toRound 1 r).expect

-- | The decoders the tests read: each is the shape's decoder, with the core value it makes.
capability :: String -> Either JsonDecodeError Capability
capability s = decodeJsonWith Authored capabilityShape (fromString s)

rule :: Json -> Either JsonDecodeError (RuleF Q)
rule = decodeJsonWith Authored ruleShape

intent :: Json -> Either JsonDecodeError Intent
intent = decodeJsonWith Authored intentShape

-- | A setting shape reports a failed form as text: the message names the sum that failed.
isTemplateError :: String -> Boolean
isTemplateError m = contains (Pattern "Template form") m

-- | Two Choose blocks, each with two options of different size.

-- | A quantity literal in a test value.
qv :: String -> Q
qv t = fromMaybe (Q.fromInt 0) (Q.parse t)

-- | One Strategy per form the setting format writes, so every constructor of the AST round-trips.
roundTripStrategies :: Array Strategy
roundTripStrategies =
  [ { rules: [ D.rule [] [ asTemplate (D.tradeAt { market: D.named "m", quantity: lit 1, limit: Nothing }) ] ], sign: [] }
  , { rules:
        [ D.rule [ gt (priceAt (named "m")) (lit "2.5"), ge (holding "E") (param "p"), le (roundQ) (lit 3), eqQ (factor "d") (lit 0) ]
            [ asTemplate (route { base: "E", quote: "M", quantity: lit (-4), limit: lit 2 })
            , asTemplate (selfTransform { transformation: "t", quantity: mulQ (lit 2) (max' (lit 1) (subQ (lit 4) (lit 3))) })
            , asTemplate (transfer { from: "self", to: "X", resource: "E", quantity: lit 1 })
            , asTemplate (selfEnact (FlowRule { kind: Id "tariff", scope: { governor: Id "self", counterparty: Id "CN", direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: lit "0.5", basis: PerUnit, in: Id "M", to: LevyToDestination } }))
            , asTemplate (selfEnact (FlowRule { kind: Id "accessBan", scope: { governor: Id "self", counterparty: Id "CN", direction: EitherWay, over: [ Id "E" ] }, effect: Block }))
            , asTemplate (enactRule (HoldingRule { kind: Id "withholding", resource: Id "c", reduce: lit 2 }))
            , asTemplate (repeal { rule: "r2/US/0/rule-0" })
            ]
        , D.rule [ not' (eqQ (lit 1) (lit 2)), transformedLastRound { by: "US", transformation: "t", atLeast: lit 1 }, restrictedLastRound { by: "US", target: "CN" }, restrictionInForce { by: "EU" } ]
            [ reciprocate { target: "US" }
            , choose [ noAction, asTemplate (D.tradeAt { market: D.named "m", quantity: lit 1, limit: Just (lit 3) }) ]
            ]
        ]
    , sign: [ signIf [ ge roundQ (lit 5) ] (signProcedures [ "p" ]), signFacility "f", signAll ]
    }
  , { rules:
        [ D.rule [] [ procedure { id: "proc", signers: [ "DE" ], intents: [ transfer { from: "self", to: "EU", resource: "E", quantity: lit 8 } ] } ]
        , D.rule [] [ joint { signers: [ Id "DE", Id "IN" ], transfers: [ { from: Id "self", to: Id "IN", resource: Id "E", quantity: lit 2 } ], transforms: [ { transformation: Id "t", quantity: lit 1 } ] } ]
        ]
    , sign: []
    }
  ]

-- | One Intent of each kind, with explicit entities, in the fixture format (the setting tags).
roundTripIntents :: Array Intent
roundTripIntents =
  [ Trade { market: Id "m", quantity: Q.fromInt 10, limitPrice: Nothing }
  , Trade { market: Id "m", quantity: Q.fromInt 10, limitPrice: Just (qv "2.5") }
  , Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt (-3), limitPrice: Nothing }
  , Transfer { from: Id "US", to: Id "CN", resource: Id "E", quantity: Q.fromInt 2 }
  , Transform { by: Id "US", transformation: Id "t", quantity: Q.fromInt 1 }
  , ChangeRule (Enact (tariffR { jurisdiction: Id "US", origin: Id "CN", rate: qv "0.25" }))
  , ChangeRule (Enact (banR { jurisdiction: Id "US", target: Id "IN" }))
  , ChangeRule (Enact (holdR { capacity: Id "semiconductors", units: Q.fromInt 5 }))
  ]

roundTripCapabilities :: Array Capability
roundTripCapabilities =
  [ MayTrade (Id "m")
  , MayTransfer (Id "E")
  , MayTransform (Id "t")
  , MayEnact (Id "tariff")
  , MayEnact (Id "accessBan")
  , MayEnact (Id "withholding")
  ]

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Round trips: decode (encode x) == x for each setting shape" do
    it "every Strategy form, in the setting format" do
      for_ roundTripStrategies \p ->
        (decodeStrategy (encodeStrategy p) == Right p) `shouldEqual` true
    it "every Intent kind, in the fixture format" do
      for_ roundTripIntents \i ->
        (decodeJsonWith Authored intentShape (encodeWith Authored intentShape i) == Right i) `shouldEqual` true
    it "every Capability, in the setting format" do
      for_ roundTripCapabilities \c ->
        (decodeJsonWith Authored capabilityShape (encodeWith Authored capabilityShape c) == Right c) `shouldEqual` true

  describe "Joint pass rule (docs/04 §11)" do
    let
      known e = e == Id "US" || e == Id "EU"
      said = [ { seat: Id "US", text: "I will buy ten energy" } ]
      joint = { author: Id "US", intents: [ Transform { by: Id "US", transformation: Id "dep", quantity: Q.fromInt 2 } ], signers: [ Id "EU" ], agreed: [ Id "EU", Id "US" ] }
    it "drops a Proposal whose author did not speak, and one with no Intents" do
      map _.author (jointProposals known said [ joint { author = Id "EU" }, joint { intents = [] } ]) `shouldEqual` []
    it "keeps a speaker's Proposal, and signs for speakers only" do
      map _.signed (jointProposals known said [ joint ]) `shouldEqual` [ [ Id "US" ] ]
    it "keeps as signers only known Entities other than the author" do
      map _.draft.signers (jointProposals known said [ joint { signers = [ Id "US", Id "Ghost", Id "EU" ] } ]) `shouldEqual` [ [ Id "EU" ] ]

  describe "Reservations are checked on the setting document" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "a facility may not escrow a Resource the document does not mark reservable" do
      let
        doc reservable = """{ "id": "t", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": [4, 8], "resources": { "M": { "transferability": "Free" """ <> reservable <> """ } }, "markets": {}, "transformations": { "dep": { "draws": [ { "resource": "M", "from": "Executor", "perUnit": "1" } ] } }, "entities": {} }"""
      isLeft (decodeGameboard (json (doc ""))) `shouldEqual` true
      isLeft (decodeGameboard (json (doc """, "reservable": true"""))) `shouldEqual` false

  describe "Rule forms" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "a combination that would do nothing has no form" do
      isLeft (decodeJsonWith Authored ruleShape (json """{ "flows": { "kind": "x", "scope": { "governor": "A", "counterparty": "B", "direction": "inbound", "over": ["E"] }, "effect": { "reduce": "1" } } }""")) `shouldEqual` true
      isLeft (decodeJsonWith Authored ruleShape (json """{ "holding": { "kind": "x", "resource": "c", "levy": { "rate": "1", "basis": "perUnit", "in": "M", "to": "destination" } } }""")) `shouldEqual` true

  describe "Capability tags" do
    it "decodes each capability kind" do
      capability "trade:energy-atlantic" `shouldEqual` Right (MayTrade (Id "energy-atlantic"))
      capability "transfer:E" `shouldEqual` Right (MayTransfer (Id "E"))
      capability "transform:defense-procurement" `shouldEqual` Right (MayTransform (Id "defense-procurement"))
      capability "enact:withholding" `shouldEqual` Right (MayEnact (Id "withholding"))
      capability "enact:tariff" `shouldEqual` Right (MayEnact (Id "tariff"))
    it "rejects an unknown capability kind" do
      isLeft (capability "launch:missile") `shouldEqual` true
    it "an enactment names a kind by id; the load check rejects an undeclared one" do
      capability "enact:embargo" `shouldEqual` Right (MayEnact (Id "embargo"))
    it "rejects a capability tag without its argument" do
      isLeft (capability "trade") `shouldEqual` true

  describe "Rules" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "decodes each rule key, including withholding" do
      rule (json """{ "holding": { "kind": "withholding", "resource": "semiconductors", "reduce": "3" } }""")
        `shouldEqual` Right (holdR { capacity: Id "semiconductors", units: Q.fromInt 3 })
      rule (json """{ "flows": { "kind": "accessBan", "scope": { "governor": "US", "counterparty": "CN", "direction": "eitherWay", "over": ["E"] }, "effect": "block" } }""")
        `shouldEqual` Right (banR { jurisdiction: Id "US", target: Id "CN" })
    -- Pins the setting format the schema decoders read (docs/03).
    it "rejects an unknown rule key with a typed error" do
      case rule (json """{ "embargo": {} }""") of
        Left _ -> pure unit
        other -> fail ("expected a rule error, got " <> show (map (const "a rule") other))
    -- Pins the setting format the schema decoders read (docs/03).
    it "rejects a rule object with two keys" do
      isLeft (rule (json """{ "flows": { "kind": "accessBan", "scope": { "governor": "US", "counterparty": "CN", "direction": "sideways" }, "effect": "block" } }"""))
        `shouldEqual` true

  describe "Intents" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "decodes a trade with an optional limit" do
      intent (json """{ "trade": { "market": "energy-atlantic", "quantity": "10", "limit": "2.04" } }""")
        `shouldEqual` Right (Trade { market: Id "energy-atlantic", quantity: Q.fromInt 10, limitPrice: Just (q "2.04") })
    -- Pins the setting format the schema decoders read (docs/03).
    it "a Trade has no form in a joint Proposal: it cannot be built by `joint` (a compile error), and as JSON does not read as one" do
      isLeft (decodeStrategy (json """{"rules":[{"when":[],"propose":[{"joint":{"signers":["DE"],"transfers":[{"trade":{"market":"E","quantity":"1"}}]}}]}]}""")) `shouldEqual` true

    -- Pins the setting format the schema decoders read (docs/03).
    it "a routed order's template must state its limit: no limit, or a null one, does not decode" do
      isLeft (decodeStrategy (json """{"rules":[{"when":[],"propose":[{"route":{"base":"E","quote":"M","quantity":"1"}}]}]}""")) `shouldEqual` true
      isLeft (decodeStrategy (json """{"rules":[{"when":[],"propose":[{"route":{"base":"E","quote":"M","quantity":"1","limit":null}}]}]}""")) `shouldEqual` true
      isLeft (decodeStrategy (json """{"rules":[{"when":[],"propose":[{"route":{"base":"E","quote":"M","quantity":"1","limit":"2"}}]}]}""")) `shouldEqual` false

    -- Pins the setting format the schema decoders read (docs/03).
    it "decodes a routed trade, with a null market, by its pair" do
      intent (json """{ "trade": { "market": null, "base": "E", "quote": "M", "quantity": "10", "limit": "2.04" } }""")
        `shouldEqual` Right (Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt 10, limitPrice: Just (q "2.04") })
    -- Pins the setting format the schema decoders read (docs/03).
    it "decodes an enactment through its rule" do
      intent (json """{ "enact": { "flows": { "kind": "accessBan", "scope": { "governor": "US", "counterparty": "CN", "direction": "eitherWay", "over": ["E"] }, "effect": "block" } } }""")
        `shouldEqual` Right (ChangeRule (Enact (banR { jurisdiction: Id "US", target: Id "CN" })))
    -- Pins the setting format the schema decoders read (docs/03).
    it "a Repeal round-trips through the setting form, as one key naming the Rule" do
      intent (json """{ "repeal": { "rule": "r2/US/0/rule-0" } }""") `shouldEqual` Right (ChangeRule (Repeal (Id "r2/US/0/rule-0")))
      intent (encodeWith Authored intentShape (repeal { rule: "r2/US/0/rule-0" })) `shouldEqual` Right (ChangeRule (Repeal (Id "r2/US/0/rule-0")))

    -- Pins the setting format the schema decoders read (docs/03).
    it "rejects an intent with two keys" do
      case intent (json """{ "trade": { "market": "m", "quantity": "1" }, "transfer": { "from": "a", "to": "b", "resource": "E", "quantity": "1" } }""") of
        Left _ -> pure unit
        other -> fail ("expected an intent error, got " <> show (map (const "an intent") other))
    -- Pins the setting format the schema decoders read (docs/03).
    it "rejects an unknown intent key" do
      isLeft (intent (json """{ "gift": {} }""")) `shouldEqual` true

  describe "Setting tag pairs" do
    it "a trade's description lists its body's fields" do
      contains (Pattern "market") (prose intentShape.desc) `shouldEqual` true
      contains (Pattern "quantity") (prose intentShape.desc) `shouldEqual` true

  describe "Round expectations" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "an absent expectation is absent, and an empty list is a checked empty list" do
      let
        e = expectOf """{ "expect": { "deliveries": [] } }"""
      e.deliveries `shouldEqual` Just []
      e.accounts `shouldEqual` Map.empty
      e.rejected `shouldEqual` Nothing
    -- Pins the setting format the schema decoders read (docs/03).
    it "a proposal is named by its author, or by author and index" do
      let
        e = expectOf """{ "expect": { "rejected": [ "US", { "author": "US", "index": 1 } ] } }"""
      e.rejected `shouldEqual` Just [ proposalId 1 (Id "US") 0, proposalId 1 (Id "US") 1 ]

  describe "Gameboard documents" do
    it "decodes a minimal valid Gameboard and its roundQ range" do
      gb <- orThrow (decodeGameboard (json (minimalGameboard "[4, 8]")))
      gb.rounds `shouldEqual` { min: 4, max: 8 }
    it "rejects a roundQ range that is malformed, not ordered, or below one" do
      isLeft (decodeGameboard (json (minimalGameboard "[8, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[0, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[4]"))) `shouldEqual` true
    -- Pins the setting format the schema decoders read (docs/03).
    it "has no default roundQ range" do
      isLeft (decodeGameboard (json """{ "id": "t", "version": "v0", "numeraire": "M", "horizon": 5, "resources": {}, "markets": {}, "transformations": {}, "entities": {} }""")) `shouldEqual` true

  describe "Circulation flows" do
    -- a minimal Gameboard with one flow of the given amount
    let withFlow amount = """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": [4, 8], "resources": {}, "markets": {}, "transformations": {}, "entities": {}, "circulation": { "flows": [ { "from": "a", "to": "b", "resource": "M", "amount": """ <> amount <> """ } ] } }"""
    it "a negative flow amount is a typed validation error at decode" do
      case decodeGameboard (json (withFlow "\"-1\"")) of
        Left _ -> pure unit
        Right _ -> fail "a negative flow decoded"
    it "a zero flow amount is valid" do
      gb <- orThrow (decodeGameboard (json (withFlow "\"0\"")))
      map _.amount gb.world.recurring `shouldEqual` [ FixedAmount Q.zero ]
    it "a decoded Gameboard never carries a negative flow, so the allocate guard is not reachable from it" do
      case decodeGameboard (json (withFlow "\"-0.0001\"")) of
        Left _ -> pure unit
        Right gb -> fail ("a negative flow reached a decoded Gameboard: " <> show (map _.amount gb.world.recurring))

  describe "Decoding" do
    -- Pins the setting format the schema decoders read (docs/03).
    it "a tagged object must have exactly one key" do
      case decodeText """{ "rules": [ { "propose": [ { "trade": { "market": "m", "quantity": "1" }, "transform": { "transformation": "t", "quantity": "1" } } ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected a multi-key template error, got " <> show (map (const "a decoded strategy") other))
      case decodeText """{ "rules": [ { "propose": [ {} ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected an empty template error, got " <> show (map (const "a decoded strategy") other))
    -- Pins the setting format the schema decoders read (docs/03).
    it "an expression with two keys is rejected" do
      isLeft (decodeText """{ "rules": [ { "propose": [ { "trade": { "market": "m", "quantity": { "round": null, "param": "x" } } } ] } ] }""")
        `shouldEqual` true
    -- Pins the setting format the schema decoders read (docs/03).
    it "an unknown tag is a typed error naming its kind" do
      case decodeText """{ "rules": [ { "propose": [ { "bogus": {} } ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected a template error, got " <> show (map (const "a decoded strategy") other))
      case decodeText """{ "rules": [ { "when": [ { "bogus": [] } ], "propose": [] } ] }""" of
        Left (TypeMismatch m) | contains (Pattern "Cond form") m -> pure unit
        other -> fail ("expected a condition error, got " <> show (map (const "a decoded strategy") other))
    -- Pins the setting format the schema decoders read (docs/03).
    it "a well-formed strategy decodes" do
      isLeft (decodeText """{ "rules": [ { "when": [ { "ge": [ { "round": null }, "1" ] } ], "propose": [ { "noAction": null } ] } ], "sign": [ { "all": null } ] }""")
        `shouldEqual` false

-- Rules of the three kinds the test settings declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id EntityTag, origin :: Id EntityTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination } }

banR :: forall v. { jurisdiction :: Id EntityTag, target :: Id EntityTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "withholding", resource: r.capacity, reduce: r.units }

