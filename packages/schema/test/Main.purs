module Test.Schema.Main where

import Prelude

import Data.Map as Map

import Core.Types (proposalId, Capability(..), Id(..), Intent, IntentF(..), RuleF(..), RuleChangeF(..), RuleKind(..))

import Core.Num as Q
import Core.Strategy (Strategy)
import Core.Shape (Format(..), encodeWith, decodeJsonWith)
import Data.Foldable (for_)
import Schema.Build.Decision (selfAccessBan, selfTariff, selfTransform, asTemplate, choose, deal, eqQ, factor, ge, gt, holding, le, lit, max', mulQ, noAction, not', param, price, procedure, reciprocate, restrictedLastRound, restrictionInForce, roundQ, signAll, signFacility, signIf, signProcedures, subQ, transformedLastRound)
import Schema.Build.Decision as D
import Schema.Build.Intent (enactWithholding, repeal, route, transfer)
import Schema.Premise.Intent (capabilityShape, intentShape, ruleShape)
import Schema.Premise.Strategy (decodeStrategy, encodeStrategy)
import Core.Num (Q)
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Premise.Fixture (roundShape, toRound)
import Schema.Round (Expect)
import Schema.Premise.Gameboard (decodeGameboard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Testkit.Suite (strategyOrThrow)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Parse JSON text, crashing the test on a syntax error.
json :: String -> Json
json text = case jsonParser text of
  Left e -> unsafeCrashWith ("test JSON does not parse: " <> e)
  Right j -> j

-- | The smallest valid Gameboard document: no resources, markets, facilities, or Entities.
minimalGameboard :: String -> String
minimalGameboard roundRange =
  """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": """ <> roundRange
    <> """, "resources": {}, "markets": {}, "transformations": {}, "entities": {} }"""

-- | The expectations of a one-round document.
expectOf :: String -> Expect
expectOf text = case decodeJsonWith Premise roundShape (json text) of
  Left e -> unsafeCrashWith ("round does not decode: " <> show e)
  Right r -> (toRound 1 r).expect

-- | The decoders the tests read: each is the shape's decoder, with the core value it makes.
capability :: String -> Either JsonDecodeError Capability
capability s = decodeJsonWith Premise capabilityShape (fromString s)

rule :: Json -> Either JsonDecodeError (RuleF Q)
rule = decodeJsonWith Premise ruleShape

intent :: Json -> Either JsonDecodeError Intent
intent = decodeJsonWith Premise intentShape

decodeText :: String -> Either JsonDecodeError Strategy
decodeText text = case jsonParser text of
  Left e -> Left (TypeMismatch e)
  Right j -> decodeStrategy j

-- | A premise shape reports a failed form as text: the message names the sum that failed.
isTemplateError :: String -> Boolean
isTemplateError m = contains (Pattern "Template form") m

-- | A seat in round 3, authorized to trade in energy-atlantic and enact access bans.
outcome :: Either JsonDecodeError Strategy -> String
outcome = case _ of
  Left e -> show e
  Right _ -> "a decoded strategy"

-- | Two Choose blocks, each with two options of different size.

-- | A quantity literal in a test value.
qv :: String -> Q
qv t = fromMaybe (Q.fromInt 0) (Q.parse t)

-- | One Strategy per form the premise format writes, so every constructor of the AST round-trips.
roundTripStrategies :: Array Strategy
roundTripStrategies =
  [ D.strategy { rules: [ D.rule [] [ asTemplate (D.tradeAt { market: D.named "m", quantity: lit 1, limit: Nothing }) ] ], sign: [] }
  , D.strategy
      { rules:
          [ D.rule [ gt (price "m") (lit "2.5"), ge (holding "E") (param "p"), le (roundQ) (lit 3), eqQ (factor "d") (lit 0) ]
              [ asTemplate (route { base: "E", quote: "M", quantity: lit (-4), limit: Just (lit 2) })
              , asTemplate (selfTransform { transformation: "t", quantity: mulQ (lit 2) (max' (lit 1) (subQ (lit 4) (lit 3))) })
              , asTemplate (transfer { from: "self", to: "X", resource: "E", quantity: lit 1 })
              , asTemplate (selfTariff { origin: "CN", rate: lit "0.5" })
              , asTemplate (selfAccessBan { target: "CN" })
              , asTemplate (enactWithholding { capacity: "c", units: lit 2 })
              , asTemplate (repeal { rule: "r2/US/0/rule-0" })
              ]
          , D.rule [ not' (eqQ (lit 1) (lit 2)), transformedLastRound { by: "US", transformation: "t", atLeast: lit 1 }, restrictedLastRound { by: "US", target: "CN" }, restrictionInForce { by: "EU" } ]
              [ reciprocate { target: "US" }
              , choose [ noAction, asTemplate (D.tradeAt { market: D.named "m", quantity: lit 1, limit: Just (lit 3) }) ]
              ]
          ]
      , sign: [ signIf [ ge roundQ (lit 5) ] (signProcedures [ "p" ]), signFacility "f", signAll ]
      }
  , D.strategy
      { rules:
          [ D.rule [] [ procedure { id: "proc", signers: [ "DE" ], intents: [ transfer { from: "self", to: "EU", resource: "E", quantity: lit 8 } ] } ]
          , D.rule [] [ deal { signers: [ "DE", "IN" ], intents: [ transfer { from: "self", to: "IN", resource: "E", quantity: lit 2 }, selfTransform { transformation: "t", quantity: lit 1 } ] } ]
          ]
      , sign: []
      }
  ]

-- | One Intent of each kind, with explicit entities, in the fixture format (the premise tags).
roundTripIntents :: Array Intent
roundTripIntents =
  [ Trade { market: Id "m", quantity: Q.fromInt 10, limitPrice: Nothing }
  , Trade { market: Id "m", quantity: Q.fromInt 10, limitPrice: Just (qv "2.5") }
  , Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt (-3), limitPrice: Nothing }
  , Transfer { from: Id "US", to: Id "CN", resource: Id "E", quantity: Q.fromInt 2 }
  , Transform { by: Id "US", transformation: Id "t", quantity: Q.fromInt 1 }
  , ChangeRule (Enact (Tariff { jurisdiction: Id "US", origin: Id "CN", rate: qv "0.25" }))
  , ChangeRule (Enact (AccessBan { jurisdiction: Id "US", target: Id "IN" }))
  , ChangeRule (Enact (Withholding { capacity: Id "semiconductors", units: Q.fromInt 5 }))
  ]

roundTripCapabilities :: Array Capability
roundTripCapabilities =
  [ MayTrade (Id "m")
  , MayTransfer (Id "E")
  , MayTransform (Id "t")
  , MayEnact TariffRule
  , MayEnact AccessBanRule
  , MayEnact WithholdingRule
  ]

strategy :: String -> Strategy
strategy = strategyOrThrow

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Round trips: decode (encode x) == x for each premise shape" do
    it "every Strategy form, in the premise format" do
      for_ roundTripStrategies \p ->
        (decodeStrategy (encodeStrategy p) == Right p) `shouldEqual` true
    it "every Intent kind, in the fixture format" do
      for_ roundTripIntents \i ->
        (decodeJsonWith Premise intentShape (encodeWith Premise intentShape i) == Right i) `shouldEqual` true
    it "every Capability, in the premise format" do
      for_ roundTripCapabilities \c ->
        (decodeJsonWith Premise capabilityShape (encodeWith Premise capabilityShape c) == Right c) `shouldEqual` true

  describe "Capability tags" do
    it "decodes each capability kind" do
      capability "trade:energy-atlantic" `shouldEqual` Right (MayTrade (Id "energy-atlantic"))
      capability "transfer:E" `shouldEqual` Right (MayTransfer (Id "E"))
      capability "transform:defense-procurement" `shouldEqual` Right (MayTransform (Id "defense-procurement"))
      capability "enact:withholding" `shouldEqual` Right (MayEnact WithholdingRule)
      capability "enact:tariff" `shouldEqual` Right (MayEnact TariffRule)
    it "rejects an unknown capability kind" do
      isLeft (capability "launch:missile") `shouldEqual` true
    it "rejects an unknown enactment" do
      isLeft (capability "enact:embargo") `shouldEqual` true
    it "rejects a capability tag without its argument" do
      isLeft (capability "trade") `shouldEqual` true

  describe "Rules" do
    it "decodes each rule key, including withholding" do
      rule (json """{ "withholding": { "capacity": "semiconductors", "units": "3" } }""")
        `shouldEqual` Right (Withholding { capacity: Id "semiconductors", units: Q.fromInt 3 })
      rule (json """{ "accessBan": { "jurisdiction": "US", "target": "CN" } }""")
        `shouldEqual` Right (AccessBan { jurisdiction: Id "US", target: Id "CN" })
    it "rejects an unknown rule key with a typed error" do
      case rule (json """{ "embargo": {} }""") of
        Left _ -> pure unit
        other -> fail ("expected a rule error, got " <> show (map (const "a rule") other))
    it "rejects a rule object with two keys" do
      isLeft (rule (json """{ "accessBan": { "jurisdiction": "US", "target": "CN" }, "tariff": { "jurisdiction": "US", "origin": "CN", "rate": "0.2" } }"""))
        `shouldEqual` true

  describe "Intents" do
    it "decodes a trade with an optional limit" do
      intent (json """{ "trade": { "market": "energy-atlantic", "quantity": "10", "limit": "2.04" } }""")
        `shouldEqual` Right (Trade { market: Id "energy-atlantic", quantity: Q.fromInt 10, limitPrice: Just (fromMaybe Q.zero (Q.parse "2.04")) })
    it "decodes a routed trade, with a null market, by its pair" do
      intent (json """{ "trade": { "market": null, "base": "E", "quote": "M", "quantity": "10", "limit": "2.04" } }""")
        `shouldEqual` Right (Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt 10, limitPrice: Just (fromMaybe Q.zero (Q.parse "2.04")) })
    it "decodes an enactment through its rule" do
      intent (json """{ "enact": { "accessBan": { "jurisdiction": "US", "target": "CN" } } }""")
        `shouldEqual` Right (ChangeRule (Enact (AccessBan { jurisdiction: Id "US", target: Id "CN" })))
    it "rejects an intent with two keys" do
      case intent (json """{ "trade": { "market": "m", "quantity": "1" }, "transfer": { "from": "a", "to": "b", "resource": "E", "quantity": "1" } }""") of
        Left _ -> pure unit
        other -> fail ("expected an intent error, got " <> show (map (const "an intent") other))
    it "rejects an unknown intent key" do
      isLeft (intent (json """{ "gift": {} }""")) `shouldEqual` true

  describe "Round expectations" do
    it "an absent expectation is absent, and an empty list is a checked empty list" do
      let
        e = expectOf """{ "expect": { "deliveries": [] } }"""
      e.deliveries `shouldEqual` Just []
      e.accounts `shouldEqual` Map.empty
      e.rejected `shouldEqual` Nothing
    it "a proposal is named by its author, or by author and index" do
      let
        e = expectOf """{ "expect": { "rejected": [ "US", { "author": "US", "index": 1 } ] } }"""
      e.rejected `shouldEqual` Just [ proposalId 1 (Id "US") 0, proposalId 1 (Id "US") 1 ]

  describe "Gameboard documents" do
    it "decodes a minimal valid Gameboard and its roundQ range" do
      case decodeGameboard (json (minimalGameboard "[4, 8]")) of
        Left e -> fail (show e)
        Right gb -> gb.rounds `shouldEqual` { min: 4, max: 8 }
    it "rejects a roundQ range that is malformed, not ordered, or below one" do
      isLeft (decodeGameboard (json (minimalGameboard "[8, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[0, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[4]"))) `shouldEqual` true
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
      case decodeGameboard (json (withFlow "\"0\"")) of
        Left e -> fail (show e)
        Right gb -> map _.amount gb.world.circulation.flows `shouldEqual` [ Q.zero ]
    it "a decoded Gameboard never carries a negative flow, so the allocate guard is not reachable from it" do
      case decodeGameboard (json (withFlow "\"-0.0001\"")) of
        Left _ -> pure unit
        Right gb -> fail ("a negative flow reached a decoded Gameboard: " <> show (map _.amount gb.world.circulation.flows))

  describe "Decoding" do
    it "a tagged object must have exactly one key" do
      case decodeText """{ "rules": [ { "propose": [ { "trade": { "market": "m", "quantity": "1" }, "transform": { "transformation": "t", "quantity": "1" } } ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected a multi-key template error, got " <> outcome other)
      case decodeText """{ "rules": [ { "propose": [ {} ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected an empty template error, got " <> outcome other)
    it "an expression with two keys is rejected" do
      isLeft (decodeText """{ "rules": [ { "propose": [ { "trade": { "market": "m", "quantity": { "round": null, "param": "x" } } } ] } ] }""")
        `shouldEqual` true
    it "an unknown tag is a typed error naming its kind" do
      case decodeText """{ "rules": [ { "propose": [ { "bogus": {} } ] } ] }""" of
        Left (TypeMismatch m) | isTemplateError m -> pure unit
        other -> fail ("expected a template error, got " <> outcome other)
      case decodeText """{ "rules": [ { "when": [ { "bogus": [] } ], "propose": [] } ] }""" of
        Left (TypeMismatch m) | contains (Pattern "Cond form") m -> pure unit
        other -> fail ("expected a condition error, got " <> outcome other)
    it "a well-formed strategy decodes" do
      isLeft (decodeText """{ "rules": [ { "when": [ { "ge": [ { "round": null }, "1" ] } ], "propose": [ { "noAction": null } ] } ], "sign": [ { "all": null } ] }""")
        `shouldEqual` false

