module Test.Schema.Main where

import Prelude

import Core.Num as Q
import Core.Strategy (Strategy)
import Core.Types (Capability(..), Id(..), IntentF(..), RuleF(..), RuleChangeF(..), RuleKind(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Decode.Common (capability)
import Schema.Decode.Gameboard (decodeConfig, decodeGameboard)
import Schema.Decode.Intents (intent, rule)
import Schema.Decode.Rounds (Expect, decodeRound)
import Schema.Decode.Strategy (decodeStrategy)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
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
expectOf text = case decodeRound 1 (json text) of
  Left e -> unsafeCrashWith ("round does not decode: " <> show e)
  Right r -> r.expect

decodeText :: String -> Either JsonDecodeError Strategy
decodeText text = case jsonParser text of
  Left e -> Left (TypeMismatch e)
  Right j -> decodeStrategy j

-- | Decode, crashing the test on any error: a strategy that fails to decode must never yield
-- | an empty strategy that passes vacuously.
strategy :: String -> Strategy
strategy text = case decodeText text of
  Left e -> unsafeCrashWith ("strategy does not decode: " <> show e)
  Right p -> p

-- | A seat in round 3, authorized to trade in energy-atlantic and enact access bans.
outcome :: Either JsonDecodeError Strategy -> String
outcome = case _ of
  Left e -> show e
  Right _ -> "a decoded strategy"

-- | Two Choose blocks, each with two options of different size.

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
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
        Left (Named "rule" (UnexpectedValue _)) -> pure unit
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
        Left (Named "intent" (UnexpectedValue _)) -> pure unit
        other -> fail ("expected an intent error, got " <> show (map (const "an intent") other))
    it "rejects an unknown intent key" do
      isLeft (intent (json """{ "gift": {} }""")) `shouldEqual` true

  describe "Round expectations" do
    it "an absent expectation is absent, and an empty list is a checked empty list" do
      let
        e = expectOf """{ "expect": { "deliveries": [] } }"""
      e.deliveries `shouldEqual` Just []
      e.accounts `shouldEqual` Nothing
      e.rejected `shouldEqual` Nothing
    it "a proposal is named by its author, or by author and index" do
      let
        e = expectOf """{ "expect": { "rejected": [ "US", { "author": "US", "index": 1 } ] } }"""
      map (map _.index) e.rejected `shouldEqual` Just [ 0, 1 ]

  describe "Gameboard documents" do
    it "decodes a minimal valid Gameboard and its round range" do
      case decodeGameboard (json (minimalGameboard "[4, 8]")) of
        Left e -> fail (show e)
        Right gb -> gb.rounds `shouldEqual` { min: 4, max: 8 }
    it "rejects a round range that is malformed, not ordered, or below one" do
      isLeft (decodeGameboard (json (minimalGameboard "[8, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[0, 4]"))) `shouldEqual` true
      isLeft (decodeGameboard (json (minimalGameboard "[4]"))) `shouldEqual` true
    it "has no default round range" do
      isLeft (decodeGameboard (json """{ "id": "t", "version": "v0", "numeraire": "M", "horizon": 5, "resources": {}, "markets": {}, "transformations": {}, "entities": {} }""")) `shouldEqual` true

  describe "Circulation flows" do
    -- a minimal Gameboard with one flow of the given amount
    let withFlow amount = """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": [4, 8], "resources": {}, "markets": {}, "transformations": {}, "entities": {}, "circulation": { "flows": [ { "from": "a", "to": "b", "resource": "M", "amount": """ <> amount <> """ } ] } }"""
    it "a negative flow amount is a typed validation error at decode" do
      case decodeGameboard (json (withFlow "\"-1\"")) of
        Left (Named "flow amount" (UnexpectedValue _)) -> pure unit
        Left e -> fail ("expected a flow amount error, got " <> show e)
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
        Left (Named "template" (UnexpectedValue _)) -> pure unit
        other -> fail ("expected a multi-key template error, got " <> outcome other)
      case decodeText """{ "rules": [ { "propose": [ {} ] } ] }""" of
        Left (Named "template" (UnexpectedValue _)) -> pure unit
        other -> fail ("expected an empty template error, got " <> outcome other)
    it "an expression with two keys is rejected" do
      isLeft (decodeText """{ "rules": [ { "propose": [ { "trade": { "market": "m", "quantity": { "round": null, "param": "x" } } } ] } ] }""")
        `shouldEqual` true
    it "an unknown tag is a typed error naming its kind" do
      case decodeText """{ "rules": [ { "propose": [ { "bogus": {} } ] } ] }""" of
        Left (Named "template" (UnexpectedValue _)) -> pure unit
        other -> fail ("expected a template error, got " <> outcome other)
      case decodeText """{ "rules": [ { "when": [ { "bogus": [] } ], "propose": [] } ] }""" of
        Left (Named "condition" (UnexpectedValue _)) -> pure unit
        other -> fail ("expected a condition error, got " <> outcome other)
    it "a well-formed strategy decodes" do
      isLeft (decodeText """{ "rules": [ { "when": [ { "ge": [ { "round": null }, "1" ] } ], "propose": [ { "noAction": null } ] } ], "sign": [ { "all": null } ] }""")
        `shouldEqual` false

