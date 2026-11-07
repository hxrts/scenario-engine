module Test.Board.Main where

import Prelude

import Board.Decode.Common (capability)
import Board.Decode.Intents (intent, rule)
import Board.Decode.Rounds (Expect, decodeRound)
import Board.Decode.World (decodeFixture)
import Board.Gameboard.Instantiate (instantiate, mergeRequirements)
import Board.Gameboard.Schema (decodeConfig, decodeGameboard, withCatalog)
import Core.Num as Q
import Core.Types (Capability(..), Id(..), Intent(..), Rule(..), RuleChange(..), RuleKind(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as Object
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafeCrashWith)
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

  describe "Requirement merge (docs/04 §4.2)" do
    -- an operating requirement of the US in money, with the given amount
    let req amount = { entity: Id "US", resource: Id "M", amount, penalty: Q.zero }
    it "two explicit amounts for one Entity and Resource are summed" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req (Just (Q.fromInt 4)) ]) `shouldEqual` [ Just (Q.fromInt 7) ]
    it "an explicit amount and one equal to purchases both survive the merge, neither replaced by the other" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req Nothing ]) `shouldEqual` [ Just (Q.fromInt 3), Nothing ]
    it "two requirements equal to purchases stay one" do
      map _.amount (mergeRequirements [ req Nothing, req Nothing ]) `shouldEqual` [ Nothing ]

  describe "Gameboard and fixture documents" do
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
    it "a decoded Gameboard's world carries no seed; instantiation sets it" do
      case decodeGameboard (json (minimalGameboard "[4, 8]")), decodeConfig (json """{ "seed": 9, "rounds": 4 }""") of
        Left e, _ -> fail (show e)
        _, Left e -> fail (show e)
        Right raw, Right config -> do
          -- the decoded world is a record with no seed field, not a seed placeholder
          Object.member "seed" (unsafeCoerce raw.world :: Object.Object Json) `shouldEqual` false
          case instantiate (withCatalog Map.empty raw) config of
            Left es -> fail (show es)
            Right initial -> show initial.world.seed `shouldEqual` "9"
    it "a fixture requires its seed" do
      isLeft (decodeFixture (json """{ "numeraire": "M", "resources": {}, "markets": {}, "transformations": {}, "entities": {} }""")) `shouldEqual` true
      case decodeFixture (json """{ "seed": 7, "numeraire": "M", "resources": {}, "markets": {}, "transformations": {}, "entities": {} }""") of
        Left e -> fail (show e)
        Right d -> show d.world.seed `shouldEqual` "7"

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
