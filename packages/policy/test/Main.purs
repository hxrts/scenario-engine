module Test.Policy.Main where

import Prelude

import Core.Num as Q
import Core.Types (Transferability(..), Capability(..), Controller(..), EntityKind(..), Id(..), Intent, IntentF(..), RefKind(..), RuleF(..), RuleChangeF(..), RuleKind(..), proposalId)
import Core.World (Event(..), View)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Foreign.Object as Object
import Core.Proposal (ProposalBody)
import Policy.Interpret (Env, nameOnly, propose, sign)
import Core.Strategy (Strategy, SignRule(..))
import Data.Array as Array
import Policy.Strategy (strategyRefs)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Testkit.Json (strategyOrThrow, decodeText)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Fixtures ----------------------------------------------------------------------------

-- | A seat in round 3, authorized to trade in energy-atlantic and enact access bans.
seat :: View
seat =
  { round: 3
  , self: { id: Id "EU", accounts: Map.empty, capacities: Map.empty, capabilities: [ MayTrade (Id "energy-atlantic"), MayEnact AccessBanRule ], kind: Actor (Seat (Id "EU")) }
  , prices: Map.fromFoldable [ Tuple (Id "energy-atlantic") (Q.fromInt 2) ]
  , rules: Map.empty
  , transferability: Map.empty
  , events: []
  , factors: Map.empty
  , toSign: []
  , markets: [ { id: Id "energy-atlantic", base: Id "E", quote: Id "M" } ]
  }

-- | The seat with access to two Energy Markets, Atlantic at 2 and Asia at 3, and a defence Market at 5.
twoMarkets :: View
twoMarkets = seat
  { self = seat.self { capabilities = [ MayTrade (Id "energy-atlantic"), MayTrade (Id "energy-asia"), MayTrade (Id "defence") ] }
  , prices = Map.fromFoldable [ Tuple (Id "energy-atlantic") (Q.fromInt 2), Tuple (Id "energy-asia") (Q.fromInt 3), Tuple (Id "defence") (Q.fromInt 5) ]
  , markets = [ { id: Id "energy-asia", base: Id "E", quote: Id "M" }, { id: Id "energy-atlantic", base: Id "E", quote: Id "M" }, { id: Id "defence", base: Id "D", quote: Id "M" } ]
  }

-- | Every draw is zero.
env :: Env
env = { view: seat, params: Object.empty, random: const Q.zero, market: nameOnly }

-- | Every draw except index 0 is 0.9.
highDraws :: Int -> Q.Q
highDraws i = if i == 0 then Q.zero else fromMaybe Q.zero (Q.parse "0.9")

trade :: String -> String -> Intent
trade m quantity = Trade { market: Id m, quantity: fromMaybe Q.zero (Q.parse quantity), limitPrice: Nothing }

intentsOf :: Array ProposalBody -> Array (Array Intent)
intentsOf = map _.intents

-- | The Drafts a text Strategy proposes in an environment.
drafts :: String -> Env -> Array ProposalBody
drafts text e = propose (strategyOrThrow text) e

-- | What a decode produced, for a failure message.
outcome :: Either JsonDecodeError Strategy -> String
outcome = case _ of
  Left e -> show e
  Right _ -> "a decoded strategy"

-- | Two Choose blocks, each with two options of different size.
chooseTwice :: String
chooseTwice =
  """{ "rules": [
  { "propose": [ { "choose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } }, { "trade": { "market": "energy-atlantic", "quantity": "2" } } ] } ] },
  { "propose": [ { "choose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } }, { "trade": { "market": "energy-atlantic", "quantity": "2" } } ] } ] }
] }"""

-- Tests -------------------------------------------------------------------------------

strategyParams :: Strategy -> Array String
strategyParams = map snd <<< Array.filter ((_ == ParameterRef) <<< fst) <<< strategyRefs

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Unknown parameters" do
    it "a clause reading a parameter the premise lacks does not fire, even under not" do
      let
        reads = strategyOrThrow """{"rules":[{"when":[{"gt":[{"param":"missing"},"0"]}],"propose":[{"trade":{"market":"energy-atlantic","quantity":"1"}}]}]}"""
        negated = strategyOrThrow """{"rules":[{"when":[{"not":{"gt":[{"param":"missing"},"0"]}}],"propose":[{"trade":{"market":"energy-atlantic","quantity":"1"}}]}]}"""
      propose reads env `shouldEqual` []
      propose negated env `shouldEqual` []
  describe "Choose draws" do
    it "two Choose blocks in one strategy draw independently" do
      intentsOf (propose (strategyOrThrow chooseTwice) env { random = highDraws })
        `shouldEqual` [ [ trade "energy-atlantic" "1" ], [ trade "energy-atlantic" "2" ] ]
    it "a Choose in a clause whose condition fails draws nothing" do
      let
        text =
          """{ "rules": [
          { "when": [ { "gt": [ "1", "2" ] } ], "propose": [ { "choose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] },
          { "propose": [ { "choose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } }, { "trade": { "market": "energy-atlantic", "quantity": "2" } } ] } ] }
        ] }"""
      -- the skipped clause draws nothing, so the second clause's first draw (index 0) picks option 1
      intentsOf (drafts text env { random = highDraws }) `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]

  describe "Templates" do
    it "a Deal drafts its transfers with self bound to the author, and its signers" do
      let
        text = """{ "rules": [ { "propose": [ { "deal": { "signers": ["DE"], "transfers": [ { "from": "self", "to": "DE", "resource": "E", "quantity": "2" } ] } } ] } ] }"""
      drafts text env `shouldEqual`
        [ { intents: [ Transfer { from: Id "EU", to: Id "DE", resource: Id "E", quantity: Q.fromInt 2 } ], signers: [ Id "DE" ], procedure: Nothing } ]
    it "a Procedure drafts its intents, its signers and its id" do
      let
        text = """{ "rules": [ { "propose": [ { "procedure": { "id": "eu-solidarity", "signers": ["DE"], "intents": [ { "trade": { "market": "energy-atlantic", "quantity": "-8" } } ] } } ] } ] }"""
      drafts text env `shouldEqual`
        [ { intents: [ trade "energy-atlantic" "-8" ], signers: [ Id "DE" ], procedure: Just (Id "eu-solidarity") } ]
    it "a Procedure may not contain a nested Procedure, Deal or Choose" do
      let nested inner = """{ "rules": [ { "propose": [ { "procedure": { "id": "p", "signers": [], "intents": [ """ <> inner <> """ ] } } ] } ] }"""
      isLeft (decodeText (nested """{ "deal": { "signers": [], "transfers": [] } }""")) `shouldEqual` true
      isLeft (decodeText (nested """{ "choose": [] }""")) `shouldEqual` true
      isLeft (decodeText (nested """{ "procedure": { "id": "q", "signers": [], "intents": [] } }""")) `shouldEqual` true
    it "noAction proposes nothing" do
      drafts """{ "rules": [ { "propose": [ { "noAction": null } ] } ] }""" env `shouldEqual` []
    it "a standalone transfer from self resolves to the acting Entity, as a Deal's does" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "transfer": { "from": "self", "to": "DE", "resource": "E", "quantity": "2" } } ] } ] }""" env { view = seat { self = seat.self { capabilities = [ MayTransfer (Id "E") ] } } })
        `shouldEqual` [ [ Transfer { from: Id "EU", to: Id "DE", resource: Id "E", quantity: Q.fromInt 2 } ] ]
    it "every entity position resolves self, including a condition's jurisdiction" do
      let e = env { view = seat { events = [ Enacted { round: 3, id: Id "rule-0", rule: AccessBan { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (drafts """{ "rules": [ { "when": [ { "restrictedLastRound": { "by": "CN", "target": "self" } } ], "propose": [ { "reciprocate": { "target": "CN" } } ] } ] }""" e)
        `shouldEqual` [ [ ChangeRule (Enact (AccessBan { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "each runs the clause once per accessible Market for the Resource, in id order" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": { "each": "E" }, "quantity": "3" } } ] } ] }""" env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-asia" "3" ], [ trade "energy-atlantic" "3" ] ]
    it "cheapest and costliest pick by price, and the condition and the order see the same Market" do
      intentsOf (drafts """{ "rules": [ { "when": [ { "le": [ { "price": { "cheapest": "E" } }, "2" ] } ], "propose": [ { "trade": { "market": { "cheapest": "E" }, "quantity": "3" } } ] } ] }""" env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-atlantic" "3" ] ]
      intentsOf (drafts """{ "rules": [ { "when": [ { "gt": [ { "price": { "costliest": "E" } }, "2" ] } ], "propose": [ { "trade": { "market": { "costliest": "E" }, "quantity": "-3" } } ] } ] }""" env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-asia" "-3" ] ]
    it "two selectors in one clause range over the product of their picks" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": { "cheapest": "E" }, "quantity": "1" } }, { "trade": { "market": { "each": "E" }, "quantity": "2" } } ] } ] }""" env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-atlantic" "1" ], [ trade "energy-asia" "2" ], [ trade "energy-atlantic" "1" ], [ trade "energy-atlantic" "2" ] ]
    it "a selector with no accessible Market proposes nothing, and a named Market in the same Strategy still does" do
      let none = env { view = seat { markets = [] } }
      intentsOf (drafts """{ "rules": [ { "when": [ { "le": [ { "price": { "cheapest": "E" } }, "9" ] } ], "propose": [ { "trade": { "market": { "cheapest": "E" }, "quantity": "3" } } ] }, { "propose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] }""" none)
        `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
    it "a price selector skips a Market with no price; each does not" do
      let unpriced = env { view = twoMarkets { prices = Map.fromFoldable [ Tuple (Id "energy-asia") (Q.fromInt 3) ] } }
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": { "cheapest": "E" }, "quantity": "1" } } ] }, { "propose": [ { "trade": { "market": { "each": "E" }, "quantity": "2" } } ] } ] }""" unpriced)
        `shouldEqual` [ [ trade "energy-asia" "1" ], [ trade "energy-asia" "2" ], [ trade "energy-atlantic" "2" ] ]
    it "a one-Market selector draws as a named Market does" do
      let one = """{ "rules": [ { "propose": [ { "choose": [ { "noAction": null }, { "trade": { "market": MARKET, "quantity": "3" } } ] } ] } ] }"""
      drafts (replaceAll (Pattern "MARKET") (Replacement "{ \"each\": \"E\" }") one) env { random = highDraws } `shouldEqual` drafts (replaceAll (Pattern "MARKET") (Replacement "\"energy-atlantic\"") one) env { random = highDraws }
    it "an unknown Market id is a load-time name" do
      strategyRefs (strategyOrThrow """{ "rules": [ { "propose": [ { "trade": { "market": "$home", "quantity": "3" } } ] } ] }""") `shouldEqual` [ Tuple Market "$home" ]
    it "a route drafts a routed Trade with its pair and limit" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "route": { "base": "E", "quote": "M", "quantity": "3", "limit": "2" } } ] } ] }""" env)
        `shouldEqual` [ [ Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt 3, limitPrice: Just (Q.fromInt 2) } ] ]
    it "a zero quantity yields no Trade or Transform; a negative Transform is passed on for the engine to reject" do
      let
        text =
          """{ "rules": [ { "propose": [
          { "trade": { "market": "energy-atlantic", "quantity": "0" } },
          { "transform": { "transformation": "t", "quantity": "0" } },
          { "transform": { "transformation": "t", "quantity": "-1" } }
        ] } ] }"""
      let
        e = env { view = seat { self = seat.self { capabilities = [ MayTransform (Id "t") ] } } }
      intentsOf (drafts text e) `shouldEqual` [ [ Transform { by: Id "EU", transformation: Id "t", quantity: Q.fromInt (-1) } ] ]

  describe "Conditions and restrictions" do
    it "reciprocate enacts the mirror of a restriction enacted against this Entity last round" do
      let
        e = env { view = seat { events = [ Enacted { round: 3, id: Id "rule-0", rule: AccessBan { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (drafts """{ "rules": [ { "propose": [ { "reciprocate": { "target": "CN" } } ] } ] }""" e)
        `shouldEqual` [ [ ChangeRule (Enact (AccessBan { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "reciprocate draws nothing when the restriction was enacted at an earlier boundary" do
      let
        e = env { view = seat { events = [ Enacted { round: 2, id: Id "rule-0", rule: AccessBan { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (drafts """{ "rules": [ { "propose": [ { "reciprocate": { "target": "CN" } } ] } ] }""" e) `shouldEqual` []
    it "restrictedLastRound holds only for a restriction enacted at this round's boundary" do
      let
        text = """{ "rules": [ { "when": [ { "restrictedLastRound": { "by": "CN", "target": "EU" } } ], "propose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] }"""
        fires events = intentsOf (propose (strategyOrThrow text) env { view = seat { events = events } })
        fired = [ Enacted { round: 3, id: Id "r", rule: AccessBan { jurisdiction: Id "CN", target: Id "EU" } } ]
        stale = [ Enacted { round: 2, id: Id "r", rule: AccessBan { jurisdiction: Id "CN", target: Id "EU" } } ]
      fires fired `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      fires stale `shouldEqual` []
    it "restrictionInForce holds when a rule by that jurisdiction is in force" do
      let
        text = """{ "rules": [ { "when": [ { "restrictionInForce": { "by": "US" } } ], "propose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] }"""
        at r = intentsOf (drafts text env { view = seat { rules = Map.singleton (Id "r") r } })
      at (AccessBan { jurisdiction: Id "US", target: Id "CN" }) `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      at (Tariff { jurisdiction: Id "CN", origin: Id "US", rate: Q.zero }) `shouldEqual` []

  describe "Parameters and factors" do
    it "transformedLastRound reads its threshold as an expression, so a parameter sweep changes the outcome" do
      let
        text = """{ "rules": [ { "when": [ { "transformedLastRound": { "by": "US", "transformation": "major-military-action", "atLeast": { "param": "threshold" } } } ], "propose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] }"""
        fires threshold = drafts text env { params = Object.fromFoldable [ Tuple "threshold" (fromMaybe Q.zero (Q.parse threshold)) ], view = seat { events = [ Transformed { round: 2, by: Id "US", transformation: Id "major-military-action", quantity: Q.fromInt 10, paid: Q.zero } ] } }
      intentsOf (fires "6") `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      intentsOf (fires "24") `shouldEqual` []
      strategyParams (strategyOrThrow text) `shouldEqual` [ "threshold" ]
    it "a Factor with no stored factor is 1" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": "energy-atlantic", "quantity": { "mul": [ "5", { "factor": "E" } ] } } } ] } ] }""" env)
        `shouldEqual` [ [ trade "energy-atlantic" "5" ] ]
    it "a Factor uses the stored factor" do
      let
        e = env { view = seat { factors = Map.fromFoldable [ Tuple (Id "E") (fromMaybe Q.zero (Q.parse "0.5")) ] } }
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": "energy-atlantic", "quantity": { "mul": [ "5", { "factor": "E" } ] } } } ] } ] }""" e)
        `shouldEqual` [ [ trade "energy-atlantic" "2.5" ] ]
    it "a Param the premise does not define evaluates to zero, so an order of that size is dropped" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "trade": { "market": "energy-atlantic", "quantity": { "param": "missing" } } } ] } ] }""" env)
        `shouldEqual` []
    it "strategyParams lists every premise parameter a Strategy reads" do
      strategyParams (strategyOrThrow """{ "rules": [ { "when": [ { "gt": [ { "param": "limit" }, "1" ] } ], "propose": [ { "trade": { "market": "m", "quantity": { "param": "size" } } } ] } ] }""")
        `shouldEqual` [ "limit", "size" ]

  describe "Signing" do
    it "signs by rule: procedures, facility, or all" do
      let
        procP = { id: proposalId 3 (Id "A") 0, author: Id "A", intents: [], signers: [], procedure: Just (Id "eu-trade") }
        facP = { id: proposalId 3 (Id "A") 1, author: Id "A", intents: [ Transform { by: Id "A", transformation: Id "energy-shipping", quantity: Q.fromInt 1 } ], signers: [], procedure: Nothing }
        plainP = { id: proposalId 3 (Id "A") 2, author: Id "A", intents: [], signers: [], procedure: Nothing }
        signedBy rules = sign { rules: [], sign: rules } env { view = seat { toSign = [ procP, facP, plainP ] } }
      signedBy [ SignProcedures [ Id "eu-trade" ] ] `shouldEqual` [ procP.id ]
      signedBy [ SignFacility (Id "energy-shipping") ] `shouldEqual` [ facP.id ]
      signedBy [ SignAll ] `shouldEqual` [ procP.id, facP.id, plainP.id ]
      signedBy [] `shouldEqual` []

  describe "Decision language: bans, withholding, repeal, conditional signing, deal transforms" do
    let
      -- the seat, authorized to enact the given kinds of Rule (the interpreter filters by authority)
      enacting caps = env { view = seat { self = seat.self { capabilities = caps } } }
    it "enactAccessBan drafts a ban by the author's jurisdiction on its target" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "enactAccessBan": { "target": "CN" } } ] } ] }""" (enacting [ MayEnact AccessBanRule ]))
        `shouldEqual` [ [ ChangeRule (Enact (AccessBan { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "enactWithholding drafts a withholding of units of a capacity the author holds" do
      intentsOf (drafts """{ "rules": [ { "propose": [ { "enactWithholding": { "capacity": "semiconductors", "units": "5" } } ] } ] }""" (enacting [ MayEnact WithholdingRule ]))
        `shouldEqual` [ [ ChangeRule (Enact (Withholding { capacity: Id "semiconductors", units: Q.fromInt 5 })) ] ]
    it "repeal drafts a repeal of the named rule, and needs enactment authority" do
      let text = """{ "rules": [ { "propose": [ { "repeal": { "rule": "r2/US/0/rule-0" } } ] } ] }"""
      let inForce e = e { view = e.view { rules = Map.singleton (Id "r2/US/0/rule-0") (AccessBan { jurisdiction: Id "EU", target: Id "CN" }) } }
      intentsOf (drafts text (inForce (enacting [ MayEnact AccessBanRule ]))) `shouldEqual` [ [ ChangeRule (Repeal (Id "r2/US/0/rule-0")) ] ]
      intentsOf (drafts text (inForce (enacting []))) `shouldEqual` []
      -- the engine also needs the capability for the repealed Rule's kind, and a Rule in force
      intentsOf (drafts text (inForce (enacting [ MayEnact TariffRule ]))) `shouldEqual` []
      intentsOf (drafts text (enacting [ MayEnact AccessBanRule ])) `shouldEqual` []
    it "a Free Resource is drafted without a capability, and a Restricted one needs it" do
      let
        text = """{ "rules": [ { "propose": [ { "transfer": { "from": "self", "to": "DE", "resource": "M", "quantity": "2" } } ] } ] }"""
        as t caps = (enacting caps) { view = (enacting caps).view { transferability = Map.singleton (Id "M") t } }
        give = [ [ Transfer { from: Id "EU", to: Id "DE", resource: Id "M", quantity: Q.fromInt 2 } ] ]
      intentsOf (drafts text (as Free [])) `shouldEqual` give
      intentsOf (drafts text (as Restricted [])) `shouldEqual` []
      intentsOf (drafts text (as Restricted [ MayTransfer (Id "M") ])) `shouldEqual` give
    it "a Deal bundles its transforms with its transfers, all by the author" do
      let text = """{ "rules": [ { "propose": [ { "deal": { "signers": [ "EU", "IN" ], "transfers": [], "transforms": [ { "transformation": "energy-shipping", "quantity": "5" } ] } } ] } ] }"""
      intentsOf (drafts text env) `shouldEqual` [ [ Transform { by: Id "EU", transformation: Id "energy-shipping", quantity: Q.fromInt 5 } ] ]
    it "signIf signs a Proposal only while its conditions hold" do
      let
        procP = { id: proposalId 3 (Id "A") 0, author: Id "A", intents: [], signers: [], procedure: Just (Id "eu-solidarity") }
        signedIn r = sign (strategyOrThrow """{ "sign": [ { "signIf": { "when": [ { "ge": [ { "round": null }, "4" ] } ], "rule": { "procedures": [ "eu-solidarity" ] } } } ] }""") env { view = seat { round = r, toSign = [ procP ] } }
      signedIn 3 `shouldEqual` []
      signedIn 4 `shouldEqual` [ procP.id ]

  describe "Load-time references" do
    it "a transfer to an entity the Gameboard lacks is a load-time name, and self is never one" do
      strategyRefs (strategyOrThrow """{ "rules": [ { "propose": [ { "transfer": { "from": "self", "to": "bogus", "resource": "E", "quantity": "2" } } ] } ] }""") `shouldEqual`
        [ Tuple ResourceRef "E", Tuple EntityRef "bogus" ]
