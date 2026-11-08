module Test.Policy.Main where

import Prelude

import Core.Num as Q
import Core.Draw (Seed(..))
import Core.Map (values)
import Core.Engine.Prepare (observe, prepareRound)
import Core.Engine.Screen (screen)
import Core.Types (EntityId, Transferability(..), Capability(..), Controller(..), EntityKind(..), Id(..), Intent, IntentF(..), RefKind(..), RuleF(..), RuleChangeF(..), RuleKind(..), proposalId, samePositions, traverseIntent)
import Core.World (Event(..), RejectReason(..), View, WorldState)
import Data.Array.NonEmpty as NEA
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck.Gen (Gen, elements, oneOf)
import Test.Spec.QuickCheck (quickCheck)
import Testkit.Meridian (unseated)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either(..), either, isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Foreign.Object as Object
import Core.Proposal (ProposalBody)
import Policy.Interpret (Env, envFor, nameOnly, propose, sign)
import Core.Strategy (Expr(..), MarketRef(..), Strategy, SignRule(..), Template(..))
import Data.Array as Array
import Policy.Strategy (strategyRefs)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Testkit.Json (strategyOrThrow, decodeText)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- The agreement property ----------------------------------------------------------------

pick :: forall a. a -> Array a -> Gen a
pick x xs = elements (NEA.cons' x xs)

-- | Screening's outcome for one Intent is settled by what a View cannot answer: the rest is `validIntent`.
engineOnly :: RejectReason -> Boolean
engineOnly = case _ of
  OwnerMustSign _ -> true
  CrossRegionNeedsTransport -> true
  NoTransportFacility -> true
  ShipmentBanned -> true
  _ -> false

-- | An Intent over the world's names, plus a name it lacks at every position.
genIntent :: WorldState -> EntityId -> Gen Intent
genIntent w self = do
  let
    entity = pick self (Array.fromFoldable (Map.keys w.entities) <> [ Id "ghost" ])
    market = pick (Id "ghost") (Array.fromFoldable (Map.keys w.markets))
    resource = pick (Id "ghost") (Array.fromFoldable (Map.keys w.resources))
    facility = pick (Id "ghost") (Array.fromFoldable (Map.keys w.transformations))
    capacity = pick (Id "semiconductors") [ Id "ghost", Id "energy" ]
    rule = pick (Id "r-tariff") [ Id "r-held", Id "ghost" ]
    quantity = map Q.fromInt (pick 1 [ 2, -1, 30, 100 ])
    limit = pick Nothing [ Just (Q.fromInt 2) ]
    enacted = oneOf
      ( NEA.cons'
          (map Tariff ({ jurisdiction: _, origin: _, rate: _ } <$> entity <*> entity <*> quantity))
          [ map AccessBan ({ jurisdiction: _, target: _ } <$> entity <*> entity)
          , map Withholding ({ capacity: _, units: _ } <$> capacity <*> quantity)
          ]
      )
  oneOf
    ( NEA.cons'
        (map Trade ({ market: _, quantity: _, limitPrice: _ } <$> market <*> quantity <*> limit))
        [ map Route ((\base quote qty lim -> { pair: { base, quote }, quantity: qty, limitPrice: lim }) <$> resource <*> resource <*> quantity <*> limit)
        , map Transfer ({ from: _, to: _, resource: _, quantity: _ } <$> entity <*> entity <*> resource <*> quantity)
        , map Transform ({ by: _, transformation: _, quantity: _ } <$> entity <*> facility <*> quantity)
        , map (ChangeRule <<< Enact) enacted
        , map (ChangeRule <<< Repeal) rule
        ]
    )

-- | The interpreter's filter and screening's static checks agree on one generated Intent.
agreement :: WorldState -> Gen Result
agreement w = do
  self <- pick (fromMaybe' (\_ -> unsafeCrashWith "no entities") (Array.head entities)) entities
  i <- genIntent w self.id
  let
    template = un Identity (traverseIntent (samePositions { market = Identity <<< Named, value = Identity <<< Lit }) i)
    strategy = { rules: [ { when: [], propose: [ IntentT template ] } ], sign: [] }
    drafted = not (Array.null (propose strategy (envFor { params: Object.empty, seed: Seed 1, source: "property" } self.id (observe w self))))
    submitted = { proposal: { id: proposalId w.round self.id 0, author: self.id, intents: [ i ], signers: [], procedure: Nothing }, signatures: [] }
    refused = case _ of
      Rejected x -> not (engineOnly x.reason)
      _ -> true
    accepted = either (not <<< Array.any refused) (const true) (screen w [] submitted)
  pure (drafted == accepted <?> show i <> " by " <> show self.id <> ": drafted " <> show drafted <> ", accepted " <> show accepted)
  where
  entities = values w.entities

-- Fixtures ----------------------------------------------------------------------------

-- | A seat in round 3, authorized to trade in energy-atlantic and enact access bans.
seat :: View
seat =
  { round: 3
  , self: { id: Id "EU", accounts: Map.empty, capacities: Map.empty, capabilities: [ MayTrade (Id "energy-atlantic"), MayEnact AccessBanRule ], kind: Actor (Seat (Id "EU")) }
  , prices: Map.fromFoldable [ Tuple (Id "energy-atlantic") (Q.fromInt 2) ]
  , rules: Map.empty
  , transferability: Map.fromFoldable [ Tuple (Id "E") Free, Tuple (Id "M") Free ]
  , events: []
  , factors: Map.empty
  , toSign: []
  , entities: [ Id "CN", Id "DE", Id "EU" ]
  , transformations: Map.singleton (Id "t") { minimum: Q.zero, maximum: Nothing }
  , ownedCapacities: [ Id "semiconductors" ]
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
    it "a selector with no accessible Market proposes nothing, and a named Market the Entity has no access to is not drafted" do
      let none = env { view = seat { markets = [] } }
      intentsOf (drafts """{ "rules": [ { "when": [ { "le": [ { "price": { "cheapest": "E" } }, "9" ] } ], "propose": [ { "trade": { "market": { "cheapest": "E" }, "quantity": "3" } } ] }, { "propose": [ { "trade": { "market": "energy-atlantic", "quantity": "1" } } ] } ] }""" none)
        `shouldEqual` []
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
    it "a zero quantity yields no Trade or Transform; a Transform below its minimum is not drafted" do
      let
        text =
          """{ "rules": [ { "propose": [
          { "trade": { "market": "energy-atlantic", "quantity": "0" } },
          { "transform": { "transformation": "t", "quantity": "0" } },
          { "transform": { "transformation": "t", "quantity": "-1" } }
        ] } ] }"""
      let
        e = env { view = seat { self = seat.self { capabilities = [ MayTransform (Id "t") ] } } }
      intentsOf (drafts text e) `shouldEqual` []

  describe "One validity rule" do
    it "over generated Intents, the interpreter drafts exactly those that screening does not refuse for a static reason" do
      g <- unseated
      let
        w = (prepareRound g.world)
          { rules = Map.fromFoldable
              [ Tuple (Id "r-tariff") (Tariff { jurisdiction: Id "US", origin: Id "CN", rate: Q.fromInt 1 })
              , Tuple (Id "r-held") (Withholding { capacity: Id "semiconductors", units: Q.fromInt 1 })
              ]
          }
      quickCheck (agreement w)

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
