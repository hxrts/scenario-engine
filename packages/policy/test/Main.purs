-- | Pins: strategy JSON is the setting format of docs/03 §11 (marked `Pins`), and the expected Proposals are the policy semantics of docs/02 §7.
module Test.Policy.Main where

import Schema.Build.Decision (asTemplate, cheapest, choose, costliest, each, factor, ge, gt, joint, le, lit, mulQ, named, noAction, not', orderAt, param, priceAt, procedure, reciprocate, restrictedLastRound, restrictionInForce, roundQ, rule, selfEnact, selfEntity, selfTransform, signIf, signProcedures, tradeAt, transformedLastRound)
import Schema.Build.Intent (repeal, route, transfer)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, fromHolder, emptyWorld, entitiesOf, noEntity, noFacility, noMarket, noResource, resourcesOf)
import Core.World (seeded, Cause(..), Event(..), Mechanism(..), RejectReason(..), View, WorldState, coupled)
import Prelude

import Core.Num as Q
import Core.Draw (Seed(..))
import Core.Map (values)
import Core.Engine.Prepare (observe, prepareRound)
import Core.Engine.Screen (screen)
import Core.Types (ResourceId, Capability(..), Direction(..), EntityId, EntityTag, FlowEffect(..), FlowGovernance(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), RefKind(..), RuleChangeF(..), RuleF(..), RuleForm(..), RuleKindDecl, RuleKindTag, Transferability(..), proposalId, samePositions, traverseIntent)
import Core.Slice (event, pickOf)
import Data.Array.NonEmpty as NEA
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck.Gen (Gen, elements, oneOf)
import Test.Spec.QuickCheck (quickCheck)
import Testkit.Meridian (unseated)
import Data.Either (either, isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe)
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
import Testkit.Json (decodeText)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- The agreement property ----------------------------------------------------------------

pick :: forall a. a -> Array a -> Gen a
pick x xs = elements (NEA.cons' x xs)

-- | Screening's outcome for one Intent is settled by what a View cannot answer: the rest is `validIntent`.
engineOnly :: RejectReason -> Boolean
engineOnly = case _ of
  OwnerMustSign _ -> true
  MissingBundledOrder -> true
  Blocked _ -> true
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
          (map tariffR ({ jurisdiction: _, origin: _, rate: _ } <$> entity <*> entity <*> quantity))
          [ map banR ({ jurisdiction: _, target: _ } <$> entity <*> entity)
          , map holdR ({ capacity: _, units: _ } <$> capacity <*> quantity)
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
    refused = (\ev_ -> maybe true (\x -> not (engineOnly x.reason)) (pickOf (event @"Rejected") ev_))
    accepted = either (not <<< Array.any refused) (const true) (screen w [] submitted)
  pure (drafted == accepted <?> show i <> " by " <> show self.id <> ": drafted " <> show drafted <> ", accepted " <> show accepted)
  where
  entities = values w.entities

-- Fixtures ----------------------------------------------------------------------------

-- | A seat in round 3, authorized to trade in energy-atlantic and enact access bans.
seat :: View
seat = observe w (fromMaybe' (\_ -> unsafeCrashWith "no EU") (Map.lookup (Id "EU") w.entities))
  where
  w = seeded (Seed 1) emptyWorld
    { round = 3
    , ruleKinds = testKinds
    , entities = entitiesOf
        [ Tuple "CN" noEntity
        , Tuple "DE" noEntity
        , Tuple "EU" noEntity { accounts = Map.singleton (Id "semiconductors") (Q.fromInt 1), capabilities = [ MayTrade (Id "energy-atlantic"), MayEnact (Id "accessBan") ] }
        ]
    , resources = resourcesOf [ Tuple "E" noResource, Tuple "M" noResource ]
    , markets = byKey [ Tuple "energy-atlantic" noMarket { base = Id "E", quote = Id "M", cost = { intercept: Q.fromInt 2, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }, access = [ Id "EU" ] } ]
    , transformations = byKey [ Tuple "t" noFacility { draws = [ fromHolder (Id "semiconductors") Q.one ] } ]
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
highDraws i = if i == 0 then Q.zero else q "0.9"

trade :: String -> String -> Intent
trade m quantity = Trade { market: Id m, quantity: q quantity, limitPrice: Nothing }

intentsOf :: Array ProposalBody -> Array (Array Intent)
intentsOf = map _.intents

-- | Two Choose blocks, each with two options of different size.
chooseTwice :: Strategy
chooseTwice =
  { rules: [ rule [] [ choose [ orderAt (named "energy-atlantic") (lit 1) Nothing, orderAt (named "energy-atlantic") (lit 2) Nothing ] ], rule [] [ choose [ orderAt (named "energy-atlantic") (lit 1) Nothing, orderAt (named "energy-atlantic") (lit 2) Nothing ] ] ], sign: [] }

-- Tests -------------------------------------------------------------------------------

strategyParams :: Strategy -> Array String
strategyParams = map snd <<< Array.filter ((_ == RefParameter) <<< fst) <<< strategyRefs

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Unknown parameters" do
    it "a clause reading a parameter the setting lacks does not fire, even under not" do
      let
        reads = ({ rules: [ rule [ gt (param "missing") (lit 0) ] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] })
        negated = ({ rules: [ rule [ not' (gt (param "missing") (lit 0)) ] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] })
      propose reads env `shouldEqual` []
      propose negated env `shouldEqual` []
  describe "Choose draws" do
    it "two Choose blocks in one strategy draw independently" do
      intentsOf (propose (chooseTwice) env { random = highDraws })
        `shouldEqual` [ [ trade "energy-atlantic" "1" ], [ trade "energy-atlantic" "2" ] ]
    it "a Choose in a clause whose condition fails draws nothing" do
      let
        text =
          { rules: [ rule [ gt (lit 1) (lit 2) ] [ choose [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], rule [] [ choose [ orderAt (named "energy-atlantic") (lit 1) Nothing, orderAt (named "energy-atlantic") (lit 2) Nothing ] ] ], sign: [] }
      -- the skipped clause draws nothing, so the second clause's first draw (index 0) picks option 1
      intentsOf (propose text env { random = highDraws }) `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]

  describe "Templates" do
    it "a joint Proposal drafts its transfers with self bound to the author, and its signers" do
      let
        text = { rules: [ rule [] [ joint { signers: [ Id "DE" ], transfers: [ { from: Id "self", to: Id "DE", resource: Id "E", quantity: lit 2 } ], transforms: [] } ] ], sign: [] }
      propose text env `shouldEqual`
        [ { intents: [ Transfer { from: Id "EU", to: Id "DE", resource: Id "E", quantity: Q.fromInt 2 } ], signers: [ Id "DE" ], procedure: Nothing } ]
    it "a Procedure drafts its intents, its signers and its id" do
      let
        text = { rules: [ rule [] [ procedure { id: "eu-solidarity", signers: [ "DE" ], intents: [ tradeAt { market: (named "energy-atlantic"), quantity: lit (-8), limit: Nothing } ] } ] ], sign: [] }
      propose text env `shouldEqual`
        [ { intents: [ trade "energy-atlantic" "-8" ], signers: [ Id "DE" ], procedure: Just (Id "eu-solidarity") } ]
    -- Pins the Strategy JSON format: a Procedure rejects nested forms (docs/02 §4.4).
    it "a Procedure may not contain a nested Procedure, Joint or Choose" do
      let nested inner = """{ "rules": [ { "propose": [ { "procedure": { "id": "p", "signers": [], "intents": [ """ <> inner <> """ ] } } ] } ] }"""
      isLeft (decodeText (nested """{ "joint": { "signers": [], "transfers": [] } }""")) `shouldEqual` true
      isLeft (decodeText (nested """{ "choose": [] }""")) `shouldEqual` true
      isLeft (decodeText (nested """{ "procedure": { "id": "q", "signers": [], "intents": [] } }""")) `shouldEqual` true
    it "noAction proposes nothing" do
      propose ({ rules: [ rule [] [ noAction ] ], sign: [] }) env `shouldEqual` []
    it "a standalone transfer from self resolves to the acting Entity, as a joint Proposal's does" do
      intentsOf (propose ({ rules: [ rule [] [ asTemplate (transfer { from: "self", to: "DE", resource: "E", quantity: lit 2 }) ] ], sign: [] }) env { view = seat { self = seat.self { capabilities = [ MayTransfer (Id "E") ] } } })
        `shouldEqual` [ [ Transfer { from: Id "EU", to: Id "DE", resource: Id "E", quantity: Q.fromInt 2 } ] ]
    it "every entity position resolves self, including a condition's jurisdiction" do
      let e = env { view = seat { events = [ Enacted { round: 3, id: Id "rule-0", rule: banR { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (propose ({ rules: [ rule [ restrictedLastRound { by: "CN", target: "self" } ] [ reciprocate { target: "CN" } ] ], sign: [] }) e)
        `shouldEqual` [ [ ChangeRule (Enact (banR { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "each runs the clause once per accessible Market for the Resource, in id order" do
      intentsOf (propose ({ rules: [ rule [] [ orderAt (each "E") (lit 3) Nothing ] ], sign: [] }) env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-asia" "3" ], [ trade "energy-atlantic" "3" ] ]
    it "cheapest and costliest pick by price, and the condition and the order see the same Market" do
      intentsOf (propose ({ rules: [ rule [ le (priceAt (cheapest "E")) (lit 2) ] [ orderAt (cheapest "E") (lit 3) Nothing ] ], sign: [] }) env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-atlantic" "3" ] ]
      intentsOf (propose ({ rules: [ rule [ gt (priceAt (costliest "E")) (lit 2) ] [ orderAt (costliest "E") (lit (-3)) Nothing ] ], sign: [] }) env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-asia" "-3" ] ]
    it "two selectors in one clause range over the product of their picks" do
      intentsOf (propose ({ rules: [ rule [] [ orderAt (cheapest "E") (lit 1) Nothing, orderAt (each "E") (lit 2) Nothing ] ], sign: [] }) env { view = twoMarkets })
        `shouldEqual` [ [ trade "energy-atlantic" "1" ], [ trade "energy-asia" "2" ], [ trade "energy-atlantic" "1" ], [ trade "energy-atlantic" "2" ] ]
    it "a selector with no accessible Market proposes nothing, and a named Market the Entity has no access to is not drafted" do
      let none = env { view = seat { markets = [] } }
      intentsOf (propose ({ rules: [ rule [ le (priceAt (cheapest "E")) (lit 9) ] [ orderAt (cheapest "E") (lit 3) Nothing ], rule [] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] }) none)
        `shouldEqual` []
    it "a price selector skips a Market with no price; each does not" do
      let unpriced = env { view = twoMarkets { prices = Map.fromFoldable [ Tuple (Id "energy-asia") (Q.fromInt 3) ] } }
      intentsOf (propose ({ rules: [ rule [] [ orderAt (cheapest "E") (lit 1) Nothing ], rule [] [ orderAt (each "E") (lit 2) Nothing ] ], sign: [] }) unpriced)
        `shouldEqual` [ [ trade "energy-asia" "1" ], [ trade "energy-asia" "2" ], [ trade "energy-atlantic" "2" ] ]
    it "a one-Market selector draws as a named Market does" do
      let one m = { rules: [ rule [] [ choose [ noAction, orderAt m (lit 3) Nothing ] ] ], sign: [] }
      propose (one (each "E")) env { random = highDraws } `shouldEqual` propose (one (named "energy-atlantic")) env { random = highDraws }
    it "an unknown Market id is a load-time name" do
      strategyRefs (({ rules: [ rule [] [ orderAt (named "$home") (lit 3) Nothing ] ], sign: [] })) `shouldEqual` [ Tuple RefMarket "$home" ]
    it "a route drafts a routed Trade with its pair and limit" do
      intentsOf (propose ({ rules: [ rule [] [ asTemplate (route { base: "E", quote: "M", quantity: lit 3, limit: lit 2 }) ] ], sign: [] }) env)
        `shouldEqual` [ [ Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt 3, limitPrice: Just (Q.fromInt 2) } ] ]
    it "a zero quantity yields no Trade or Transform; a Transform below its minimum is not drafted" do
      let
        text =
          { rules: [ rule [] [ orderAt (named "energy-atlantic") (lit 0) Nothing, asTemplate (selfTransform { transformation: "t", quantity: lit 0 }), asTemplate (selfTransform { transformation: "t", quantity: lit (-1) }) ] ], sign: [] }
      let
        e = env { view = seat { self = seat.self { capabilities = [ MayTransform (Id "t") ] } } }
      intentsOf (propose text e) `shouldEqual` []

  describe "One validity rule" do
    it "over generated Intents, the interpreter drafts exactly those that screening does not refuse for a static reason" do
      g <- unseated
      let
        w = (prepareRound g.world)
          { rules = Map.fromFoldable
              [ Tuple (Id "r-tariff") (tariffR { jurisdiction: Id "US", origin: Id "CN", rate: Q.fromInt 1 })
              , Tuple (Id "r-held") (holdR { capacity: Id "semiconductors", units: Q.fromInt 1 })
              ]
          }
      quickCheck (agreement w)

  describe "Conditions and restrictions" do
    it "reciprocate enacts the mirror of a restriction enacted against this Entity last round" do
      let
        e = env { view = seat { events = [ Enacted { round: 3, id: Id "rule-0", rule: banR { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (propose ({ rules: [ rule [] [ reciprocate { target: "CN" } ] ], sign: [] }) e)
        `shouldEqual` [ [ ChangeRule (Enact (banR { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "reciprocate draws nothing when the restriction was enacted at an earlier boundary" do
      let
        e = env { view = seat { events = [ Enacted { round: 2, id: Id "rule-0", rule: banR { jurisdiction: Id "CN", target: Id "EU" } } ] } }
      intentsOf (propose ({ rules: [ rule [] [ reciprocate { target: "CN" } ] ], sign: [] }) e) `shouldEqual` []
    it "restrictedLastRound holds only for a restriction enacted at this round's boundary" do
      let
        text = { rules: [ rule [ restrictedLastRound { by: "CN", target: "EU" } ] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] }
        fires events = intentsOf (propose (text) env { view = seat { events = events } })
        fired = [ Enacted { round: 3, id: Id "r", rule: banR { jurisdiction: Id "CN", target: Id "EU" } } ]
        stale = [ Enacted { round: 2, id: Id "r", rule: banR { jurisdiction: Id "CN", target: Id "EU" } } ]
      fires fired `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      fires stale `shouldEqual` []
    it "restrictionInForce holds when a rule by that jurisdiction is in force" do
      let
        text = { rules: [ rule [ restrictionInForce { by: "US" } ] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] }
        at r = intentsOf (propose text env { view = seat { rules = Map.singleton (Id "r") r } })
      at (banR { jurisdiction: Id "US", target: Id "CN" }) `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      at (tariffR { jurisdiction: Id "CN", origin: Id "US", rate: Q.zero }) `shouldEqual` []

  describe "Parameters and factors" do
    it "transformedLastRound reads its threshold as an expression, so a parameter sweep changes the outcome" do
      let
        text = { rules: [ rule [ transformedLastRound { by: "US", transformation: "major-military-action", atLeast: param "threshold" } ] [ orderAt (named "energy-atlantic") (lit 1) Nothing ] ], sign: [] }
        fires threshold = propose text env { params = Object.fromFoldable [ Tuple "threshold" (q threshold) ], view = seat { events = [ coupled 2 [] Nothing (OfIntent { proposal: Id "p", intent: 0, mechanism: Conversion { by: Id "US", transformation: Id "major-military-action", units: Q.fromInt 10 } }) ] } }
      intentsOf (fires "6") `shouldEqual` [ [ trade "energy-atlantic" "1" ] ]
      intentsOf (fires "24") `shouldEqual` []
      strategyParams (text) `shouldEqual` [ "threshold" ]
    it "a Factor with no stored factor is 1" do
      intentsOf (propose ({ rules: [ rule [] [ orderAt (named "energy-atlantic") (mulQ (lit 5) (factor "E")) Nothing ] ], sign: [] }) env)
        `shouldEqual` [ [ trade "energy-atlantic" "5" ] ]
    it "a Factor uses the stored factor" do
      let
        e = env { view = seat { factors = Map.fromFoldable [ Tuple (Id "E") (q "0.5") ] } }
      intentsOf (propose ({ rules: [ rule [] [ orderAt (named "energy-atlantic") (mulQ (lit 5) (factor "E")) Nothing ] ], sign: [] }) e)
        `shouldEqual` [ [ trade "energy-atlantic" "2.5" ] ]
    it "a Param the setting does not define evaluates to zero, so an order of that size is dropped" do
      intentsOf (propose ({ rules: [ rule [] [ orderAt (named "energy-atlantic") (param "missing") Nothing ] ], sign: [] }) env)
        `shouldEqual` []
    it "strategyParams lists every setting parameter a Strategy reads" do
      strategyParams (({ rules: [ rule [ gt (param "limit") (lit 1) ] [ orderAt (named "m") (param "size") Nothing ] ], sign: [] }))
        `shouldEqual` [ "limit", "size" ]

  describe "Signing" do
    it "signs by rule: procedures, facility, or all" do
      let
        procP = { id: proposalId 3 (Id "A") 0, author: Id "A", intents: [], signers: [], procedure: Just (Id "eu-trade") }
        facP = { id: proposalId 3 (Id "A") 1, author: Id "A", intents: [ Transform { by: Id "A", transformation: Id "energy-shipping", quantity: Q.fromInt 1 } ], signers: [], procedure: Nothing }
        plainP = { id: proposalId 3 (Id "A") 2, author: Id "A", intents: [], signers: [], procedure: Nothing }
        signedBy rules = sign { rules: [], sign: rules } env { view = seat { toSign = [ procP, facP, plainP ] } }
      signedBy [ SignProcedures [ Id "eu-trade" ] ] `shouldEqual` [ procP.id ]
      signedBy [ SignConversion (Id "energy-shipping") ] `shouldEqual` [ facP.id ]
      signedBy [ SignAll ] `shouldEqual` [ procP.id, facP.id, plainP.id ]
      signedBy [] `shouldEqual` []

  describe "Decision language: bans, withholding, repeal, conditional signing, joint transforms" do
    let
      -- the seat, authorized to enact the given kinds of Rule (the interpreter filters by authority)
      enacting caps = env { view = seat { self = seat.self { capabilities = caps } } }
    it "enactAccessBan drafts a ban by the author's jurisdiction on its target" do
      intentsOf (propose ({ rules: [ rule [] [ asTemplate (selfEnact (banR { jurisdiction: selfEntity, target: Id "CN" })) ] ], sign: [] }) (enacting [ MayEnact (Id "accessBan") ]))
        `shouldEqual` [ [ ChangeRule (Enact (banR { jurisdiction: Id "EU", target: Id "CN" })) ] ]
    it "enactWithholding drafts a withholding of units of a capacity the author holds" do
      intentsOf (propose ({ rules: [ rule [] [ asTemplate (selfEnact (holdR { capacity: Id "semiconductors", units: lit 5 })) ] ], sign: [] }) (enacting [ MayEnact (Id "withholding") ]))
        `shouldEqual` [ [ ChangeRule (Enact (holdR { capacity: Id "semiconductors", units: Q.fromInt 5 })) ] ]
    it "repeal drafts a repeal of the named rule, and needs enactment authority" do
      let text = { rules: [ rule [] [ asTemplate (repeal { rule: "r2/US/0/rule-0" }) ] ], sign: [] }
      let inForce e = e { view = e.view { rules = Map.singleton (Id "r2/US/0/rule-0") (banR { jurisdiction: Id "EU", target: Id "CN" }) } }
      intentsOf (propose text (inForce (enacting [ MayEnact (Id "accessBan") ]))) `shouldEqual` [ [ ChangeRule (Repeal (Id "r2/US/0/rule-0")) ] ]
      intentsOf (propose text (inForce (enacting []))) `shouldEqual` []
      -- the engine also needs the capability for the repealed Rule's kind, and a Rule in force
      intentsOf (propose text (inForce (enacting [ MayEnact (Id "tariff") ]))) `shouldEqual` []
      intentsOf (propose text (enacting [ MayEnact (Id "accessBan") ])) `shouldEqual` []
    it "a Free Resource is drafted without a capability, and a Restricted one needs it" do
      let
        text = { rules: [ rule [] [ asTemplate (transfer { from: "self", to: "DE", resource: "M", quantity: lit 2 }) ] ], sign: [] }
        as t caps = (enacting caps) { view = (enacting caps).view { transferability = Map.singleton (Id "M") t } }
        give = [ [ Transfer { from: Id "EU", to: Id "DE", resource: Id "M", quantity: Q.fromInt 2 } ] ]
      intentsOf (propose text (as Free [])) `shouldEqual` give
      intentsOf (propose text (as Restricted [])) `shouldEqual` []
      intentsOf (propose text (as Restricted [ MayTransfer (Id "M") ])) `shouldEqual` give
    it "a joint Proposal bundles its transforms with its transfers, all by the author" do
      let text = { rules: [ rule [] [ joint { signers: [ Id "EU", Id "IN" ], transfers: [], transforms: [ { transformation: Id "energy-shipping", quantity: lit 5 } ] } ] ], sign: [] }
      intentsOf (propose text env) `shouldEqual` [ [ Transform { by: Id "EU", transformation: Id "energy-shipping", quantity: Q.fromInt 5 } ] ]
    it "signIf signs a Proposal only while its conditions hold" do
      let
        procP = { id: proposalId 3 (Id "A") 0, author: Id "A", intents: [], signers: [], procedure: Just (Id "eu-solidarity") }
        signedIn r = sign (({ rules: [], sign: [ signIf [ ge roundQ (lit 4) ] (signProcedures [ "eu-solidarity" ]) ] })) env { view = seat { round = r, toSign = [ procP ] } }
      signedIn 3 `shouldEqual` []
      signedIn 4 `shouldEqual` [ procP.id ]

  describe "Load-time references" do
    it "a transfer to an entity the Gameboard lacks is a load-time name, and self is never one" do
      strategyRefs (({ rules: [ rule [] [ asTemplate (transfer { from: "self", to: "bogus", resource: "E", quantity: lit 2 }) ] ], sign: [] })) `shouldEqual`
        [ Tuple RefResource "E", Tuple RefEntity "bogus" ]

-- Rules of the three kinds the test settings declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id EntityTag, origin :: Id EntityTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination } }

banR :: forall v. { jurisdiction :: Id EntityTag, target :: Id EntityTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "withholding", resource: r.capacity, reduce: r.units }

testKinds :: Map.Map (Id RuleKindTag) RuleKindDecl
testKinds = Map.fromFoldable
  [ Tuple (Id "tariff") { form: LevyFlows GovernedByDestination, text: "" }
  , Tuple (Id "accessBan") { form: BlockFlows GovernedByEither, text: "" }
  , Tuple (Id "withholding") { form: ReduceHolding, text: "" }
  ]
