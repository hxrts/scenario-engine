-- | Pins: premise section JSON (docs/03), the validation checks over the reference composition (Pacific,
-- | docs/05), and the fixture documents.
module Test.Premises.Main where

import Testkit.Run (orThrow)
import Prelude

import Testkit.Edit (onExplicitActor, onActor, onExchange, onTransformation)

import Schema.Premise.PlayableActor (boundShape, metricShape)
import Core.Shape (Format(..), encodeWith, decodeJsonWith)
import Checks.Round (Failure(..), Field(..))
import Schema.Composition (PremiseComposition, InstantiationConfig)
import World.Error (ValidationError(..))
import Assembly.Error (AssemblyError(..))
import World.Composition.Validate (validateComposition)
import Core.Num as Q
import Premises.Kit.Resources (capacity)
import Laws.Types (LevyPayer(..), ResourceId, DrawSource(..), LevyBasis(..), LevyRecipient(..), Direction(..), FlowEffect(..), RuleF(..), ActorTag, Id(..), RefKind(..))
import Laws.World (UnseededWorld)
import Schema.Recurring (requirementRule)
import Data.Argonaut.Core (stringify, toString)
import Data.Array (any, elem, filter)
import Data.Either (Either(..), isLeft, isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Laws.StrategyRules (SignRule(..))
import Strategy.StrategyRules (strategyRulesRefs)
import Schema.Premise.StrategyRules (decodeStrategyRules, encodeStrategyRules)
import Schema.Premise.Intent (capabilityShape)
import Schema.Build.World (matching, noResource, own)
import Schema.Build.Base (holdingOf, mayTrade, mayTransfer, mayTransform)
import Schema.Build.Decision (selfTransform, asTemplate, joint, lit, max', procedure, rule, signProcedures)
import Adapters.Filesystem (fileOps)
import Schema.Premise.Fixture (decodeLiteral, fixtureShape)
import Testkit.Fixture (checkLiteral)
import Testkit.Json (json)
import Premises.Load (loadFrom)
import Assembly.Composition (CompositionParts, checkComposition)
import Data.Bifunctor (lmap)
import Premises.Pacific (composition)
import Premises.Fixtures (fixtures)
import Schema.Premise.Composition (compositionShape)
import Test.Premises.Compose (spec) as Compose
import Test.Premises.Dials as Dials
import Test.Premises.Agreements as Agreements
import Test.Premises.Lease as Lease
import Test.Premises.Suite as Suite
import Test.Premises.Flows as Flows
import Test.Premises.Golden (failWith, fixture, golden)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

namesOf :: RefKind -> Array (Tuple RefKind String) -> Array String
namesOf kind = map snd <<< filter ((_ == kind) <<< fst)

-- | The reference composition and configuration, as the loader reads them.
loaded :: Aff CompositionParts
loaded = do
  r <- liftEffect (loadFrom fileOps "pacific/v0")
  case r of
    Left e -> failWith e
    Right x -> pure x

-- Edits for the validation tests --------------------------------------------------------

type Edit = PremiseComposition -> InstantiationConfig -> Tuple PremiseComposition InstantiationConfig

editWorld :: (UnseededWorld -> UnseededWorld) -> Edit
editWorld f g c = Tuple g { world = f g.world } c

editConfig :: (InstantiationConfig -> InstantiationConfig) -> Edit
editConfig f g c = Tuple g (f c)

-- | A test that the edited reference composition fails validation with exactly this error.
rejects :: String -> Edit -> ValidationError -> Spec Unit
rejects name edit err = it name do
  { composition: c, config } <- loaded
  let Tuple gb cfg = edit c config
  case validateComposition gb cfg of
    Left es -> (err `elem` es) `shouldEqual` true
    Right _ -> fail ("expected " <> show err)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  golden
  Flows.spec
  Dials.spec
  Lease.spec
  Suite.spec
  Agreements.spec
  Compose.spec
  describe "Premise DSL builders" do
    it "max' and signProcedures compile to a strategyRules the decoder accepts" do
      let
        p =
          { rules: [ rule [] [ asTemplate (selfTransform { transformation: "us-firm-investment", quantity: max' (lit 1) (lit 2) }) ] ]
          , sign: [ signProcedures [ "council" ] ]
          }
      prog <- orThrow (decodeStrategyRules (encodeStrategyRules p))
      (prog == p) `shouldEqual` true
      case prog.sign of
        [ SignProcedures ids ] -> ids `shouldEqual` [ Id "council" ]
        _ -> fail "expected one procedures sign rule"
    it "every capability the DSL writes is a tag the PremiseComposition decoder reads" do
      for_ [ mayTrade "technology-world", mayTransfer "USD", mayTransform "us-firm-investment" ] \c ->
        case toString (encodeWith Authored capabilityShape c) of
          Nothing -> fail "a capability is not a string"
          Just _ -> isRight (decodeJsonWith Authored capabilityShape (encodeWith Authored capabilityShape c)) `shouldEqual` true
    it "a joint CandidateTransaction of a Transfer and a Transform round-trips through the premise form" do
      let
        p = { rules: [ rule [] [ joint { signers: [ Id "CN" ], transfers: [ { from: Id "self", to: Id "CN", resource: Id "T", quantity: lit 2 } ], transforms: [ { transformation: Id "us-firm-investment", quantity: lit 1 } ] } ] ], sign: [] }
      (decodeStrategyRules (encodeStrategyRules p) == Right p) `shouldEqual` true
    it "a joint CandidateTransaction decodes, and the parties and Resource it names are reported" do
      let
        p = { rules: [ rule [] [ joint { signers: [ Id "CN" ], transfers: [ { from: Id "self", to: Id "CN", resource: Id "T", quantity: lit 2 } ], transforms: [] } ] ], sign: [] }
      prog <- orThrow (decodeStrategyRules (encodeStrategyRules p))
      (prog == p) `shouldEqual` true
      let refs = strategyRulesRefs prog
      namesOf RefResource refs `shouldEqual` [ "T" ]
      namesOf RefActor refs `shouldEqual` [ "CN", "CN" ]
    it "a bound given as a metric is the metric object itself" do
      let m = own "CN" (holdingOf "RMB")
      stringify (encodeWith Authored boundShape (matching m)) `shouldEqual` stringify (encodeWith Authored metricShape m)

  describe "Premise round trips" do
    it "every compiled fixture decodes and encodes back to the same JSON, key for key" do
      for_ fixtures \(Tuple name j) -> case decodeJsonWith Authored fixtureShape j of
        Left e -> fail (name <> ": " <> show e)
        Right d -> stringify (encodeWith Authored fixtureShape d) `shouldEqual` stringify j
    it "the Pacific PremiseComposition decodes and encodes back to the same JSON" do
      let j = encodeWith Authored compositionShape composition
      d <- orThrow (decodeJsonWith Authored compositionShape j)
      stringify (encodeWith Authored compositionShape d) `shouldEqual` stringify j

  describe "PremiseComposition validation: each check reports its own error" do
    rejects "UnknownReference: an exchange access list names no Actor"
      (editWorld \w -> onExchange (Id "technology-world") (\m -> m { access = m.access <> [ Id "NOBODY" ] }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an exchange's quote is not a Resource"
      (editWorld \w -> onExchange (Id "industry-world") (\m -> m { quote = Id "X" }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: an exchange's base is not a Resource"
      (editWorld \w -> onExchange (Id "technology-world") (\m -> m { base = Id "X" }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: an exchange's reserve is not a Resource"
      (editWorld \w -> onExchange (Id "industry-world") (\m -> m { reserves = Map.insert (Id "X") (Q.fromInt 1) m.reserves }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation's payee is not an Actor"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { payment = Just { curve: { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }, resource: Id "USD", to: Id "NOBODY" } }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a transformation input is not a Resource"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { inputs = Map.insert (Id "X") (Q.fromInt 1) t.inputs }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation output is not a Resource"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { outputs = Map.insert (Id "X") (Q.fromInt 1) t.outputs }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation effect names a capacity no Actor holds"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { effects = map (\x -> x { resource = Id "nowhere" }) t.effects }) w)
      (UnknownReference RefResource "nowhere")
    rejects "UnknownReference: a transformation effect's holder is not an Actor"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { effects = map (\x -> x { holder = Just (Id "NOBODY") }) t.effects }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a capability names a Rule kind the premise does not declare"
      (editWorld \w -> w { ruleKinds = Map.delete (Id "us-import-tariff") w.ruleKinds })
      (UnknownReference RefRuleKind "us-import-tariff")
    rejects "UnknownReference: a group is not an Actor"
      (editWorld \w -> w { groups = Map.insert (Id "US-firm") (Id "NOBODY") w.groups })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an Actor account in a Resource that does not exist"
      (editWorld (onActor (Id "US") \e -> e { accounts = Map.insert (Id "X") (Q.fromInt 1) e.accounts }))
      (UnknownReference RefResource "X")
    rejects "UnknownReference: an Actor capacity no transformation or production uses"
      (editWorld (\w -> w { resources = Map.insert (Id "unused") (capacity { id = Id "unused", unit = "unused" }) w.resources, actors = Map.update (Just <<< \e -> e { accounts = Map.insert (Id "unused") (Q.fromInt 1) e.accounts }) (Id "US") w.actors }))
      (UnknownReference RefResource "unused")
    rejects "UnknownReference: a requirement on an Actor that does not exist"
      (editWorld \w -> w { recurring = w.recurring <> [ requirementRule { actor: Id "NOBODY", resource: Id "T", amount: Just (Q.fromInt 1), penalty: Q.zero } ] })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a ban whose counterparty is not an Actor"
      (editWorld \w -> w { rules = Map.insert (Id "rule-y") (banR { jurisdiction: Id "US", target: Id "NOBODY" }) w.rules })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a tariff whose origin is not an Actor"
      (editWorld \w -> w { rules = Map.insert (Id "rule-z") (tariffR { jurisdiction: Id "US", origin: Id "NOBODY", rate: Q.zero }) w.rules })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a Withholding rule names a capacity no Actor holds"
      (editWorld \w -> w { rules = Map.insert (Id "rule-x") (holdR { capacity: Id "nowhere", units: Q.fromInt 1 }) w.rules })
      (UnknownReference RefResource "nowhere")
    rejects "NoDefinition: an Actor with neither a definition nor an aggregate block"
      (\g c -> Tuple (g { explicitActors = Map.delete (Id "US-firm") g.explicitActors }) c)
      (NoDefinition (Id "US-firm"))
    rejects "NoCapabilities: a defined Actor that can do nothing"
      (editWorld \w -> w { actors = Map.update (Just <<< _ { capabilities = [] }) (Id "US-firm") w.actors })
      (NoCapabilities (Id "US-firm"))
    rejects "NoDependency: a defined Actor that depends on nothing"
      (\g c -> Tuple (onExplicitActor (Id "US-firm") (\d -> d { dependency = "" }) g) c)
      (NoDependency (Id "US-firm"))
    rejects "MissingObjective: an explicit Actor with no objective"
      (\g c -> Tuple (onExplicitActor (Id "US-firm") (\d -> d { objective = Nothing }) g) c)
      (MissingObjective (Id "US-firm"))
    rejects "NegativeHolding: an Actor opening below zero"
      (editWorld \w -> w { actors = Map.update (Just <<< \a -> a { accounts = Map.insert (Id "USD") (Q.fromInt (-1)) a.accounts }) (Id "US-firm") w.actors })
      (NegativeHolding (Id "US-firm"))
    rejects "InfeasibleExchange: a position outside its cost domain"
      (editWorld \w -> w { exchanges = Map.update (Just <<< _ { position = Q.fromInt 10000000 }) (Id "technology-world") w.exchanges })
      (InfeasibleExchange "technology-world")
    rejects "DuplicateHolder: two Actors hold what a holder draw names"
      (editWorld \w -> onTransformation (Id "us-firm-investment") (\t -> t { draws = t.draws <> [ { resource: Id "K_I", from: HolderOf, perUnit: Q.one } ] }) w)
      (DuplicateHolder "K_I")
    rejects "DrawWithoutHolder: a holder draw on a Resource nothing holds"
      (editWorld \w -> onTransformation (Id "cn-firm-investment") (\t -> t { draws = t.draws <> [ { resource: Id "Z", from: HolderOf, perUnit: Q.one } ] }) (w { resources = Map.insert (Id "Z") noResource w.resources }))
      (DrawWithoutHolder "cn-firm-investment" "Z")
    rejects "TooManyHolderDraws: a Transformation with more than one holder draw"
      (editWorld \w -> onTransformation (Id "cn-firm-investment") (\t -> t { draws = t.draws <> [ { resource: Id "S_I", from: HolderOf, perUnit: Q.one }, { resource: Id "K_I", from: HolderOf, perUnit: Q.one } ] }) w)
      (TooManyHolderDraws "cn-firm-investment")
    rejects "RequiredNotSelected: a required explicit Actor with no seat or NonPlayableActor"
      (editConfig \c -> c { seats = Map.delete (Id "US") c.seats })
      (RequiredNotSelected (Id "US"))
    rejects "ControllerNotAllowed: a seat for an Actor that is not an explicit Actor"
      (editConfig \c -> c { seats = Map.insert (Id "NOBODY") "seat-99" c.seats })
      (ControllerNotAllowed (Id "NOBODY"))
    rejects "RoundsOutOfRange: a configured round count outside the supported range"
      (editConfig \c -> c { rounds = 13 })
      (RoundsOutOfRange { rounds: 13, min: 1, max: 12 })
    it "a valid PremiseComposition reports no errors" do
      { composition: c, config } <- loaded
      map (const unit) (validateComposition c config) `shouldEqual` Right unit

  describe "Premise loader: strategies fail at load time" do
    it "a procedure a StrategyRules names that no Aggregate holds fails the check: in a sign rule, and in a CandidateTransaction template" do
      { composition: c, config } <- loaded
      let
        signing = { rules: [], sign: [ signProcedures [ "no-such-procedure" ] ] }
        proposing = { rules: [ rule [] [ procedure { id: "no-such-procedure", signers: ([] :: Array String), intents: [] } ] ], sign: [] }
        check p = map (const unit) (lmap show (checkComposition { composition: c, config, presets: Object.singleton "Bad" p }))
      check signing `shouldEqual` Left (show (Unresolved "preset Bad" RefProcedure "no-such-procedure"))
      check proposing `shouldEqual` Left (show (Unresolved "preset Bad" RefProcedure "no-such-procedure"))
    it "a preset that names an unknown transformation fails the check" do
      { composition: c, config } <- loaded
      let bad = { rules: [ rule [] [ asTemplate (selfTransform { transformation: "no-such-transformation", quantity: lit 1 }) ] ], sign: [] }
      p <- orThrow (decodeStrategyRules (encodeStrategyRules bad))
      map (const unit) (lmap show (checkComposition { composition: c, config, presets: Object.singleton "Bad" p }))
        `shouldEqual` Left (show (Unresolved "preset Bad" RefTransformation "no-such-transformation"))

  describe "Fixture documents" do
    -- Pins a premise file as `Premises.Load` reads it (docs/03).
    it "a fixture requires its seed" do
      isLeft (decodeLiteral (json """{ "premise": { "numeraire": "M", "resources": {}, "exchanges": {}, "transformations": {}, "actors": {} }, "bindings": [], "expectations": [] }""")) `shouldEqual` true
      d <- orThrow (decodeLiteral (json """{ "premise": { "seed": 7, "numeraire": "M", "resources": {}, "exchanges": {}, "transformations": {}, "actors": {} }, "bindings": [], "expectations": [] }"""))
      show d.world.seed `shouldEqual` "7"
    -- Pins a premise file as `Premises.Load` reads it (docs/03).
    it "an expectation must name a round that has a binding" do
      let world = """"premise": { "seed": 7, "numeraire": "M", "resources": {}, "exchanges": {}, "transformations": {}, "actors": {} }"""
      isLeft (decodeLiteral (json ("{ " <> world <> """, "bindings": [ { "candidateTransactions": [] } ], "expectations": [ { "round": 2, "expect": {} } ] }"""))) `shouldEqual` true
      isLeft (decodeLiteral (json ("{ " <> world <> """, "bindings": [ { "candidateTransactions": [] } ], "expectations": [ { "round": 1, "expect": {} }, { "round": 1, "expect": {} } ] }"""))) `shouldEqual` true
      isLeft (decodeLiteral (json ("{ " <> world <> """, "bindings": [ { "candidateTransactions": [] } ], "expectations": [ { "round": 1, "expect": {} } ] }"""))) `shouldEqual` false

  describe "Fixture expectations" do
    it "an asserted empty list of deliveries fails when a delivery happened" do
      f <- fixture "P-C"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      any isDeliveryMismatch (checkLiteral asserted) `shouldEqual` true
    it "an asserted empty list of deliveries passes when nothing was delivered" do
      f <- fixture "P-A"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      checkLiteral asserted `shouldEqual` []
    it "an asserted balance the engine does not produce fails" do
      f <- fixture "P-A"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { accounts = Map.singleton (Id "US") (Map.singleton (Id "USD") (Just (Q.fromInt 12345))) } }) f.rounds }
      any isBalanceMismatch (checkLiteral asserted) `shouldEqual` true
  where
  isDeliveryMismatch = case _ of
    Mismatch _ Deliveries _ _ -> true
    _ -> false
  isBalanceMismatch = case _ of
    Mismatch _ (Balance _ _) _ _ -> true
    _ -> false

-- Rules of the three kinds the validation tests add (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id ActorTag, origin :: Id ActorTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "us-import-tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "I" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "USD", to: LevyToDestination, payer: PaidBySender } }

banR :: forall v. { jurisdiction :: Id ActorTag, target :: Id ActorTag } -> RuleF v
banR r = FlowRule { kind: Id "us-export-control", scope: { governor: r.jurisdiction, counterparty: r.target, direction: Outbound, over: [ Id "T" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "us-export-control", resource: r.capacity, reduce: r.units }
