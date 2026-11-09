-- | Pins: premise section JSON (docs/03) and Meridian's catalog values (docs/05 §2 to §18).
module Test.Premises.Main where

import Testkit.Meridian (orThrow)
import Prelude

import Testkit.Edit (onExplicitActor, onActor, onExchange, onTransformation)

import Core.Map (values)

import Schema.Premise.Intent (intentShape, ruleShape, capabilityShape)
import Schema.Premise.PlayableActor (boundShape, metricShape)
import Core.Shape (Format(..), encodeWith, decodeJsonWith)
import Checks.Round (Failure(..), Field(..))
import World.PossibleWorld.Instantiate (instantiate)
import World.PossibleWorld.Reconcile (reconcile)
import Schema.PossibleWorld (Fallback(..), PossibleWorld, InstantiationConfig)
import World.Error (Inconsistency(..), ValidationError(..))
import Assembly.Error (AssemblyError(..))
import World.PossibleWorld.Validate (validatePossibleWorld)
import Core.Num (Q)
import Core.Num as Q
import Premises.Kit.Resources (capacity)
import Laws.Types (ResourceId, DrawSource(..), idText, LevyBasis(..), LevyRecipient(..), Direction(..), FlowEffect(..), RuleF(..), ActorTag, Capability(..), Actor, ActorKind(..), Id(..), IntentF(..), RefKind(..), RuleChangeF(..))
import Laws.World (Cause(..), Event, Mechanism(..), RecurringAmount(..), RecurringDestination(..), UnseededWorld, View, settledEvent, seeded)
import Schema.Recurring (flowRule, kinds, productionRule, requirementRule, salesOf, shareRule)
import Data.Argonaut.Core (Json, stringify, toString)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (any, elem, filter, length, mapMaybe)
import Data.Either (Either(..), isLeft, isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Strategy.Interpret (Env, nameOnly, propose)
import Laws.StrategyRules (StrategyRules, SignRule(..))
import Strategy.StrategyRules (strategyRulesRefs)
import Schema.Premise.StrategyRules (decodeStrategyRules, encodeStrategyRules, strategiesShape)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, noActor, noTransformation, noExchange, matching, own)
import Laws.Engine.Prepare (observe)
import Core.Draw (Seed(..))
import Partial.Unsafe (unsafeCrashWith)
import Premises.Meridian.Rules (accessBan, tariff, withholding)
import Schema.Build.Base (holdingOf, mayTrade, mayTransfer, mayTransform)
import Schema.Build.Decision (selfTransform, asTemplate, joint, lit, max', named, param, procedure, rule, signProcedures, tradeAt)
import Adapters.Filesystem (fileOps)
import Schema.Premise.Fixture (decodeLiteral, fixtureShape)
import Testkit.Fixture (checkLiteral)
import Premises.Load (presetsFrom, fromFiles, loadFrom)
import Premises.Meridian.Names (meridianDir)
import Assembly.Premise (PossibleWorldParts, checkPremise)
import Data.Bifunctor (lmap)
import Premises.Compose (files, presetsJson) as Compose
import Premises.Meridian (defaultDials, possibleWorld, meridian)
import Premises.Meridian.Fixtures (fixtures)
import Schema.Premise.PossibleWorld (possibleWorldShape)
import Premises.Meridian.Strategies (strategies)
import Test.Premises.Compose (spec) as Compose
import Test.Premises.Dials as Dials
import Test.Premises.Agreements as Agreements
import Test.Premises.Lease as Lease
import Test.Premises.Flows as Flows
import Test.Premises.Golden (failWith, fixture, golden)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Json (json)

namesOf :: RefKind -> Array (Tuple RefKind String) -> Array String
namesOf kind = map snd <<< filter ((_ == kind) <<< fst)

-- | Initial holdings of the reference instance (docs/05_meridian.md §15), restated as literals
-- | on purpose: a cross-check against `Premises.Meridian.Holdings`, not a copy of it.
holdings :: Array { actor :: String, m :: Int, e :: Int, mil :: Int, s :: Int }
holdings =
  [ { actor: "US", m: 260, e: 70, mil: 95, s: 60 }
  , { actor: "DE", m: 125, e: 38, mil: 24, s: 45 }
  , { actor: "IN", m: 180, e: 65, mil: 65, s: 35 }
  , { actor: "JP", m: 150, e: 40, mil: 36, s: 48 }
  , { actor: "UK", m: 130, e: 30, mil: 38, s: 47 }
  , { actor: "BR", m: 105, e: 85, mil: 25, s: 32 }
  , { actor: "SA", m: 155, e: 110, mil: 32, s: 28 }
  , { actor: "NG", m: 85, e: 60, mil: 18, s: 24 }
  , { actor: "ENCO", m: 140, e: 140, mil: 0, s: 20 }
  , { actor: "CHIP", m: 135, e: 55, mil: 0, s: 30 }
  , { actor: "SHIP", m: 110, e: 65, mil: 0, s: 25 }
  , { actor: "DEF", m: 110, e: 30, mil: 5, s: 18 }
  , { actor: "LABOR", m: 30, e: 4, mil: 0, s: 42 }
  , { actor: "SME", m: 15, e: 5, mil: 0, s: 12 }
  , { actor: "AID", m: 45, e: 12, mil: 0, s: 55 }
  , { actor: "CN", m: 255, e: 95, mil: 88, s: 57 }
  , { actor: "EU", m: 320, e: 125, mil: 50, s: 75 }
  , { actor: "CN_PRIVATE", m: 210, e: 85, mil: 0, s: 25 }
  , { actor: "US_WORKERS", m: 260, e: 60, mil: 0, s: 45 }
  , { actor: "GLOBAL_SMES", m: 300, e: 80, mil: 0, s: 25 }
  , { actor: "ENERGY_SUPPLIERS", m: 200, e: 240, mil: 0, s: 22 }
  , { actor: "WORLD_HOUSEHOLDS", m: 360, e: 90, mil: 0, s: 35 }
  ]

-- | The reference instance's possible World and configuration, as the loader reads them.
loaded :: Aff PossibleWorldParts
loaded = do
  r <- liftEffect (loadFrom fileOps meridianDir)
  case r of
    Left e -> failWith e
    Right x -> pure x

-- | The reference instance's Meridian files, as name-and-JSON sections.
-- | Meridian's presets, encoded and decoded exactly as a compiled premise is read.
shippedPresets :: Either String (Object StrategyRules)
shippedPresets = presetsFrom (Compose.presetsJson (meridian defaultDials))

sections :: Array { name :: String, json :: Json }
sections = map (\(Tuple name json) -> { name, json }) (Compose.files (meridian defaultDials))

-- | The sections with one replaced.
withSection :: String -> Json -> Array { name :: String, json :: Json }
withSection name json = map (\s -> if s.name == name then s { json = json } else s) sections

-- | A transformation's settlement in a round.
transformationEvent :: Int -> String -> String -> Int -> Event
transformationEvent round by t units = settledEvent round [] Nothing (OfIntent { candidateTransaction: Id "p", intent: 0, mechanism: Transformation { by: Id by, transformation: Id t, units: Q.fromInt units } })

-- | China in round 2, after a major US action in round 1, with the capability to procure.
chinaView :: View
chinaView = observe w (fromMaybe' (\_ -> unsafeCrashWith "no CN") (Map.lookup (Id "CN") w.actors))
  where
  w = seeded (Seed 1) emptyWorld
    { round = 2
    , actors = Map.singleton (Id "CN") noActor { id = Id "CN", capabilities = [ MayTransform (Id "defense-procurement-cn"), MayTrade (Id "energy-asia") ], kind = NonPlayableActor (Id "china-strategic-v0") }
    , exchanges = byKey [ Tuple "energy-asia" noExchange { base = Id "E", quote = Id "M", cost = { intercept: Q.fromInt 2, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }, access = [ Id "CN" ] } ]
    , transformations = byKey [ Tuple "defense-procurement-cn" noTransformation, Tuple "military-deployment" noTransformation ]
    , events = [ transformationEvent 1 "US" "major-military-action" 12 ]
    }

-- | An environment whose every draw is `u`.
envWith :: View -> Q -> Env
envWith v u = { view: v, params: Object.empty, random: const u, exchange: nameOnly }

-- | The Strategy named in the catalog, decoded.
strategyNamed :: PossibleWorld -> String -> Aff StrategyRules
strategyNamed gb name = case Map.lookup (Id name) gb.strategies of
  Nothing -> failWith ("no strategy named " <> name)
  Just p -> pure p

-- | A load outcome without its (non-comparable) premise, for equality checks.
outcome :: forall a. Either String a -> Either String Unit
outcome = map (const unit)

-- | A load outcome, checked as the game checks it: the decoded sections, then `checkPremise` (the error text).
checked :: Array { name :: String, json :: Json } -> Either String Unit
checked files = do
  d <- (fromFiles files)
  map (const unit) (lmap show (checkPremise { possibleWorld: d.possibleWorld, config: d.config, presets: Object.empty }))

-- Edits for the validation tests --------------------------------------------------------

type Edit = PossibleWorld -> InstantiationConfig -> Tuple PossibleWorld InstantiationConfig

editWorld :: (UnseededWorld -> UnseededWorld) -> Edit
editWorld f g c = Tuple g { world = f g.world } c

editConfig :: (InstantiationConfig -> InstantiationConfig) -> Edit
editConfig f g c = Tuple g (f c)

-- | One Actor of the world, edited.
-- | An Aggregate whose every procedure weighs an extra, unknown Actor.
withWeightOf :: String -> Actor -> Actor
withWeightOf name e = case e.kind of
  Aggregate a -> e { kind = Aggregate a { procedures = map (\p -> p { weights = Map.insert (Id name) (Q.fromInt 1) p.weights }) a.procedures } }
  _ -> e

-- | A test that the edited reference instance fails validation with exactly this error.
rejects :: String -> Edit -> ValidationError -> Spec Unit
rejects name edit err = it name do
  { possibleWorld, config } <- loaded
  let Tuple gb cfg = edit possibleWorld config
  case validatePossibleWorld gb cfg of
    Left es -> (err `elem` es) `shouldEqual` true
    Right _ -> fail ("expected " <> show err)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  golden
  Flows.spec
  Dials.spec
  Lease.spec
  Agreements.spec
  Compose.spec

  describe "Premise DSL builders" do
    it "max' and signProcedures compile to a strategyRules the decoder accepts" do
      let
        p =
          { rules: [ rule [] [ asTemplate (selfTransform { transformation: "defense-procurement", quantity: max' (lit 1) (lit 2) }) ] ]
          , sign: [ signProcedures [ "EU" ] ]
          }
      prog <- orThrow (decodeStrategyRules (encodeStrategyRules p))
      (prog == p) `shouldEqual` true
      length prog.rules `shouldEqual` 1
      case prog.sign of
        [ SignProcedures ids ] -> ids `shouldEqual` [ Id "EU" ]
        _ -> fail "expected one procedures sign rule"
    it "enactAccessBanI decodes to an AccessBan enactment" do
      case decodeJsonWith Authored intentShape (encodeWith Authored intentShape (accessBan.enact { jurisdiction: Id "US", target: Id "CN" })) of
        Left e -> fail (show e)
        Right (ChangeRule (Enact (FlowRule { scope: a }))) -> do
          a.governor `shouldEqual` Id "US"
          a.counterparty `shouldEqual` Id "CN"
        Right other -> fail ("unexpected intent " <> show other)
    it "a tariff Rule decodes to a Levy" do
      case decodeJsonWith Authored ruleShape (encodeWith Authored ruleShape (tariff.rule { jurisdiction: Id "US", origin: Id "CN", rate: q "0.25" })) of
        Left e -> fail (show e)
        Right (FlowRule { scope: t }) -> do
          t.governor `shouldEqual` Id "US"
          t.counterparty `shouldEqual` Id "CN"
        Right other -> fail ("unexpected rule " <> show other)
    it "a withholding rule and an enactment of one decode to the Withholding rule" do
      decodeJsonWith Authored ruleShape (encodeWith Authored ruleShape (withholding.rule { capacity: Id "semiconductors", units: q 3 }))
        `shouldEqual` Right (holdR { capacity: Id "semiconductors", units: Q.fromInt 3 })
      decodeJsonWith Authored intentShape (encodeWith Authored intentShape (withholding.enact { capacity: Id "semiconductors", units: q 3 }))
        `shouldEqual` Right (ChangeRule (Enact (holdR { capacity: Id "semiconductors", units: Q.fromInt 3 })))
    it "every capability the DSL writes is a tag the PossibleWorld decoder reads" do
      for_ [ mayTrade "energy-atlantic", mayTransfer "M", mayTransform "energy-shipping", tariff.may, accessBan.may, withholding.may ] \c ->
        case toString (encodeWith Authored capabilityShape c) of
          Nothing -> fail "a capability is not a string"
          Just _ -> isRight (decodeJsonWith Authored capabilityShape (encodeWith Authored capabilityShape c)) `shouldEqual` true
    it "a joint CandidateTransaction of a Transfer and a Transform round-trips through the premise form" do
      let
        p = { rules: [ rule [] [ joint { signers: [ Id "DE" ], transfers: [ { from: Id "self", to: Id "DE", resource: Id "E", quantity: lit 2 } ], transforms: [ { transformation: Id "plant", quantity: lit 1 } ] } ] ], sign: [] }
      (decodeStrategyRules (encodeStrategyRules p) == Right p) `shouldEqual` true

    it "a joint CandidateTransaction decodes, and the parties and Resource it names are reported" do
      let
        p = { rules: [ rule [] [ joint { signers: [ Id "DE" ], transfers: [ { from: Id "self", to: Id "DE", resource: Id "E", quantity: lit 2 } ], transforms: [] } ] ], sign: [] }
      prog <- orThrow (decodeStrategyRules (encodeStrategyRules p))
      (prog == p) `shouldEqual` true
      let refs = strategyRulesRefs prog
      namesOf RefResource refs `shouldEqual` [ "E" ]
      namesOf RefActor refs `shouldEqual` [ "DE", "DE" ]
    it "a bound given as a metric is the metric object itself" do
      let m = own "CN" (holdingOf "Mil")
      stringify (encodeWith Authored boundShape (matching m)) `shouldEqual` stringify (encodeWith Authored metricShape m)

  describe "Premise round trips" do
    it "every compiled fixture decodes and encodes back to the same JSON, key for key" do
      for_ fixtures \(Tuple name j) -> case decodeJsonWith Authored fixtureShape j of
        Left e -> fail (name <> ": " <> show e)
        Right d -> stringify (encodeWith Authored fixtureShape d) `shouldEqual` stringify j
    it "the Meridian PossibleWorld decodes and encodes back to the same JSON" do
      let j = encodeWith Authored possibleWorldShape possibleWorld
      d <- orThrow (decodeJsonWith Authored possibleWorldShape j)
      stringify (encodeWith Authored possibleWorldShape d) `shouldEqual` stringify j

  describe "Meridian" do
    let
      instantiated = do
        { possibleWorld, config } <- loaded
        case instantiate possibleWorld config of
          Left es -> failWith (show es)
          Right x -> pure { possibleWorld, config, initial: x }
      amount w e r = fromMaybe Q.zero (Map.lookup (Id e) w.actors >>= \x -> Map.lookup (Id r) x.accounts)
      capacity w e c = fromMaybe Q.zero (Map.lookup (Id e) w.actors >>= \x -> Map.lookup (Id c) x.accounts)

    it "validates, and its shipped presets resolve against it" do
      { possibleWorld, config } <- loaded
      map (const unit) (validatePossibleWorld possibleWorld config) `shouldEqual` Right unit
      case shippedPresets of
        Left e -> fail e
        Right ps -> map (const unit) (lmap show (checkPremise { possibleWorld, config, presets: ps })) `shouldEqual` Right unit

    it "instantiates with the initial holdings of §15" do
      { initial } <- instantiated
      for_ holdings \h -> do
        let w = initial.world
        { actor: h.actor, m: amount w h.actor "M", e: amount w h.actor "E", mil: amount w h.actor "Mil", s: amount w h.actor "S" }
          `shouldEqual` { actor: h.actor, m: Q.fromInt h.m, e: Q.fromInt h.e, mil: Q.fromInt h.mil, s: Q.fromInt h.s }

    it "instantiates the exchange and capacity state of §16" do
      { initial } <- instantiated
      let w = initial.world
      map (\m -> m.reserves) (Map.lookup (Id "energy-atlantic") w.exchanges) `shouldEqual` Just (Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 350), Tuple (Id "M") (Q.fromInt 700) ])
      map (\m -> m.reserves) (Map.lookup (Id "energy-asia") w.exchanges) `shouldEqual` Just (Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 250), Tuple (Id "M") (Q.fromInt 500) ])
      capacity w "CHIP" "semiconductors" `shouldEqual` Q.fromInt 15
      capacity w "SHIP" "transport" `shouldEqual` Q.fromInt 60
      map (\x -> x.kind) (Map.lookup (Id "CN") w.actors) `shouldEqual` Just (NonPlayableActor (Id "china-strategic-v0"))
      length w.representations `shouldEqual` 1

    it "the production list states exactly the energy capacity the Actors hold" do
      { possibleWorld, initial } <- instantiated
      let
        w = initial.world
        listed = mapMaybe
          ( \p -> case p.destination, p.amount of
              ToActor e, HeldAmount c -> Just (capacity w (unwrap e) (unwrap c))
              _, _ -> Nothing
          )
          possibleWorld.world.recurring
        held = map (\e -> fromMaybe Q.zero (Map.lookup (Id "energy-production") e.accounts)) (values w.actors)
      Q.sumQ listed `shouldEqual` Q.sumQ held

    it "reconciles Germany explicit against Germany implicit (§14.2)" do
      { possibleWorld, config, initial } <- instantiated
      case instantiate possibleWorld config { seats = Map.delete (Id "DE") config.seats } of
        Left es -> fail (show es)
        Right implicit -> do
          let eu w = map (amount w "EU") [ "M", "E", "Mil", "S" ]
          eu implicit.world `shouldEqual` map Q.fromInt [ 445, 163, 74, 120 ]
          reconcile initial implicit `shouldEqual` Right unit
          let
            residual p = Map.lookup (Id "EU") implicit.world.actors >>= \x -> case x.kind of
              Aggregate a -> map _.residualWeight (Map.lookup (Id p) a.procedures)
              _ -> Nothing
          residual "eu-solidarity" `shouldEqual` Q.parse "0.6"
          residual "eu-trade" `shouldEqual` Q.parse "0.68"

  describe "PossibleWorld validation: each check reports its own error" do
    rejects "UnknownReference: an exchange access list names no Actor"
      (editWorld \w -> onExchange (Id "energy-atlantic") (\m -> m { access = m.access <> [ Id "NOBODY" ] }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an exchange's quote is not a Resource"
      (editWorld \w -> onExchange (Id "energy-asia") (\m -> m { quote = Id "X" }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation's payee is not an Actor"
      (editWorld \w -> onTransformation (Id "energy-shipping") (\t -> t { payment = map (_ { to = Id "NOBODY" }) t.payment }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a capability names a Rule kind the premise does not declare"
      (editWorld \w -> w { ruleKinds = Map.delete (Id "tariff") w.ruleKinds })
      (UnknownReference RefRuleKind "tariff")
    rejects "UnknownReference: a jurisdiction is not an Actor"
      (editWorld \w -> w { groups = Map.insert (Id "DE") (Id "NOBODY") w.groups })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: what a Representation scheme observes is not a Resource"
      (\g c -> Tuple g { schemes = map (\s -> s { observes = Id "NOPE" }) g.schemes } c)
      (UnknownReference RefResource "NOPE")
    rejects "UnknownReference: an Aggregate member is not an Actor"
      (editWorld (onActor (Id "EU") \e -> e { kind = Aggregate { strategy: Id "eu-coordination-v0", members: [ Id "NOBODY" ], procedures: Map.empty } }))
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an explicit Actor's NonPlayableActor strategy is not in the catalog"
      (\g c -> Tuple (onExplicitActor (Id "CN") (\d -> d { nonPlayableActor = [ Id "no-such-strategy" ] }) g) c)
      (UnknownReference RefStrategy "no-such-strategy")
    rejects "UnknownReference: a Withholding rule names a capacity no Actor holds"
      (editWorld \w -> w { rules = Map.insert (Id "rule-x") (holdR { capacity: Id "nowhere", units: Q.fromInt 1 }) w.rules })
      (UnknownReference RefResource "nowhere")
    rejects "MissingFallback: a non-required explicit Actor with no fallback"
      (\g c -> Tuple (onExplicitActor (Id "DE") (\d -> d { fallback = Nothing }) g) c)
      (MissingFallback (Id "DE"))
    rejects "MissingObjective: an explicit Actor with no objective"
      (\g c -> Tuple (onExplicitActor (Id "DE") (\d -> d { objective = Nothing }) g) c)
      (MissingObjective (Id "DE"))
    rejects "DuplicateHolder: two Actors hold what a holder draw names"
      (editWorld (onActor (Id "SA") \e -> e { accounts = Map.insert (Id "semiconductors") (Q.fromInt 1) e.accounts }))
      (DuplicateHolder "semiconductors")
    rejects "ProcedureExceedsAuthority: a procedure authorizes what its Aggregate may not do"
      (editWorld (onActor (Id "EU") \e -> e { capabilities = filter (_ /= MayEnact (Id "tariff")) e.capabilities }))
      (ProcedureExceedsAuthority (Id "EU"))
    rejects "DrawWithoutHolder: a holder draw on a Resource nothing holds"
      (editWorld (onActor (Id "SHIP") \e -> e { accounts = Map.delete (Id "transport") e.accounts }))
      (DrawWithoutHolder "energy-shipping" "transport")
    rejects "TooManyHolderDraws: a Transformation with more than one holder draw"
      (editWorld \w -> onTransformation (Id "energy-shipping") (\t -> t { draws = t.draws <> [ { resource: Id "semiconductors", from: HolderOf, perUnit: Q.one } ] }) w)
      (TooManyHolderDraws "energy-shipping")
    rejects "ProductionWithoutHolding: a producer that does not hold what it produces from"
      (editWorld \w -> w { recurring = w.recurring <> [ productionRule { actor: Id "US", resource: Id "E", capacity: Id "energy-production" } ] })
      (ProductionWithoutHolding (Id "US") "energy-production")
    rejects "CoverageNotAggregate: a coverage fallback that names an Actor which is not an Aggregate"
      (\g c -> Tuple (onExplicitActor (Id "DE") (\d -> d { fallback = Just (ImplicitCoverage (Id "US")) }) g) c)
      (CoverageNotAggregate (Id "DE") (Id "US"))
    rejects "SchemeTargetNotAggregate: a scheme whose target is not an Aggregate"
      (\g c -> Tuple g { schemes = map (\s -> s { target = Id "US" }) g.schemes } c)
      (SchemeTargetNotAggregate "sme-energy-sample" (Id "US"))
    rejects "RequiredNotSelected: a required explicit Actor with no seat or NonPlayableActor"
      (editConfig \c -> c { seats = Map.delete (Id "US") c.seats })
      (RequiredNotSelected (Id "US"))
    rejects "ControllerNotAllowed: a seat for an Actor that is not an explicit Actor"
      (editConfig \c -> c { seats = Map.insert (Id "NOBODY") "seat-99" c.seats })
      (ControllerNotAllowed (Id "NOBODY"))
    rejects "IneligibleRepresentative: a selected explicit Actor the scheme does not cover"
      (editConfig \c -> c { representations = c.representations <> [ { scheme: "sme-energy-sample", explicitActor: Id "US", weight: Q.fromInt 1 } ] })
      (IneligibleRepresentative (Id "US"))
    rejects "RoundsOutOfRange: a configured round count outside the supported range"
      (editConfig \c -> c { rounds = 9 })
      (RoundsOutOfRange { rounds: 9, min: 4, max: 8 })
    rejects "UnknownReference: an exchange's base is not a Resource"
      (editWorld \w -> onExchange (Id "energy-atlantic") (\m -> m { base = Id "X" }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: an exchange's reserve is not a Resource"
      (editWorld \w -> onExchange (Id "energy-asia") (\m -> m { reserves = Map.insert (Id "X") (Q.fromInt 1) m.reserves }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation input is not a Resource"
      (editWorld \w -> onTransformation (Id "defense-procurement") (\t -> t { inputs = Map.insert (Id "X") (Q.fromInt 1) t.inputs }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation output is not a Resource"
      (editWorld \w -> onTransformation (Id "defense-procurement") (\t -> t { outputs = Map.insert (Id "X") (Q.fromInt 1) t.outputs }) w)
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a transformation effect names a capacity no Actor holds"
      (editWorld \w -> onTransformation (Id "strike") (\t -> t { effects = map (\x -> x { resource = Id "nowhere" }) t.effects }) w)
      (UnknownReference RefResource "nowhere")
    rejects "UnknownReference: a transformation effect's holder is not an Actor"
      (editWorld \w -> onTransformation (Id "strike") (\t -> t { effects = map (\x -> x { holder = Just (Id "NOBODY") }) t.effects }) w)
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an Actor account in a Resource that does not exist"
      (editWorld (onActor (Id "US") \e -> e { accounts = Map.insert (Id "X") (Q.fromInt 1) e.accounts }))
      (UnknownReference RefResource "X")
    rejects "UnknownReference: an Actor capacity no transformation or production uses"
      (editWorld (\w -> w { resources = Map.insert (Id "unused") (capacity { id = Id "unused", unit = "unused" }) w.resources, actors = Map.update (Just <<< \e -> e { accounts = Map.insert (Id "unused") (Q.fromInt 1) e.accounts }) (Id "US") w.actors }))
      (UnknownReference RefResource "unused")
    rejects "UnknownReference: a production entry's Resource that does not exist"
      (editWorld \w -> w { recurring = map (\p -> if p.kind == kinds.production then p { resource = Id "X" } else p) w.recurring })
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a flow from an Actor that does not exist"
      (editWorld \w -> w { recurring = w.recurring <> [ flowRule { from: Id "NOBODY", to: Id "US", resource: Id "M", amount: Q.fromInt 1 } ] })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a flow of a Resource that does not exist"
      (editWorld \w -> w { recurring = w.recurring <> [ flowRule { from: Id "US", to: Id "DE", resource: Id "X", amount: Q.fromInt 1 } ] })
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a share sourced from a Resource that does not exist"
      (editWorld \w -> w { recurring = w.recurring <> [ shareRule (Id "M") { from: Id "US", to: Id "DE", share: Q.fromInt 1, source: salesOf (Id "X") } ] })
      (UnknownReference RefResource "X")
    rejects "UnknownReference: a requirement on an Actor that does not exist"
      (editWorld \w -> w { recurring = w.recurring <> [ requirementRule { actor: Id "NOBODY", resource: Id "E", amount: Just (Q.fromInt 1), penalty: Q.zero } ] })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: an access ban whose target is not an Actor"
      (editWorld \w -> w { rules = Map.insert (Id "rule-y") (banR { jurisdiction: Id "US", target: Id "NOBODY" }) w.rules })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a tariff whose origin is not an Actor"
      (editWorld \w -> w { rules = Map.insert (Id "rule-z") (tariffR { jurisdiction: Id "US", origin: Id "NOBODY", rate: Q.zero }) w.rules })
      (UnknownReference RefActor "NOBODY")
    rejects "UnknownReference: a transport entry whose transformation does not exist"
      (editWorld \w -> w { flowGates = [ { resource: Id "E", exemptSharedExchange: true, bundled: Just (Id "no-such-transformation") } ] })
      (UnknownReference RefTransformation "no-such-transformation")
    rejects "UnknownReference: a procedure weight on an Actor that does not exist"
      (editWorld (onActor (Id "EU") (withWeightOf "NOBODY")))
      (UnknownReference RefActor "NOBODY")
    it "ResolutionInconsistent: an implicit explicit Actor whose authority its Aggregate would not keep" do
      { possibleWorld, config } <- loaded
      -- DE may enact tariffs, but once the EU covers DE, DE's authority disappears
      let
        gb = possibleWorld { world = onActor (Id "DE") (\e -> e { capabilities = e.capabilities <> [ MayEnact (Id "tariff") ] }) possibleWorld.world }
      case validatePossibleWorld gb config of
        Left es -> (ResolutionInconsistent (Id "DE") [ AuthorityDiffers ] `elem` es) `shouldEqual` true
        Right _ -> fail "expected ResolutionInconsistent"
    it "a valid PossibleWorld reports no errors" do
      { possibleWorld, config } <- loaded
      map (const unit) (validatePossibleWorld possibleWorld config) `shouldEqual` Right unit

  describe "Shipped Strategies and presets" do
    it "china-strategic-v0 procures after a major US action, and only with the capability" do
      { possibleWorld } <- loaded
      p <- strategyNamed possibleWorld "china-strategic-v0"
      let shipped env = env { params = Object.singleton "major-action-threshold" (Q.fromInt 12) }
      map _.intents (propose p (shipped (envWith chinaView Q.zero)))
        `shouldEqual` [ [ Transform { by: Id "CN", transformation: Id "defense-procurement-cn", quantity: Q.fromInt 4 } ] ]
      map _.intents (propose p (shipped (envWith (chinaView { self = chinaView.self { capabilities = [] } }) Q.zero))) `shouldEqual` []
    it "china-strategic-v0's major-action threshold is a parameter: a sweep changes whether China responds" do
      { possibleWorld } <- loaded
      p <- strategyNamed possibleWorld "china-strategic-v0"
      let
        -- a major action of 10 units: below the shipped threshold of 12, above a sweep's 6
        major = chinaView { events = [ transformationEvent 1 "US" "major-military-action" 10 ] }
        responds threshold = map _.intents (propose p ((envWith major Q.zero) { params = Object.singleton "major-action-threshold" (Q.fromInt threshold) }))
        procures = [ [ Transform { by: Id "CN", transformation: Id "defense-procurement-cn", quantity: Q.fromInt 4 } ] ]
      responds 6 `shouldEqual` procures
      responds 24 `shouldEqual` []
    it "RandomValid's choose picks an option by its own seeded draw" do
      case shippedPresets of
        Left e -> fail e
        Right ps -> case Object.lookup "RandomValid" ps of
          Nothing -> fail "no RandomValid preset"
          Just p -> do
            let
              sme = chinaView { self = chinaView.self { id = Id "SME", capabilities = [ MayTrade (Id "energy-asia") ] } }
              at u = map _.intents (propose p (envWith sme u))
            -- three options: noAction, buy 5 at 3, sell 5 at 2; a draw of 0.9 picks the last
            at Q.zero `shouldEqual` []
            at (q "0.9") `shouldEqual` [ [ Trade { exchange: Id "energy-asia", quantity: Q.fromInt (-5), limitPrice: Just (Q.fromInt 2) } ] ]

  describe "Reservations in the DSL" do
    it "the shipped military-deployment reserves Mil per unit and consumes and produces nothing" do
      { possibleWorld } <- loaded
      case Map.lookup (Id "military-deployment") possibleWorld.world.transformations of
        Nothing -> fail "military-deployment is missing"
        Just t -> do
          t.draws `shouldEqual` [ { resource: Id "Mil", from: Executor, perUnit: Q.fromInt 1 } ]
          Map.isEmpty t.inputs `shouldEqual` true
          Map.isEmpty t.outputs `shouldEqual` true
    it "the Deployer preset proposes a reserving deployment and a Mil sale only while the Atlantic price is above 3" do
      case shippedPresets of
        Left e -> fail e
        Right ps -> case Object.lookup "Deployer" ps of
          Nothing -> fail "no Deployer preset"
          Just p -> do
            let
              us = chinaView { self = chinaView.self { id = Id "US", capabilities = [ MayTransform (Id "military-deployment"), MayTrade (Id "defence-goods") ] }, exchanges = chinaView.exchanges <> [ { id: Id "defence-goods", base: Id "D", quote: Id "M" } ] }
              at price = map _.intents (propose p (envWith (us { prices = Map.fromFoldable [ Tuple (Id "energy-atlantic") (q price) ] }) Q.zero))
            at "3" `shouldEqual` []
            -- one candidate Transaction per template: the deployment, then the Mil sale it competes with
            at "3.01" `shouldEqual`
              [ [ Transform { by: Id "US", transformation: Id "military-deployment", quantity: Q.fromInt 60 } ]
              , [ Trade { exchange: Id "defence-goods", quantity: Q.fromInt (-40), limitPrice: Just (Q.parse "2.4" # fromMaybe Q.zero) } ]
              ]

  describe "Premise loader: files and strategies fail at load time" do
    it "a key that two section files both define is an error, not an overwrite" do
      outcome (fromFiles (sections <> [ { name: "extra.json", json: encodeJson { resources: Object.empty :: Object Json } } ]))
        `shouldEqual` Left "extra.json: key resources is already defined by another section"
    it "a section that is not a JSON object is an error" do
      outcome (fromFiles (withSection "premise.json" (encodeJson "not an object")))
        `shouldEqual` Left "premise.json: a section must be a JSON object"
    it "a strategy that names an unknown exchange fails the load" do
      let
        bad = { rules: [ rule [] [ asTemplate (tradeAt { exchange: named "energy-nowhere", quantity: lit 1, limit: Nothing }) ] ], sign: [] }
        catalog = strategies <> [ Tuple (Id "bad-v0") bad ]
      checked (withSection "strategies.json" (encodeWith Authored strategiesShape (map (\(Tuple k v) -> Tuple (idText k) v) catalog)))
        `shouldEqual` Left (show (Unresolved "strategy bad-v0" RefExchange "energy-nowhere"))
    it "a strategy that reads an unknown parameter fails the load" do
      let
        bad = { rules: [ rule [] [ asTemplate (tradeAt { exchange: named "energy-atlantic", quantity: param "no-such-parameter", limit: Nothing }) ] ], sign: [] }
        catalog = strategies <> [ Tuple (Id "bad-v0") bad ]
      checked (withSection "strategies.json" (encodeWith Authored strategiesShape (map (\(Tuple k v) -> Tuple (idText k) v) catalog)))
        `shouldEqual` Left (show (Unresolved "strategy bad-v0" RefParameter "no-such-parameter"))
    it "a procedure a StrategyRules names that no Aggregate holds fails the check: in a sign rule, and in a CandidateTransaction template" do
      { possibleWorld, config } <- loaded
      let
        signing = { rules: [], sign: [ signProcedures [ "no-such-procedure" ] ] }
        proposing = { rules: [ rule [] [ procedure { id: "no-such-procedure", signers: ([] :: Array String), intents: [] } ] ], sign: [] }
        check p = map (const unit) (lmap show (checkPremise { possibleWorld, config, presets: Object.singleton "Bad" p }))
      check signing `shouldEqual` Left (show (Unresolved "preset Bad" RefProcedure "no-such-procedure"))
      check proposing `shouldEqual` Left (show (Unresolved "preset Bad" RefProcedure "no-such-procedure"))
    it "a capacity a Withholding names that no Actor holds fails the check" do
      { possibleWorld, config } <- loaded
      let bad = { rules: [ rule [] [ asTemplate (withholding.enact { capacity: Id "no-such-capacity", units: lit 1 }) ] ], sign: [] }
      map (const unit) (lmap show (checkPremise { possibleWorld, config, presets: Object.singleton "Bad" bad }))
        `shouldEqual` Left (show (Unresolved "preset Bad" RefResource "no-such-capacity"))
    it "a preset that names an unknown transformation fails the check" do
      { possibleWorld, config } <- loaded
      let bad = { rules: [ rule [] [ asTemplate (selfTransform { transformation: "no-such-transformation", quantity: lit 1 }) ] ], sign: [] }
      p <- orThrow (decodeStrategyRules (encodeStrategyRules bad))
      map (const unit) (lmap show (checkPremise { possibleWorld, config, presets: Object.singleton "Bad" p }))
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
      f <- fixture "F"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      any isDeliveryMismatch (checkLiteral asserted) `shouldEqual` true
    it "an asserted empty list of deliveries passes when nothing was delivered" do
      f <- fixture "A"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      checkLiteral asserted `shouldEqual` []
    it "an asserted balance the engine does not produce fails" do
      f <- fixture "A"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { accounts = Map.singleton (Id "US") (Map.singleton (Id "M") (Just (Q.fromInt 12345))) } }) f.rounds }
      any isBalanceMismatch (checkLiteral asserted) `shouldEqual` true
  where
  isDeliveryMismatch = case _ of
    Mismatch _ Deliveries _ _ -> true
    _ -> false
  isBalanceMismatch = case _ of
    Mismatch _ (Balance _ _) _ _ -> true
    _ -> false

-- Rules of the three kinds the test premises declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id ActorTag, origin :: Id ActorTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination } }

banR :: forall v. { jurisdiction :: Id ActorTag, target :: Id ActorTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "withholding", resource: r.capacity, reduce: r.units }

