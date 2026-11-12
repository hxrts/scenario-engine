-- | Pins: the composition premise JSON of docs/03 in each `-- Pins` case; the rest use `Schema.Build`.
module Test.World.Main where

import Testkit.Run (orThrow)
import Schema.Build.Decision (passive)
import Schema.Build.World (emptyWorld, noResource)
import Prelude

import Testkit.Edit (onActor)

import Core.Map (values)

import World.Error (ValidationError(..))
import World.Composition.Instantiate (instantiate, mergeRequirements)
import World.Composition.Validate (validateComposition)
import Core.Draw (Seed(..))
import Core.Num as Q
import Laws.Types (LevyPayer(..), ResourceId, Capability(..), Direction(..), Actor, ActorId, ActorKind(..), ActorTag, FlowEffect(..), FlowGovernance(..), Id(..), LevyBasis(..), LevyRecipient(..), RefKind(..), Rule, RuleF(..), RuleForm(..), RuleId, RuleKindDecl, RuleKindTag, Transformation, TransformationId)
import Laws.World (RecurringAmount(..), UnseededWorld)
import Schema.Recurring (requirementRule)
import Data.Array (concatMap, elem, filter, mapMaybe)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Schema.Composition (ActorDefinition, Bound(..), Fallback(..), PremiseComposition, Measure(..), Span(..))
import Schema.Premise.Composition (decodeConfig, decodeComposition)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Json (json)
import Testkit.Composition (minimalComposition)

-- | A composition in which the Actor US is covered by the Aggregate EU (ImplicitCoverage). US holds
-- | the capacity `cap` and is the payee and effect holder of a transformation, the origin and target of
-- | Rules in force, and an eligible Actor of a Representation scheme.
coverageComposition :: PremiseComposition
coverageComposition =
  { id: "coverage-merge"
  , version: "v0"
  , premises: [ { id: "coverage", version: "v0" } ]
  , world: world
  , authored: { circulation: { production: [], flows: [], shares: [], requirements: [] }, reservable: [], decays: [] }
  , explicitActors: Map.singleton us explicitActor
  , schemes: [ { id: "s", eligibleActors: [ us ], target: eu, domain: Id "d", observes: money, estimator: estimator } ]
  , parameters: Object.empty
  , horizon: 1
  , rounds: { min: 1, max: 4 }
  , label: Nothing
  , strategies: Map.singleton (Id "p") passive
  }
  where
  us = Id "US" :: ActorId
  eu = Id "EU" :: ActorId
  money = Id "M"
  cap = Id "cap"
  estimator = { reference: Q.zero, shrinkage: Q.fromInt 1, deviationBound: Nothing, noiseSpread: Q.zero }

  explicitActor :: ActorDefinition
  explicitActor = { required: false, seat: true, nonPlayableActor: [], routine: Nothing, fallback: Just (ImplicitCoverage eu), objective: Nothing, description: "", dependency: "" }

  actor :: ActorId -> Actor
  actor i = { id: i, accounts: Map.empty, capabilities: [], kind: NonPlayableActor (Id "p") }
  usActor = (actor us) { accounts = Map.fromFoldable [ Tuple money (Q.fromInt 5), Tuple cap (Q.fromInt 1) ] }
  euActor = (actor eu) { kind = Aggregate { strategy: Id "p", members: [ us ], procedures: Map.empty } }

  transformation :: Transformation
  transformation =
    { id: Id "fac"
    , inputs: Map.empty
    , outputs: Map.empty
    , draws: []
    , payment: Just { curve: { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 10 } }, resource: money, to: us }
    , delay: 0
    , minimum: Q.zero
    , maximum: Nothing
    , ownerSigns: false
    , effects: [ { resource: cap, holder: Just us, perUnit: Q.fromInt 1, rounds: Nothing } ]
    }

  world :: UnseededWorld
  world = emptyWorld
    { round = 0
    , actors = Map.fromFoldable [ Tuple us usActor, Tuple eu euActor ]
    , resources = Map.singleton money noResource { id = money, unit = "USD" }
    , transformations = Map.singleton (Id "fac") transformation
    , numeraire = money
    , ruleKinds = testKinds
    , rules = Map.fromFoldable
        [ Tuple (Id "tariff") (tariffR { jurisdiction: eu, origin: us, rate: Q.fromInt 1 })
        , Tuple (Id "ban") (banR { jurisdiction: eu, target: us })
        ]
    }

-- | An Aggregate's member list; an Actor has none.
membersOf :: Actor -> Array ActorId
membersOf x = case x.kind of
  Aggregate a -> a.members
  _ -> []

-- | The Actor ids a World State names outside its Actor map: the transformation payees and effect
-- | holders, and the Actors of every Rule in force.
worldActorRefs :: forall r. { transformations :: Map.Map TransformationId Transformation, rules :: Map.Map RuleId Rule | r } -> Array ActorId
worldActorRefs w =
  map _.to (mapMaybe _.payment transforms)
    <> concatMap (\t -> mapMaybe _.holder t.effects) transforms
    <> concatMap ruleActors (map Just (values w.rules))
  where
  transforms = values w.transformations

  ruleActors :: Maybe Rule -> Array ActorId
  ruleActors = case _ of
    Just (FlowRule { scope: f }) -> [ f.governor, f.counterparty ]
    _ -> []

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Requirement merge (docs/04 §4.2)" do
    -- an operating requirement of the US in money, with the given amount
    let req amount = requirementRule { actor: Id "US", resource: Id "M", amount, penalty: Q.zero }
    it "two explicit amounts for one Actor and Resource are summed" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req (Just (Q.fromInt 4)) ]) `shouldEqual` [ FixedAmount (Q.fromInt 7) ]
    it "an explicit amount and one equal to purchases both survive the merge, neither replaced by the other" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req Nothing ]) `shouldEqual` [ FixedAmount (Q.fromInt 3), (req Nothing).amount ]
    it "two requirements equal to purchases stay one" do
      map _.amount (mergeRequirements [ req Nothing, req Nothing ]) `shouldEqual` [ (req Nothing).amount ]

  describe "Coverage merge retargets references (docs/04 §4.2)" do
    let config = { seed: Seed 1, rounds: 2, seats: Map.empty, nonPlayableActors: Map.empty, fallbacks: Map.empty, representations: [], preset: Nothing, alternatives: Map.empty }
    it "a payee, an effect holder, and Rules naming a merged PlayableActor name its Aggregate instead" do
      case instantiate coverageComposition config of
        Left es -> fail (show es)
        Right initial -> do
          filter (\e -> not (Map.member e initial.world.actors)) (worldActorRefs initial.world) `shouldEqual` []
          map (map _.to <<< _.payment) (values initial.world.transformations) `shouldEqual` [ Just (Id "EU") ]
          -- the Aggregate's member list does not name the Actor it now is
          map membersOf (Map.lookup (Id "EU") initial.world.actors) `shouldEqual` Just []
    it "a Representation scheme whose target is an PlayableActor is a validation error" do
      let gb = coverageComposition { schemes = map (_ { target = Id "US" }) coverageComposition.schemes }
      case validateComposition gb config of
        Left es -> (SchemeTargetNotAggregate "s" (Id "US") `elem` es) `shouldEqual` true
        Right _ -> fail "expected a scheme-target error"

    it "an institutional fallback naming no catalog Strategy is an unknown reference" do
      let gb = coverageComposition { explicitActors = map (_ { fallback = Just (InstitutionalStrategy (Id "nope")) }) coverageComposition.explicitActors }
      case validateComposition gb config of
        Left es -> (UnknownReference RefStrategy "nope" `elem` es) `shouldEqual` true
        Right _ -> fail "expected an unknown strategy"
    it "a configured fallback override naming no catalog Strategy is an unknown reference" do
      case validateComposition coverageComposition (config { fallbacks = Map.singleton (Id "US") (InstitutionalStrategy (Id "nope")) }) of
        Left es -> (UnknownReference RefStrategy "nope" `elem` es) `shouldEqual` true
        Right _ -> fail "expected an unknown strategy"
    it "a transformation with a negative delay is a validation error" do
      let gb = coverageComposition { world = coverageComposition.world { transformations = map (_ { delay = -1 }) coverageComposition.world.transformations } }
      case validateComposition gb config of
        Left es ->
          ( mapMaybe
              ( \e -> case e of
                  NegativeDelay t -> Just t
                  _ -> Nothing
              )
              es /= []
          ) `shouldEqual` true
        Right _ -> fail "expected a negative delay"

  describe "Unknown references at load (docs/03 §12)" do
    let
      config = { seed: Seed 1, rounds: 2, seats: Map.empty, nonPlayableActors: Map.empty, fallbacks: Map.empty, representations: [], preset: Nothing, alternatives: Map.empty }
      w = coverageComposition.world
      us = Id "US" :: ActorId
      eu = Id "EU" :: ActorId
      -- the listed unknown references are all reported
      reports gb cfg expected = case validateComposition gb cfg of
        Left es -> filter (\e -> not (e `elem` es)) expected `shouldEqual` []
        Right _ -> fail "expected unknown references"
      withCapabilities cs = coverageComposition { world = w { actors = map (_ { capabilities = cs }) w.actors } }
      ref kind name = UnknownReference kind name
      objective measure = { description: "o", criteria: [ { metric: { actors: [ Id "ghost" ], measure, negated: false }, over: Final, atLeast: Fixed Q.zero, also: [] } ] }
      withObjective measure = coverageComposition { explicitActors = map (_ { objective = Just (objective measure) }) coverageComposition.explicitActors }
    it "a capability naming an unknown Exchange, Resource or Transformation" do
      reports (withCapabilities [ MayTrade (Id "m0"), MayTransfer (Id "r0"), MayTransform (Id "t0") ]) config
        [ ref RefExchange "m0", ref RefResource "r0", ref RefTransformation "t0" ]
    it "a procedure authorizing an unknown name" do
      let
        procedure = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: Q.fromInt 1, authorizes: [ MayTrade (Id "m1"), MayTransform (Id "t1") ] }
        gb = coverageComposition { world = onActor eu (\x -> x { kind = Aggregate { strategy: Id "p", members: [ us ], procedures: Map.singleton (Id "c") procedure } }) w }
      reports gb config [ ref RefExchange "m1", ref RefTransformation "t1" ]
    it "an objective's Actors and Measure" do
      reports (withObjective (HoldingOf (Id "r2"))) config [ ref RefActor "ghost", ref RefResource "r2" ]
      reports (withObjective (HoldingOf (Id "c2"))) config [ ref RefResource "c2" ]
      reports (withObjective (ShortfallOf (Id "r3"))) config [ ref RefResource "r3" ]
      reports (withObjective (ShortfallRounds (Id "r4"))) config [ ref RefResource "r4" ]
      reports (withObjective (VolumeOf (Id "t2"))) config [ ref RefTransformation "t2" ]
    it "a bound that is another metric is read too" do
      let
        metric = { actors: [ us ], measure: HoldingOf (Id "M"), negated: false }
        bounded = { description: "o", criteria: [ { metric, over: Final, atLeast: Of (metric { measure = HoldingOf (Id "r5") }), also: [] } ] }
      reports (coverageComposition { explicitActors = map (_ { objective = Just bounded }) coverageComposition.explicitActors }) config [ ref RefResource "r5" ]
    it "an implicit coverage fallback naming no Actor" do
      reports (coverageComposition { explicitActors = map (_ { fallback = Just (ImplicitCoverage (Id "nowhere")) }) coverageComposition.explicitActors }) config [ ref RefActor "nowhere" ]
    it "a configured fallback for an unknown Actor, or covering by an unknown Actor" do
      reports coverageComposition (config { fallbacks = Map.singleton (Id "ghost") PermittedOmission }) [ ref RefActor "ghost" ]
      reports coverageComposition (config { fallbacks = Map.singleton us (ImplicitCoverage (Id "nowhere")) }) [ ref RefActor "nowhere" ]
    it "a numeraire that is not a Resource" do
      reports (coverageComposition { world = w { numeraire = Id "gold" } }) config [ ref RefResource "gold" ]

  describe "Instantiation" do
    -- Pins the instantiation configuration file format (docs/03 §5.2).
    it "a decoded PremiseComposition's world carries no seed; instantiation sets it" do
      raw <- orThrow (decodeComposition (json (minimalComposition "[4, 8]")))
      config <- orThrow (decodeConfig (json """{ "seed": 9, "rounds": 4 }"""))
      initial <- orThrow (instantiate raw config)
      show initial.world.seed `shouldEqual` "9"

-- Rules of the three kinds the test premises declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id ActorTag, origin :: Id ActorTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination, payer: PaidBySender } }

banR :: forall v. { jurisdiction :: Id ActorTag, target :: Id ActorTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "withholding", resource: r.capacity, reduce: r.units }

testKinds :: Map.Map (Id RuleKindTag) RuleKindDecl
testKinds = Map.fromFoldable
  [ Tuple (Id "tariff") { form: LevyFlows GovernedByDestination, text: "" }
  , Tuple (Id "accessBan") { form: BlockFlows GovernedByEither, text: "" }
  , Tuple (Id "withholding") { form: ReduceHolding, text: "" }
  ]
