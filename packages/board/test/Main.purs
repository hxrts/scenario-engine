module Test.Board.Main where

import Prelude

import Testkit.Edit (onEntity)

import Core.Map (values)

import Board.Error (ValidationError(..))
import Board.Gameboard.Instantiate (instantiate, mergeRequirements)
import Board.Gameboard.Validate (validateGameboard)
import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (LevyBasis(..), LevyRecipient(..), CapacityTag, Direction(..), FlowEffect(..), RuleForm(..), EntityTag, RuleKindDecl, RuleKindTag, RuleF(..), FlowGovernance(..))
import Core.Types (Capability(..), Controller(..), Entity, EntityId, EntityKind(..), Id(..), RefKind(..), Rule, RuleId, Transferability(..), Transformation, TransformationId)
import Core.World (RecurringAmount(..), UnseededWorld)
import Schema.Recurring (requirementRule)
import Data.Argonaut.Core (Json)
import Data.Array (concatMap, elem, filter, mapMaybe)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Schema.Gameboard (ActorDefinition, Bound(..), Fallback(..), Gameboard, Measure(..), Span(..))
import Schema.Premise.Gameboard (decodeConfig, decodeGameboard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Unsafe.Coerce (unsafeCoerce)
import Testkit.Json (json)
import Testkit.Gameboard (minimalGameboard)

-- | A Gameboard in which the Actor US is covered by the Aggregate EU (ImplicitCoverage). US holds
-- | the capacity `cap` and is the payee and effect holder of a facility, the origin and target of
-- | Rules (in force and scheduled), and an eligible Actor of a Representation scheme.
coverageGameboard :: Gameboard
coverageGameboard =
  { id: "coverage-merge"
  , version: "v0"
  , world: world
  , authored: { circulation: { production: [], flows: [], shares: [], requirements: [] }, reservable: [], decays: [] }
  , actors: Map.singleton us actor
  , schemes: [ { id: "s", eligibleActors: [ us ], target: eu, domain: Id "d", observes: money, estimator: estimator } ]
  , parameters: Object.empty
  , horizon: 1
  , rounds: { min: 1, max: 4 }
  , label: Nothing
  , policies: Map.singleton (Id "p") { rules: [], sign: [] }
  }
  where
  us = Id "US" :: EntityId
  eu = Id "EU" :: EntityId
  money = Id "M"
  cap = Id "cap"
  estimator = { reference: Q.zero, shrinkage: Q.fromInt 1, deviationBound: Nothing, noiseSpread: Q.zero }

  actor :: ActorDefinition
  actor = { required: false, seat: true, npc: [], fallback: Just (ImplicitCoverage eu), objective: Nothing, description: "", dependency: "" }

  entity :: EntityId -> Entity
  entity i = { id: i, accounts: Map.empty, capacities: Map.empty, capabilities: [], kind: Actor (NPC (Id "p")) }
  usEntity = (entity us) { accounts = Map.singleton money (Q.fromInt 5), capacities = Map.singleton cap (Q.fromInt 1) }
  euEntity = (entity eu) { kind = Aggregate { policy: Id "p", members: [ us ], procedures: Map.empty } }

  facility :: Transformation
  facility =
    { id: Id "fac"
    , inputs: Map.empty
    , outputs: Map.empty
    , escrowPerUnit: Map.empty
    , payment: Just { curve: { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 10 } }, resource: money, to: us }
    , capacity: Nothing
    , delay: 0
    , minimum: Q.zero
    , maximum: Nothing
    , capacityUse: Q.zero
    , ownerSigns: false
    , effects: [ { capacity: cap, holder: Just us, perUnit: Q.fromInt 1, rounds: Nothing } ]
    }

  world :: UnseededWorld
  world =
    { round: 0
    , entities: Map.fromFoldable [ Tuple us usEntity, Tuple eu euEntity ]
    , resources: Map.singleton money { id: money, unit: "USD", transferability: Free }
    , markets: Map.empty
    , transformations: Map.singleton (Id "fac") facility
    , representations: []
    , numeraire: money
    , focus: Nothing
    , flowGates: []
    , groups: Map.empty
    , ruleKinds: testKinds
    , rules: Map.fromFoldable
        [ Tuple (Id "tariff") (tariffR { jurisdiction: eu, origin: us, rate: Q.fromInt 1 })
        , Tuple (Id "ban") (banR { jurisdiction: eu, target: us })
        ]
    , scheduled: Map.singleton (Id "later") (Just (banR { jurisdiction: eu, target: us }))
    , deliveries: []
    , factors: Map.empty
    , recurring: []
    , events: []
    }

-- | An Aggregate's member list; an Actor has none.
membersOf :: Entity -> Array EntityId
membersOf x = case x.kind of
  Aggregate a -> a.members
  _ -> []

-- | The Entity ids a World State names outside its Entity map: the facility payees and effect
-- | holders, and the Entities of every Rule in force or scheduled.
worldEntityRefs :: forall r. { transformations :: Map.Map TransformationId Transformation, rules :: Map.Map RuleId Rule, scheduled :: Map.Map RuleId (Maybe Rule) | r } -> Array EntityId
worldEntityRefs w =
  map _.to (mapMaybe _.payment transforms)
    <> concatMap (\t -> mapMaybe _.holder t.effects) transforms
    <> concatMap ruleEntities (map Just (values w.rules) <> values w.scheduled)
  where
  transforms = values w.transformations

  ruleEntities :: Maybe Rule -> Array EntityId
  ruleEntities = case _ of
    Just (FlowRule { scope: f }) -> [ f.governor, f.counterparty ]
    _ -> []

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Requirement merge (docs/04 §4.2)" do
    -- an operating requirement of the US in money, with the given amount
    let req amount = requirementRule { entity: Id "US", resource: Id "M", amount, penalty: Q.zero }
    it "two explicit amounts for one Entity and Resource are summed" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req (Just (Q.fromInt 4)) ]) `shouldEqual` [ FixedAmount (Q.fromInt 7) ]
    it "an explicit amount and one equal to purchases both survive the merge, neither replaced by the other" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req Nothing ]) `shouldEqual` [ FixedAmount (Q.fromInt 3), (req Nothing).amount ]
    it "two requirements equal to purchases stay one" do
      map _.amount (mergeRequirements [ req Nothing, req Nothing ]) `shouldEqual` [ (req Nothing).amount ]

  describe "Coverage merge retargets references (docs/04 §4.2)" do
    let config = { seed: Seed 1, rounds: 2, seats: Map.empty, npcs: Map.empty, fallbacks: Map.empty, representations: [] }
    it "a payee, an effect holder, and Rules naming a merged Actor name its Aggregate instead" do
      case instantiate coverageGameboard config of
        Left es -> fail (show es)
        Right initial -> do
          filter (\e -> not (Map.member e initial.world.entities)) (worldEntityRefs initial.world) `shouldEqual` []
          map (map _.to <<< _.payment) (values initial.world.transformations) `shouldEqual` [ Just (Id "EU") ]
          -- the Aggregate's member list does not name the Actor it now is
          map membersOf (Map.lookup (Id "EU") initial.world.entities) `shouldEqual` Just []
    it "a Representation scheme whose target is an Actor is a validation error" do
      let gb = coverageGameboard { schemes = map (_ { target = Id "US" }) coverageGameboard.schemes }
      case validateGameboard gb config of
        Left es -> (SchemeTargetNotAggregate "s" (Id "US") `elem` es) `shouldEqual` true
        Right _ -> fail "expected a scheme-target error"

    it "an institutional fallback naming no catalog Policy is an unknown reference" do
      let gb = coverageGameboard { actors = map (_ { fallback = Just (InstitutionalPolicy (Id "nope")) }) coverageGameboard.actors }
      case validateGameboard gb config of
        Left es -> (UnknownReference PolicyRef "nope" `elem` es) `shouldEqual` true
        Right _ -> fail "expected an unknown policy"
    it "a configured fallback override naming no catalog Policy is an unknown reference" do
      case validateGameboard coverageGameboard (config { fallbacks = Map.singleton (Id "US") (InstitutionalPolicy (Id "nope")) }) of
        Left es -> (UnknownReference PolicyRef "nope" `elem` es) `shouldEqual` true
        Right _ -> fail "expected an unknown policy"
    it "a facility with a negative delay is a validation error" do
      let gb = coverageGameboard { world = coverageGameboard.world { transformations = map (_ { delay = -1 }) coverageGameboard.world.transformations } }
      case validateGameboard gb config of
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
      config = { seed: Seed 1, rounds: 2, seats: Map.empty, npcs: Map.empty, fallbacks: Map.empty, representations: [] }
      w = coverageGameboard.world
      us = Id "US" :: EntityId
      eu = Id "EU" :: EntityId
      -- the listed unknown references are all reported
      reports gb cfg expected = case validateGameboard gb cfg of
        Left es -> filter (\e -> not (e `elem` es)) expected `shouldEqual` []
        Right _ -> fail "expected unknown references"
      withCapabilities cs = coverageGameboard { world = w { entities = map (_ { capabilities = cs }) w.entities } }
      ref kind name = UnknownReference kind name
      objective measure = { description: "o", criteria: [ { metric: { entities: [ Id "ghost" ], measure, negated: false }, over: Final, atLeast: Fixed Q.zero } ] }
      withObjective measure = coverageGameboard { actors = map (_ { objective = Just (objective measure) }) coverageGameboard.actors }
    it "a capability naming an unknown Market, Resource or Transformation" do
      reports (withCapabilities [ MayTrade (Id "m0"), MayTransfer (Id "r0"), MayTransform (Id "t0") ]) config
        [ ref Market "m0", ref ResourceRef "r0", ref TransformationRef "t0" ]
    it "a procedure authorizing an unknown name" do
      let
        procedure = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: Q.fromInt 1, authorizes: [ MayTrade (Id "m1"), MayTransform (Id "t1") ] }
        gb = coverageGameboard { world = onEntity eu (\x -> x { kind = Aggregate { policy: Id "p", members: [ us ], procedures: Map.singleton (Id "c") procedure } }) w }
      reports gb config [ ref Market "m1", ref TransformationRef "t1" ]
    it "an objective's Entities and Measure" do
      reports (withObjective (HoldingOf (Id "r2"))) config [ ref EntityRef "ghost", ref ResourceRef "r2" ]
      reports (withObjective (CapacityAmount (Id "c2"))) config [ ref CapacityName "c2" ]
      reports (withObjective (ShortfallOf (Id "r3"))) config [ ref ResourceRef "r3" ]
      reports (withObjective (ShortfallRounds (Id "r4"))) config [ ref ResourceRef "r4" ]
      reports (withObjective (VolumeOf (Id "t2"))) config [ ref TransformationRef "t2" ]
    it "a bound that is another metric is read too" do
      let
        metric = { entities: [ us ], measure: HoldingOf (Id "M"), negated: false }
        bounded = { description: "o", criteria: [ { metric, over: Final, atLeast: Of (metric { measure = HoldingOf (Id "r5") }) } ] }
      reports (coverageGameboard { actors = map (_ { objective = Just bounded }) coverageGameboard.actors }) config [ ref ResourceRef "r5" ]
    it "an implicit coverage fallback naming no Entity" do
      reports (coverageGameboard { actors = map (_ { fallback = Just (ImplicitCoverage (Id "nowhere")) }) coverageGameboard.actors }) config [ ref EntityRef "nowhere" ]
    it "a configured fallback for an unknown Actor, or covering by an unknown Entity" do
      reports coverageGameboard (config { fallbacks = Map.singleton (Id "ghost") PermittedOmission }) [ ref EntityRef "ghost" ]
      reports coverageGameboard (config { fallbacks = Map.singleton us (ImplicitCoverage (Id "nowhere")) }) [ ref EntityRef "nowhere" ]
    it "a numeraire that is not a Resource" do
      reports (coverageGameboard { world = w { numeraire = Id "gold" } }) config [ ref ResourceRef "gold" ]

  describe "Instantiation" do
    it "a decoded Gameboard's world carries no seed; instantiation sets it" do
      case decodeGameboard (json (minimalGameboard "[4, 8]")), decodeConfig (json """{ "seed": 9, "rounds": 4 }""") of
        Left e, _ -> fail (show e)
        _, Left e -> fail (show e)
        Right raw, Right config -> do
          -- the decoded world is a record with no seed field, not a seed placeholder
          Object.member "seed" (unsafeCoerce raw.world :: Object.Object Json) `shouldEqual` false
          case instantiate raw config of
            Left es -> fail (show es)
            Right initial -> show initial.world.seed `shouldEqual` "9"

-- Rules of the three kinds the test premises declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id EntityTag, origin :: Id EntityTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination } }

banR :: forall v. { jurisdiction :: Id EntityTag, target :: Id EntityTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: Id CapacityTag, units :: v } -> RuleF v
holdR r = CapacityRule { kind: Id "withholding", capacity: r.capacity, reduce: r.units }

testKinds :: Map.Map (Id RuleKindTag) RuleKindDecl
testKinds = Map.fromFoldable
  [ Tuple (Id "tariff") { form: LevyFlows GovernedByDestination, text: "" }
  , Tuple (Id "accessBan") { form: BlockFlows GovernedByEither, text: "" }
  , Tuple (Id "withholding") { form: ReduceCapacity, text: "" }
  ]
