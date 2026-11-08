module Test.Board.Main where

import Prelude

import Board.Error (ValidationError(..))
import Board.Gameboard.Instantiate (instantiate, mergeRequirements)
import Board.Gameboard.Validate (validateGameboard)
import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (Controller(..), Entity, EntityId, EntityKind(..), Id(..), RefKind(..), Rule, RuleF(..), RuleId, Transferability(..), Transformation, TransformationId)
import Core.World (UnseededWorld)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concatMap, elem, filter, fromFoldable, mapMaybe)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Schema.Gameboard (ActorDefinition, Fallback(..), Gameboard)
import Schema.Premise.Gameboard (decodeConfig, decodeGameboard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Unsafe.Coerce (unsafeCoerce)

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

-- | A Gameboard in which the Actor US is covered by the Aggregate EU (ImplicitCoverage). US holds
-- | the capacity `cap` and is the payee and effect holder of a facility, the origin and target of
-- | Rules (in force and scheduled), and an eligible Actor of a Representation scheme.
coverageGameboard :: Gameboard
coverageGameboard =
  { id: "coverage-merge"
  , version: "v0"
  , world: world
  , actors: Map.singleton us actor
  , schemes: [ { id: "s", eligibleActors: [ us ], target: eu, domain: Id "d", estimator: estimator } ]
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
    , reserves: Map.empty
    , cost: Just { base: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 10 } }
    , capacity: Nothing
    , payee: Just us
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
    , resources: Map.singleton money { id: money, unit: "USD", transferability: Free, decay: Nothing, reservable: false }
    , markets: Map.empty
    , transformations: Map.singleton (Id "fac") facility
    , representations: []
    , numeraire: money
    , transport: Map.empty
    , jurisdiction: Map.empty
    , rules: Map.fromFoldable
        [ Tuple (Id "tariff") (Tariff { jurisdiction: eu, origin: us, rate: Q.fromInt 1 })
        , Tuple (Id "ban") (AccessBan { jurisdiction: eu, target: us })
        ]
    , scheduled: Map.singleton (Id "later") (Just (AccessBan { jurisdiction: eu, target: us }))
    , deliveries: []
    , factors: Map.empty
    , circulation: { production: [], flows: [], shares: [], requirements: [] }
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
  mapMaybe _.payee transforms
    <> concatMap (\t -> mapMaybe _.holder t.effects) transforms
    <> concatMap ruleEntities (map Just (fromFoldable (Map.values w.rules)) <> fromFoldable (Map.values w.scheduled))
  where
  transforms = fromFoldable (Map.values w.transformations)

  ruleEntities :: Maybe Rule -> Array EntityId
  ruleEntities = case _ of
    Just (Tariff x) -> [ x.jurisdiction, x.origin ]
    Just (AccessBan x) -> [ x.jurisdiction, x.target ]
    _ -> []

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Requirement merge (docs/04 §4.2)" do
    -- an operating requirement of the US in money, with the given amount
    let req amount = { entity: Id "US", resource: Id "M", amount, penalty: Q.zero }
    it "two explicit amounts for one Entity and Resource are summed" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req (Just (Q.fromInt 4)) ]) `shouldEqual` [ Just (Q.fromInt 7) ]
    it "an explicit amount and one equal to purchases both survive the merge, neither replaced by the other" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req Nothing ]) `shouldEqual` [ Just (Q.fromInt 3), Nothing ]
    it "two requirements equal to purchases stay one" do
      map _.amount (mergeRequirements [ req Nothing, req Nothing ]) `shouldEqual` [ Nothing ]

  describe "Coverage merge retargets references (docs/04 §4.2)" do
    let config = { seed: Seed 1, rounds: 2, seats: Map.empty, npcs: Map.empty, fallbacks: Map.empty, representations: [] }
    it "a payee, an effect holder, and Rules naming a merged Actor name its Aggregate instead" do
      case instantiate coverageGameboard config of
        Left es -> fail (show es)
        Right initial -> do
          filter (\e -> not (Map.member e initial.world.entities)) (worldEntityRefs initial.world) `shouldEqual` []
          map _.payee (fromFoldable (Map.values initial.world.transformations)) `shouldEqual` [ Just (Id "EU") ]
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
