-- | Validation (docs/03_gameboard.md §12): structural references, economic and Representation
-- | checks, resolution consistency, and feasibility. Imports Instantiate and Reconcile, so
-- | the instantiation and reconciliation checks run as part of validation.
module Board.Gameboard.Validate
  ( validateGameboard
  , unknownNames
  , Names
  , worldNames
  , gameboardNames
  ) where

import Prelude

import Core.Map (entries, values)

import Board.Gameboard.Instantiate (instantiate, withoutActor)
import Board.Gameboard.Reconcile (reconcile)
import Schema.Gameboard (Bound(..), Fallback(..), Gameboard, InstantiationConfig, Measure(..), MetricRef)
import Schema.Gameboard as Schema
import Board.Error (ValidationError(..))
import Core.Query (aggregateOf, isAggregate)
import Core.Types (Entity, EntityId, Id(..), Market, MarketId, PolicyId, RefKind(..), ResourceDefinition, ResourceId, RuleKindDecl, RuleKindId, Transformation, TransformationId, idText)
import Core.World (RecurringAmount(..), RecurringDestination(..), WorldFields, WorldPositions, foldWorld, ignoreWorld)
import Data.Const (Const(..))
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Structural, economic, Representation, resolution-consistency, and feasibility checks.
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
validateGameboard gb config =
  let
    structural = references <> delays <> capacities <> fallbacks <> objectives <> procedures <> roundCount
    feasibility = either identity (const []) (instantiate gb config)
    consistency = if null (structural <> feasibility) then concatMap consistent (fromFoldable (Map.keys gb.actors)) else []
    errors = structural <> feasibility <> consistency
  in
    if null errors then Right gb else Left errors
  where
  w = gb.world
  entities = values w.entities
  facilities = values w.transformations
  aggregates = mapMaybe (\e -> map { entity: e, aggregate: _ } (aggregateOf e)) entities
  -- whether the Entity with this id is an Aggregate (an unknown id is not)
  isAggregateId e = maybe false isAggregate (Map.lookup e w.entities)
  -- capacities an Entity holds, by name
  held = nub (concatMap (\e -> map show (fromFoldable (Map.keys e.capacities))) entities)
  -- capacities a facility, an effect, or the production list refers to
  referenced = nub
    ( map show (mapMaybe _.capacity facilities)
        <> concatMap (\t -> map (show <<< _.capacity) t.effects) facilities
        <> map show (mapMaybe producedFrom w.recurring)
    )

  references = unknownNames (gameboardNames gb) (worldRefs w <> gameboardRefs gb config)
    <> map (UnknownReference RefCapacity) (filter (\c -> not (c `elem` referenced)) held)
    <> map (UnknownReference RefEntity <<< idText) (filter (\e -> not (Map.member e gb.actors)) (fromFoldable (Map.keys config.fallbacks)))
  delays = map (NegativeDelay <<< _.id) (filter (\t -> t.delay < 0) facilities)
  capacities = concatMap productionHeld w.recurring
    <> concatMap coverageAggregate (entries gb.actors)
    <> concatMap schemeTarget gb.schemes
    <> (map (\g -> DuplicateCapacityOwner (show (NEA.head g))) (filter (\g -> NEA.length g > 1) (group (sort (concatMap (\e -> fromFoldable (Map.keys e.capacities)) entities # filter (\c -> not (sharedProduction c)))))))
  -- production capacity is held by each producer under one name; every other capacity has one owner
  sharedProduction c = any (\p -> producedFrom p == Just c) w.recurring
  productionHeld p = case p.destination, p.amount of
    ToEntity e, CapacityHeld c | Just x <- Map.lookup e w.entities, not (Map.member c x.capacities) -> [ ProductionWithoutCapacity e (show c) ]
    _, _ -> []
  -- the capacity a rule produces from
  producedFrom r = case r.amount of
    CapacityHeld c -> Just c
    _ -> Nothing

  coverageAggregate (Tuple e d) = case d.fallback of
    Just (ImplicitCoverage agg) | Map.member agg w.entities && not (isAggregateId agg) -> [ CoverageNotAggregate e agg ]
    _ -> []
  schemeTarget s = if Map.member s.target w.entities && not (isAggregateId s.target) then [ SchemeTargetNotAggregate s.id s.target ] else []
  fallbacks = concatMap (\(Tuple e d) -> if not d.required && isNothing d.fallback then [ MissingFallback e ] else []) (entries gb.actors)
  objectives = concatMap (\(Tuple e d) -> if isNothing d.objective then [ MissingObjective e ] else []) (entries gb.actors)
  procedures = concatMap
    (\x -> if any (\p -> not (all (_ `elem` x.entity.capabilities) p.authorizes)) (values x.aggregate.procedures) then [ ProcedureExceedsAuthority x.entity.id ] else [])
    aggregates
  roundCount =
    if config.rounds < gb.rounds.min || config.rounds > gb.rounds.max then [ RoundsOutOfRange { rounds: config.rounds, min: gb.rounds.min, max: gb.rounds.max } ]
    else []
  consistent e = case Map.lookup e gb.actors of
    Just { fallback: Just (ImplicitCoverage _) } ->
      case instantiate gb config, instantiate gb (withoutActor e config) of
        Right a, Right b -> either (\ds -> [ ResolutionInconsistent e ds ]) (const []) (reconcile a b)
        _, _ -> []
    _ -> []

-- | Every name the world refers to, each with what it must name.
worldRefs :: forall r. Record (WorldFields r) -> Array (Tuple RefKind String)
worldRefs = foldWorld
  (ignoreWorld :: WorldPositions (Const (Array (Tuple RefKind String))))
    { entity = ref RefEntity
    , groupKey = ref RefEntity
    , resource = ref RefResource
    , market = ref RefMarket
    , transformation = ref RefTransformation
    , capacity = ref RefCapacity
    , ruleKind = ref RefRuleKind
    , members = \_ -> Const <<< map (name RefEntity)
    , weights = Const <<< map (name RefEntity) <<< fromFoldable <<< Map.keys
    }
  where
  name :: forall t. RefKind -> Id t -> Tuple RefKind String
  name kind x = Tuple kind (idText x)

  ref :: forall t b. RefKind -> Id t -> Const (Array (Tuple RefKind String)) b
  ref kind x = Const [ name kind x ]

-- | The names the Gameboard's objectives, schemes, and fallbacks refer to, and the configuration's.
gameboardRefs :: Gameboard -> InstantiationConfig -> Array (Tuple RefKind String)
gameboardRefs gb config =
  concatMap metricRefs (concatMap _.criteria (mapMaybe _.objective actors) >>= \c -> [ c.metric ] <> boundMetric c.atLeast)
    <> map (name RefEntity) (concatMap coverageOf fallbacks)
    <> concatMap (\s -> [ name RefEntity s.target ] <> map (name RefEntity) s.eligibleActors <> [ name RefResource s.observes ]) gb.schemes
    <> map (name RefPolicy) (map _.policy (mapMaybe aggregateOf (values gb.world.entities)) <> concatMap _.npc actors <> concatMap institutional fallbacks)
  where
  actors = values gb.actors
  -- an institutional fallback binds a catalog Policy, declared or overridden in the configuration
  fallbacks = mapMaybe _.fallback actors <> values config.fallbacks

  name :: forall t. RefKind -> Id t -> Tuple RefKind String
  name kind x = Tuple kind (idText x)
  -- a criterion's metric, and the metric its bound reads when it is one
  boundMetric = case _ of
    Fixed _ -> []
    Of m -> [ m ]

  metricRefs :: MetricRef -> Array (Tuple RefKind String)
  metricRefs m = map (name RefEntity) m.entities <> case m.measure of
    HoldingOf r -> [ name RefResource r ]
    CapacityAmount c -> [ name RefCapacity c ]
    ShortfallOf r -> [ name RefResource r ]
    ShortfallRounds r -> [ name RefResource r ]
    Schema.PayeeReceipts -> []
    VolumeOf t -> [ name RefTransformation t ]
  -- the Aggregate a coverage fallback names
  coverageOf = case _ of
    ImplicitCoverage agg -> [ agg ]
    _ -> []
  institutional = case _ of
    InstitutionalPolicy p -> [ p ]
    _ -> []

-- | What a reference can name.
type Names m r t k =
  { entities :: Map EntityId Entity
  , markets :: Map MarketId m
  , resources :: Map ResourceId r
  , transformations :: Map TransformationId t
  , ruleKinds :: Map RuleKindId k
  , parameters :: Set String
  , policies :: Set PolicyId
  }

-- | The names of a world on its own: a literal setting has no parameters, schemes, or Policies.
worldNames :: forall m r t k rest. { entities :: Map EntityId Entity, markets :: Map MarketId m, resources :: Map ResourceId r, transformations :: Map TransformationId t, ruleKinds :: Map RuleKindId k | rest } -> Names m r t k
worldNames w =
  { entities: w.entities
  , markets: w.markets
  , resources: w.resources
  , transformations: w.transformations
  , ruleKinds: w.ruleKinds
  , parameters: Set.empty
  , policies: Set.empty
  }

-- | The names a Gameboard offers a reference.
gameboardNames :: Gameboard -> Names Market ResourceDefinition Transformation RuleKindDecl
gameboardNames gb = (worldNames gb.world)
  { parameters = Set.fromFoldable (Object.keys gb.parameters)
  , policies = Map.keys gb.policies
  }

-- | The names the scope lacks: one error per unknown name, for every kind of reference.
unknownNames :: forall m r t k. Names m r t k -> Array (Tuple RefKind String) -> Array ValidationError
unknownNames w = map (\(Tuple kind name) -> UnknownReference kind name) <<< filter (\(Tuple kind name) -> not (known kind name))
  where
  known kind name = case kind of
    RefEntity -> Map.member (Id name) w.entities
    RefMarket -> Map.member (Id name) w.markets
    RefResource -> Map.member (Id name) w.resources
    RefTransformation -> Map.member (Id name) w.transformations
    RefParameter -> Set.member name w.parameters
    RefCapacity -> any (Map.member (Id name) <<< _.capacities) (values w.entities)
    RefProcedure -> any (maybe false (Map.member (Id name) <<< _.procedures) <<< aggregateOf) (values w.entities)
    RefPolicy -> Set.member (Id name) w.policies
    RefRuleKind -> Map.member (Id name) w.ruleKinds
