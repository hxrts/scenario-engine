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

import Core.Map (values)

import Board.Gameboard.Instantiate (instantiate, withoutActor)
import Board.Gameboard.Reconcile (reconcile)
import Schema.Gameboard (ActorDefinition, Bound(..), Fallback(..), Gameboard, InstantiationConfig, Measure(..), MetricRef)
import Schema.Gameboard as Schema
import Board.Error (ValidationError(..))
import Core.Query (aggregateOf, isAggregate)
import Core.Types (Capability(..), Entity, EntityId, Id(..), Market, MarketId, PolicyId, RefKind(..), ResourceDefinition, ResourceId, RuleKindDecl, RuleKindId, Scope(..), Transformation, TransformationId, domainResource, idText)
import Core.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShareBasis(..))
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Structural, economic, Representation, resolution-consistency, and feasibility checks.
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
validateGameboard gb config =
  let
    structural = references <> delays <> reservations <> capacities <> payees <> fallbacks <> objectives <> procedures <> roundCount
    feasibility = either identity (const []) (instantiate gb config)
    consistency = if null (structural <> feasibility) then concatMap consistent (fromFoldable (Map.keys gb.actors)) else []
    errors = structural <> feasibility <> consistency
  in
    if null errors then Right gb else Left errors
  where
  w = gb.world
  entities = values w.entities
  markets = values w.markets
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

  -- An unknown name in any position: one error per name.
  unknown :: forall t. RefKind -> Array (Id t) -> Array ValidationError
  unknown kind = unknownNames (gameboardNames gb) <<< map (Tuple kind <<< idText)
  unknownEntity = unknown EntityRef
  unknownResource = unknown ResourceRef
  unknownTransformation = unknown TransformationRef
  -- a capacity named by a facility, effect, rule, or production entry must be held by some Entity
  unknownCapacity xs = map (UnknownReference CapacityName <<< show) (filter (\c -> not (show c `elem` held)) xs)

  references =
    unknownEntity (concatMap (\m -> m.access) markets)
      <> unknownResource (concatMap (\m -> [ m.base, m.quote ] <> fromFoldable (Map.keys m.reserves)) markets)
      <> unknownResource (concatMap (\t -> fromFoldable (Map.keys t.inputs) <> fromFoldable (Map.keys t.outputs) <> fromFoldable (Map.keys t.escrowPerUnit)) facilities)
      <> unknownCapacity (mapMaybe _.capacity facilities)
      <> unknownCapacity (concatMap (\t -> map _.capacity t.effects) facilities)
      <> unknownEntity (concatMap (\t -> mapMaybe _.holder t.effects) facilities)
      <> unknownEntity (mapMaybe _.payee facilities)
      <> unknownResource (concatMap (\e -> fromFoldable (Map.keys e.accounts)) entities)
      <> map (UnknownReference CapacityName) (filter (\c -> not (c `elem` referenced)) held)
      <> concatMap recurringRefs w.recurring
      <> unknownEntity (concatMap (\(Tuple k v) -> [ k, v ]) (Map.toUnfoldable w.groups :: Array _))
      <> concatMap ruleRefs (values w.rules)
      <> concatMap capabilityRefs (concatMap _.capabilities (values w.entities))
      <> concatMap capabilityRefs (concatMap (\x -> concatMap _.authorizes (values x.aggregate.procedures)) aggregates)
      <> unknownResource [ w.numeraire ]
      <> concatMap metricRefs (concatMap _.criteria (mapMaybe _.objective (values gb.actors)) >>= \c -> [ c.metric ] <> boundMetric c.atLeast)
      <> unknownEntity (concatMap coverageOf (mapMaybe _.fallback (values gb.actors) <> values config.fallbacks))
      <> map (UnknownReference EntityRef <<< idText) (filter (\e -> not (Map.member e gb.actors)) (fromFoldable (Map.keys config.fallbacks)))
      <> unknownResource (map (\(Tuple k _) -> k) (Map.toUnfoldable w.transferConditions :: Array _))
      <> unknownTransformation (map (\(Tuple _ t) -> t.bundled) (Map.toUnfoldable w.transferConditions :: Array _))
      <> unknownEntity (map _.target gb.schemes)
      <> unknownEntity (concatMap _.eligibleActors gb.schemes)
      <> map (\s -> UnknownReference ResourceRef (show s.domain)) (filter (\s -> not (Map.member (domainResource s.domain) w.resources)) gb.schemes)
      <> unknownEntity (concatMap _.aggregate.members aggregates)
      <> unknownEntity
        (concatMap (\x -> concatMap (\p -> fromFoldable (Map.keys p.weights)) (values x.aggregate.procedures)) aggregates)
      <> map (\x -> UnknownReference PolicyRef (show x)) (filter (\p -> not (Map.member p gb.policies)) (aggregatePolicies <> concatMap _.npc (values gb.actors) <> fallbackPolicies))
  -- a reservation must name a reservable Resource; an unknown Resource is reported by `references`
  reservations = concatMap (\t -> map (ReservesNonReservable t.id) (filter notReservable (fromFoldable (Map.keys t.escrowPerUnit)))) facilities
  notReservable r = maybe false (not <<< _.reservable) (Map.lookup r w.resources)
  ruleRefs r = (if Map.member r.kind w.ruleKinds then [] else unknownKind r.kind) <> case r.scope of
    Flows f -> unknownEntity [ f.governor, f.counterparty ]
    OnCapacity c -> unknownCapacity [ c ]
  unknownKind k = [ UnknownReference RuleKindRef (show k) ]
  -- a capability names a Market, a Resource, a Transformation, or a declared Rule kind
  capabilityRefs = case _ of
    MayTrade m -> unknown Market [ m ]
    MayTransfer r -> unknownResource [ r ]
    MayTransform t -> unknownTransformation [ t ]
    MayEnact k -> if Map.member k w.ruleKinds then [] else unknownKind k
  -- a criterion's metric, and the metric its bound reads when it is one
  boundMetric = case _ of
    Fixed _ -> []
    Of m -> [ m ]

  metricRefs :: MetricRef -> Array ValidationError
  metricRefs m = unknownEntity m.entities <> case m.measure of
    HoldingOf r -> unknownResource [ r ]
    CapacityAmount c -> unknownCapacity [ c ]
    ShortfallOf r -> unknownResource [ r ]
    ShortfallRounds r -> unknownResource [ r ]
    Schema.PayeeReceipts -> []
    VolumeOf t -> unknownTransformation [ t ]
  -- the Aggregate a coverage fallback names
  coverageOf = case _ of
    ImplicitCoverage agg -> [ agg ]
    _ -> []
  delays = map (NegativeDelay <<< _.id) (filter (\t -> t.delay < 0) facilities)
  -- an institutional fallback binds a catalog Policy, declared or overridden in the configuration
  fallbackPolicies = concatMap institutional (mapMaybe _.fallback (values gb.actors) <> values config.fallbacks)
  institutional = case _ of
    InstitutionalPolicy p -> [ p ]
    _ -> []
  aggregatePolicies = map _.aggregate.policy aggregates
  capacities = concatMap productionHeld w.recurring
    <> concatMap coverageAggregate (Map.toUnfoldable gb.actors :: Array (Tuple EntityId ActorDefinition))
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

  -- every name a recurring rule uses
  recurringRefs :: RecurringFlow -> Array ValidationError
  recurringRefs r = unknownResource [ r.resource ] <> unknownEntity (sources r) <> unknownEntity (dests r) <> amountRefs r.amount <> conditionRefs r
  sources r = case r.source of
    FromEntity e -> [ e ]
    _ -> []
  dests r = case r.destination of
    ToEntity e -> [ e ]
    ToSink -> []
  amountRefs = case _ of
    ShareOf { basis: NetSales res } -> unknownResource [ res ]
    _ -> []
  conditionRefs r = case r.condition of
    Just c -> unknownEntity [ c.holder ] <> unknownResource [ c.resource ]
    Nothing -> []
  coverageAggregate (Tuple e d) = case d.fallback of
    Just (ImplicitCoverage agg) | Map.member agg w.entities && not (isAggregateId agg) -> [ CoverageNotAggregate e agg ]
    _ -> []
  schemeTarget s = if Map.member s.target w.entities && not (isAggregateId s.target) then [ SchemeTargetNotAggregate s.id s.target ] else []
  payees = map (\t -> FacilityWithoutPayee t.id) (filter (\t -> isJust t.cost && isNothing t.payee) facilities)
  fallbacks = concatMap (\(Tuple e d) -> if not d.required && isNothing d.fallback then [ MissingFallback e ] else []) (Map.toUnfoldable gb.actors :: Array _)
  objectives = concatMap (\(Tuple e d) -> if isNothing d.objective then [ MissingObjective e ] else []) (Map.toUnfoldable gb.actors :: Array _)
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

-- | The names of a world on its own: a literal premise has no parameters, schemes, or Policies.
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
    EntityRef -> Map.member (Id name) w.entities
    Market -> Map.member (Id name) w.markets
    ResourceRef -> Map.member (Id name) w.resources
    TransformationRef -> Map.member (Id name) w.transformations
    ParameterRef -> Set.member name w.parameters
    CapacityName -> any (Map.member (Id name) <<< _.capacities) (values w.entities)
    ProcedureRef -> any (maybe false (Map.member (Id name) <<< _.procedures) <<< aggregateOf) (values w.entities)
    PolicyRef -> Set.member (Id name) w.policies
    RuleKindRef -> Map.member (Id name) w.ruleKinds
