-- | Validation (docs/03_gameboard.md §12): structural references, economic and Representation
-- | checks, resolution consistency, and feasibility. Imports Instantiate and Reconcile, so
-- | the instantiation and reconciliation checks run as part of validation.
module Board.Gameboard.Validate
  ( validateGameboard
  , unknownNames
  ) where

import Prelude

import Core.Map (values)

import Board.Gameboard.Instantiate (instantiate, withoutActor)
import Board.Gameboard.Reconcile (reconcile)
import Schema.Gameboard (ActorDefinition, Fallback(..), Gameboard, InstantiationConfig)
import Board.Error (ValidationError(..))
import Core.Query (aggregateOf, isAggregate)
import Core.Types (Capability(..), Entity, EntityId, Id(..), MarketId, RefKind(..), ResourceId, Scope(..), TransformationId, domainResource, idText)
import Core.World (ShareSource(..))
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
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
        <> map (show <<< _.capacity) w.circulation.production
    )

  -- An unknown name in any position: one error per name.
  unknown :: forall t. RefKind -> Array (Id t) -> Array ValidationError
  unknown kind = unknownNames w gb.parameters <<< map (Tuple kind <<< idText)
  unknownEntity = unknown EntityRef
  unknownResource = unknown ResourceRef
  unknownTransformation = unknown TransformationRef
  -- a capacity named by a facility, effect, rule, or production entry must be held by some Entity
  unknownCapacity xs = map (UnknownReference CapacityName <<< show) (filter (\c -> not (show c `elem` held)) xs)

  references =
    unknownEntity (concatMap (\m -> m.access) markets)
      <> unknownResource (concatMap (\m -> [ m.base, m.quote ] <> fromFoldable (Map.keys m.reserves)) markets)
      <> unknownResource (concatMap (\t -> fromFoldable (Map.keys t.inputs) <> fromFoldable (Map.keys t.outputs) <> fromFoldable (Map.keys t.reserves)) facilities)
      <> unknownCapacity (mapMaybe _.capacity facilities)
      <> unknownCapacity (concatMap (\t -> map _.capacity t.effects) facilities)
      <> unknownEntity (concatMap (\t -> mapMaybe _.holder t.effects) facilities)
      <> unknownEntity (mapMaybe _.payee facilities)
      <> unknownResource (concatMap (\e -> fromFoldable (Map.keys e.accounts)) entities)
      <> map (UnknownReference CapacityName) (filter (\c -> not (c `elem` referenced)) held)
      <> unknownEntity (concatMap (\p -> [ p.entity ]) w.circulation.production)
      <> unknownResource (concatMap (\p -> [ p.resource ]) w.circulation.production)
      <> unknownEntity (concatMap (\f -> [ f.from, f.to ]) w.circulation.flows)
      <> unknownResource (map _.resource w.circulation.flows)
      <> unknownEntity (concatMap (\s -> [ s.from, s.to ]) w.circulation.shares)
      <> unknownResource
        ( concatMap
            ( \s -> case s.source of
                SalesOf r -> [ r ]
                _ -> []
            )
            w.circulation.shares
        )
      <> unknownEntity (map _.entity w.circulation.requirements)
      <> unknownResource (map _.resource w.circulation.requirements)
      <> unknownEntity (concatMap (\(Tuple k v) -> [ k, v ]) (Map.toUnfoldable w.groups :: Array _))
      <> concatMap ruleRefs (values w.rules)
      <> concatMap enactRefs (concatMap _.capabilities (values w.entities))
      <> concatMap enactRefs (concatMap (\x -> concatMap _.authorizes (values x.aggregate.procedures)) aggregates)
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
  reservations = concatMap (\t -> map (ReservesNonReservable t.id) (filter notReservable (fromFoldable (Map.keys t.reserves)))) facilities
  notReservable r = maybe false (not <<< _.reservable) (Map.lookup r w.resources)
  ruleRefs r = (if Map.member r.kind w.ruleKinds then [] else unknownKind r.kind) <> case r.scope of
    Flows f -> unknownEntity [ f.governor, f.counterparty ]
    OnCapacity c -> unknownCapacity [ c ]
  unknownKind k = [ UnknownReference RuleKindRef (show k) ]
  -- a capability to enact names a declared kind
  enactRefs = case _ of
    MayEnact k | not (Map.member k w.ruleKinds) -> unknownKind k
    _ -> []
  delays = map (NegativeDelay <<< _.id) (filter (\t -> t.delay < 0) facilities)
  -- an institutional fallback binds a catalog Policy, declared or overridden in the configuration
  fallbackPolicies = concatMap institutional (mapMaybe _.fallback (values gb.actors) <> values config.fallbacks)
  institutional = case _ of
    InstitutionalPolicy p -> [ p ]
    _ -> []
  aggregatePolicies = map _.aggregate.policy aggregates
  capacities = concatMap productionHeld w.circulation.production
    <> concatMap coverageAggregate (Map.toUnfoldable gb.actors :: Array (Tuple EntityId ActorDefinition))
    <> concatMap schemeTarget gb.schemes
    <> (map (\g -> DuplicateCapacityOwner (show (NEA.head g))) (filter (\g -> NEA.length g > 1) (group (sort (concatMap (\e -> fromFoldable (Map.keys e.capacities)) entities # filter (\c -> not (sharedProduction c)))))))
  -- production capacity is held by each producer under one name; every other capacity has one owner
  sharedProduction c = any (\p -> p.capacity == c) w.circulation.production
  productionHeld p = case Map.lookup p.entity w.entities of
    Just e | not (Map.member p.capacity e.capacities) -> [ ProductionWithoutCapacity p.entity (show p.capacity) ]
    _ -> []
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

-- | The names a world and its parameters lack: one error per unknown name of a kind this can check
-- | (an Entity, Market, Resource, Transformation, parameter, capacity, or collective procedure). Other kinds are checked where they
-- | are held, so they pass here.
unknownNames
  :: forall m r t p rest
   . { entities :: Map EntityId Entity, markets :: Map MarketId m, resources :: Map ResourceId r, transformations :: Map TransformationId t | rest }
  -> Object p
  -> Array (Tuple RefKind String)
  -> Array ValidationError
unknownNames w parameters = map (\(Tuple kind name) -> UnknownReference kind name) <<< filter (\(Tuple kind name) -> not (known kind name))
  where
  known kind name = case kind of
    EntityRef -> Map.member (Id name) w.entities
    Market -> Map.member (Id name) w.markets
    ResourceRef -> Map.member (Id name) w.resources
    TransformationRef -> Map.member (Id name) w.transformations
    ParameterRef -> Object.member name parameters
    CapacityName -> any (Map.member (Id name) <<< _.capacities) (values w.entities)
    ProcedureRef -> any (maybe false (Map.member (Id name) <<< _.procedures) <<< aggregateOf) (values w.entities)
    _ -> true
