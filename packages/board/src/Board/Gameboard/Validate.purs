-- | Validation (docs/03_gameboard.md §12): structural references, economic and Representation
-- | checks, resolution consistency, and feasibility. Imports Instantiate and Reconcile, so
-- | the instantiation and reconciliation checks run as part of validation.
module Board.Gameboard.Validate
  ( validateGameboard
  ) where

import Prelude

import Board.Gameboard.Instantiate (instantiate)
import Board.Gameboard.Reconcile (reconcile)
import Board.Gameboard.Schema (ActorDefinition, Fallback(..), Gameboard, InstantiationConfig, RefKind(..), ValidationError(..))
import Core.Types (EntityId, EntityKind(..), Id(..), Rule(..))
import Core.World (ShareSource(..))
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Structural, economic, Representation, resolution-consistency, and feasibility checks.
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
validateGameboard gb config =
  let
    structural = references <> reservations <> capacities <> payees <> fallbacks <> objectives <> procedures <> roundCount
    feasibility = either identity (const []) (instantiate gb config)
    consistency = if null (structural <> feasibility) then concatMap consistent (fromFoldable (Map.keys gb.actors)) else []
    errors = structural <> feasibility <> consistency
  in
    if null errors then Right gb else Left errors
  where
  w = gb.world
  entities = fromFoldable (Map.values w.entities)
  markets = fromFoldable (Map.values w.markets)
  facilities = fromFoldable (Map.values w.transformations)
  aggregates = concatMap
    ( \e -> case e.kind of
        Aggregate a -> [ Tuple e.id a ]
        _ -> []
    )
    entities
  isAggregate e = any (\(Tuple id _) -> id == e) aggregates
  -- capacities an Entity holds, by name
  held = nub (concatMap (\e -> map show (fromFoldable (Map.keys e.capacities))) entities)
  -- capacities a facility, an effect, or the production list refers to
  referenced = nub
    ( map show (mapMaybe _.capacity facilities)
        <> concatMap (\t -> map (show <<< _.capacity) t.effects) facilities
        <> map (show <<< _.capacity) w.circulation.production
    )

  -- An unknown name in any position: one error per name.
  missing :: forall k v. Ord k => (k -> String) -> RefKind -> Map k v -> Array k -> Array ValidationError
  missing name kind m xs = map (UnknownReference kind <<< name) (filter (\x -> not (Map.member x m)) xs)
  unknownEntity = missing show EntityRef w.entities
  unknownResource = missing show ResourceRef w.resources
  unknownTransformation = missing show TransformationRef w.transformations
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
      <> unknownEntity (concatMap (\(Tuple k v) -> [ k, v ]) (Map.toUnfoldable w.jurisdiction :: Array _))
      <> concatMap ruleRefs (fromFoldable (Map.values w.rules))
      <> unknownResource (map (\(Tuple k _) -> k) (Map.toUnfoldable w.transport :: Array _))
      <> unknownTransformation (map (\(Tuple _ t) -> t) (Map.toUnfoldable w.transport :: Array _))
      <> unknownEntity (map _.target gb.schemes)
      <> unknownEntity (concatMap _.eligibleActors gb.schemes)
      <> map (\s -> UnknownReference ResourceRef (show s.domain)) (filter (\s -> not (Map.member (Id (unwrap s.domain)) w.resources)) gb.schemes)
      <> unknownEntity
        ( concatMap
            ( \e -> case e.kind of
                Aggregate a -> a.members
                _ -> []
            )
            entities
        )
      <> unknownEntity
        ( concatMap
            ( \e -> case e.kind of
                Aggregate a -> concatMap (\p -> fromFoldable (Map.keys p.weights)) (fromFoldable (Map.values a.procedures))
                _ -> []
            )
            entities
        )
      <> map (\x -> UnknownReference PolicyRef (show x)) (filter (\p -> not (Object.member (unwrap p) gb.policies)) (aggregatePolicies <> concatMap _.npc (fromFoldable (Map.values gb.actors))))
  -- a reservation must name a reservable Resource; an unknown Resource is reported by `references`
  reservations = concatMap (\t -> map (ReservesNonReservable t.id) (filter notReservable (fromFoldable (Map.keys t.reserves)))) facilities
  notReservable r = maybe false (not <<< _.reservable) (Map.lookup r w.resources)
  ruleRefs = case _ of
    Tariff x -> unknownEntity [ x.jurisdiction, x.origin ]
    AccessBan x -> unknownEntity [ x.jurisdiction, x.target ]
    Withholding x -> unknownCapacity [ x.capacity ]
  aggregatePolicies = concatMap
    ( \e -> case e.kind of
        Aggregate a -> [ a.policy ]
        _ -> []
    )
    entities
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
    Just (ImplicitCoverage agg) | Map.member agg w.entities && not (isAggregate agg) -> [ CoverageNotAggregate e agg ]
    _ -> []
  schemeTarget s = if Map.member s.target w.entities && not (isAggregate s.target) then [ SchemeTargetNotAggregate s.id s.target ] else []
  payees = map (\t -> FacilityWithoutPayee t.id) (filter (\t -> isJust t.cost && isNothing t.payee) facilities)
  fallbacks = concatMap (\(Tuple e d) -> if not d.required && isNothing d.fallback then [ MissingFallback e ] else []) (Map.toUnfoldable gb.actors :: Array _)
  objectives = concatMap (\(Tuple e d) -> if isNothing d.objective then [ MissingObjective e ] else []) (Map.toUnfoldable gb.actors :: Array _)
  procedures = concatMap
    ( \e -> case e.kind of
        Aggregate a | any (\p -> not (all (_ `elem` e.capabilities) p.authorizes)) (fromFoldable (Map.values a.procedures)) -> [ ProcedureExceedsAuthority e.id ]
        _ -> []
    )
    entities
  roundCount =
    if config.rounds < gb.rounds.min || config.rounds > gb.rounds.max then [ RoundsOutOfRange { rounds: config.rounds, min: gb.rounds.min, max: gb.rounds.max } ]
    else []
  consistent e = case Map.lookup e gb.actors of
    Just { fallback: Just (ImplicitCoverage _) } ->
      let
        without = config { seats = Map.delete e config.seats, npcs = Map.delete e config.npcs, representations = filter (\r -> r.actor /= e) config.representations }
      in
        case instantiate gb config, instantiate gb without of
          Right a, Right b -> either (\ds -> [ ResolutionInconsistent e ds ]) (const []) (reconcile a b)
          _, _ -> []
    _ -> []
