-- | Instantiation (docs/04_instantiated_game.md §10): a Gameboard and its configuration become
-- | the initial World State, with controllers, fallbacks, account partitioning, procedure
-- | weights, and Representations resolved.
module Board.Gameboard.Instantiate
  ( instantiate
  , withoutActor
  , mergeRequirements
  ) where

import Prelude

import Core.Map (sumMaps)

import Schema.Gameboard (Fallback(..), Gameboard, gameboardRef, InitialWorld, InstantiationConfig)
import Schema.Recurring (isRequirement, kinds, normalize)
import Board.Error (ValidationError(..))
import Core.Num as Q
import Core.Query (aggregateOf, lookupQ)
import Core.Types (Controller(..), EntityId, EntityKind(..), Id(..))
import Core.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), UnseededWorld, manifestOf, sameWorld, seeded, traverseWorld)
import Data.Array (concatMap, elem, filter, foldl, fromFoldable, mapMaybe, nub, nubByEq, null)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))

-- | Controllers and fallbacks, account partitioning and procedure weights, Representation
-- | assignment, then the World State and manifest. Atomic: errors or a complete world.
instantiate :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
instantiate gb config =
  let
    errors = controllerErrors <> representationErrors
    world = foldl applyFallback (gb.world { entities = map setController gb.world.entities }) (Map.toUnfoldable unselected :: Array _)
    final = (seeded config.seed world)
      { representations = mapMaybe toRepresentation config.representations }
  in
    if not (null errors) then Left errors
    else Right
      { world: final
      , manifest: manifestOf (gameboardRef gb) final
      }
  where
  selected e = Map.member e config.seats || Map.member e config.npcs
  unselected = Map.mapMaybeWithKey (\e d -> if selected e then Nothing else Just (fromMaybe (fromMaybe PermittedOmission d.fallback) (Map.lookup e config.fallbacks))) gb.actors
  controllerErrors =
    concatMap (\(Tuple e d) -> if d.required && not (selected e) then [ RequiredNotSelected e ] else []) (Map.toUnfoldable gb.actors :: Array _)
      <> concatMap
        ( \e -> case Map.lookup e gb.actors of
            Just d | d.seat -> []
            _ -> [ ControllerNotAllowed e ]
        )
        (fromFoldable (Map.keys config.seats))
      <> concatMap
        ( \(Tuple e p) -> case Map.lookup e gb.actors of
            Just d | p `elem` d.npc -> []
            _ -> [ ControllerNotAllowed e ]
        )
        (Map.toUnfoldable config.npcs :: Array _)
  representationErrors = concatMap
    ( \r -> case schemeOf r.scheme of
        Just s | r.actor `elem` s.eligibleActors && selected r.actor -> []
        Just _ -> [ IneligibleRepresentative r.actor ]
        Nothing -> [ UnknownScheme r.scheme ]
    )
    config.representations
  schemeOf id = case filter (\s -> s.id == id) gb.schemes of
    [ s ] -> Just s
    _ -> Nothing
  -- a configured Representation of a scheme the Gameboard defines; an unknown scheme is an error
  -- (`representationErrors`), so the instantiation never returns it
  toRepresentation r = (\s -> { id: Id s.id, actor: r.actor, aggregate: s.target, domain: s.domain, observes: s.observes, weight: r.weight, estimator: s.estimator }) <$> schemeOf r.scheme
  setController e = case Map.lookup e.id config.seats, Map.lookup e.id config.npcs of
    Just seat, _ -> e { kind = Actor (Seat (Id seat)) }
    _, Just p -> e { kind = Actor (NPC p) }
    _, _ -> e

-- | The configuration with the Actor not explicit: its seat, NPC binding and Representations removed.
withoutActor :: EntityId -> InstantiationConfig -> InstantiationConfig
withoutActor e config = config { seats = Map.delete e config.seats, npcs = Map.delete e config.npcs, representations = filter (\r -> r.actor /= e) config.representations }

-- | Resolve one unselected Actor by its fallback.
applyFallback :: UnseededWorld -> Tuple EntityId Fallback -> UnseededWorld
applyFallback w (Tuple e fb) = case fb of
  InstitutionalPolicy p -> w { entities = Map.update (\x -> Just x { kind = Actor (NPC p) }) e w.entities }
  PermittedOmission -> w
  ImplicitCoverage agg -> mergeInto agg e w

-- | Implicit coverage: the Actor's accounts, capacities, procedure weight, market access,
-- | circulation, and jurisdiction pass to the Aggregate that covers it.
mergeInto :: EntityId -> EntityId -> UnseededWorld -> UnseededWorld
mergeInto agg e w = case Map.lookup e w.entities of
  Nothing -> w
  Just gone ->
    let
      moved = un Identity (traverseWorld retargeting (w { entities = Map.update (Just <<< absorb gone) agg (Map.delete e w.entities), groups = Map.delete e w.groups }))
    in
      moved
        { markets = map (\m -> m { access = nub m.access }) moved.markets
        , recurring = normalize (mergeRequirements (dedupProduction (filter notSelfFlow moved.recurring)))
        }
  where
  retarget x = if x == e then agg else x
  -- every reference to the Actor is to the Aggregate now: rules, payments, holders, and access lists alike
  retargeting = sameWorld
    { entity = Identity <<< retarget
    , members = \self -> Identity <<< members self
    , weights = Identity <<< moveWeight
    }
  notSelfFlow r = case r.source, r.destination of
    FromEntity a, ToEntity b -> a /= b
    _, _ -> true
  -- production that became identical once retargeted stays one rule, as a merged Entity holds one capacity
  dedupProduction = nubByEq sameProduction
  sameProduction a b = a.kind == kinds.production && b.kind == kinds.production && a.source == b.source && a.destination == b.destination && a.resource == b.resource && a.amount == b.amount
  absorb gone a = a
    { accounts = sumMaps a.accounts gone.accounts
    , capacities = sumMaps a.capacities gone.capacities
    , kind = foldWeights a
    }
  -- a member's procedure weight folds into the Aggregate's residual weight
  foldWeights a = case aggregateOf a of
    Just x -> Aggregate x { procedures = map (\p -> p { residualWeight = Q.add p.residualWeight (lookupQ e p.weights), weights = Map.delete e p.weights }) x.procedures }
    Nothing -> a.kind
  -- the Actor's membership passes to the Aggregate in every Aggregate's list; an Aggregate is never its own
  -- member (lists that do not name the Actor are left as written)
  members self ms = if elem e ms then filter (_ /= self) (nub (map retarget ms)) else ms
  -- its procedure weight passes the same way
  moveWeight ws = case Map.lookup e ws of
    Nothing -> ws
    Just wt -> Map.insert agg (maybe wt (Q.add wt) (Map.lookup agg ws)) (Map.delete e ws)

-- | Requirements of one Entity and Resource, merged as docs/04 §4.2 says: their explicit amounts
-- | are summed. Two requirements that are both "equal to purchases" stay one. An explicit amount
-- | and a purchases-based one cannot be summed into one amount, so they stay two requirements:
-- | each consumes from what the Entity holds and records its own shortfall, which totals what the
-- | sum would. Every other rule keeps its place.
mergeRequirements :: Array RecurringFlow -> Array RecurringFlow
mergeRequirements = foldl add []
  where
  add acc r
    | isRequirement r = case filter (sameKey r) acc of
        [ prior ] -> case mergedAmount prior.amount r.amount of
          Just amount -> map (\x -> if sameKey r x then x { amount = amount } else x) acc
          Nothing -> acc <> [ r ]
        _ -> acc <> [ r ]
    | otherwise = acc <> [ r ]
  sameKey r x = isRequirement x && x.source == r.source && x.resource == r.resource

  -- Just the merged amount when one exists, Nothing when the two must stay apart
  mergedAmount :: RecurringAmount -> RecurringAmount -> Maybe RecurringAmount
  mergedAmount a b = case a, b of
    FixedAmount x, FixedAmount y -> Just (FixedAmount (Q.add x y))
    Measured x, Measured y | x == y -> Just (Measured x)
    _, _ -> Nothing
