-- | Instantiation (docs/04_run.md §10): a possible World and its configuration become
-- | the initial World State, with controllers, fallbacks, account partitioning, procedure
-- | weights, and Representations resolved.
module World.PossibleWorld.Instantiate
  ( instantiate
  , withoutExplicitActor
  , mergeRequirements
  ) where

import Prelude

import Core.Map (entries, sumMaps)

import Schema.PossibleWorld (Fallback(..), PossibleWorld, possibleWorldRef, InitialWorld, InstantiationConfig)
import Schema.Recurring (isRequirement, kinds, normalize)
import World.Error (ValidationError(..))
import Core.Num as Q
import Laws.Query (aggregateOf, lookupQ)
import Laws.Types (ActorId, ActorKind(..), Id(..))
import Laws.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), UnseededWorld, manifestOf, sameWorld, seeded, traverseWorld)
import Data.Array (concatMap, elem, filter, foldl, fromFoldable, mapMaybe, nub, nubByEq, null)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))

-- | Controllers and fallbacks, account partitioning and procedure weights, Representation
-- | assignment, then the World State and manifest. Atomic: errors or a complete world.
instantiate :: PossibleWorld -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
instantiate gb config =
  let
    errors = controllerErrors <> representationErrors
    world = foldl applyFallback (gb.world { actors = map setController gb.world.actors }) (entries unselected)
    final = (seeded config.seed world)
      { representations = mapMaybe toRepresentation config.representations }
  in
    if not (null errors) then Left errors
    else Right
      { world: final
      , manifest: manifestOf (possibleWorldRef gb) final
      }
  where
  selected e = Map.member e config.seats || Map.member e config.nonPlayableActors
  unselected = Map.mapMaybeWithKey (\e d -> if selected e then Nothing else Just (fromMaybe (fromMaybe PermittedOmission d.fallback) (Map.lookup e config.fallbacks))) gb.explicitActors
  controllerErrors =
    concatMap (\(Tuple e d) -> if d.required && not (selected e) then [ RequiredNotSelected e ] else []) (entries gb.explicitActors)
      <> concatMap
        ( \e -> case Map.lookup e gb.explicitActors of
            Just d | d.seat -> []
            _ -> [ ControllerNotAllowed e ]
        )
        (fromFoldable (Map.keys config.seats))
      <> concatMap
        ( \(Tuple e p) -> case Map.lookup e gb.explicitActors of
            Just d | p `elem` d.nonPlayableActor -> []
            _ -> [ ControllerNotAllowed e ]
        )
        (entries config.nonPlayableActors)
  representationErrors = concatMap
    ( \r -> case schemeOf r.scheme of
        Just s | r.explicitActor `elem` s.eligibleActors && selected r.explicitActor -> []
        Just _ -> [ IneligibleRepresentative r.explicitActor ]
        Nothing -> [ UnknownScheme r.scheme ]
    )
    config.representations
  schemeOf id = case filter (\s -> s.id == id) gb.schemes of
    [ s ] -> Just s
    _ -> Nothing
  -- a configured Representation of a scheme the possible World defines; an unknown scheme is an error
  -- (`representationErrors`), so the instantiation never returns it
  toRepresentation r = (\s -> { id: Id s.id, explicitActor: r.explicitActor, aggregate: s.target, domain: s.domain, observes: s.observes, weight: r.weight, estimator: s.estimator }) <$> schemeOf r.scheme
  setController e = case Map.lookup e.id config.seats, Map.lookup e.id config.nonPlayableActors of
    Just seat, _ -> e { kind = PlayableActor (Id seat) }
    _, Just p -> e { kind = NonPlayableActor p }
    _, _ -> e

-- | The configuration with the Actor not explicit: its seat, non-playable Actor binding and Representations removed.
withoutExplicitActor :: ActorId -> InstantiationConfig -> InstantiationConfig
withoutExplicitActor e config = config { seats = Map.delete e config.seats, nonPlayableActors = Map.delete e config.nonPlayableActors, representations = filter (\r -> r.explicitActor /= e) config.representations }

-- | Resolve one unselected Actor by its fallback.
applyFallback :: UnseededWorld -> Tuple ActorId Fallback -> UnseededWorld
applyFallback w (Tuple e fb) = case fb of
  InstitutionalStrategy p -> w { actors = Map.update (\x -> Just x { kind = NonPlayableActor p }) e w.actors }
  PermittedOmission -> w
  ImplicitCoverage agg -> mergeInto agg e w

-- | Implicit coverage: the Actor's accounts, procedure weight, exchange access,
-- | circulation, and jurisdiction pass to the Aggregate that covers it.
mergeInto :: ActorId -> ActorId -> UnseededWorld -> UnseededWorld
mergeInto agg e w = case Map.lookup e w.actors of
  Nothing -> w
  Just gone ->
    let
      moved = un Identity (traverseWorld retargeting (w { actors = Map.update (Just <<< absorb gone) agg (Map.delete e w.actors), groups = Map.delete e w.groups }))
    in
      moved
        { exchanges = map (\m -> m { access = nub m.access }) moved.exchanges
        , recurring = normalize (mergeRequirements (dedupProduction (filter notSelfFlow moved.recurring)))
        }
  where
  retarget x = if x == e then agg else x
  -- every reference to the Actor is to the Aggregate now: rules, payments, holders, and access lists alike
  retargeting = sameWorld
    { actor = Identity <<< retarget
    , members = \self -> Identity <<< members self
    , weights = Identity <<< moveWeight
    }
  notSelfFlow r = case r.source, r.destination of
    FromActor a, ToActor b -> a /= b
    _, _ -> true
  -- production that became identical once retargeted stays one rule, as a merged Actor holds one amount of it
  dedupProduction = nubByEq sameProduction
  sameProduction a b = a.kind == kinds.production && b.kind == kinds.production && a.source == b.source && a.destination == b.destination && a.resource == b.resource && a.amount == b.amount
  absorb gone a = a
    { accounts = sumMaps a.accounts gone.accounts
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

-- | Requirements of one Actor and Resource, merged as docs/04 §4.2 says: their explicit amounts
-- | are summed. Two requirements that are both "equal to purchases" stay one. An explicit amount
-- | and a purchases-based one cannot be summed into one amount, so they stay two requirements:
-- | each consumes from what the Actor holds and records its own shortfall, which totals what the
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
