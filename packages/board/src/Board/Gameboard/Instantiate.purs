-- | Instantiation (docs/04_instantiated_game.md §10): a Gameboard and its configuration become
-- | the initial World State, with controllers, fallbacks, account partitioning, procedure
-- | weights, and Representations resolved. `checkHomes` is the startup check that follows it.
module Board.Gameboard.Instantiate
  ( instantiate
  , checkHomes
  ) where

import Prelude

import Board.Gameboard.Schema (Fallback(..), Gameboard, InitialWorld, InstantiationConfig, RefKind(..), ValidationError(..))
import Core.Draw (drawVersion)
import Core.Num as Q
import Core.Query (homeMarket, lookupQ)
import Core.Types (Controller(..), Entity, EntityId, EntityKind(..), Id(..))
import Core.World (WorldState)
import Data.Array (concatMap, elem, filter, find, foldl, fromFoldable, nub, null)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))

-- | Controllers and fallbacks, account partitioning and procedure weights, Representation
-- | assignment, then the World State and manifest. Atomic: errors or a complete world.
instantiate :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
instantiate gb config =
  let
    errors = controllerErrors <> representationErrors
    world = foldl applyFallback (gb.world { entities = map setController gb.world.entities }) (Map.toUnfoldable unselected :: Array _)
    final = world
      { seed = config.seed
      , representations = map toRepresentation config.representations
      }
  in
    if not (null errors) then Left errors
    else Right
      { world: final
      , manifest: { gameboard: gb.id <> "@" <> gb.version, engine: "sp-engine/v1", draw: drawVersion, initial: final }
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
        Nothing -> [ UnknownReference SchemeRef r.scheme ]
    )
    config.representations
  schemeOf id = case filter (\s -> s.id == id) gb.schemes of
    [ s ] -> Just s
    _ -> Nothing
  toRepresentation r = case schemeOf r.scheme of
    Just s -> { id: Id s.id, actor: r.actor, aggregate: s.target, domain: s.domain, weight: r.weight, estimator: s.estimator }
    Nothing -> { id: Id r.scheme, actor: r.actor, aggregate: r.actor, domain: Id "", weight: Q.zero, estimator: { reference: Q.fromInt 1, shrinkage: Q.zero, deviationBound: Nothing, noiseSpread: Q.zero } }
  setController e = case Map.lookup e.id config.seats, Map.lookup e.id config.npcs of
    Just seat, _ -> e { kind = Actor (Seat (Id seat)) }
    _, Just p -> e { kind = Actor (NPC p) }
    _, _ -> e

-- | Resolve one unselected Actor by its fallback.
applyFallback :: WorldState -> Tuple EntityId Fallback -> WorldState
applyFallback w (Tuple e fb) = case fb of
  InstitutionalPolicy p -> w { entities = Map.update (\x -> Just x { kind = Actor (NPC p) }) e w.entities }
  PermittedOmission -> w
  ImplicitCoverage agg -> mergeInto agg e w

-- | Implicit coverage: the Actor's accounts, capacities, procedure weight, market access,
-- | circulation, and jurisdiction pass to the Aggregate that covers it.
mergeInto :: EntityId -> EntityId -> WorldState -> WorldState
mergeInto agg e w = case Map.lookup e w.entities of
  Nothing -> w
  Just gone -> w
    { entities = Map.update (Just <<< absorb gone) agg (Map.delete e w.entities)
    , markets = map (\m -> m { access = nub (map retarget m.access) }) w.markets
    , circulation = c
        { flows = filter (\f -> f.from /= f.to) (map (\f -> f { from = retarget f.from, to = retarget f.to }) c.flows)
        , production = nub (map (\p -> p { entity = retarget p.entity }) c.production)
        , shares = filter (\s -> s.from /= s.to) (map (\s -> s { from = retarget s.from, to = retarget s.to }) c.shares)
        , requirements = foldRequirements (map (\r -> r { entity = retarget r.entity }) c.requirements)
        }
    , jurisdiction = map retarget (Map.delete e w.jurisdiction)
    }
  where
  c = w.circulation
  retarget x = if x == e then agg else x
  absorb gone a = a
    { accounts = Map.unionWith Q.add a.accounts gone.accounts
    , capacities = Map.unionWith Q.add a.capacities gone.capacities
    , kind = foldWeights a.kind
    }
  -- a member's procedure weight folds into the Aggregate's residual weight
  foldWeights = case _ of
    Aggregate x -> Aggregate x { procedures = map (\p -> p { residualWeight = Q.add p.residualWeight (lookupQ e p.weights), weights = Map.delete e p.weights }) x.procedures }
    k -> k
  -- requirements that now share an Entity and Resource are summed
  foldRequirements = foldl
    ( \acc r -> case filter (\x -> x.entity == r.entity && x.resource == r.resource) acc of
        [ prior ] -> map (\x -> if x.entity == r.entity && x.resource == r.resource then x { amount = Q.add <$> prior.amount <*> r.amount } else x) acc
        _ -> acc <> [ r ]
    )
    []

-- | Every Entity that acts (`acts`: a seat, or an Entity whose Policy runs) needs a home market,
-- | because `$home` names it. An acting Entity with none is a startup error, named here, before any round.
checkHomes :: (Entity -> Boolean) -> WorldState -> Either String Unit
checkHomes acts w = case find (\e -> acts e && isNothing (homeMarket w e.id)) (fromFoldable (Map.values w.entities)) of
  Just e -> Left ("entity " <> show e.id <> " acts but has no accessible market, so it has no home market")
  Nothing -> Right unit
