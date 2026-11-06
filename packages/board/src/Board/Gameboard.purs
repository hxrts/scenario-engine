-- | The Gameboard and Instantiation Configuration (docs/03_gameboard.md,
-- | docs/04_instantiated_game.md): decoding, validation, instantiation, reconciliation.
module Board.Gameboard
  ( Gameboard
  , ActorDefinition
  , Fallback(..)
  , Objective
  , Criterion
  , MetricRef
  , Measure(..)
  , Span(..)
  , Bound(..)
  , Scheme
  , InstantiationConfig
  , InitialWorld
  , ValidationError(..)
  , decodeGameboard
  , decodeConfig
  , validateGameboard
  , instantiate
  , reconcile
  ) where

import Prelude

import Board.Decode (decodeWorld, toMap)
import Core.Draw (Seed(..), drawVersion)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (BehaviorDomain, Capability(..), Controller(..), EntityId, EntityKind(..), Estimator, Id(..), PolicyId, ResourceId, CapacityRef, TransformationId)
import Core.World (Manifest, WorldState)
import Core.Query (lookupQ)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Array (all, any, concatMap, elem, filter, foldl, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- Types --------------------------------------------------------------------------

type Gameboard =
  { id :: String
  , version :: String
  , world :: WorldState -- resources, Markets, facilities, circulation, rules, and every Entity
  , actors :: Map EntityId ActorDefinition
  , schemes :: Array Scheme
  , policies :: Object Json -- the Policy catalog (declarative programs)
  , parameters :: Object Q -- named scenario parameters read by Policies
  , horizon :: Int
  , rounds :: { min :: Int, max :: Int }
  }

-- | What an Actor definition adds to its Entity (§6.2).
type ActorDefinition =
  { required :: Boolean
  , seat :: Boolean -- may be a seat
  , npc :: Array PolicyId -- NPC Policies it may take
  , fallback :: Maybe Fallback
  , objective :: Maybe Objective
  , capability :: String
  , dependency :: String
  }

data Fallback
  = ImplicitCoverage EntityId -- accounts and activity retained in an Aggregate
  | InstitutionalPolicy PolicyId -- the same accounts, driven by an in-world Policy
  | PermittedOmission

derive instance Eq Fallback

-- | Measurable criteria over the recorded game (§10.5). Objectives never affect the economy.
type Objective = { description :: String, criteria :: Array Criterion }

type Criterion = { metric :: MetricRef, over :: Span, atLeast :: Bound }

-- | A measurement summed over the listed Entities; `negated` expresses "at most".
type MetricRef = { entities :: Array EntityId, measure :: Measure, negated :: Boolean }

data Measure
  = HoldingOf ResourceId
  | CapacityAmount CapacityRef
  | ShortfallOf ResourceId
  | ShortfallRounds ResourceId
  | PayeeReceipts
  | VolumeOf TransformationId

data Span = Final | EveryRound | Cumulative

data Bound = Fixed Q | Of MetricRef

type Scheme =
  { id :: String
  , eligibleActors :: Array EntityId
  , target :: EntityId
  , domain :: BehaviorDomain
  , estimator :: Estimator
  }

type InstantiationConfig =
  { seed :: Seed
  , rounds :: Int
  , seats :: Map EntityId String -- Actor -> seat
  , npcs :: Map EntityId PolicyId -- Actor -> in-world Policy
  , fallbacks :: Map EntityId Fallback -- overrides of the declared fallback
  , representations :: Array { scheme :: String, actor :: EntityId, weight :: Q }
  }

type InitialWorld = { world :: WorldState, manifest :: Manifest }

data ValidationError
  = UnknownReference String
  | DuplicateCapacityOwner CapacityRef
  | FacilityWithoutPayee TransformationId
  | MissingFallback EntityId
  | MissingObjective EntityId
  | RequiredNotSelected EntityId
  | ControllerNotAllowed EntityId
  | ProcedureExceedsAuthority EntityId
  | IneligibleRepresentative EntityId
  | ResolutionInconsistent EntityId (Array String)

derive instance Eq ValidationError
instance Show ValidationError where
  show = case _ of
    UnknownReference r -> "unknown reference: " <> r
    DuplicateCapacityOwner c -> "capacity has more than one owner: " <> show c
    FacilityWithoutPayee t -> "facility has a cost but no payee: " <> show t
    MissingFallback e -> "Actor has no fallback: " <> show e
    MissingObjective e -> "Actor has no objective: " <> show e
    RequiredNotSelected e -> "required Actor not selected: " <> show e
    ControllerNotAllowed e -> "controller not allowed for " <> show e
    ProcedureExceedsAuthority e -> "procedure authorizes more than its Aggregate holds: " <> show e
    IneligibleRepresentative e -> "Actor not eligible for its Representation: " <> show e
    ResolutionInconsistent e ds -> "making " <> show e <> " implicit changes the economy: " <> show ds

-- Decoding -----------------------------------------------------------------------

decodeGameboard :: Json -> Either JsonDecodeError Gameboard
decodeGameboard j = do
  o <- decodeJson j
  id <- o .: "id"
  version <- o .: "version"
  { world } <- decodeWorld j
  defs <- o .: "entities" >>= traverseWithIndex \k v -> do
    x <- decodeJson v
    maybeDef <- x .:? "definition"
    traverse (definition k) maybeDef
  schemes <- o .:? "schemes" .!= [] >>= traverse scheme
  policies <- o .:? "policies" .!= Object.empty
  parameters <- o .:? "parameters" .!= Object.empty
  horizon <- o .:? "horizon" .!= 5
  rs <- o .:? "roundRange" .!= [ 4, 8 ]
  pure
    { id
    , version
    , world
    , actors: Map.mapMaybe identity (toMap defs)
    , schemes
    , policies
    , parameters
    , horizon
    , rounds: case rs of
        [ a, b ] -> { min: a, max: b }
        _ -> { min: 1, max: horizon }
    }
  where
  definition _ j' = do
    d <- decodeJson j'
    required <- d .:? "required" .!= false
    seat <- d .:? "seat" .!= true
    npc <- d .:? "npc" .!= []
    fallback <- d .:? "fallback" >>= traverse decodeFallback
    objective <- d .:? "objective" >>= traverse decodeObjective
    capability' <- d .:? "capability" .!= ""
    dependency <- d .:? "dependency" .!= ""
    pure { required, seat, npc: map Id npc, fallback, objective, capability: capability', dependency }
  scheme j' = do
    s <- decodeJson j'
    id <- s .: "id"
    eligible <- s .: "eligibleActors"
    target <- s .: "target"
    domain <- s .: "domain"
    e <- s .: "estimator"
    reference <- e .: "reference"
    shrinkage <- e .: "shrinkage"
    bound <- e .:? "bound"
    noiseSpread <- e .:? "noiseSpread" .!= Q.zero
    pure
      { id
      , eligibleActors: map Id eligible
      , target: Id target
      , domain: Id domain
      , estimator: { reference, shrinkage, deviationBound: map (\b -> { lo: Q.neg b, hi: b }) bound, noiseSpread }
      }

decodeFallback :: Json -> Either JsonDecodeError Fallback
decodeFallback j = do
  s <- decodeJson j
  case split (Pattern ":") s of
    [ "coverage", agg ] -> Right (ImplicitCoverage (Id agg))
    [ "policy", p ] -> Right (InstitutionalPolicy (Id p))
    [ "omission" ] -> Right PermittedOmission
    _ -> Left (TypeMismatch ("fallback " <> s))

decodeObjective :: Json -> Either JsonDecodeError Objective
decodeObjective j = do
  o <- decodeJson j
  description <- o .: "description"
  criteria <- o .: "criteria" >>= traverse \c -> do
    x <- decodeJson c
    metric <- x .: "metric" >>= metricRef
    over <- x .:? "over" .!= "final" >>= case _ of
      "final" -> Right Final
      "everyRound" -> Right EveryRound
      "cumulative" -> Right Cumulative
      s -> Left (TypeMismatch ("span " <> s))
    atLeast <- x .: "atLeast" >>= \b -> case decodeJson b of
      Right q -> Right (Fixed q)
      Left _ -> Of <$> metricRef b
    pure { metric, over, atLeast }
  pure { description, criteria }
  where
  metricRef j' = do
    m <- decodeJson j'
    entities <- m .: "entities"
    negated <- m .:? "negated" .!= false
    s <- m .: "measure"
    measure <- case split (Pattern ":") s of
      [ "holding", r ] -> Right (HoldingOf (Id r))
      [ "capacity", c ] -> Right (CapacityAmount (Id c))
      [ "shortfall", r ] -> Right (ShortfallOf (Id r))
      [ "shortfallRounds", r ] -> Right (ShortfallRounds (Id r))
      [ "payeeReceipts" ] -> Right PayeeReceipts
      [ "volume", t ] -> Right (VolumeOf (Id t))
      _ -> Left (TypeMismatch ("measure " <> s))
    pure { entities: map Id entities, measure, negated }

decodeConfig :: Json -> Either JsonDecodeError InstantiationConfig
decodeConfig j = do
  o <- decodeJson j
  seed <- o .: "seed"
  rounds <- o .: "rounds"
  seats <- o .:? "seats" .!= Object.empty
  npcs <- o .:? "npcs" .!= Object.empty
  fallbacks <- o .:? "fallbacks" .!= Object.empty >>= traverse decodeFallback
  reps <- o .:? "representations" .!= [] >>= traverse \r -> do
    x <- decodeJson r
    s <- x .: "scheme"
    actor <- x .: "actor"
    weight <- x .:? "weight" .!= Q.fromInt 1
    pure { scheme: s, actor: Id actor, weight }
  pure { seed: Seed seed, rounds, seats: toMap seats, npcs: map Id (toMap npcs), fallbacks: toMap fallbacks, representations: reps }

-- Validation (§12) -----------------------------------------------------------------

-- | Structural, economic, Representation, resolution-consistency, and feasibility checks.
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
validateGameboard gb config =
  let
    structural = references <> owners <> payees <> fallbacks <> objectives <> procedures
    feasibility = either identity (const []) (instantiate gb config)
    consistency = if null (structural <> feasibility) then concatMap consistent (fromFoldable (Map.keys gb.actors)) else []
    errors = structural <> feasibility <> consistency
  in
    if null errors then Right gb else Left errors
  where
  w = gb.world
  ids = Map.keys w.entities
  known e = e `elem` fromFoldable ids
  unknown what = UnknownReference what
  references =
    map (unknown <<< show) (filter (not <<< known) (concatMap (\m -> m.access) (fromFoldable (Map.values w.markets))))
      <> map (unknown <<< show) (filter (not <<< known) (mapMaybe _.payee (fromFoldable (Map.values w.transformations))))
      <> map (unknown <<< show) (filter (not <<< known) (concatMap (\f -> [ f.from, f.to ]) w.circulation.flows))
      <> map (unknown <<< show) (filter (not <<< known) (map _.target gb.schemes))
      <> map (unknown <<< show) (filter (\p -> not (Object.member (unwrap p) gb.policies)) (aggregatePolicies <> concatMap _.npc (fromFoldable (Map.values gb.actors))))
  aggregatePolicies = concatMap
    ( \e -> case e.kind of
        Aggregate a -> [ a.policy ]
        _ -> []
    )
    (fromFoldable (Map.values w.entities))
  owners = map (\g -> DuplicateCapacityOwner (NEA.head g)) (filter (\g -> NEA.length g > 1) (group (sort (concatMap (\e -> fromFoldable (Map.keys e.capacities)) (fromFoldable (Map.values w.entities)) # filter (\c -> not (sharedProduction c))))))
  -- production capacity is held by each producer under one name; every other capacity has one owner
  sharedProduction c = any (\p -> p.capacity == c) w.circulation.production
  payees = map (\t -> FacilityWithoutPayee t.id) (filter (\t -> isJust t.cost && isNothing t.payee) (fromFoldable (Map.values w.transformations)))
  fallbacks = concatMap (\(Tuple e d) -> if not d.required && isNothing d.fallback then [ MissingFallback e ] else []) (Map.toUnfoldable gb.actors :: Array _)
  objectives = concatMap (\(Tuple e d) -> if isNothing d.objective then [ MissingObjective e ] else []) (Map.toUnfoldable gb.actors :: Array _)
  procedures = concatMap
    ( \e -> case e.kind of
        Aggregate a | any (\p -> not (all (_ `elem` e.capabilities) p.authorizes)) (fromFoldable (Map.values a.procedures)) -> [ ProcedureExceedsAuthority e.id ]
        _ -> []
    )
    (fromFoldable (Map.values w.entities))
  consistent e = case Map.lookup e gb.actors of
    Just { fallback: Just (ImplicitCoverage _) } ->
      let
        without = config { seats = Map.delete e config.seats, npcs = Map.delete e config.npcs, representations = filter (\r -> r.actor /= e) config.representations }
      in
        case instantiate gb config, instantiate gb without of
          Right a, Right b -> either (\ds -> [ ResolutionInconsistent e ds ]) (const []) (reconcile a b)
          _, _ -> []
    _ -> []

-- Instantiation (docs/04_instantiated_game.md §10) ------------------------------------

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
        Nothing -> [ UnknownReference r.scheme ]
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

-- Resolution consistency (§4.3, INV-14) -----------------------------------------------

-- | Two instantiations differing only in explicitness must describe the same initial
-- | economy: stocks, capacities, institutional authority, and procedure weights.
reconcile :: InitialWorld -> InitialWorld -> Either (Array String) Unit
reconcile a b =
  let
    diffs = compare' "stock" (stocks a.world) (stocks b.world)
      <> compare' "capacity" (capacities a.world) (capacities b.world)
      <> compare' "procedure weight" (weights a.world) (weights b.world)
      <> (if authority a.world == authority b.world then [] else [ "institutional authority differs" ])
  in
    if null diffs then Right unit else Left diffs
  where
  compare' :: forall k. Ord k => Show k => String -> Map k Q -> Map k Q -> Array String
  compare' what x y = concatMap (diff what x y) (nub (fromFoldable (Map.keys x) <> fromFoldable (Map.keys y)))

  diff :: forall k. Ord k => Show k => String -> Map k Q -> Map k Q -> k -> Array String
  diff what x y k =
    let
      l = lookupQ k x
      r = lookupQ k y
    in
      if l == r then [] else [ what <> " " <> show k <> ": " <> show l <> " vs " <> show r ]
  stocks w = sumMaps (map _.accounts (fromFoldable (Map.values w.entities)) <> map _.reserves (fromFoldable (Map.values w.markets)))
  capacities w = sumMaps (map _.capacities (fromFoldable (Map.values w.entities)))

  -- Data.Map here has no unionsWith
  sumMaps :: forall k. Ord k => Array (Map k Q) -> Map k Q
  sumMaps = foldl (Map.unionWith Q.add) Map.empty
  weights w = sumMaps
    ( concatMap
        ( \e -> case e.kind of
            Aggregate x -> map (\(Tuple pid p) -> Map.singleton pid (Q.add p.residualWeight (Q.sumQ (fromFoldable (Map.values p.weights))))) (Map.toUnfoldable x.procedures)
            _ -> []
        )
        (fromFoldable (Map.values w.entities))
    )
  authority w = sort (concatMap (\e -> map (\c -> show e.id <> " " <> show c) (filter isEnact e.capabilities)) (fromFoldable (Map.values w.entities)))
  isEnact = case _ of
    MayEnact _ -> true
    _ -> false
