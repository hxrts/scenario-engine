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
  , RefKind(..)
  , ValidationError(..)
  , Difference(..)
  , decodeGameboard
  , gameIdOf
  , decodeConfig
  , validateGameboard
  , instantiate
  , reconcile
  ) where

import Prelude

import Board.Decode (decodeWorld, toMap)
import Board.Tags (capacityKind, cumulativeKind, coverageKind, everyRoundKind, finalKind, holdingKind, omissionKind, payeeReceiptsKind, policyKind, shortfallKind, shortfallRoundsKind, splitTag, volumeKind)
import Core.Draw (Seed(..), drawVersion)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (lookupQ)
import Core.Types (BehaviorDomain, Capability(..), CapacityRef, Controller(..), EntityId, EntityKind(..), Estimator, Id(..), PolicyId, ResourceId, Rule(..), TransformationId)
import Core.World (Manifest, ShareSource(SalesOf), WorldState)
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Array (all, any, concatMap, elem, filter, foldl, fromFoldable, group, mapMaybe, nub, null, sort)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
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
  , policies :: Object Json -- the Policy catalog (declarative programs), decoded and checked by the scenario loader
  , parameters :: Object Q -- named scenario parameters read by Policies
  , horizon :: Int -- years
  , rounds :: { min :: Int, max :: Int } -- the supported round counts (docs/03_gameboard.md §4)
  , label :: Maybe String -- the test-instance label, which names the game (docs/05_test_instance.md §14)
  }

-- | The game id a run records: the Gameboard's id and its test-instance label, `<id>/<label>`.
-- | Nothing when the Gameboard names no label.
gameIdOf :: Gameboard -> Maybe String
gameIdOf gb = (\l -> gb.id <> "/" <> l) <$> gb.label

-- | What an Actor definition adds to its Entity (§6.2).
type ActorDefinition =
  { required :: Boolean
  , seat :: Boolean -- may be a seat
  , npc :: Array PolicyId -- NPC Policies it may take
  , fallback :: Maybe Fallback
  , objective :: Maybe Objective
  , description :: String -- what the Actor does; prose for briefings, not an authority
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

-- | The kind of thing a reference names, for `UnknownReference`.
data RefKind
  = ResourceRef
  | MarketRef
  | TransformationRef
  | EntityRef
  | CapacityName
  | SchemeRef
  | PolicyRef
  | ParameterRef

derive instance Eq RefKind

instance Show RefKind where
  show = case _ of
    ResourceRef -> "resource"
    MarketRef -> "market"
    TransformationRef -> "transformation"
    EntityRef -> "entity"
    CapacityName -> "capacity"
    SchemeRef -> "scheme"
    PolicyRef -> "policy"
    ParameterRef -> "parameter"

-- | Every way a Gameboard, its configuration, or an instantiation can be wrong. Each
-- | constructor names one check, so a caller and its tests match on the specific failure.
data ValidationError
  = UnknownReference RefKind String
  | DuplicateCapacityOwner String
  | ProductionWithoutCapacity EntityId String -- a producer does not hold the capacity it produces from
  | FacilityWithoutPayee TransformationId
  | ReservesNonReservable TransformationId ResourceId -- a facility reserves a Resource that is not reservable
  | MissingFallback EntityId
  | MissingObjective EntityId
  | RequiredNotSelected EntityId
  | ControllerNotAllowed EntityId
  | ProcedureExceedsAuthority EntityId
  | IneligibleRepresentative EntityId
  | ResolutionInconsistent EntityId (Array Difference) -- implicit coverage would change the economy
  | CoverageNotAggregate EntityId EntityId -- an Actor's coverage fallback names a non-Aggregate
  | SchemeTargetNotAggregate String EntityId -- a Representation scheme's target is not an Aggregate
  | RoundsOutOfRange { rounds :: Int, min :: Int, max :: Int }

derive instance Eq ValidationError

instance Show ValidationError where
  show = case _ of
    UnknownReference k r -> "unknown " <> show k <> " reference: " <> r
    DuplicateCapacityOwner c -> "capacity has more than one owner: " <> c
    ProductionWithoutCapacity e c -> show e <> " produces from " <> c <> " but does not hold it"
    FacilityWithoutPayee t -> "facility has a cost but no payee: " <> show t
    ReservesNonReservable t r -> "facility " <> show t <> " reserves " <> show r <> ", which is not reservable"
    MissingFallback e -> "Actor has no fallback: " <> show e
    MissingObjective e -> "Actor has no objective: " <> show e
    RequiredNotSelected e -> "required Actor not selected: " <> show e
    ControllerNotAllowed e -> "controller not allowed for " <> show e
    ProcedureExceedsAuthority e -> "procedure authorizes more than its Aggregate holds: " <> show e
    IneligibleRepresentative e -> "Actor not eligible for its Representation: " <> show e
    ResolutionInconsistent e ds -> "making " <> show e <> " implicit changes the economy: " <> show ds
    CoverageNotAggregate a agg -> "coverage fallback of " <> show a <> " names " <> show agg <> ", which is not an Aggregate"
    SchemeTargetNotAggregate s t -> "scheme " <> s <> " targets " <> show t <> ", which is not an Aggregate"
    RoundsOutOfRange r -> "configured " <> show r.rounds <> " rounds is outside the supported range " <> show r.min <> " to " <> show r.max

-- | A difference between two instantiations of the same economy (§14.2).
data Difference
  = StockDiffers String Q Q
  | CapacityDiffers String Q Q
  | WeightDiffers String Q Q
  | AuthorityDiffers

derive instance Eq Difference

instance Show Difference where
  show = case _ of
    StockDiffers k l r -> "stock " <> k <> ": " <> show l <> " vs " <> show r
    CapacityDiffers k l r -> "capacity " <> k <> ": " <> show l <> " vs " <> show r
    WeightDiffers k l r -> "procedure weight " <> k <> ": " <> show l <> " vs " <> show r
    AuthorityDiffers -> "institutional authority differs"

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
  horizon <- o .: "horizon"
  rounds <- o .: "roundRange" >>= roundRange
  label <- o .:? "label"
  pure
    { id
    , version
    , world
    , actors: Map.mapMaybe identity (toMap defs)
    , schemes
    , policies
    , parameters
    , horizon
    , rounds
    , label
    }
  where
  definition _ j' = do
    d <- decodeJson j'
    required <- d .:? "required" .!= false
    seat <- d .:? "seat" .!= true
    npc <- d .:? "npc" .!= []
    fallback <- d .:? "fallback" >>= traverse decodeFallback
    objective <- d .:? "objective" >>= traverse decodeObjective
    description <- d .:? "description" .!= ""
    dependency <- d .:? "dependency" .!= ""
    pure { required, seat, npc: map Id npc, fallback, objective, description, dependency }
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

-- | `[min, max]`: the supported round counts, with 1 <= min <= max.
roundRange :: Json -> Either JsonDecodeError { min :: Int, max :: Int }
roundRange j = do
  xs <- decodeJson j
  case xs of
    [ a, b ] | a >= 1 && a <= b -> Right { min: a, max: b }
    _ -> Left (Named "roundRange" (UnexpectedValue j))

-- | A fallback tag: `coverage:<aggregate>`, `policy:<policy>`, or `omission`.
decodeFallback :: Json -> Either JsonDecodeError Fallback
decodeFallback j = do
  s <- decodeJson j
  case splitTag s of
    { kind, arg: Just a } | kind == coverageKind -> Right (ImplicitCoverage (Id a))
    { kind, arg: Just a } | kind == policyKind -> Right (InstitutionalPolicy (Id a))
    { kind, arg: Nothing } | kind == omissionKind -> Right PermittedOmission
    _ -> Left (Named "fallback" (UnexpectedValue j))

decodeObjective :: Json -> Either JsonDecodeError Objective
decodeObjective j = do
  o <- decodeJson j
  description <- o .: "description"
  criteria <- o .: "criteria" >>= traverse \c -> do
    x <- decodeJson c
    metric <- x .: "metric" >>= metricRef
    over <- x .:? "over" .!= finalKind >>= span
    atLeast <- x .: "atLeast" >>= \b -> case decodeJson b of
      Right q -> Right (Fixed q)
      Left _ -> Of <$> metricRef b
    pure { metric, over, atLeast }
  pure { description, criteria }
  where
  span s
    | s == finalKind = Right Final
    | s == everyRoundKind = Right EveryRound
    | s == cumulativeKind = Right Cumulative
    | otherwise = Left (Named "span" (UnexpectedValue (fromString s)))
  metricRef j' = do
    m <- decodeJson j'
    entities <- m .: "entities"
    negated <- m .:? "negated" .!= false
    s <- m .: "measure"
    measure <- case splitTag s of
      { kind, arg: Just r } | kind == holdingKind -> Right (HoldingOf (Id r))
      { kind, arg: Just c } | kind == capacityKind -> Right (CapacityAmount (Id c))
      { kind, arg: Just r } | kind == shortfallKind -> Right (ShortfallOf (Id r))
      { kind, arg: Just r } | kind == shortfallRoundsKind -> Right (ShortfallRounds (Id r))
      { kind, arg: Nothing } | kind == payeeReceiptsKind -> Right PayeeReceipts
      { kind, arg: Just t } | kind == volumeKind -> Right (VolumeOf (Id t))
      _ -> Left (Named "measure" (UnexpectedValue (fromString s)))
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

-- Resolution consistency (§4.3, INV-14) -----------------------------------------------

-- | Two instantiations differing only in explicitness must describe the same initial
-- | economy: stocks, capacities, institutional authority, and procedure weights.
reconcile :: InitialWorld -> InitialWorld -> Either (Array Difference) Unit
reconcile a b =
  let
    diffs = compare' StockDiffers (stocks a.world) (stocks b.world)
      <> compare' CapacityDiffers (capacities a.world) (capacities b.world)
      <> compare' WeightDiffers (weights a.world) (weights b.world)
      <> (if authority a.world == authority b.world then [] else [ AuthorityDiffers ])
  in
    if null diffs then Right unit else Left diffs
  where
  compare' :: forall k. Ord k => Show k => (String -> Q -> Q -> Difference) -> Map k Q -> Map k Q -> Array Difference
  compare' mk x y = concatMap (diff mk x y) (nub (fromFoldable (Map.keys x) <> fromFoldable (Map.keys y)))

  diff :: forall k. Ord k => Show k => (String -> Q -> Q -> Difference) -> Map k Q -> Map k Q -> k -> Array Difference
  diff mk x y k =
    let
      l = lookupQ k x
      r = lookupQ k y
    in
      if l == r then [] else [ mk (show k) l r ]
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
