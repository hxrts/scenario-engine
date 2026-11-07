-- | The Gameboard and Instantiation Configuration schema (docs/03_gameboard.md,
-- | docs/04_instantiated_game.md): the types, their JSON decoders, and the error and
-- | difference types that the other Gameboard modules share. Validate, Instantiate, and
-- | Reconcile all import this module; it imports none of them.
module Board.Gameboard.Schema
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
  , gameIdOf
  , decodeGameboard
  , decodeConfig
  ) where

import Prelude

import Board.Decode.Common (decodeEstimator, toMap)
import Board.Decode.World (decodeWorld)
import Board.Tags (argOf, capacityKind, cumulativeKind, coverageKind, everyRoundKind, finalKind, holdingKind, isBare, omissionKind, payeeReceiptsKind, policyKind, shortfallKind, shortfallRoundsKind, volumeKind)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (BehaviorDomain, CapacityRef, CollectiveProcedureId, EntityId, Estimator, Id(..), PolicyId, ResourceId, TransformationId)
import Core.World (Manifest, WorldState)
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign.Object (Object)
import Foreign.Object as Object

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
  = StockDiffers ResourceId Q Q
  | CapacityDiffers CapacityRef Q Q
  | WeightDiffers CollectiveProcedureId Q Q
  | AuthorityDiffers

derive instance Eq Difference

instance Show Difference where
  show = case _ of
    StockDiffers k l r -> "stock " <> show k <> ": " <> show l <> " vs " <> show r
    CapacityDiffers k l r -> "capacity " <> show k <> ": " <> show l <> " vs " <> show r
    WeightDiffers k l r -> "procedure weight " <> show k <> ": " <> show l <> " vs " <> show r
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
    estimator <- s .: "estimator" >>= decodeEstimator
    pure
      { id
      , eligibleActors: map Id eligible
      , target: Id target
      , domain: Id domain
      , estimator
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
  if isBare omissionKind s then Right PermittedOmission
  else case argOf coverageKind s, argOf policyKind s of
    Just a, _ -> Right (ImplicitCoverage (Id a))
    _, Just a -> Right (InstitutionalPolicy (Id a))
    _, _ -> Left (Named "fallback" (UnexpectedValue j))

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
    measure <- measureOf s
    pure { entities: map Id entities, measure, negated }
  measureOf s
    | Just r <- argOf holdingKind s = Right (HoldingOf (Id r))
    | Just c <- argOf capacityKind s = Right (CapacityAmount (Id c))
    | Just r <- argOf shortfallKind s = Right (ShortfallOf (Id r))
    | Just r <- argOf shortfallRoundsKind s = Right (ShortfallRounds (Id r))
    | isBare payeeReceiptsKind s = Right PayeeReceipts
    | Just t <- argOf volumeKind s = Right (VolumeOf (Id t))
    | otherwise = Left (Named "measure" (UnexpectedValue (fromString s)))

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
