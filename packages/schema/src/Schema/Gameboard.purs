-- | The premise's schema types (docs/03_gameboard.md, docs/04_instantiated_game.md): the Gameboard,
-- | its Actors, objectives and criteria, and the Instantiation Configuration. These are the types
-- | of the premise format; their decoders are in `Schema.Decode.Gameboard`, and their realization
-- | (instantiate, validate, reconcile) is in `board`.
module Schema.Gameboard
  ( Gameboard
  , ActorDefinition
  , Fallback(..)
  , Objective
  , Criterion
  , Outcome(..)
  , MetricRef
  , Measure(..)
  , Span(..)
  , Bound(..)
  , Scheme
  , InstantiationConfig
  , InitialWorld
  , gameIdOf
  ) where

import Prelude

import Core.Draw (Seed)
import Core.Num (Q)
import Core.Strategy (Strategy)
import Core.Types (BehaviorDomain, CapacityRef, EntityId, Estimator, PolicyId, ResourceId, TransformationId)
import Core.World (Manifest, UnseededWorld, WorldState)
import Data.Map (Map)
import Foreign.Object (Object)
import Data.Maybe (Maybe)

-- | A Gameboard: its world (unseeded), its Actors, Schemes, parameters, horizon, supported round
-- | counts, optional test-instance label, and its Policy catalog (the Strategies the in-world
-- | Policies run).
type Gameboard =
  { id :: String
  , version :: String
  , world :: UnseededWorld -- resources, Markets, facilities, circulation, rules, and every Entity; seeded by instantiation
  , actors :: Map EntityId ActorDefinition
  , schemes :: Array Scheme
  , parameters :: Object Q -- named premise parameters read by Policies
  , horizon :: Int -- years
  , rounds :: { min :: Int, max :: Int } -- the supported round counts (docs/03_gameboard.md §4)
  , label :: Maybe String -- the test-instance label, which names the game (docs/05_test_instance.md §14)
  , policies :: Map PolicyId Strategy
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

-- | A criterion or objective is met, not met, or not evaluated. A run with no rounds, or a
-- | criterion with no series, is not evaluated: it is neither a pass nor a failure.
data Outcome = Met | NotMet | NotEvaluated

derive instance Eq Outcome

-- | The words the views show; the stored DSL spells the same outcomes in its own form (the capstone's expectations, Premises.Capstone).
instance Show Outcome where
  show = case _ of
    Met -> "met"
    NotMet -> "not met"
    NotEvaluated -> "not evaluated"

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
