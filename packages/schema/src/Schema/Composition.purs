-- | The premise's schema types (docs/03_premise.md, docs/04_run.md): the composition,
-- | its Actors, objectives and criteria, and the Instantiation Configuration. These are the types
-- | of the premise format; their decoders are in `Schema.Decode.composition`, and their realization
-- | (instantiate, validate, reconcile) is in `board`.
module Schema.Composition
  ( PremiseComposition
  , PremiseRef
  , premiseRefShape
  , premiseRefText
  , premisesText
  , ActorDefinition
  , Fallback(..)
  , Outcome(..)
  , Objective
  , Criterion
  , MetricRef
  , Measure(..)
  , Span(..)
  , Bound(..)
  , Scheme
  , InstantiationConfig
  , InitialWorld
  , gameIdOf
  , compositionRef
  , capacityCategory
  ) where

import Prelude

import Schema.Recurring (Authored)

import Core.Draw (Seed)
import Core.Num (Q)
import Core.Shape (Shape, recordShape, stringShape)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (BehaviorDomain, ActorId, Estimator, StrategyId, ResourceId, TransformationId)
import Laws.World (Manifest, UnseededWorld, WorldState)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.String (joinWith)
import Foreign.Object (Object)
import Data.Maybe (Maybe)

-- | A Premise a composition is made from: its id and version (docs/03 §12a). A Run records them.
type PremiseRef = { id :: String, version :: String }

premiseRefShape :: Shape PremiseRef
premiseRefShape = recordShape "PremiseRef" { id: stringShape, version: stringShape }

-- | A Premise's reference, `id@version`.
premiseRefText :: PremiseRef -> String
premiseRefText r = r.id <> "@" <> r.version

-- | The Premises composed, each `id@version`, comma separated, as a Run's spec records them.
premisesText :: PremiseComposition -> String
premisesText = joinWith "," <<< map premiseRefText <<< _.premises

-- | A composition's reference, `id@version`, as a manifest and the consoles print it.
compositionRef :: forall r. { id :: String, version :: String | r } -> String
compositionRef g = g.id <> "@" <> g.version

-- | What a PremiseComposition compiles to: its world (unseeded), its Actors, Schemes, parameters, horizon,
-- | supported round counts, optional test-instance label, its Strategy catalog, and the Premises it was
-- | composed from, in order.
type PremiseComposition =
  { id :: String
  , version :: String
  , premises :: Array PremiseRef -- the Premises composed, in order (docs/03 §12a)
  , authored :: Authored -- what the premise authors that the World does not hold
  , world :: UnseededWorld -- resources, Exchanges, transformations, circulation, rules, and every Actor; seeded by instantiation
  , explicitActors :: Map ActorId ActorDefinition
  , schemes :: Array Scheme
  , parameters :: Object Q -- named premise parameters read by Strategies
  , horizon :: Int -- years
  , rounds :: { min :: Int, max :: Int } -- the supported round counts (docs/03_premise.md §4)
  , label :: Maybe String -- the test-instance label, which names the game (docs/05_meridian.md §14)
  , strategies :: Map StrategyId StrategyRules
  }

-- | The game id a run records: the composition's id and its test-instance label, `<id>/<label>`.
-- | Nothing when the composition names no label.
gameIdOf :: PremiseComposition -> Maybe String
gameIdOf gb = (\l -> gb.id <> "/" <> l) <$> gb.label

-- | What an Actor definition adds to its Actor (§6.2).
type ActorDefinition =
  { required :: Boolean
  , seat :: Boolean -- may be a seat
  , nonPlayableActor :: Array StrategyId -- non-playable Actor Strategies it may take
  , fallback :: Maybe Fallback
  , objective :: Maybe Objective
  , description :: String -- what the Actor does; prose for briefings, not an authority
  , dependency :: String
  }

data Fallback
  = ImplicitCoverage ActorId -- accounts and activity retained in an Aggregate
  | InstitutionalStrategy StrategyId -- the same accounts, driven by an in-world Strategy
  | PermittedOmission

derive instance Generic Fallback _
derive instance Eq Fallback

-- | Measurable criteria over the recorded game (§10.5). Objectives never affect the economy.
type Objective = { description :: String, criteria :: Array Criterion }

type Criterion = { metric :: MetricRef, over :: Span, atLeast :: Bound }

-- | A measurement summed over the listed Actors; `negated` expresses "at most".
type MetricRef = { actors :: Array ActorId, measure :: Measure, negated :: Boolean }

-- | The category a premise declares for a Resource used as capacity (docs/03): views leave it out of holdings.
capacityCategory :: String
capacityCategory = "capacity"

data Measure
  = HoldingOf ResourceId
  | ShortfallOf ResourceId
  | ShortfallRounds ResourceId
  | PayeeReceipts
  | VolumeOf TransformationId

derive instance Generic Measure _
derive instance Eq Measure

data Span = Final | EveryRound | Cumulative

derive instance Generic Span _
derive instance Eq Span

data Bound = Fixed Q | Of MetricRef

derive instance Generic Bound _
derive instance Eq Bound

type Scheme =
  { id :: String
  , eligibleActors :: Array ActorId
  , target :: ActorId
  , domain :: BehaviorDomain
  , observes :: ResourceId
  , estimator :: Estimator
  }

type InstantiationConfig =
  { seed :: Seed
  , rounds :: Int
  , seats :: Map ActorId String -- Actor -> seat
  , nonPlayableActors :: Map ActorId StrategyId -- Actor -> in-world Strategy
  , fallbacks :: Map ActorId Fallback -- overrides of the declared fallback
  , representations :: Array { scheme :: String, explicitActor :: ActorId, weight :: Q }
  }

type InitialWorld = { world :: WorldState, manifest :: Manifest }

-- | The outcome of an objective or a criterion. A run with no rounds, or a criterion with no series,
-- | is not evaluated: it is neither a pass nor a failure. The premise spells the outcomes "met",
-- | "notMet" and "notEvaluated" (the capstone's expectations, `Premises.Meridian.Capstone`).
data Outcome = Met | NotMet | NotEvaluated

derive instance Eq Outcome

-- | The words the views show.
instance Show Outcome where
  show = case _ of
    Met -> "met"
    NotMet -> "not met"
    NotEvaluated -> "not evaluated"
