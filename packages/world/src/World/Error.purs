-- | The errors and differences of possible World realization (docs/03_premise.md, docs/04_run.md
-- | §14.2): the failed checks of `World.possible World.Validate` and the differences between two
-- | instantiations of one economy. Realization owns these; the premise format does not.
module World.Error
  ( ValidationError(..)
  , Inconsistency(..)
  ) where

import Prelude

import Core.Num (Q)
import Laws.Types (CollectiveProcedureId, ActorId, RefKind, ResourceId, TransformationId)

-- | Every way a possible World, its configuration, or an instantiation can be wrong. Each
-- | constructor names one check, so a caller and its tests match on the specific failure.
data ValidationError
  = UnknownReference RefKind String
  | UnknownScheme String -- a Representation scheme the possible World does not define
  | DuplicateHolder String -- a Resource a holder draw names has more than one holder
  | ProductionWithoutHolding ActorId String -- a producer does not hold what it produces from
  | DrawWithoutHolder String String -- a Transformation draws a Resource from its holder and nothing holds it
  | TooManyHolderDraws String -- a Transformation with more than one holder draw
  | MissingFallback ActorId
  | MissingObjective ActorId
  | RequiredNotSelected ActorId
  | ControllerNotAllowed ActorId
  | ProcedureExceedsAuthority ActorId
  | IneligibleRepresentative ActorId
  | ResolutionInconsistent ActorId (Array Inconsistency) -- implicit coverage would change the economy
  | CoverageNotAggregate ActorId ActorId -- an Actor's coverage fallback names a non-Aggregate
  | SchemeTargetNotAggregate String ActorId -- a Representation scheme's target is not an Aggregate
  | RoundsOutOfRange { rounds :: Int, min :: Int, max :: Int }
  | NegativeDelay TransformationId

derive instance Eq ValidationError

instance Show ValidationError where
  show = case _ of
    UnknownReference k r -> "unknown " <> show k <> " reference: " <> r
    UnknownScheme s -> "unknown scheme reference: " <> s
    DuplicateHolder r -> "Resource drawn from its holder has more than one holder: " <> r
    ProductionWithoutHolding e r -> show e <> " produces from " <> r <> " but does not hold it"
    DrawWithoutHolder t r -> "transformation " <> t <> " draws " <> r <> " from its holder, but nothing holds it"
    TooManyHolderDraws t -> "transformation " <> t <> " has more than one draw from a holder"
    MissingFallback e -> "Actor has no fallback: " <> show e
    MissingObjective e -> "Actor has no objective: " <> show e
    RequiredNotSelected e -> "required explicit Actor not selected: " <> show e
    ControllerNotAllowed e -> "controller not allowed for " <> show e
    ProcedureExceedsAuthority e -> "procedure authorizes more than its Aggregate holds: " <> show e
    IneligibleRepresentative e -> "PlayableActor not eligible for its Representation: " <> show e
    ResolutionInconsistent e ds -> "making " <> show e <> " implicit changes the economy: " <> show ds
    CoverageNotAggregate a agg -> "coverage fallback of " <> show a <> " names " <> show agg <> ", which is not an Aggregate"
    SchemeTargetNotAggregate s t -> "scheme " <> s <> " targets " <> show t <> ", which is not an Aggregate"
    NegativeDelay t -> "transformation " <> show t <> " has a negative delay"
    RoundsOutOfRange r -> "configured " <> show r.rounds <> " rounds is outside the supported range " <> show r.min <> " to " <> show r.max

-- | A difference between two instantiations of the same economy (§14.2).
data Inconsistency
  = StockDiffers ResourceId Q Q
  | WeightDiffers CollectiveProcedureId Q Q
  | AuthorityDiffers

derive instance Eq Inconsistency

instance Show Inconsistency where
  show = case _ of
    StockDiffers k l r -> "stock " <> show k <> ": " <> show l <> " vs " <> show r
    WeightDiffers k l r -> "procedure weight " <> show k <> ": " <> show l <> " vs " <> show r
    AuthorityDiffers -> "institutional authority differs"
