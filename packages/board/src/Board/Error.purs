-- | The errors and differences of Gameboard realization (docs/03_gameboard.md, docs/04_instantiated_game.md
-- | §14.2): the failed checks of `Board.Gameboard.Validate` and the differences between two
-- | instantiations of one economy. Realization owns these; the premise format does not.
module Board.Error
  ( ValidationError(..)
  , Inconsistency(..)
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (CapacityRef, CollectiveProcedureId, EntityId, RefKind, ResourceId, TransformationId)

-- | Every way a Gameboard, its configuration, or an instantiation can be wrong. Each
-- | constructor names one check, so a caller and its tests match on the specific failure.
data ValidationError
  = UnknownReference RefKind String
  | UnknownScheme String -- a Representation scheme the Gameboard does not define
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
  | ResolutionInconsistent EntityId (Array Inconsistency) -- implicit coverage would change the economy
  | CoverageNotAggregate EntityId EntityId -- an Actor's coverage fallback names a non-Aggregate
  | SchemeTargetNotAggregate String EntityId -- a Representation scheme's target is not an Aggregate
  | RoundsOutOfRange { rounds :: Int, min :: Int, max :: Int }
  | NegativeDelay TransformationId

derive instance Eq ValidationError

instance Show ValidationError where
  show = case _ of
    UnknownReference k r -> "unknown " <> show k <> " reference: " <> r
    UnknownScheme s -> "unknown scheme reference: " <> s
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
    NegativeDelay t -> "facility " <> show t <> " has a negative delay"
    RoundsOutOfRange r -> "configured " <> show r.rounds <> " rounds is outside the supported range " <> show r.min <> " to " <> show r.max

-- | A difference between two instantiations of the same economy (§14.2).
data Inconsistency
  = StockDiffers ResourceId Q Q
  | CapacityDiffers CapacityRef Q Q
  | WeightDiffers CollectiveProcedureId Q Q
  | AuthorityDiffers

derive instance Eq Inconsistency

instance Show Inconsistency where
  show = case _ of
    StockDiffers k l r -> "stock " <> show k <> ": " <> show l <> " vs " <> show r
    CapacityDiffers k l r -> "capacity " <> show k <> ": " <> show l <> " vs " <> show r
    WeightDiffers k l r -> "procedure weight " <> show k <> ": " <> show l <> " vs " <> show r
    AuthorityDiffers -> "institutional authority differs"
