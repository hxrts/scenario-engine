-- | The closed vocabularies of a stored run (docs/06_simulator.md §6, §9.1): its status, and how it
-- | was derived from another. Each has one spelling, here, and its shape. The Run Store keeps the
-- | same text it always kept; `Adapters.SQLite` converts at the boundary.
module Core.Run
  ( RunStatus(..)
  , runStatusName
  , runStatusShape
  , DeriveKind(..)
  , deriveKindName
  , deriveKindShape
  ) where

import Prelude

import Core.Shape (Shape, prim)
import Data.Argonaut.Core (fromString, toString)
import Data.Array (find)
import Data.Either (note)
import Data.String (joinWith)

-- | A run's lifecycle state.
data RunStatus = Running | Completed | Failed

derive instance Eq RunStatus

instance Show RunStatus where
  show = runStatusName

-- | A status as the store and the views write it.
runStatusName :: RunStatus -> String
runStatusName = case _ of
  Running -> "running"
  Completed -> "completed"
  Failed -> "failed"

runStatusShape :: Shape RunStatus
runStatusShape = nameShape "status" runStatusName [ Running, Completed, Failed ]

-- | How a run is derived from another.
data DeriveKind = Duplicate | Rerun | Variant | Counterfactual

derive instance Eq DeriveKind

instance Show DeriveKind where
  show = deriveKindName

-- | A kind as a request and a stored spec write it.
deriveKindName :: DeriveKind -> String
deriveKindName = case _ of
  Duplicate -> "duplicate"
  Rerun -> "rerun"
  Variant -> "variant"
  Counterfactual -> "counterfactual"

deriveKindShape :: Shape DeriveKind
deriveKindShape = nameShape "kind" deriveKindName [ Duplicate, Rerun, Variant, Counterfactual ]

-- | One of a closed set of values, written by name.
nameShape :: forall a. String -> (a -> String) -> Array a -> Shape a
nameShape label name values = prim label (fromString <<< name) \j ->
  note message (toString j >>= \s -> find (\v -> name v == s) values)
  where
  message = label <> " must be one of " <> joinWith ", " (map name values)
