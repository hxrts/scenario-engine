-- | The closed vocabularies of a stored run (docs/06_simulator.md §6, §9.1): its status, and how it
-- | was derived from another. Each has one spelling, here, and its shape. The Run Store keeps the
-- | same text it always kept; `Adapters.SQLite` converts at the boundary.
module Schema.Run
  ( RunStatus(..)
  , runStatusName
  , runStatusShape
  , DeriveKind(..)
  , deriveKindName
  , deriveKindShape
  , Spec
  , emptySpec
  , specShape
  , specKey
  , specPairs
  ) where

import Prelude

import Core.Shape (Desc(..), Format(..), Shape, encodeWith, fieldOf, object, optionalOf, partialIso, prim, stringShape)
import Data.Argonaut.Core (fromString, toObject, toString)
import Data.Array (find, mapMaybe)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

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

-- | A run's stored specification: what it was played from and where it came from. `overrides` is the
-- | JSON text of the overrides (`Game.Override`). A run stored without a field reads as Nothing.
type Spec =
  { gameId :: Maybe String
  , preset :: Maybe String
  , playerSeed :: Maybe Int
  , overrides :: Maybe String
  , rounds :: Maybe Int
  , source :: Maybe String
  , kind :: Maybe DeriveKind
  , model :: Maybe String
  , humans :: Maybe String
  }

-- | A specification with no field recorded.
emptySpec :: Spec
emptySpec = { gameId: Nothing, preset: Nothing, playerSeed: Nothing, overrides: Nothing, rounds: Nothing, source: Nothing, kind: Nothing, model: Nothing, humans: Nothing }

-- | A whole number stored as text, an absent key being Nothing.
storedInt :: String -> Shape (Maybe Int)
storedInt what = partialIso (optionalOf stringShape) read (map show)
  where
  read = case _ of
    Nothing -> Right Nothing
    Just text -> Just <$> note ("the stored spec's " <> what <> " is not an integer: " <> text) (Int.fromString text)

-- | The keys of a stored specification, in one place.
specKey :: { gameId :: String, preset :: String, playerSeed :: String, overrides :: String, rounds :: String, source :: String, kind :: String, model :: String, humans :: String }
specKey = { gameId: "gameId", preset: "preset", playerSeed: "playerSeed", overrides: "overrides", rounds: "rounds", source: "source", kind: "kind", model: "model", humans: "humans" }

-- | The stored specification. Every value is stored as text, in the premise
-- | format, so an absent field is not written.
specShape :: Shape Spec
specShape = object "Spec" ado
  gameId <- fieldOf specKey.gameId "the Gameboard's game id" (optionalOf stringShape) _.gameId
  preset <- fieldOf specKey.preset "the preset every seat plays" (optionalOf stringShape) _.preset
  playerSeed <- fieldOf specKey.playerSeed "the player seed the run was resolved to" (storedInt "player seed") _.playerSeed
  overrides <- fieldOf specKey.overrides "the overrides, as JSON text" (optionalOf stringShape) _.overrides
  rounds <- fieldOf specKey.rounds "the round count; absent is the configuration's" (storedInt "rounds") _.rounds
  source <- fieldOf specKey.source "the run this one was derived from" (optionalOf stringShape) _.source
  kind <- fieldOf specKey.kind "how it was derived" (optionalOf deriveKindShape) _.kind
  model <- fieldOf specKey.model "a live session's model" (optionalOf stringShape) _.model
  humans <- fieldOf specKey.humans "a live session's human seats" (optionalOf stringShape) _.humans
  in { gameId, preset, playerSeed, overrides, rounds, source, kind, model, humans }

-- | The recorded fields of a specification as key and text pairs, in the shape's order, for a
-- | configuration diff.
specPairs :: Spec -> Array (Tuple String String)
specPairs s = case specShape.desc of
  DRec _ fields -> mapMaybe (\f -> Tuple f.key <$> (Object.lookup f.key written >>= toString)) fields
  _ -> []
  where
  written = case toObject (encodeWith Premise specShape s) of
    Just o -> o
    Nothing -> Object.empty

-- | One of a closed set of values, written by name.
nameShape :: forall a. String -> (a -> String) -> Array a -> Shape a
nameShape label name values = prim label (fromString <<< name) \j ->
  note message (toString j >>= \s -> find (\v -> name v == s) values)
  where
  message = label <> " must be one of " <> joinWith ", " (map name values)
