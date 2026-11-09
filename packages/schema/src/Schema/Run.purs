-- | The closed vocabularies of a stored run (docs/06_simulator.md §6, §9.1): its status, and how it
-- | was derived from another. Each has one spelling, here, and its shape. The Run Store keeps the
-- | same text it always kept; `Adapters.SQLite` converts at the boundary.
module Schema.Run
  ( RunStatus(..)
  , runStatusShape
  , DeriveKind(..)
  , deriveKindShape
  , Spec
  , emptySpec
  , specShape
  , specKey
  , specPairs
  ) where

import Prelude

import Core.Shape (class EnumCtors, genericNameShape, lowerName, Desc(..), Format(..), Shape, encodeWith, fieldOf, object, optionalOf, partialIso, stringShape)
import Data.Generic.Rep (class Generic)

import Data.Argonaut.Core (toObject, toString)
import Data.Array (mapMaybe)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | A run's lifecycle state.
data RunStatus = Running | Completed | Failed

derive instance Eq RunStatus

derive instance Generic RunStatus _

-- | Written as its name, in the store and the views: renaming a constructor changes stored text.
instance Show RunStatus where
  show = lowerName

runStatusShape :: Shape RunStatus
runStatusShape = closedName "status"

-- | How a run is derived from another.
data DeriveKind = Duplicate | Rerun | Variant | Counterfactual

derive instance Eq DeriveKind

derive instance Generic DeriveKind _

-- | Written as its name, in requests and stored specs: renaming a constructor changes stored text.
instance Show DeriveKind where
  show = lowerName

deriveKindShape :: Shape DeriveKind
deriveKindShape = closedName "kind"

-- | A run's stored specification: what it was played from and where it came from. `overrides` is the
-- | JSON text of the overrides (`Assembly.Override`). A run stored without a field reads as Nothing.
type Spec =
  { gameId :: Maybe String
  , composition :: Maybe String
  , premises :: Maybe String
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
emptySpec = { gameId: Nothing, composition: Nothing, premises: Nothing, preset: Nothing, playerSeed: Nothing, overrides: Nothing, rounds: Nothing, source: Nothing, kind: Nothing, model: Nothing, humans: Nothing }

-- | A whole number stored as text, an absent key being Nothing.
storedInt :: String -> Shape (Maybe Int)
storedInt what = partialIso (optionalOf stringShape) read (map show)
  where
  read = case _ of
    Nothing -> Right Nothing
    Just text -> Just <$> note ("the stored spec's " <> what <> " is not an integer: " <> text) (Int.fromString text)

-- | The keys of a stored specification, in one place.
specKey :: { gameId :: String, composition :: String, premises :: String, preset :: String, playerSeed :: String, overrides :: String, rounds :: String, source :: String, kind :: String, model :: String, humans :: String }
specKey = { gameId: "gameId", composition: "composition", premises: "premises", preset: "preset", playerSeed: "playerSeed", overrides: "overrides", rounds: "rounds", source: "source", kind: "kind", model: "model", humans: "humans" }

-- | The stored specification. Every value is stored as text, in the premise
-- | format, so an absent field is not written.
specShape :: Shape Spec
specShape = object "Spec" ado
  gameId <- fieldOf specKey.gameId "the PremiseComposition's game id" (optionalOf stringShape) _.gameId
  composition <- fieldOf specKey.composition "the PremiseComposition played, id@version" (optionalOf stringShape) _.composition
  premises <- fieldOf specKey.premises "the Premises composed, in order, each id@version, comma separated" (optionalOf stringShape) _.premises
  preset <- fieldOf specKey.preset "the preset every seat plays" (optionalOf stringShape) _.preset
  playerSeed <- fieldOf specKey.playerSeed "the player seed the run was resolved to" (storedInt "player seed") _.playerSeed
  overrides <- fieldOf specKey.overrides "the overrides, as JSON text" (optionalOf stringShape) _.overrides
  rounds <- fieldOf specKey.rounds "the round count; absent is the configuration's" (storedInt "rounds") _.rounds
  source <- fieldOf specKey.source "the run this one was derived from" (optionalOf stringShape) _.source
  kind <- fieldOf specKey.kind "how it was derived" (optionalOf deriveKindShape) _.kind
  model <- fieldOf specKey.model "a live session's model" (optionalOf stringShape) _.model
  humans <- fieldOf specKey.humans "a live session's human seats" (optionalOf stringShape) _.humans
  in { gameId, composition, premises, preset, playerSeed, overrides, rounds, source, kind, model, humans }

-- | The recorded fields of a specification as key and text pairs, in the shape's order, for a
-- | configuration diff.
specPairs :: Spec -> Array (Tuple String String)
specPairs s = case specShape.desc of
  DRec _ fields -> mapMaybe (\f -> Tuple f.key <$> (Object.lookup f.key written >>= toString)) fields
  _ -> []
  where
  written = case toObject (encodeWith Authored specShape s) of
    Just o -> o
    Nothing -> Object.empty

-- | One of a closed set of values, written by its lower-case name.
closedName :: forall a rep. Generic a rep => EnumCtors rep => String -> Shape a
closedName label = genericNameShape label (\names -> label <> " must be one of " <> joinWith ", " names) lowerName
