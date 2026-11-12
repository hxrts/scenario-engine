-- | The closed vocabularies of a stored run (docs/07_simulator.md §6, §9.1): its status, and how it
-- | was derived from another. Each has one spelling, here, and its shape. The Run Store keeps the
-- | same text it always kept; `Adapters.SQLite` converts at the boundary.
module Schema.Run
  ( RunStatus(..)
  , runStatusShape
  , RunClass(..)
  , runClassShape
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

-- | A run's lifecycle state. A live run that is cleared stops at once and keeps the rounds it played.
data RunStatus = Running | Completed | Failed | Cleared

derive instance Eq RunStatus

derive instance Generic RunStatus _

-- | Written as its name, in the store and the views: renaming a constructor changes stored text.
instance Show RunStatus where
  show = lowerName

runStatusShape :: Shape RunStatus
runStatusShape = closedName "status"

-- | Which kind of run: a simulated run of the static suite, or an interactive (live) run.
data RunClass = Simulated | Interactive

derive instance Eq RunClass

derive instance Generic RunClass _

-- | Written as its name, in the store and the views.
instance Show RunClass where
  show = lowerName

runClassShape :: Shape RunClass
runClassShape = closedName "class"

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
  , model :: Maybe String
  , humans :: Maybe String
  , cast :: Maybe String -- the cast, as JSON text (`Schema.Cast`); absent is every seat on `preset`
  , runClass :: Maybe RunClass -- simulated or interactive; absent reads as simulated
  , cacheKey :: Maybe String -- a simulated run's suite cache key
  , tag :: Maybe String -- an interactive run's tag: mock or production
  , metadata :: Maybe String -- an interactive run's metadata, as JSON text
  }

-- | A specification with no field recorded.
emptySpec :: Spec
emptySpec = { gameId: Nothing, composition: Nothing, premises: Nothing, preset: Nothing, playerSeed: Nothing, overrides: Nothing, rounds: Nothing, model: Nothing, humans: Nothing, cast: Nothing, runClass: Nothing, cacheKey: Nothing, tag: Nothing, metadata: Nothing }

-- | A whole number stored as text, an absent key being Nothing.
storedInt :: String -> Shape (Maybe Int)
storedInt what = partialIso (optionalOf stringShape) read (map show)
  where
  read = case _ of
    Nothing -> Right Nothing
    Just text -> Just <$> note ("the stored spec's " <> what <> " is not an integer: " <> text) (Int.fromString text)

-- | The keys of a stored specification, in one place.
specKey :: { gameId :: String, composition :: String, premises :: String, preset :: String, playerSeed :: String, overrides :: String, rounds :: String, model :: String, humans :: String, cast :: String, runClass :: String, cacheKey :: String, tag :: String, metadata :: String }
specKey = { gameId: "gameId", composition: "composition", premises: "premises", preset: "preset", playerSeed: "playerSeed", overrides: "overrides", rounds: "rounds", model: "model", humans: "humans", cast: "cast", runClass: "class", cacheKey: "cacheKey", tag: "runTag", metadata: "metadata" }

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
  model <- fieldOf specKey.model "a live session's model" (optionalOf stringShape) _.model
  humans <- fieldOf specKey.humans "a live session's human seats" (optionalOf stringShape) _.humans
  cast <- fieldOf specKey.cast "the cast: each castable Actor's role, as JSON text" (optionalOf stringShape) _.cast
  runClass <- fieldOf specKey.runClass "simulated or interactive; absent is simulated" (optionalOf runClassShape) _.runClass
  cacheKey <- fieldOf specKey.cacheKey "a simulated run's suite cache key" (optionalOf stringShape) _.cacheKey
  tag <- fieldOf specKey.tag "an interactive run's tag: mock or production" (optionalOf stringShape) _.tag
  metadata <- fieldOf specKey.metadata "an interactive run's metadata, as JSON text" (optionalOf stringShape) _.metadata
  in { gameId, composition, premises, preset, playerSeed, overrides, rounds, model, humans, cast, runClass, cacheKey, tag, metadata }

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
