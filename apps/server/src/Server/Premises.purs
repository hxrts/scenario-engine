-- | The premises a server has loaded (docs/06_simulator.md §8.1): every premise found under the
-- | premise root. A launch names one, and a stored run is served from its own, through its `Spec.gameId`.
module Server.Premises
  ( Premises
  , chosen
  , ofSpec
  , ofRun
  ) where

import Prelude

import Data.Array (find, head)
import Data.Either (Either(..), hush, note)
import Data.Maybe (Maybe(..), fromMaybe)
import Assembly.Premise (PossibleWorldParts)
import Schema.PossibleWorld (PossibleWorld, gameIdOf)
import Schema.Run (Spec)
import Views.Common (StoredRun)

-- | Every loaded premise, in the order the launch form offers them.
type Premises = Array PossibleWorldParts

-- | The premise a request names by its id, or the first one when it names none.
chosen :: Premises -> Maybe String -> Either String PossibleWorldParts
chosen ps = case _ of
  Nothing -> note "the server has no premise loaded" (head ps)
  Just name -> note ("no premise named " <> name) (find (\p -> p.possibleWorld.id == name) ps)

-- | The premise a stored run was played on.
ofSpec :: Premises -> Spec -> Either String PossibleWorldParts
ofSpec ps spec = case spec.gameId of
  Nothing -> Left "the run's specification names no game"
  Just game -> note ("no loaded premise plays the game " <> game) (find (\p -> fromMaybe "" (gameIdOf p.possibleWorld) == game) ps)

-- | The possible World of a stored run, when its premise is loaded.
ofRun :: Premises -> StoredRun -> Maybe PossibleWorld
ofRun ps r = _.possibleWorld <$> hush (ofSpec ps r.spec)
