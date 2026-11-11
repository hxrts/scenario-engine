-- | The compositions a server has loaded (docs/07_simulator.md §8.1): every composition found under the
-- | premise root. A launch names one, and a stored run is served from its own, through its `Spec.gameId`.
module Server.Compositions
  ( Compositions
  , chosen
  , ofSpec
  , ofRun
  ) where

import Prelude

import Data.Array (find, head)
import Data.Either (Either(..), hush, note)
import Data.Maybe (Maybe(..), fromMaybe)
import Assembly.Composition (CompositionParts)
import Schema.Composition (PremiseComposition, gameIdOf, premisesText)
import Schema.Run (Spec)
import Views.Common (StoredRun)

-- | Every loaded composition, in the order the launch form offers them.
type Compositions = Array CompositionParts

-- | The composition a request names by its id, or the first one when it names none.
chosen :: Compositions -> Maybe String -> Either String CompositionParts
chosen ps = case _ of
  Nothing -> note "the server has no composition loaded" (head ps)
  Just name -> note ("no composition named " <> name) (find (\p -> p.composition.id == name) ps)

-- | The composition a stored run was played on.
ofSpec :: Compositions -> Spec -> Either String CompositionParts
ofSpec ps spec = case spec.gameId of
  Nothing -> Left "the run's specification names no game"
  Just game -> do
    p <- note ("no loaded composition plays the game " <> game) (find (\c -> fromMaybe "" (gameIdOf c.composition) == game) ps)
    -- a run replays only over the Premises and versions it recorded
    case spec.premises of
      Just recorded | recorded /= premisesText p.composition -> Left ("the run recorded the Premises " <> recorded <> " but the loaded composition has " <> premisesText p.composition)
      _ -> Right p

-- | The composition of a stored run, when its composition is loaded.
ofRun :: Compositions -> StoredRun -> Maybe PremiseComposition
ofRun ps r = _.composition <$> hush (ofSpec ps r.spec)
