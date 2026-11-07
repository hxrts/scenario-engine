-- | The Meridian game the adapter suites start from: the premise's Gameboard premise, and games assembled
-- | from it with the seats they play. Test support only (`testkit` depends on adapters, so it is not used here).
module Test.Adapters.Game (parts, presetGame, capstoneGame, orThrow) where

import Prelude

import Adapters.Filesystem (fileOps)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Core.Types (Id(..))
import Game.Assemble (assemble, presetBindings, withRounds)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Game)
import Premises.Capstone as C
import Premises.Load (loadPremise)
import Premises.Tree (readPremise)

orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = either (throwError <<< error <<< show) pure

-- | The shipped Meridian premise: its Gameboard, configuration and presets, checked.
parts :: Aff GameboardParts
parts = liftEffect (readPremise fileOps "meridian/v0") >>= orThrow >>= orThrow <<< loadPremise

-- | The Meridian game with the named seats playing one preset, and the round count (or the configuration's).
presetGame :: Array String -> String -> Maybe Int -> Aff Game
presetGame ids preset rounds = do
  p <- parts
  bindings <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  g <- orThrow (assemble (GameboardPremise p) bindings [])
  pure (withRounds rounds g)

-- | The capstone's game: the Meridian premise with the capstone's seat bindings, for its full round count.
capstoneGame :: Aff Game
capstoneGame = do
  p <- parts
  g <- orThrow (assemble (GameboardPremise p) C.seatBindings [])
  pure (withRounds (Just C.rounds) g)
