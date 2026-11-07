-- | The Meridian premise the analysis and simulator test suites share, and the fifteen seats every test
-- | plays. Test support only; no suite's source depends on it.
module Testkit.Meridian (premise, seats, presetGame, orThrow) where

import Prelude

import Adapters.Filesystem (fileOps)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Game.Assemble (assemblePreset)
import Game.Override (Override)
import Game.Premise (GameboardParts)
import Game.Types (Game)
import Premises.Load (loadPremise)
import Premises.Tree (readPremise)

-- | The Meridian premise: its Gameboard, configuration and presets, checked.
premise :: Aff GameboardParts
premise = do
  loaded <- liftEffect (map (_ >>= loadPremise) (readPremise fileOps "meridian/v0"))
  orThrow loaded

-- | The Meridian seats every test plays.
seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

-- | The Meridian premise assembled with every configured seat bound to one preset (see `assemblePreset`).
presetGame :: GameboardParts -> String -> Array Override -> Maybe Int -> Aff Game
presetGame p preset overrides rounds = orThrow (assemblePreset p preset overrides rounds)

-- | A value or the error it failed with, as an Aff error.
orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = case _ of
  Left e -> throwError (error (show e))
  Right x -> pure x
