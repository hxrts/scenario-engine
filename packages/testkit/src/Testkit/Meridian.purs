-- | The Meridian premise the views, experiments and simulator test suites share, and the fifteen seats every test
-- | plays. Test support only; no suite's source depends on it.
module Testkit.Meridian (premise, seats, unseated, presetGame, presetSeats, orThrow) where

import Prelude

import Adapters.Filesystem (fileOps)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Core.Types (Id(..))
import Game.Assemble (assemble, assemblePreset, presetBindings, withRounds)
import Game.Override (Override)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Game)
import Premises.Load (loadPremise)
import Premises.Tree (readPremise)

-- | The Meridian premise: its Gameboard, configuration and presets, checked.
premise :: Aff GameboardParts
premise = do
  loaded <- liftEffect (map (_ >>= loadPremise) (readPremise fileOps "meridian/v0"))
  orThrow loaded

-- | The Meridian game with no seat bound.
unseated :: Aff Game
unseated = do
  p <- premise
  orThrow (assemble (GameboardPremise p) Map.empty [])

-- | The Meridian seats every test plays.
seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

-- | The Meridian premise assembled with every configured seat bound to one preset (see `assemblePreset`).
presetGame :: GameboardParts -> String -> Array Override -> Maybe Int -> Aff Game
presetGame p preset overrides rounds = orThrow (assemblePreset p preset overrides rounds)

-- | The Meridian game with the named seats playing one preset, and the round count (or the configuration's).
-- | Unlike `presetGame`, only the listed seats are bound (the adapter suites play a few seats).
presetSeats :: Array String -> String -> Maybe Int -> Aff Game
presetSeats ids preset rounds = do
  p <- premise
  bindings <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  g <- orThrow (assemble (GameboardPremise p) bindings [])
  pure (withRounds rounds g)

-- | A value or the error it failed with, as an Aff error.
orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = case _ of
  Left e -> throwError (error (show e))
  Right x -> pure x
