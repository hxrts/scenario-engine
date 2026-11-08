-- | The Meridian premise the views, experiments and simulator test suites share, and the fifteen seats every test
-- | plays. Test support only; no suite's source depends on it.
module Testkit.Meridian (price, everySeat, run, valid, initialOf, premise, anyRounds, seats, unseated, presetGame, presetSeats, orThrow) where

import Prelude

import Checks.Run (checkRun)
import Core.Draw (Seed)
import Core.Num as Q
import Core.Query (priceOf)
import Core.World (RoundResult, WorldState)
import Data.Maybe (Maybe(..), fromMaybe)
import Simulator.Simulate (SimRun, simulate)
import Test.Spec.Assertions (shouldEqual)

import Adapters.Filesystem (fileOps)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Core.Types (Id(..))
import Game.Assemble (assemble, assemblePreset, configuredSeats, presetBindings, withRounds)
import Game.Override (Override)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Bindings, Game, SeatBinding(..))
import Premises.Load (loadFrom)
import Premises.Names (meridianDir, seatActors)

-- | Every configured seat bound to the empty script: it submits and signs nothing.
silent :: GameboardParts -> Bindings
silent p = Map.fromFoldable (map (\e -> Tuple e (Script [])) (configuredSeats p.config))

-- | The Meridian premise: its Gameboard, configuration and presets, checked.
premise :: Aff GameboardParts
premise = do
  loaded <- liftEffect (loadFrom fileOps meridianDir)
  orThrow loaded

-- | The Meridian game with every seat bound to an empty script (silent).
unseated :: Aff Game
unseated = do
  p <- premise
  orThrow (assemble (GameboardPremise p) (silent p) [])

-- | The Meridian seats every test plays.
seats :: Array String
seats = seatActors

-- | The premise with its supported round counts opened up, for suites that play a game shorter than the
-- | premise allows and read something else from it.
anyRounds :: GameboardParts -> GameboardParts
anyRounds p = p { gameboard = p.gameboard { rounds = { min: 1, max: 8 } } }

-- | The Meridian premise assembled with every configured seat bound to one preset (see `assemblePreset`).
presetGame :: GameboardParts -> String -> Array Override -> Maybe Int -> Aff Game
presetGame p preset overrides rounds = orThrow (assemblePreset p preset overrides rounds)

-- | The Meridian game with the named seats playing one preset, and the round count (or the configuration's).
-- | Unlike `presetGame`, only the listed seats play; the rest are silent (`assemble` requires every seat bound).
presetSeats :: Array String -> String -> Maybe Int -> Aff Game
presetSeats ids preset rounds = do
  p <- premise
  listed <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  g <- orThrow (assemble (GameboardPremise p) (Map.union listed (silent p)) [])
  pure (withRounds rounds g)

-- | A value or the error it failed with, as an Aff error.
orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = case _ of
  Left e -> throwError (error (show e))
  Right x -> pure x

-- | A Market's marginal price at the end of a round; an unknown Market reads as zero.
price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (priceOf r.world (Id m))

-- | Every configured seat plays one preset, for the configuration's rounds.
everySeat :: GameboardParts -> String -> Aff Game
everySeat p preset = presetGame p preset [] Nothing

-- | Play a game to its end under a player seed; a failure is an error.
run :: Seed -> Game -> Aff SimRun
run seed g = do
  r <- orThrow (simulate seed g)
  case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r

-- | Every round's invariants hold.
valid :: WorldState -> SimRun -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: SimRun -> WorldState
initialOf r = r.manifest.initial
