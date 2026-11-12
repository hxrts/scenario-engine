-- | What the test suites share to load a compiled composition and play it: its parts, its seats bound silent
-- | or to a preset, a run under a player seed, and the run's invariants. Test support only; no suite's source
-- | depends on it. `pacificParts` is the reference composition every suite plays.
module Testkit.Run (silent, price, everySeat, run, valid, initialOf, partsOf, pacificParts, anyRounds, unseated, presetGame, presetSeats, orThrow) where

import Prelude

import Checks.Run (checkRun)
import Core.Draw (Seed)
import Core.Num as Q
import Laws.Query (priceOf)
import Laws.World (RoundResult, WorldState)
import Data.Maybe (Maybe(..), fromMaybe)
import Handlers.Simulate (SimRun, simulate)
import Test.Spec.Assertions (shouldEqual)

import Adapters.Filesystem (fileOps)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Laws.Types (Id(..))
import Assembly.Assemble (assemble, assemblePreset, configuredSeats, presetBindings, withRounds)
import Assembly.Override (Override)
import Assembly.Composition (CompositionParts, Origin(..))
import Assembly.Types (Bindings, PreparedRun, SeatBinding(..))
import Premises.Load (loadFrom)

-- | Every configured seat bound to the empty script: it submits and signs nothing.
silent :: CompositionParts -> Bindings
silent p = Map.fromFoldable (map (\e -> Tuple e (Script [])) (configuredSeats p.config))

-- | A compiled composition by its directory under the compositions root (`pacific/v0`), checked.
partsOf :: String -> Aff CompositionParts
partsOf dir = do
  loaded <- liftEffect (loadFrom fileOps dir)
  orThrow loaded

-- | The Pacific composition (docs/05_pacific.md).
pacificParts :: Aff CompositionParts
pacificParts = partsOf "pacific/v0"

-- | A composition's game with every seat bound to an empty script (silent).
unseated :: CompositionParts -> Aff PreparedRun
unseated p = orThrow (assemble (FromComposition p) (silent p) [])

-- | The composition with its supported round counts opened up, for suites that play a game shorter or longer
-- | than the composition allows and read something else from it.
anyRounds :: CompositionParts -> CompositionParts
anyRounds p = p { composition = p.composition { rounds = { min: 1, max: 8 } } }

-- | The composition assembled with every configured seat bound to one preset (see `assemblePreset`).
presetGame :: CompositionParts -> String -> Array Override -> Maybe Int -> Aff PreparedRun
presetGame p preset overrides rounds = orThrow (assemblePreset p preset overrides rounds)

-- | The game with the named seats playing one preset, and the round count (or the configuration's). Unlike
-- | `presetGame`, only the listed seats play; the rest are silent (`assemble` requires every seat bound).
presetSeats :: CompositionParts -> Array String -> String -> Maybe Int -> Aff PreparedRun
presetSeats p ids preset rounds = do
  listed <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  g <- orThrow (assemble (FromComposition p) (Map.union listed (silent p)) [])
  pure (withRounds rounds g)

-- | A value or the error it failed with, as an Aff error.
orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = case _ of
  Left e -> throwError (error (show e))
  Right x -> pure x

-- | An Exchange's marginal price at the end of a round; an unknown Exchange reads as zero.
price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (priceOf r.world (Id m))

-- | Every configured seat plays one preset, for the configuration's rounds.
everySeat :: CompositionParts -> String -> Aff PreparedRun
everySeat p preset = presetGame p preset [] Nothing

-- | Play a game to its end under a player seed; a failure is an error.
run :: Seed -> PreparedRun -> Aff SimRun
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
