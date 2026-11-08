-- | The helpers the test suites share: decoding a Strategy from its premise JSON, playing a game, and
-- | checking a fixture and a run's invariants. Test support only.
module Testkit.Suite
  ( strategyOrThrow
  , fixture
  , price
  , everySeat
  , run
  , valid
  , initialOf
  ) where

import Prelude

import Adapters.Filesystem (fileOps)
import Checks.Run (checkRun)
import Core.Draw (Seed)
import Core.Num as Q
import Core.Query (priceOf)
import Core.Types (Id(..))
import Core.Strategy (Strategy)
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Game.Premise (GameboardParts)
import Game.Types (Game)
import Partial.Unsafe (unsafeCrashWith)
import Premises.Names (fixturePath)
import Premises.Load (loadLiteral)
import Premises.Tree (readRootFile)
import Schema.Premise.Strategy (decodeStrategy)
import Simulator.Simulate (SimRun)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Capstone (runOf)
import Testkit.Fixture (checkLiteral)
import Testkit.Meridian (presetGame)

-- | A Strategy written as premise JSON. A malformed one fails the suite: it never becomes a Strategy
-- | that silently does nothing.
strategyOrThrow :: String -> Strategy
strategyOrThrow text = case jsonParser text of
  Left e -> unsafeCrashWith ("strategy JSON: " <> e)
  Right j -> case decodeStrategy j of
    Left e -> unsafeCrashWith ("strategy: " <> printJsonDecodeError e)
    Right s -> s

-- | A named fixture file, played through the shared checker: every expectation holds.
fixture :: String -> Aff Unit
fixture name = liftEffect (map (_ >>= loadLiteral name) (readRootFile fileOps (fixturePath name))) >>= case _ of
  Left e -> fail (show e)
  Right f -> checkLiteral f `shouldEqual` []

-- | A Market's marginal price at the end of a round; an unknown Market reads as zero.
price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (priceOf r.world (Id m))

-- | Every configured seat plays one preset, for the configuration's rounds.
everySeat :: GameboardParts -> String -> Aff Game
everySeat p preset = presetGame p preset [] Nothing

-- | Play a game to its end under a player seed; a failure is an error.
run :: Seed -> Game -> Aff SimRun
run = runOf

-- | Every round's invariants hold.
valid :: WorldState -> SimRun -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: SimRun -> WorldState
initialOf r = r.manifest.initial
