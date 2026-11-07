-- | Launching a run (`POST /api/runs`, docs/06_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( Launched
  , launch
  , start
  , failInterrupted
  , Scenario
  , scenarioInfo
  , nextRunId
  ) where

import Prelude

import Adapters.Clock (nowIso)
import Adapters.Random (resolveSeed)
import Schema.Gameboard (gameIdOf)
import Core.Draw (Seed(..))
import Data.Array (concatMap, filter, foldl, mapMaybe)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Exception (message, throwException, try)
import Effects.Api (Launch)
import Effects.Observe (listSummaries)
import Effects.Store (createRun, failRun, failRunning, finish)
import Foreign.Object as Object
import Views.Metrics (roundMetrics)
import Premises.Presets (presetNames)
import Handlers.Store (StoreBackend, persister, runObserve, runStore)
import Game.Assemble (assemblePreset)
import Game.Override (Override)
import Game.Premise (GameboardParts)
import Game.Recipe (Derivation, encodeSpec)
import Simulator.Simulate (simulateWith)

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

-- | A run to start: its name, what it is played from, and, when it is derived, its source and kind.
type Start =
  { name :: String
  , preset :: String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , overrides :: Array Override
  , derivation :: Maybe Derivation
  }

-- | The startup policy (docs/06_simulator.md §9.1): a run still marked running was left by a process
-- | that stopped before it finished, so it is failed with that reason. Returns how many were failed.
failInterrupted :: StoreBackend -> Effect Int
failInterrupted store = runStore store (failRunning "interrupted: the server stopped before the run finished")

-- | Launch a run from a preset on every seat.
launch :: GameboardParts -> StoreBackend -> Launch -> Effect (Either String Launched)
launch p store l = start p store { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the game assembles. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: GameboardParts -> StoreBackend -> Start -> Effect (Either String Launched)
start p store l = do
  id <- nextRunId store
  seed <- resolveSeed (Seed <$> l.playerSeed)
  case assemblePreset p l.preset l.overrides l.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      let
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf p.gameboard), playerSeed: seed, preset: l.preset, rounds: l.rounds, overrides: l.overrides, derivation: l.derivation }
        source = _.source <$> l.derivation
        kind = _.kind <$> l.derivation
      created <- nowIso
      attempt <- try $ simulateWith seed game \manifest -> do
        runStore store (createRun { id, name: l.name, created, spec, manifest, source, kind })
        persister store roundMetrics id manifest.initial
      case attempt of
        Left ex -> do
          runStore store (failRun id ("exception: " <> message ex))
          throwException ex
        Right (Left e) -> pure (Left (show e))
        Right (Right r) -> runStore store (finish id r) $> Right { id, playerSeed: unwrap seed }

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: StoreBackend -> Effect String
nextRunId store = do
  rows <- runObserve store listSummaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure r.source) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | `GET /api/scenario`: the scenario the launch form runs. Its supported round counts (`min`, `max`)
-- | and the round count a launch without `rounds` plays (`default`); the game seed the scenario fixes;
-- | the default run name (the next run id); and the presets every seat may play, in the scenario's order.
type Scenario =
  { name :: String
  , version :: String
  , rounds :: { min :: Int, max :: Int, default :: Int }
  , gameSeed :: Int
  , defaultName :: String
  , presets :: Array String
  }

scenarioInfo :: GameboardParts -> StoreBackend -> Effect Scenario
scenarioInfo p store = do
  next <- nextRunId store
  pure
    { name: p.gameboard.id
    , version: p.gameboard.version
    , rounds: { min: p.gameboard.rounds.min, max: p.gameboard.rounds.max, default: p.config.rounds }
    , gameSeed: unwrap p.config.seed
    , defaultName: next
    , presets: filter (\n -> Object.member n p.presets) presetNames
    }
