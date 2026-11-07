-- | Launching a run (`POST /api/runs`, docs/06_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( Launch
  , Launched
  , readLaunch
  , launch
  , start
  , presetSpec
  , seats
  , Scenario
  , scenarioInfo
  ) where

import Prelude

import Adapters.Seed (resolveSeed)
import Adapters.Store (nowIso)
import Handlers.Store (StorePort, createRun, finish, persister, reader)
import Board.Decode.Common (readJson)
import Board.Gameboard.Schema (gameIdOf)
import Core.Draw (Seed(..))
import Core.Types (EntityId)
import Data.Argonaut.Core (stringify)
import Data.Array (concatMap, foldl, fromFoldable, mapMaybe)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (message, throwException, try)
import Foreign.Object as Object
import Simulator.Overrides (encodeOverrides)
import Simulator.Simulate (Context, Override, SimulationSpec, presetSeats, simulateWith)

-- | `POST /api/runs` body: every seat plays one preset. An absent or null `playerSeed` is drawn once
-- | at launch; the reply carries the seed used either way.
type Launch = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

-- | A run to start: its name, what it is played from, and, when it is derived, its source and kind.
type Start =
  { name :: String
  , preset :: String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , overrides :: Array Override
  , derivation :: Maybe { source :: String, kind :: String }
  }

-- | Decode a launch body.
readLaunch :: String -> Either String Launch
readLaunch = readJson

-- | Launch a run from a preset on every seat.
launch :: Context -> StorePort -> Launch -> Effect (Either String Launched)
launch ctx store l = start ctx store { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the spec instantiates. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: Context -> StorePort -> Start -> Effect (Either String Launched)
start ctx store l = do
  id <- nextRunId store
  seed <- resolveSeed (Seed <$> l.playerSeed)
  let
    spec = Object.fromFoldable $
      [ Tuple "gameId" (fromMaybe "" (gameIdOf ctx.gameboard))
      , Tuple "preset" l.preset
      , Tuple "playerSeed" (show (unwrap seed))
      , Tuple "rounds" (maybe "default" show l.rounds)
      , Tuple "overrides" (stringify (encodeOverrides l.overrides))
      ] <> maybe [] (\d -> [ Tuple "source" d.source, Tuple "kind" d.kind ]) l.derivation
    source = _.source <$> l.derivation
    kind = _.kind <$> l.derivation
  created <- nowIso
  attempt <- try $ simulateWith ctx (presetSpec ctx l.preset seed l.overrides l.rounds) \manifest -> do
    createRun store { id, name: l.name, created, spec, manifest, source, kind }
    persister store id manifest.initial
  case attempt of
    Left ex -> do
      store.failRun id ("exception: " <> message ex)
      throwException ex
    Right (Left e) -> pure (Left e)
    Right (Right r) -> finish store id r $> Right { id, playerSeed: unwrap seed }

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: StorePort -> Effect String
nextRunId store = do
  rows <- (reader store).summaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure r.source) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | `GET /api/scenario`: the scenario the launch form runs. Its supported round counts (`min`, `max`)
-- | and the round count a launch without `rounds` plays (`default`); the game seed the scenario fixes.
type Scenario = { name :: String, version :: String, rounds :: { min :: Int, max :: Int, default :: Int }, gameSeed :: Int }

scenarioInfo :: Context -> Scenario
scenarioInfo ctx =
  { name: ctx.gameboard.id
  , version: ctx.gameboard.version
  , rounds: { min: ctx.gameboard.rounds.min, max: ctx.gameboard.rounds.max, default: ctx.config.rounds }
  , gameSeed: unwrap ctx.config.seed
  }

-- | The scenario's seats, as configured.
seats :: Context -> Array EntityId
seats ctx = fromFoldable (Map.keys ctx.config.seats)

-- | Every seat plays one preset.
presetSpec :: Context -> String -> Seed -> Array Override -> Maybe Int -> SimulationSpec
presetSpec ctx preset playerSeed overrides rounds = { seats: presetSeats (const preset) (seats ctx), playerSeed, overrides, rounds }
