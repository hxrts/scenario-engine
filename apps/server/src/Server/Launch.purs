-- | Launching a run (`POST /api/runs`, docs/06_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( Launched
  , launchedShape
  , launch
  , start
  , checkRunId
  , play
  , failInterrupted
  , premiseInfo
  , batchedShape
  , launchBatch
  ) where

import Prelude

import Adapters.Clock (nowIso)
import Adapters.Random (resolveSeed)
import Adapters.SQLite (processOwner)
import Schema.Gameboard (gameIdOf)
import Core.Draw (Seed(..))
import Core.Shape (Shape, intShape, recordShape, stringShape)
import Core.Engine.Commit (EngineError)
import Data.Array (concatMap, filter, foldl, mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Exception (throwException, try)
import Effects.Api (Launch, LaunchBatch)
import Effects.Observe (listSummaries)
import Effects.Store (createRun, failRunning)
import Foreign.Object as Object
import Views.Metrics (roundMetrics)
import Views.Premise (Premise)
import Premises.Presets (presetNames)
import Handlers.Store (StoreBackend, recorder, runObserve, runStore, settle, validRunId)
import Game.Assemble (assemblePreset, checkRounds)
import Game.Override (Override)
import Game.Premise (GameboardParts)
import Game.Recipe (Derivation, encodeSpec)
import Simulator.Simulate (simulateWith)

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

launchedShape :: Shape Launched
launchedShape = recordShape "Launched" { id: stringShape, playerSeed: intShape }

-- | A run to start: its name, what it is played from, and, when it is derived, its source and kind.
type Start =
  { name :: String
  , preset :: String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , overrides :: Array Override
  , derivation :: Maybe Derivation
  }

-- | The role of the runs a server creates (`Adapters.SQLite.processOwner`).
serverRole :: String
serverRole = "server"

-- | The startup policy (docs/06_simulator.md §6.1): a run a server left marked running was left by a
-- | process that stopped before it finished, so it is failed with that reason. Runs another role owns,
-- | such as a live CLI session on the same store, are not this server's to fail. Returns how many were failed.
failInterrupted :: StoreBackend -> Effect Int
failInterrupted store = runStore store (failRunning { role: serverRole, reason: "interrupted: the server stopped before the run finished" })

-- | Launch a run from a preset on every seat.
launch :: GameboardParts -> StoreBackend -> Launch -> Effect (Either String Launched)
launch p store l = either (pure <<< Left <<< show) (const (startRun)) (traverse_ (checkRounds p.gameboard) l.rounds)
  where
  startRun = start p store { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the game assembles. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: GameboardParts -> StoreBackend -> Start -> Effect (Either String Launched)
start p store l = nextRunId store >>= checkRunId >>> case _ of
  Left e -> pure (Left e)
  Right id -> startAs id p store l

-- | A minted id, or why it may not name a run. The rule is `Handlers.Store.validRunId`, the one the CLI's
-- | `--id` obeys: an id names a directory under runs/ as well as a row.
checkRunId :: String -> Either String String
checkRunId id
  | validRunId id = Right id
  | otherwise = Left ("not a valid run id: " <> id)

startAs :: String -> GameboardParts -> StoreBackend -> Start -> Effect (Either String Launched)
startAs id p store l = do
  seed <- resolveSeed (Seed <$> l.playerSeed)
  case assemblePreset p l.preset l.overrides l.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      let
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf p.gameboard), playerSeed: seed, preset: l.preset, rounds: Just game.rounds, overrides: l.overrides, derivation: l.derivation, model: Nothing, humans: Nothing }
        source = _.source <$> l.derivation
        kind = _.kind <$> l.derivation
      created <- nowIso
      owner <- processOwner serverRole
      attempt <- play store id $ simulateWith seed game \manifest -> do
        runStore store (createRun { id, name: l.name, created, spec, manifest, source, kind, owner })
        recorder store roundMetrics id manifest.initial
      pure (attempt $> { id, playerSeed: unwrap seed })

-- | Play a stored run to its end and settle it (`Handlers.Store.settle`). A thrown error fails the run,
-- | with its cause, and goes on. A game that does not start is a `Left`, with nothing stored; a game
-- | that played is the engine's result. Each error is rendered here, once.
play :: forall e a. Show e => StoreBackend -> String -> Effect (Either e (Either EngineError a)) -> Effect (Either String (Either String a))
play store id game = try game >>= case _ of
  Left ex -> settle store id exceptionPrefix (Left ex) *> throwException ex
  Right (Left e) -> pure (Left (show e))
  Right (Right r) -> settle store id exceptionPrefix (Right r) $> Right (lmap show r)
  where
  exceptionPrefix = "exception: "

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: StoreBackend -> Effect String
nextRunId store = do
  rows <- runObserve store listSummaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure r.source) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | One run of a batch: its Setting and its run.
batchedShape :: Shape { premise :: String, id :: String, playerSeed :: Int }
batchedShape = recordShape "Batched" { premise: stringShape, id: stringShape, playerSeed: intShape }

-- | Launch one run of each Setting, in order. The first that does not start stops the batch; the runs
-- | before it stay, as any launched run does.
launchBatch :: Array GameboardParts -> StoreBackend -> LaunchBatch -> Effect (Either String (Array { premise :: String, id :: String, playerSeed :: Int }))
launchBatch ps store b = foldM step (Right []) ps
  where
  step acc p = case acc of
    Left e -> pure (Left e)
    Right done -> launch p store { premise: Nothing, name: "batch " <> p.gameboard.id, preset: b.preset, playerSeed: b.playerSeed, rounds: b.rounds } <#> map (\l -> done <> [ { premise: p.gameboard.id, id: l.id, playerSeed: l.playerSeed } ])

-- | `GET /api/premise`: the premises the launch form offers (`Views.Premise`).
premiseInfo :: Array GameboardParts -> StoreBackend -> Effect (Array Premise)
premiseInfo ps store = do
  next <- nextRunId store
  pure (map (info next) ps)
  where
  info next p =
    { name: p.gameboard.id
    , version: p.gameboard.version
    , rounds: { min: p.gameboard.rounds.min, max: p.gameboard.rounds.max, default: p.config.rounds }
    , gameSeed: unwrap p.config.seed
    , defaultName: next
    , presets: filter (\n -> Object.member n p.presets) presetNames
    }
