-- | Launching a run (`POST /api/runs`, docs/06_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( Launched
  , launch
  , start
  , freshRunId
  , serverRole
  , startup
  , settingInfo
  , launchBatch
  ) where

import Prelude

import Schema.Gameboard (gameIdOf)
import Core.Draw (Seed(..))
import Data.Array (concatMap, filter, foldl, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Effects.Play (playRun)
import Effects.Signatures (PlayRequest(..), STORE)
import Effects.System (now, ownerOf, resolveSeed)
import Run (Run)
import Server.Rows (LAUNCHES)
import Type.Row (type (+))
import Effects.Api (Launch, LaunchBatch)
import Effects.Observe (listSummaries)
import Effects.Store (failRunning)
import Foreign.Object as Object
import Views.Setting (Setting)
import Settings.Presets (presetNames)
import Handlers.Store (validRunId)
import Game.Assemble (assemblePreset, checkRounds)
import Game.Override (Override)
import Game.Setting (GameboardParts)
import Game.Recipe (Derivation, encodeSpec)

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

-- | The role of the runs a server creates (`Effects.System.ownerOf`).
serverRole :: String
serverRole = "server"

-- | The startup policy (docs/06_simulator.md §6.1): a run a server left marked running was left by a
-- | process that stopped before it finished, so it is failed with that reason. Runs another role owns,
-- | such as a live CLI session on the same store, are not this server's to fail. Returns the line to report, if any.
startup :: forall r. Run (STORE + r) (Maybe String)
startup = do
  stale <- failRunning { role: serverRole, reason: "interrupted: the server stopped before the run finished" }
  pure (if stale > 0 then Just ("marked " <> show stale <> " interrupted run(s) failed") else Nothing)

-- | Launch a run from a preset on every seat.
launch :: forall r. GameboardParts -> Launch -> Run (LAUNCHES r) (Either String Launched)
launch p l = either (pure <<< Left <<< show) (const (startRun)) (traverse_ (checkRounds p.gameboard) l.rounds)
  where
  startRun = start p { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the game assembles. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: forall r. GameboardParts -> Start -> Run (LAUNCHES r) (Either String Launched)
start p l = freshRunId >>= case _ of
  Left e -> pure (Left e)
  Right id -> startAs id p l

-- | A freshly minted run id, or why it may not name a run.
freshRunId :: forall r. Run (LAUNCHES r) (Either String String)
freshRunId = checkRunId <$> nextRunId

-- | A minted id, or why it may not name a run. The rule is `Handlers.Store.validRunId`, the one the CLI's
-- | `--id` obeys: an id names a directory under runs/ as well as a row.
checkRunId :: String -> Either String String
checkRunId id
  | validRunId id = Right id
  | otherwise = Left ("not a valid run id: " <> id)

startAs :: forall r. String -> GameboardParts -> Start -> Run (LAUNCHES r) (Either String Launched)
startAs id p l = do
  seed <- resolveSeed (Seed <$> l.playerSeed)
  case assemblePreset p l.preset l.overrides l.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      let
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf p.gameboard), playerSeed: seed, preset: Just l.preset, rounds: Just game.rounds, overrides: l.overrides, derivation: l.derivation, model: Nothing, humans: Nothing }
      created <- now
      owner <- ownerOf serverRole
      attempt <- playRun (Simulated { seed, game, run: { id, name: l.name, created, spec, manifest: game.manifest, owner } })
      pure (attempt $> { id, playerSeed: unwrap seed })

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: forall r. Run (LAUNCHES r) String
nextRunId = do
  rows <- listSummaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure r.source) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | Launch one run of each Setting, in order. The first that does not start stops the batch; the runs
-- | before it stay, as any launched run does.
launchBatch :: forall r. Array GameboardParts -> LaunchBatch -> Run (LAUNCHES r) (Either String (Array { setting :: String, id :: String, playerSeed :: Int }))
launchBatch ps b = foldM step (Right []) ps
  where
  step acc p = case acc of
    Left e -> pure (Left e)
    Right done -> launch p { setting: Nothing, name: "batch " <> p.gameboard.id, preset: b.preset, playerSeed: b.playerSeed, rounds: b.rounds } <#> map (\l -> done <> [ { setting: p.gameboard.id, id: l.id, playerSeed: l.playerSeed } ])

-- | `GET /api/setting`: the settings the launch form offers (`Views.Setting`).
settingInfo :: forall r. Array GameboardParts -> Run (LAUNCHES r) (Array Setting)
settingInfo ps = do
  next <- nextRunId
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
