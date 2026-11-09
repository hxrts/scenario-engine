-- | Launching a run (`POST /api/runs`, docs/06_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( launch
  , serverRole
  , startup
  , premiseInfo
  , launchBatch
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effects.Launch (Launched, nextRunId, start)
import Effects.Signatures (STORE)
import Run (Run)
import Server.Rows (LAUNCHES)
import Type.Row (type (+))
import Effects.Api (Launch, LaunchBatch)
import Effects.Store (failRunning)
import Foreign.Object as Object
import Views.Premise (Premise)
import Premises.Presets (presetNames)
import Assembly.Assemble (checkRounds)
import Assembly.Premise (PossibleWorldParts)

-- | The role of the runs a server creates (`Effects.System.ownerOf`).
serverRole :: String
serverRole = "server"

-- | The startup strategy (docs/06_simulator.md §6.1): a run a server left marked running was left by a
-- | process that stopped before it finished, so it is failed with that reason. Runs another role owns,
-- | such as a live CLI session on the same store, are not this server's to fail. Returns the line to report, if any.
startup :: forall r. Run (STORE + r) (Maybe String)
startup = do
  stale <- failRunning { role: serverRole, reason: "interrupted: the server stopped before the run finished" }
  pure (if stale > 0 then Just ("marked " <> show stale <> " interrupted run(s) failed") else Nothing)

-- | Launch a run from a preset on every seat.
launch :: forall r. PossibleWorldParts -> Launch -> Run (LAUNCHES r) (Either String Launched)
launch p l = either (pure <<< Left <<< show) (const (startRun)) (traverse_ (checkRounds p.possibleWorld) l.rounds)
  where
  startRun = start serverRole p { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Launch one run of each Premise, in order. The first that does not start stops the batch; the runs
-- | before it stay, as any launched run does.
launchBatch :: forall r. Array PossibleWorldParts -> LaunchBatch -> Run (LAUNCHES r) (Either String (Array { premise :: String, id :: String, playerSeed :: Int }))
launchBatch ps b = foldM step (Right []) ps
  where
  step acc p = case acc of
    Left e -> pure (Left e)
    Right done -> launch p { premise: Nothing, name: "batch " <> p.possibleWorld.id, preset: b.preset, playerSeed: b.playerSeed, rounds: b.rounds } <#> map (\l -> done <> [ { premise: p.possibleWorld.id, id: l.id, playerSeed: l.playerSeed } ])

-- | `GET /api/premise`: the premises the launch form offers (`Views.Premise`).
premiseInfo :: forall r. Array PossibleWorldParts -> Run (LAUNCHES r) (Array Premise)
premiseInfo ps = do
  next <- nextRunId
  pure (map (info next) ps)
  where
  info next p =
    { name: p.possibleWorld.id
    , version: p.possibleWorld.version
    , rounds: { min: p.possibleWorld.rounds.min, max: p.possibleWorld.rounds.max, default: p.config.rounds }
    , gameSeed: unwrap p.config.seed
    , defaultName: next
    , presets: filter (\n -> Object.member n p.presets) presetNames
    }
