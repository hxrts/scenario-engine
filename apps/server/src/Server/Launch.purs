-- | Launching a run (`POST /api/runs`, docs/07_simulator.md §9.1): the request and its validation,
-- | the seed handling (a player seed is drawn once, before the run starts), the run's stored spec,
-- | and the play-to-completion that persists every round. Derived runs start through `start` too.
module Server.Launch
  ( launch
  , serverRole
  , startup
  , compositionInfo
  , launchBatch
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effects.Launch (Launched, nextRunId, start)
import Effects.Signatures (STORE)
import Run (Run)
import Server.Rows (LAUNCHES)
import Type.Row (type (+))
import Effects.Api (Launch, LaunchBatch)
import Effects.Store (failRunning)
import Views.Composition (CompositionInfo)
import Assembly.Assemble (checkRounds)
import Assembly.Composition (CompositionParts, presetNames)
import Schema.Composition (premiseRefText)

-- | The role of the runs a server creates (`Effects.System.ownerOf`).
serverRole :: String
serverRole = "server"

-- | The startup strategy (docs/07_simulator.md §6.1): a run a server left marked running was left by a
-- | process that stopped before it finished, so it is failed with that reason. Runs another role owns,
-- | such as a live CLI session on the same store, are not this server's to fail. Returns the line to report, if any.
startup :: forall r. Run (STORE + r) (Maybe String)
startup = do
  stale <- failRunning { role: serverRole, reason: "interrupted: the server stopped before the run finished" }
  pure (if stale > 0 then Just ("marked " <> show stale <> " interrupted run(s) failed") else Nothing)

-- | Launch a run from a preset on every seat.
launch :: forall r. CompositionParts -> Launch -> Run (LAUNCHES r) (Either String Launched)
launch p l = either (pure <<< Left <<< show) (const (startRun)) (traverse_ (checkRounds p.composition) l.rounds)
  where
  startRun = start serverRole p { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | Launch one run of each Composition, in order. The first that does not start stops the batch; the runs
-- | before it stay, as any launched run does.
launchBatch :: forall r. Array CompositionParts -> LaunchBatch -> Run (LAUNCHES r) (Either String (Array { composition :: String, id :: String, playerSeed :: Int }))
launchBatch ps b = foldM step (Right []) ps
  where
  step acc p = case acc of
    Left e -> pure (Left e)
    Right done -> launch p { composition: Nothing, name: "batch " <> p.composition.id, preset: b.preset, playerSeed: b.playerSeed, rounds: b.rounds } <#> map (\l -> done <> [ { composition: p.composition.id, id: l.id, playerSeed: l.playerSeed } ])

-- | `GET /api/premise`: the compositions the launch form offers (`Views.Composition`).
compositionInfo :: forall r. Array CompositionParts -> Run (LAUNCHES r) (Array CompositionInfo)
compositionInfo ps = do
  next <- nextRunId
  pure (map (info next) ps)
  where
  info next p =
    { name: p.composition.id
    , version: p.composition.version
    , premises: map premiseRefText p.composition.premises
    , rounds: { min: p.composition.rounds.min, max: p.composition.rounds.max, default: p.config.rounds }
    , gameSeed: unwrap p.config.seed
    , defaultName: next
    -- the composition's presets, in name order
    , presets: offered p
    -- the composition's own default, else the first it offers
    , defaultPreset: fromMaybe "" (p.config.preset <|> head (offered p))
    }
  offered p = presetNames p
