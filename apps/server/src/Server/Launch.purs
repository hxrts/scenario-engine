-- | The server's startup and its role, and the compositions it offers (docs/07_simulator.md §6.1, §8.1).
module Server.Launch
  ( serverRole
  , startup
  , compositionInfo
  ) where

import Prelude

import Assembly.Assemble (castable, configuredSeats)
import Laws.Types (idText)

import Schema.Run (RunStatus(..))

import Control.Alt ((<|>))
import Data.Array (head)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effects.Launch (nextRunId)
import Effects.Signatures (STORE)
import Run (Run)
import Server.Rows (LAUNCHES)
import Type.Row (type (+))
import Effects.Store (failRunning)
import Views.Composition (CompositionInfo)

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
  stale <- failRunning { role: serverRole, reason: "cleared: the server stopped while the run was live", status: Cleared }
  pure (if stale > 0 then Just ("marked " <> show stale <> " interrupted live run(s) cleared") else Nothing)

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
    , castable: map idText (castable p)
    , seats: map idText (configuredSeats p.config)
    }
  offered p = presetNames p
