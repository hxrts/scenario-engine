-- | The command-line options of `run` and `session` (docs/06 §6.2), from their parsed flags. A bad value
-- | is a usage error; `Main` reports it and exits 1.
module Options
  ( RunOptions
  , runFlags
  , sessionFlags
  ) where

import Prelude

import Adapters.Pi (defaultModel)
import Core.Draw (Seed(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Flags (Flag, checkedFlag, intFlagOf, optionalIntFlag, textFlag)
import Handlers.Store (validRunId)
import Settings.Presets (defaultPresets)
import Session (Options)

-- | The options of `run`.
type RunOptions = { preset :: String, rounds :: Maybe Int, seed :: Maybe Seed }

runFlags :: Flag RunOptions
runFlags = { preset: _, rounds: _, seed: _ }
  <$> textFlag "--preset" "name" defaultPresets.run
  <*> roundsFlag
  <*> seedFlag

-- | The rounds flag: absent means the configuration's count; the setting's range is `checkRounds`.
roundsFlag :: Flag (Maybe Int)
roundsFlag = optionalIntFlag "--rounds" "n" (_ >= 1) "a whole number of at least 1"

-- | The player seed flag: absent means draw one at start; given, it must be a whole number, negative
-- | values included.
seedFlag :: Flag (Maybe Seed)
seedFlag = map Seed <$> optionalIntFlag "--seed" "n" (const true) "a whole number"

-- | A session's flags, for its setting. The window may be zero.
sessionFlags :: String -> Flag Options
sessionFlags setting = { setting, humans: _, model: _, rounds: _, window: _, id: _, db: _, seed: _ }
  <$> textFlag "--humans" "US,DE" ""
  <*> textFlag "--model" "provider/id" defaultModel
  <*> roundsFlag
  <*> intFlagOf "--window" "seconds" 0 (_ >= 0) "a whole number of at least 0"
  <*> checkedFlag sessionId (textFlag "--id" "name" "session-1")
  <*> textFlag "--db" "file" "runs.db"
  <*> seedFlag
  where
  sessionId id = if validRunId id then Right id else Left ("--id must be 1 to 64 letters, digits, '-' or '_', starting with a letter or digit, got " <> id)
