-- | The command-line options of `run` and `session` (docs/06 §6.2), from their parsed flags. A bad value
-- | is a usage error; `Main` reports it and exits 1.
module Options
  ( RunOptions
  , runFlags
  , sessionFlags
  , ExperimentOptions
  , experimentFlags
  , experimentOf
  ) where

import Prelude

import Handlers.Llm.Backend (defaultModel)
import Core.Draw (Seed(..))
import Core.Num as Q
import Data.Either (Either(..), note)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split, trim)
import Data.Traversable (traverse)
import Game.Experiment (Experiment(..), ExperimentRequest)
import Data.Maybe (Maybe(..))
import Flags (Flag, checkedFlag, intFlagOf, optionalIntFlag, textFlag)
import Effects.Store (validRunId)
import Settings.Presets (defaultPresets)
import Cli.Session (Options)

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

-- | An experiment's flags, as typed.
type ExperimentFlags = { setting :: String, name :: String, preset :: String, rounds :: Maybe Int, seed :: Maybe Seed, db :: String, parameter :: String, values :: String }

-- | What an experiment runs, and the store it saves to.
type ExperimentOptions = { setting :: String, db :: String, request :: ExperimentRequest }

experimentFlags :: String -> Flag ExperimentFlags
experimentFlags setting = { setting, name: _, preset: _, rounds: _, seed: _, db: _, parameter: _, values: _ }
  <$> textFlag "--name" "set" "experiment"
  <*> textFlag "--preset" "name" defaultPresets.run
  <*> roundsFlag
  <*> seedFlag
  <*> textFlag "--db" "file" "runs.db"
  <*> textFlag "--parameter" "name" ""
  <*> textFlag "--values" "a,b" ""

-- | The request the flags ask for.
experimentOf :: ExperimentFlags -> Either String ExperimentOptions
experimentOf f
  | f.parameter == "" = Left "--parameter names the parameter to vary"
  | otherwise =
      do
        values <- traverse quantity (split (Pattern ",") f.values)
        pure
          { setting: f.setting
          , db: f.db
          , request: { setting: Nothing, name: f.name, preset: f.preset, playerSeed: map unwrap f.seed, rounds: f.rounds, experiment: Parameters [ { name: f.parameter, values } ] }
          }
      where
      quantity text = note ("--values must be decimal numbers, got " <> text) (Q.parse (trim text))

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
