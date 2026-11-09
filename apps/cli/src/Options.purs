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
import Core.Types (Id(..))
import Data.Int as Int
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
type ExperimentFlags = { setting :: String, name :: String, preset :: String, rounds :: Maybe Int, seed :: Maybe Seed, db :: String, parameter :: String, values :: String, combine :: String, seat :: String, alternatives :: String, playerSeeds :: String, gameSeeds :: String }

-- | What an experiment runs, and the store it saves to.
type ExperimentOptions = { setting :: String, db :: String, request :: ExperimentRequest }

experimentFlags :: String -> Flag ExperimentFlags
experimentFlags setting = { setting, name: _, preset: _, rounds: _, seed: _, db: _, parameter: _, values: _, combine: _, seat: _, alternatives: _, playerSeeds: _, gameSeeds: _ }
  <$> textFlag "--name" "set" "experiment"
  <*> textFlag "--preset" "name" defaultPresets.run
  <*> roundsFlag
  <*> seedFlag
  <*> textFlag "--db" "file" "runs.db"
  <*> textFlag "--parameter" "name" ""
  <*> textFlag "--values" "a,b" ""
  <*> textFlag "--combine" "a=1,2;b=3,4" ""
  <*> textFlag "--seat" "id" ""
  <*> textFlag "--alternatives" "A,B" ""
  <*> textFlag "--player-seeds" "1,2" ""
  <*> textFlag "--game-seeds" "1,2" ""

-- | The request the flags ask for.
experimentOf :: ExperimentFlags -> Either String ExperimentOptions
experimentOf f = do
  experiment <- case f.parameter /= "", f.combine /= "", f.seat /= "", f.playerSeeds /= "", f.gameSeeds /= "" of
    true, false, false, false, false -> (\values -> Parameters [ { name: f.parameter, values } ]) <$> traverse quantity (list f.values)
    false, true, false, false, false -> Parameters <$> traverse axis (split (Pattern ";") f.combine)
    false, false, true, false, false -> Right (SeatPolicy { seat: Id f.seat, alternatives: list f.alternatives })
    false, false, false, true, false -> PlayerSeeds <$> traverse whole (list f.playerSeeds)
    false, false, false, false, true -> GameSeeds <$> traverse whole (list f.gameSeeds)
    _, _, _, _, _ -> Left "name one of --parameter, --combine, --seat, --player-seeds or --game-seeds"
  pure
    { setting: f.setting
    , db: f.db
    , request: { setting: Nothing, name: f.name, preset: f.preset, playerSeed: map unwrap f.seed, rounds: f.rounds, experiment }
    }
  where
  list = map trim <<< split (Pattern ",")
  quantity text = note ("values must be decimal numbers, got " <> text) (Q.parse text)
  whole text = note ("seeds must be whole numbers, got " <> text) (Int.fromString text)
  axis text = case split (Pattern "=") text of
    [ name, values ] -> (\vs -> { name: trim name, values: vs }) <$> traverse quantity (list values)
    _ -> Left ("--combine takes name=values;name=values, got " <> text)

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
