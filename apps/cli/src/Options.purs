-- | The command-line options of `run` and `session` (docs/06 §6.2), from their parsed flags. A bad value
-- | is a usage error; `Main` reports it and exits 1.
module Options
  ( runOptions
  , sessionOptions
  , number
  , numberFrom
  , seedFlag
  ) where

import Prelude

import Adapters.Pi (defaultModel)
import Core.Draw (Seed(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Flags (flagOr, intFlag, optionalInt)
import Foreign.Object (Object)
import Handlers.Store (validRunId)
import Premises.Presets (defaultPresets)
import Session (Options)

runOptions :: Object String -> Either String { preset :: String, rounds :: Int, seed :: Maybe Seed }
runOptions fs = do
  rounds <- number "--rounds" 5 fs
  seed <- seedFlag fs
  pure { preset: flagOr "--preset" defaultPresets.run fs, rounds, seed }

-- | The player seed flag: absent means draw one at start; given, it must be a whole number (the one
-- | seed rule: `scripts/validate.sh` mirrors it, negative values included).
seedFlag :: Object String -> Either String (Maybe Seed)
seedFlag fs = map Seed <$> optionalInt "--seed" (const true) "a whole number" fs

sessionOptions :: String -> Object String -> Either String Options
sessionOptions premise fs = do
  rounds <- number "--rounds" 5 fs
  window <- numberFrom 0 "--window" 0 fs
  let id = flagOr "--id" "session-1" fs
  if validRunId id then pure unit else Left ("--id must be 1 to 64 letters, digits, '-' or '_', starting with a letter or digit, got " <> id)
  seed <- seedFlag fs
  pure { premise, model: flagOr "--model" defaultModel fs, humans: flagOr "--humans" "" fs, rounds, window, id, db: flagOr "--db" "runs.db" fs, seed }

-- | A whole-number flag of at least one, with its default.
number :: String -> Int -> Object String -> Either String Int
number = numberFrom 1

numberFrom :: Int -> String -> Int -> Object String -> Either String Int
numberFrom lowest name d = intFlag name d (_ >= lowest) ("a whole number of at least " <> show lowest)

