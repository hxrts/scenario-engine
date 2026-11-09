-- | Smart constructors for the system rows (`CLOCK`, `ENTROPY`, `PROCESS`, `FILES`, `CONSOLE` in
-- | `Effects.Signatures`), and the two programs built only from them: the player seed and a run's owner.
module Effects.System
  ( now
  , drawSeed
  , resolveSeed
  , identify
  , ownerOf
  , files
  , writeFile
  , readLine
  , writeLine
  , closeInput
  ) where

import Prelude

import Core.Draw (Seed(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effects.Signatures (CLOCK, CONSOLE, ClockF(..), ConsoleF(..), ENTROPY, EntropyF(..), FILES, FileError, FilesF(..), PROCESS, ProcessF(..), ProcessIdentity, _clock, _console, _entropy, _files, _process)
import Run (Run, lift)
import Type.Row (type (+))

now :: forall r. Run (CLOCK + r) String
now = lift _clock (Now identity)

drawSeed :: forall r. Run (ENTROPY + r) Int
drawSeed = lift _entropy (DrawSeed identity)

-- | A run's player seed (docs/06_simulator.md §5): as given, and only an absent one is drawn. Everything
-- | after this is a keyed draw on the result, so the caller records it and the run replays from it.
resolveSeed :: forall r. Maybe Seed -> Run (ENTROPY + r) Seed
resolveSeed = case _ of
  Just s -> pure s
  Nothing -> Seed <$> drawSeed

identify :: forall r. Run (PROCESS + r) ProcessIdentity
identify = lift _process (Identify identity)

-- | The owner token of this process in a role, written on every run it creates (docs/06_simulator.md
-- | §6.1): the role, the process id, and the process start time.
ownerOf :: forall r. String -> Run (PROCESS + r) String
ownerOf role = identify <#> \p -> role <> ":" <> p.pid <> ":" <> p.started

-- | The reads a setting tree is loaded through: `Settings.Tree.FileOps` at `Run (FILES + r)`.
files
  :: forall r
   . { env :: String -> Run (FILES + r) (Maybe String)
     , list :: String -> Run (FILES + r) (Either FileError (Array String))
     , read :: String -> Run (FILES + r) (Either FileError String)
     }
files =
  { env: \name -> lift _files (Env name identity)
  , list: \path -> lift _files (List path identity)
  , read: \path -> lift _files (ReadFile path identity)
  }

writeFile :: forall r. String -> String -> Run (FILES + r) Unit
writeFile path text = lift _files (WriteFile path text unit)

readLine :: forall r. String -> Run (CONSOLE + r) String
readLine question = lift _console (ReadLine question identity)

writeLine :: forall r. String -> Run (CONSOLE + r) Unit
writeLine line = lift _console (WriteLine line unit)

closeInput :: forall r. Run (CONSOLE + r) Unit
closeInput = lift _console (CloseInput unit)
