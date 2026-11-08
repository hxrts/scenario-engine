-- | The filesystem device: reads a text file, lists a directory, opens a file to append lines to, and
-- | looks up an environment variable. It knows no file layout; the paths are the caller's. A failure names
-- | the path it failed on.
module Adapters.Filesystem (fileOps, lineSink) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (Error, message, try)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync (appendTextFile, mkdir', readTextFile, readdir)
import Node.Path (dirname)
import Node.Process (lookupEnv)

-- | The device's operations: an environment variable, a directory's names, and a file's text.
fileOps
  :: { env :: String -> Effect (Maybe String)
     , list :: String -> Effect (Either { path :: String, reason :: String } (Array String))
     , read :: String -> Effect (Either { path :: String, reason :: String } String)
     }
fileOps = { env: lookupEnv, list: \path -> map (failed path) (try (readdir path)), read: \path -> map (failed path) (try (readTextFile UTF8 path)) }

-- | A sink that appends lines to a file. The file's directory is created here, once, not per line.
lineSink :: String -> Effect (String -> Effect Unit)
lineSink path = do
  mkdir' (dirname path) { recursive: true, mode: mkPerms all all all }
  pure \line -> appendTextFile UTF8 path (line <> "\n")

failed :: forall a. String -> Either Error a -> Either { path :: String, reason :: String } a
failed path = lmap (\ex -> { path, reason: message ex })
