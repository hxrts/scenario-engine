-- | File-system and process helpers for the adapter tests: Run Store files, scratch directories, a
-- | fake `pi` on the PATH (the process boundary without a model), and plain text reads and writes.
module Test.Adapters.Files
  ( removeFile
  , createStoreAtVersion
  , setRawSpec
  , tempDir
  , removeTree
  , readFileText
  , writeFileText
  , installPi
  , restorePath
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (chmod, mkdir', mkdtemp, readTextFile, rm', writeTextFile)
import Node.Path (dirname)
import Node.Process (lookupEnv, setEnv)

removeFile :: String -> Effect Unit
removeFile path = rm' path { force: true, maxRetries: 0, recursive: false, retryDelay: 100 }

-- | A store file at a given schema version, for the refusal tests.
foreign import createStoreAtVersion :: String -> Int -> Effect Unit

-- | Overwrite a run's stored spec with raw text, below the typed port: the store then holds text that
-- | does not decode, which the port must report as unreadable in that row.
foreign import setRawSpec :: String -> String -> String -> Effect Unit

-- | A new empty scratch directory under the system temporary directory.
tempDir :: Effect String
tempDir = do
  root <- fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  mkdtemp (root <> "/sp-adapters-")

-- | Remove a directory tree, if it exists.
removeTree :: String -> Effect Unit
removeTree path = rm' path { force: true, maxRetries: 0, recursive: true, retryDelay: 100 }

readFileText :: String -> Effect String
readFileText = readTextFile UTF8

writeFileText :: String -> String -> Effect Unit
writeFileText path text = do
  mkdir' (dirname path) { recursive: true, mode: permsAll }
  writeTextFile UTF8 path text

-- | Write an executable `pi` script (a shell body) into `binDir`, and put `binDir` first on the PATH.
-- | Returns the PATH it replaced, for `restorePath`.
installPi :: String -> String -> Effect String
installPi binDir body = do
  let file = binDir <> "/pi"
  writeFileText file ("#!/bin/sh\n" <> body <> "\n")
  chmod file permsAll
  old <- fromMaybe "" <$> lookupEnv "PATH"
  setEnv "PATH" (binDir <> ":" <> old)
  pure old

restorePath :: String -> Effect Unit
restorePath = setEnv "PATH"
