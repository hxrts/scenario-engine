-- | File-system and process helpers for the adapter tests: Run Store files, scratch directories, a
-- | fake `pi` on the PATH (the process boundary without a model), and plain text reads and writes.
module Test.Adapters.Files
  ( fileExists
  , removeFile
  , createV1Store
  , setRawSpec
  , tempDir
  , removeTree
  , readFileText
  , writeFileText
  , installPi
  , restorePath
  ) where

import Prelude

import Effect (Effect)

foreign import fileExists :: String -> Effect Boolean

foreign import removeFile :: String -> Effect Unit

foreign import createV1StoreImpl :: String -> Effect Unit

-- | A store file as schema version 1 wrote it, with one run (run-1), for upgrade tests.
createV1Store :: String -> Effect Unit
createV1Store = createV1StoreImpl

foreign import setRawSpecImpl :: String -> String -> String -> Effect Unit

-- | Overwrite a run's stored spec with raw text, below the typed port: the store then holds text that
-- | does not decode, which the port must report as unreadable in that row.
setRawSpec :: String -> String -> String -> Effect Unit
setRawSpec = setRawSpecImpl

-- | A new empty scratch directory under the system temporary directory.
foreign import tempDir :: Effect String

-- | Remove a directory tree, if it exists.
foreign import removeTree :: String -> Effect Unit

foreign import readFileText :: String -> Effect String

foreign import writeFileText :: String -> String -> Effect Unit

-- | Write an executable `pi` script (a shell body) into `binDir`, and put `binDir` first on the PATH.
-- | Returns the PATH it replaced, for `restorePath`.
foreign import installPi :: String -> String -> Effect String

foreign import restorePath :: String -> Effect Unit
