-- | File-system helpers for the Run Store tests: a store file is created, checked, and removed.
module Test.Adapters.Files (fileExists, removeFile, createV1Store) where

import Prelude

import Effect (Effect)

foreign import fileExists :: String -> Effect Boolean

foreign import removeFile :: String -> Effect Unit

foreign import createV1StoreImpl :: String -> Effect Unit

-- | A store file as schema version 1 wrote it, with one run (run-1), for upgrade tests.
createV1Store :: String -> Effect Unit
createV1Store = createV1StoreImpl
