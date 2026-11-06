-- | File-system helpers for the Run Store tests: a store file is created, checked, and removed.
module Test.Adapters.Files (fileExists, removeFile) where

import Prelude

import Effect (Effect)

foreign import fileExists :: String -> Effect Boolean

foreign import removeFile :: String -> Effect Unit
