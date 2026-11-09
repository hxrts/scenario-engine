-- | Devices with nothing of the machine in them, for running a program under fixed handlers.
module Testkit.Devices (fixedSystem, memoryFiles, capturedConsole) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Handlers.System (ConsoleDevice, FileDevice, SystemDevices)

fixedSystem :: SystemDevices
fixedSystem = { now: pure "2026-01-01T00:00:00.000Z", drawSeed: pure 42, identity: pure { pid: "1", started: "0" } }

-- | Files held in a map, and the environment empty.
memoryFiles :: Ref (Map String String) -> FileDevice
memoryFiles files =
  { env: \_ -> pure Nothing
  , list: \path -> pure (Left { path, reason: "no directories in memory" })
  , read: \path -> note { path, reason: "no such file" } <<< Map.lookup path <$> Ref.read files
  , write: \path text -> Ref.modify_ (Map.insert path text) files
  }

-- | A terminal whose output is kept and whose input has ended.
capturedConsole :: Ref (Array String) -> ConsoleDevice
capturedConsole lines =
  { readLine: \_ -> throwError (error "input ended")
  , writeLine: \line -> Ref.modify_ (_ <> [ line ]) lines
  , closeInput: pure unit
  }
