-- | The operating-system process device: this process's id and start time, and whether another process lives.
module Adapters.Process (processIdentity, processAlive) where

import Effect (Effect)

-- | This process's id and start time, as text. The start time is fixed for the process.
foreign import processIdentity :: Effect { pid :: String, started :: String }

-- | Whether a process is still running.
foreign import processAlive :: Int -> Effect Boolean
