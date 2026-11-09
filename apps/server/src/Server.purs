-- | The server's composition root (docs/06_simulator.md §8–§10): parses the flags, loads the settings, opens the
-- | Run Store, and serves the `API` on the port through `Handlers.Api.Route`. The operations are programs in
-- | `Server.Implementation`; `Server.Stack` closes them over the devices.
module Server (main) where

import Prelude

import Adapters.Exit (fail, usageError)
import Adapters.Filesystem (fileDevice)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effects.Signatures (_files)
import Effects.System (files)
import Flags (parseWith, serveFlags, usageOf)
import Handlers.Api.Route (apiPrefix, route)
import Handlers.Store (runStore)
import Handlers.System (interpretFiles)
import Http (serve)
import Node.Process (argv)
import Run (case_, on, run)
import Server.Launch (startup)
import Server.Stack (implement, localStore, newServerCache, openDatabase)
import Settings.Load (loadAll)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseWith serveFlags args of
    Left e -> usage e
    Right flags -> run (case_ # on _files (interpretFiles fileDevice)) (loadAll files) >>= case _ of
      Right settings -> do
        store <- localStore <$> openDatabase flags.db
        runStore store startup >>= flip for_ log
        cache <- newServerCache
        serve flags.port flags.static apiPrefix (route (implement settings store cache))
      Left e -> fail e
  where
  usage = usageError [ "usage: server " <> usageOf serveFlags, "  e.g. just serve --port 9000" ]
