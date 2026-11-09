-- | The server's entry point (docs/06_simulator.md §8–§10): parses the flags, opens the Run Store,
-- | and serves the `API` on the port through `Handlers.Api.Route`. The operations' semantics are in
-- | `Server.Implementation` (over `Server.Read`, `Server.Launch`, and `Server.Derive`); the views are
-- | computed in `Views`.
module Server (main) where

import Prelude

import Adapters.Filesystem (fileOps)
import Adapters.SQLite (open, sqliteBackend)
import Data.Array (drop)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Flags (parseWith, serveFlags, usageOf)
import Handlers.Api.Route (apiPrefix, route)
import Http (serve)
import Adapters.Exit (fail, usageError)
import Node.Process (argv)
import Settings.Load (loadAll)
import Server.Cache (newCache)
import Server.Launch (failInterrupted)
import Server.Implementation (implement)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseWith serveFlags args of
    Left e -> usage e
    Right flags -> loadAll fileOps >>= case _ of
      Right settings -> do
        store <- sqliteBackend <$> open flags.db
        stale <- failInterrupted store
        when (stale > 0) (log ("marked " <> show stale <> " interrupted run(s) failed"))
        cache <- newCache
        serve flags.port flags.static apiPrefix (route (implement settings store cache))
      Left e -> fail e
  where
  usage = usageError [ "usage: server " <> usageOf serveFlags, "  e.g. just serve --port 9000" ]
