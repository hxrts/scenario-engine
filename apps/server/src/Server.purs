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
import Flags (flagOr, intFlag, parseFlags)
import Handlers.Api.Route (apiPrefix, route)
import Http (serve)
import Node.Process (argv, exit')
import Premises.Load (loadFrom)
import Premises.Names (meridianDir)
import Server.Cache (newCache)
import Server.Launch (failInterrupted)
import Server.Implementation (implement)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseFlags [ "--db", "--port", "--static" ] args of
    Left e -> usage e
    -- the default port is also apps/dashboard/ports.mjs's API_PORT, which scripts/free-port reads
    Right flags -> case intFlag "--port" 8787 (\p -> p > 0 && p < 65536) "a port number" flags of
      Right port -> loadFrom fileOps meridianDir >>= case _ of
        Right ctx -> do
          store <- sqliteBackend <$> open (flagOr "--db" "runs.db" flags)
          stale <- failInterrupted store
          when (stale > 0) (log ("marked " <> show stale <> " interrupted run(s) failed"))
          cache <- newCache
          serve port (flagOr "--static" "apps/dashboard/dist" flags) apiPrefix (route (implement ctx store cache))
        Left e -> log e *> exit' 1
      Left e -> usage e
  where
  usage e = do
    log ("error: " <> e)
    log "usage: server [--db <file>] [--port <1-65535>] [--static <dir>]"
    exit' 1
