-- | The server's entry point (docs/06_simulator.md §8–§10): parses the flags, opens the Run Store,
-- | and serves the routes of `Server.Routes` on the port. The handlers live in `Server.Read`,
-- | `Server.Launch`, and `Server.Derive`; the views are computed in `Views`.
module Server (main) where

import Prelude

import Adapters.Filesystem (fileOps)
import Premises.Tree (readPremise)
import Adapters.SQLite (open, sqliteBackend)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Effect (Effect)
import Effect.Console (log)
import Simulator.Options (flagOr, parseFlags)
import Http (serve)
import Node.Process (argv, exit')
import Premises.Load (loadPremise)
import Server.Launch (failInterrupted)
import Server.Routes (route)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseFlags [ "--db", "--port", "--static" ] args of
    Left e -> usage e
    Right flags -> case Int.fromString (flagOr "--port" "8787" flags) of
      Just port | port > 0 && port < 65536 -> map (_ >>= loadPremise) (readPremise fileOps "meridian/v0") >>= case _ of
        Right ctx -> do
          store <- sqliteBackend <$> open (flagOr "--db" "runs.db" flags)
          stale <- failInterrupted store
          when (stale > 0) (log ("marked " <> show stale <> " interrupted run(s) failed"))
          serve port (flagOr "--static" "apps/dashboard/dist" flags) (route ctx store)
          log ("serving on :" <> show port)
        Left e -> log e *> exit' 1
      _ -> usage ("--port must be a port number, got " <> flagOr "--port" "" flags)
  where
  usage e = do
    log ("error: " <> e)
    log "usage: server [--db <file>] [--port <1-65535>] [--static <dir>]"
    exit' 1
