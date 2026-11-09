-- | The server's composition root (docs/06_simulator.md §8–§10): parses the flags, loads the premises, opens the
-- | Run Store, and serves the `API` on the port through `Handlers.Api.Route`. The operations are programs in
-- | `Server.Implementation`; `Server.Stack` closes them over the devices.
module Server (main) where

import Prelude

import Adapters.Exit (fail, usageError)
import Adapters.Filesystem (fileDevice)
import Data.Array (drop)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effects.Signatures (_files)
import Effects.System (files)
import Flags (parseWith, serveFlags, usageOf)
import Handlers.Api.Route (apiPrefix)
import Gate (gated, parseEnv, secretsFrom)
import Adapters.Digest (sha256)
import Data.Maybe (fromMaybe)
import Foreign.Object as Object
import Handlers.Store (runStore)
import Handlers.System (interpretFiles)
import Http (serve)
import Node.Process (argv)
import Run (case_, on, run)
import Server.Launch (startup)
import Server.Stack (implement, localStore, newServerCache, openDatabase)
import Premises.Load (loadAll)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseWith serveFlags args of
    Left e -> usage e
    Right flags -> run (case_ # on _files (interpretFiles fileDevice)) (loadAll files) >>= case _ of
      Right premises -> do
        env <- run (case_ # on _files (interpretFiles fileDevice)) (readEnv flags.env)
        let secrets = secretsFrom sha256 env
        store <- localStore <$> openDatabase flags.db
        runStore store startup >>= flip for_ log
        cache <- newServerCache
        serve flags.port flags.static apiPrefix { password: fromMaybe "" (Object.lookup "ADMIN_PASSWORD" env), session: fromMaybe "" secrets.session } (gated secrets (implement premises store cache))
      Left e -> fail e
  where
  -- a missing file is an empty one: every secret is unset, and what they guard stays locked
  readEnv path = either (const Object.empty) parseEnv <$> files.read path
  usage = usageError [ "usage: server " <> usageOf serveFlags, "  e.g. just serve --port 9000" ]
