-- | Writes each external service's OpenAPI document to `openapi/<service>.json`, for reading without a running server.
-- | With `--premise` it writes that Premise's documents to `openapi/<premise>` instead, where a reference lists the
-- | names of its World (`just openapi`).
module OpenApi (main) where

import Prelude

import Adapters.Exit (fail, usageError)
import Adapters.Filesystem (fileDevice)
import Core.Shape (prettyJson)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effects.Signatures (_files)
import Effects.System (files)
import Flags (Flag, parseWith, textFlag, usageOf)
import Handlers.System (interpretFiles)
import Node.Process (argv)
import Premises.Load (loadAll)
import Run (case_, on, run)
import Server.OpenApi (defaultServer, document, documentsOf, serviceName, services)

type Flags = { premise :: String, server :: String, out :: String }

flags :: Flag Flags
flags = { premise: _, server: _, out: _ }
  <$> textFlag "--premise" "id" ""
  <*> textFlag "--server" "url" defaultServer
  <*> textFlag "--out" "dir" ""

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseWith flags args of
    Left e -> usageError [ "usage: openapi " <> usageOf flags, "  e.g. just openapi meridian --out build/openapi" ] e
    Right f
      | f.premise == "" -> for_ services \s -> fileDevice.write ("openapi/" <> serviceName s <> ".json") (prettyJson (document s))
      | otherwise -> run (case_ # on _files (interpretFiles fileDevice)) (loadAll files) >>= case _ of
          Left e -> fail e
          Right premises -> case documentsOf premises { premise: f.premise, server: f.server } of
            Left e -> fail e
            Right documents -> for_ documents \(Tuple s doc) ->
              fileDevice.write ((if f.out == "" then "openapi/" <> f.premise else f.out) <> "/" <> serviceName s <> ".json") (prettyJson doc)
