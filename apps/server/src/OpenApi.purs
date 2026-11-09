-- | Writes each external service's OpenAPI document to `openapi/<service>.json`, for reading without a running server.
module OpenApi (main) where

import Prelude

import Adapters.Filesystem (fileDevice)
import Core.Shape (prettyJson)
import Data.Foldable (for_)
import Effect (Effect)
import Server.OpenApi (document, serviceName, services)

main :: Effect Unit
main = for_ services \s -> fileDevice.write ("openapi/" <> serviceName s <> ".json") (prettyJson (document s))
