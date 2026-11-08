module Http (serve) where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Effect (Effect)
import Effects.Api (Reply)
import Handlers.Api.Route (Request)

foreign import serveImpl :: Fn4 Int String String (Request -> Effect Reply) (Effect Unit)

-- | Serve on a port: requests whose path starts with the prefix go to the handler, the rest to the static directory.
serve :: Int -> String -> String -> (Request -> Effect Reply) -> Effect Unit
serve = runFn4 serveImpl
