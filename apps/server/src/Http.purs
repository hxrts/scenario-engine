module Http (Request, Response, serve) where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Effect (Effect)
import Foreign.Object (Object)

type Request = { method :: String, path :: String, query :: Object String, body :: String }

type Response = { status :: Int, body :: String }

foreign import serveImpl :: Fn4 Int String String (Request -> Effect Response) (Effect Unit)

-- | Serve on a port: requests whose path starts with the prefix go to the handler, the rest to the static directory.
serve :: Int -> String -> String -> (Request -> Effect Response) -> Effect Unit
serve = runFn4 serveImpl
