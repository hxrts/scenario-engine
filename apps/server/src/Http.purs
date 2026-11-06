module Http (Request, Response, serve) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Foreign.Object (Object)

type Request = { method :: String, path :: String, query :: Object String, body :: String }

type Response = { status :: Int, body :: String }

foreign import serveImpl :: Fn3 Int String (Request -> Effect Response) (Effect Unit)

serve :: Int -> String -> (Request -> Effect Response) -> Effect Unit
serve = runFn3 serveImpl
