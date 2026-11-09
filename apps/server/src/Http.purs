module Http (serve, AdminPage) where

import Prelude

import Data.Function.Uncurried (Fn5, runFn5)
import Effect (Effect)
import Effects.Api (Reply)
import Gate (HttpRequest)

foreign import serveImpl :: Fn5 Int String String AdminPage (HttpRequest -> Effect Reply) (Effect Unit)

-- | The password that opens `/admin` and the session value a login sets. Empty strings lock it.
type AdminPage = { password :: String, session :: String }

-- | Serve on a port: requests whose path starts with the prefix go to the handler, the rest to the static directory.
serve :: Int -> String -> String -> AdminPage -> (HttpRequest -> Effect Reply) -> Effect Unit
serve = runFn5 serveImpl
