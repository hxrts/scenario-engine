-- | Who may reach the API (docs/07_simulator.md §8): the secrets read from the server's `.env`, and the check
-- | each request passes before its operation runs. A secret that is not set locks what it guards.
module Gate
  ( Secrets
  , parseEnv
  , secretsFrom
  , HttpRequest
  , gated
  ) where

import Prelude

import Data.Array (any, filter, mapMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), indexOf, split, take, drop, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effects.Api (Reply)
import Foreign.Object (Object)
import Foreign.Object as Object
import Handlers.Api.Route (Access(..), Implementation, accessOf, failWith, route)

-- | The admin session value a login sets.
type Secrets = { session :: Maybe String }

-- | A `.env` file: one KEY=VALUE per line. Blank lines and lines starting with # are ignored.
parseEnv :: String -> Object String
parseEnv text = Object.fromFoldable (mapMaybe entry (filter wanted (map trim (split (Pattern "\n") text))))
  where
  wanted l = l /= "" && take 1 l /= "#"
  entry l = indexOf (Pattern "=") l <#> \i -> Tuple (trim (take i l)) (trim (drop (i + 1) l))

-- | The secrets of a parsed `.env`, given how a password becomes its session value.
secretsFrom :: (String -> String) -> Object String -> Secrets
secretsFrom digest env =
  { session: digest <<< (_ <> ":admin") <$> present "ADMIN_PASSWORD"
  }
  where
  present k = case Object.lookup k env of
    Just v | v /= "" -> Just v
    _ -> Nothing

type HttpRequest = { method :: String, path :: String, query :: Object String, body :: String, cookie :: String }

-- | Answer a request if its caller may make it. A path that names no endpoint is left to the route's 404.
gated :: Secrets -> Implementation -> HttpRequest -> Effect Reply
gated s impl r = case accessOf request of
  Just access | not (allowed access) -> pure (failWith 401 "this needs a login or a service token")
  _ -> route impl request
  where
  request = { method: r.method, path: r.path, query: r.query, body: r.body }
  admin = maybe false (\v -> any (_ == "admin=" <> v) (map trim (split (Pattern ";") r.cookie))) s.session
  allowed = case _ of
    Open _ -> true
    Admin -> admin
