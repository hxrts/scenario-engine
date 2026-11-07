-- | The `API` row (docs/06_simulator.md §8–§10, work/4.md §7.4): one operation per route of the
-- | dashboard's HTTP API. A request is a value of `ApiRequest`: its path segments, query, and body, as
-- | typed fields. A reply is the status and the body text the route answers with. `Handlers.Api.Route`
-- | maps each operation to its method and path and decodes the reply's status; `Handlers.Api.Client`
-- | answers it over a fetch device. The reply's body is the JSON of a view model the producer encodes.
-- | Those view models live in `views`, which `effects` may not import (work/4.md §9), so the body stays
-- | text here and each consumer decodes it.
module Effects.Api
  ( ApiRequest(..)
  , Launch
  , Reply
  , ApiF(..)
  , API
  , call
  ) where

import Prelude

import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Maybe (Maybe)

-- | `POST /api/runs` body: every seat plays one preset. An absent or null `playerSeed` is drawn once
-- | at launch; the reply carries the seed used either way.
type Launch = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | The reply to a request: its HTTP status, and its body (JSON text).
type Reply = { status :: Int, body :: String }

-- | One operation per route. Query values and bodies that the routes validate (the round, the seed, the
-- | confirmation, a derivation's fields) are carried as the text the request gave, so each route's
-- | validation and its messages stay with the route's semantics. A route's body is decoded as typed
-- | data where the body is a fixed record (`Launch`, a resume's round count).
data ApiRequest
  = ListRuns { archived :: Boolean }
  | Scenario
  | Glossary
  | LaunchRun Launch
  | Entities String
  | Entity String String
  | RunView { id :: String, view :: String, round :: Maybe String }
  | Compare { runs :: Array String, market :: Maybe String }
  | Agency { playerSeed :: Maybe String, preset :: Maybe String, alternative :: Maybe String }
  | Derive { id :: String, body :: String }
  | Resume { id :: String, rounds :: Int }
  | Archive String
  | Unarchive String
  | Delete { id :: String, confirm :: String }

-- | A request to the API and its reply.
data ApiF a = Call ApiRequest (Reply -> a)

derive instance Functor ApiF

type API r = (apiCall :: ApiF | r)

apiCall :: Proxy "apiCall"
apiCall = Proxy

-- | Send a request; the reply's status and body.
call :: forall r. ApiRequest -> Run (API + r) Reply
call req = lift apiCall (Call req identity)
