-- | The `API` row (docs/06_simulator.md §8–§10): one operation per route of the
-- | dashboard's HTTP API. A request is a value of `ApiRequest`: its path segments, query, and body, as
-- | typed fields. A reply is the status and the body text the route answers with. `Handlers.Api.Route`
-- | maps each operation to its method and path and decodes the reply's status; `Handlers.Api.Client`
-- | answers it over a fetch device. The reply's body is the JSON of a view model the producer encodes.
-- | Those view models live in `views`, which `effects` may not import, so the body stays
-- | text here and each consumer decodes it.
module Effects.Api
  ( ApiRequest(..)
  , ApiF(..)
  , Launch
  , LaunchBatch
  , DeriveRequest
  , AgencyQuery
  , Reply
  , API
  , call
  , _apiCall
  ) where

import Prelude

import Schema.Run (DeriveKind)
import Schema.View (View)
import Data.Generic.Rep (class Generic)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)

-- | `POST /api/runs` body: every seat plays one preset. An absent or null `playerSeed` is drawn once
-- | at launch; the reply carries the seed used either way. `setting` names one of `GET /api/setting`; absent
-- | is the first.
type Launch = { setting :: Maybe String, name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | `POST /api/batch` body: one run of one preset on every seat of each named Setting, or of every loaded
-- | one when none is named, so their results can be compared.
type LaunchBatch = { settings :: Maybe (Array String), preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | `POST /api/runs/:id/derive` body: the kind, the new run's name, and what the kind may change. A
-- | field left absent keeps the source's value. `playerSeed` is doubly optional: absent keeps the
-- | source's seed, and an explicit null draws a new one. `overrides` stays JSON because the override
-- | shape belongs to `game`, which this package may not import; `Game.Recipe` decodes it.
type DeriveRequest =
  { kind :: DeriveKind
  , name :: Maybe String
  , preset :: Maybe String
  , playerSeed :: Maybe (Maybe Int)
  , rounds :: Maybe Int
  , overrides :: Maybe Json
  }

-- | Which agency analysis: the setting, the player seed, and the two presets. An absent one takes its default.
type AgencyQuery = { setting :: Maybe String, playerSeed :: Maybe String, preset :: Maybe String, alternative :: Maybe String }

-- | The reply to a request: its HTTP status, and its body (JSON text).
type Reply = { status :: Int, body :: String }

-- | One operation per route. A query value a route part can read (the round) is typed; other query values the routes validate (the seed, the confirmation)
-- | are carried as the text the request gave, so each route's validation and its messages stay with
-- | the route's semantics. A body is decoded as typed data before the operation runs.
data ApiRequest
  = ListRuns { archived :: Boolean }
  | Setting
  | Glossary
  | LaunchRun Launch
  | LaunchBatch LaunchBatch
  | Entities String
  | Entity String String
  | RunView { id :: String, view :: View, round :: Maybe Int }
  | Compare { runs :: Array String, market :: Maybe String }
  | Agency AgencyQuery
  | AgencyDetail AgencyQuery
  | Derive { id :: String, body :: DeriveRequest }
  | Resume { id :: String, rounds :: Int }
  | Step String
  | Archive String
  | Unarchive String
  | Delete { id :: String, confirm :: String }

derive instance Eq ApiRequest
derive instance Generic ApiRequest _

-- | A request to the API and its reply.
data ApiF a = Call ApiRequest (Reply -> a)

derive instance Functor ApiF

type API r = (apiCall :: ApiF | r)

_apiCall :: Proxy "apiCall"
_apiCall = Proxy

-- | Send a request; the reply's status and body.
call :: forall r. ApiRequest -> Run (API + r) Reply
call req = lift _apiCall (Call req identity)
