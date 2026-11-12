-- | The `API` row (docs/07_simulator.md §8–§10): one operation per route of the
-- | dashboard's HTTP API. A request is a value of `ApiRequest`: its path segments, query, and body, as
-- | typed fields. A reply is the status and the body text the route answers with. `Handlers.Api.Route`
-- | maps each operation to its method and path and decodes the reply's status; `Handlers.Api.Client`
-- | answers it over a fetch device. The reply's body is the JSON of a view model the producer encodes.
-- | Those view models live in `views`, which `effects` may not import, so the body stays
-- | text here and each consumer decodes it.
module Effects.Api
  ( ApiRequest(..)
  , ApiF(..)
  , StartLive
  , Metadata
  , Submission
  , Reply
  , API
  , call
  , _apiCall
  ) where

import Prelude

import Schema.View (View)
import Schema.Cast (Cast)
import Foreign.Object (Object)
import Schema.Run (RunClass)
import Data.Generic.Rep (class Generic)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

import Data.Maybe (Maybe)

-- | The reply to a request: its HTTP status, and its body (JSON text).
type Reply = { status :: Int, body :: String }

-- | One operation per route. A query value a route part can read (the round) is typed; other query values the routes validate (the seed, the confirmation)
-- | are carried as the text the request gave, so each route's validation and its messages stay with
-- | the route's semantics. A body is decoded as typed data before the operation runs.
-- | The body of a request to host a live session.
-- | A live run's metadata: what it is, who runs it, when and where, a player's name for each human seat (by seat id),
-- | and notes. A production run needs every field but the notes; a mock run takes what it is given.
type Metadata =
  { name :: Maybe String
  , facilitator :: Maybe String
  , date :: Maybe String
  , venue :: Maybe String
  , players :: Object String
  , notes :: Maybe String
  }

-- | The body of `POST /api/live`: the composition (absent is the first), the cast (absent casts every configured
-- | seat as an agent), the rounds, the window in seconds (absent waits for every seat), the player seed (absent draws
-- | one), the tag (`mock` or `production`), the metadata, and whether to clear a run that is live first.
type StartLive =
  { composition :: Maybe String
  , cast :: Maybe Cast
  , rounds :: Maybe Int
  , window :: Maybe Int
  , playerSeed :: Maybe Int
  , tag :: String
  , metadata :: Metadata
  , replace :: Maybe Boolean
  }

-- | One seat's text for one round.
type Submission = { seat :: String, round :: Int, text :: String }

data ApiRequest
  = ListRuns { runClass :: Maybe RunClass }
  | Premise
  | Glossary
  | Actors String
  | Actor String String
  | BalanceSheetLinks String
  | BalanceSheet { id :: String, token :: String }
  | RunView { id :: String, view :: View, round :: Maybe Int }
  | RoundReport { id :: String, round :: Int }
  | OpenApi String
  | Compare { runs :: Array String, exchange :: Maybe String }
  | StartLive StartLive
  | GetLive
  | SubmitTranscript Submission -- a seat's own text
  | SubmitManual Submission -- a facilitator's text for a human seat
  | AdvanceLive
  | PauseLive
  | ResumeLive
  | EndLive
  | ClearLive

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
