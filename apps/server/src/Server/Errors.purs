-- | The HTTP reply helpers and the error vocabulary of the read and write handlers: a JSON reply
-- | with its status, the 400 for a view that cannot be given, why a stored run cannot be served,
-- | and the decode error that names what failed.
module Server.Errors
  ( describe
  , ok
  , Answer
  , ApiError
  , status
  , message
  , apiReply
  , badRequest
  , notFound
  , conflict
  , internal
  , viewOr
  , replyOr
  ) where

import Prelude

import Core.Shape (Shape)
import Data.Either (Either(..), either)
import Effects.Api (Reply)
import Effects.Signatures (RunError(..))
import Handlers.Api.Route as Route

describe :: RunError -> String
describe = case _ of
  Missing -> "not found"
  Unreadable why -> why

-- | Why a request is refused. The status is total over the constructors, and the text is the whole message.
data ApiError
  = BadRequest String
  | NotFound String
  | Conflict String
  | Internal String

status :: ApiError -> Int
status = case _ of
  BadRequest _ -> 400
  NotFound _ -> 404
  Conflict _ -> 409
  Internal _ -> 500

message :: ApiError -> String
message = case _ of
  BadRequest m -> m
  NotFound m -> m
  Conflict m -> m
  Internal m -> m

-- | What a handler produces: a reply, or why it is refused.
type Answer = Either ApiError Reply

-- | The HTTP reply for an answer.
apiReply :: Answer -> Reply
apiReply = either (\e -> Route.failWith (status e) (message e)) identity

badRequest :: forall m. Applicative m => String -> m Answer
badRequest = pure <<< Left <<< BadRequest

notFound :: forall m. Applicative m => String -> m Answer
notFound = pure <<< Left <<< NotFound

conflict :: forall m. Applicative m => String -> m Answer
conflict = pure <<< Left <<< Conflict

internal :: forall m. Applicative m => String -> m Answer
internal = pure <<< Left <<< Internal

-- | A 200 reply through the reply's shape.
ok :: forall m a. Applicative m => Shape a -> a -> m Answer
ok s = pure <<< Right <<< Route.reply s 200

-- | A view, or a 400 with the reason it cannot be given (for example an out-of-range round).
viewOr :: forall m a. Applicative m => Shape a -> Either String a -> m Answer
viewOr s = either badRequest (ok s)

-- | A run's reply, or a 404 for a run that is missing and a 500 for one that cannot be read.
replyOr :: forall m a. Applicative m => String -> (a -> m Answer) -> Either RunError a -> m Answer
replyOr id f = case _ of
  Right a -> f a
  Left Missing -> notFound ("no run " <> id)
  Left (Unreadable why) -> internal ("run " <> id <> " is unreadable: " <> why)
