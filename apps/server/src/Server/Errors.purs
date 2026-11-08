-- | The HTTP reply helpers and the error vocabulary of the read and write handlers: a JSON reply
-- | with its status, the 400 for a view that cannot be given, why a stored run cannot be served,
-- | and the decode error that names what failed.
module Server.Errors
  ( describe
  , ok
  , failWith
  , viewOr
  , replyOr
  ) where

import Prelude

import Core.Shape (Shape)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effects.Api (Reply)
import Effects.Signatures (RunError(..))
import Handlers.Api.Route as Route

describe :: RunError -> String
describe = case _ of
  Missing -> "not found"
  Unreadable why -> why

-- | A 200 reply through the reply's shape.
ok :: forall a. Shape a -> a -> Effect Reply
ok s = pure <<< Route.reply s 200

failWith :: Int -> String -> Effect Reply
failWith status = pure <<< Route.failWith status

-- | A view, or a 400 with the reason it cannot be given (for example an out-of-range round).
viewOr :: forall a. Shape a -> Either String a -> Effect Reply
viewOr s = either (failWith 400) (ok s)

-- | A run's reply, or a 404 for a run that is missing and a 500 for one that cannot be read.
replyOr :: forall a. String -> (a -> Effect Reply) -> Either RunError a -> Effect Reply
replyOr id f = case _ of
  Right a -> f a
  Left Missing -> failWith 404 ("no run " <> id)
  Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)
