-- | The HTTP reply helpers and the error vocabulary of the read and write handlers: a JSON reply
-- | with its status, the 400 for a view that cannot be given, why a stored run cannot be served,
-- | and the decode error that names what failed.
module Server.Errors
  ( LoadError(..)
  , describe
  , json
  , ok
  , failWith
  , viewOr
  , decodeAs
  ) where

import Prelude

import Schema.Decode.Common (readJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..), either)
import Effect (Effect)
import Http (Response)

-- | Why a stored run cannot be served: it is not in the store, or it is there but unreadable.
data LoadError = Missing | Unreadable String

describe :: LoadError -> String
describe = case _ of
  Missing -> "not found"
  Unreadable why -> why

json :: forall a. EncodeJson a => Int -> a -> Effect Response
json status a = pure { status, body: stringify (encodeJson a) }

ok :: forall a. EncodeJson a => a -> Effect Response
ok = json 200

failWith :: Int -> String -> Effect Response
failWith status error = json status { error }

-- | A view, or a 400 with the reason it cannot be given (for example an out-of-range round).
viewOr :: forall a. EncodeJson a => Either String a -> Effect Response
viewOr = either (failWith 400) ok

-- | Decode a stored text, naming what it was when it fails.
decodeAs :: forall a. DecodeJson a => String -> String -> Either String a
decodeAs what text = either (\e -> Left (what <> ": " <> e)) Right (readJson text)
