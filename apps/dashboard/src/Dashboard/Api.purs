-- | Calls to the read API, decoded with the shared codecs.
module Dashboard.Api (get, post, command, remove, useApi, useFetch, encode, runPath) where

import Prelude

import Board.Decode.Common (readJson)

import Control.Monad.Error.Class (try)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (message)
import JSURI (encodeURIComponent)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks.Aff (UseAff, useAff)

foreign import fetchTextImpl :: String -> String -> String -> Effect (Promise String)

-- | A failed request (network error or a non-2xx reply) is a Left with the message, never an exception.
call :: forall a. DecodeJson a => String -> String -> String -> Aff (Either String a)
call method url body = do
  reply <- try (toAffE (fetchTextImpl method url body))
  pure case reply of
    Left e -> Left (message e)
    Right text -> readJson text

get :: forall a. DecodeJson a => String -> Aff (Either String a)
get url = call "GET" url ""

post :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff (Either String b)
post url body = call "POST" url (stringify (encodeJson body))

-- | A POST with no body (archive and unarchive).
command :: forall b. DecodeJson b => String -> Aff (Either String b)
command url = call "POST" url "{}"

-- | A DELETE (the URL carries the confirmation).
remove :: forall b. DecodeJson b => String -> Aff (Either String b)
remove url = call "DELETE" url ""

-- | Percent-encode a value interpolated into a URL (a run id, a market id).
encode :: String -> String
encode s = fromMaybe s (encodeURIComponent s)

-- | The API path of a run; the id is one encoded segment.
runPath :: String -> String
runPath id = "/api/runs/" <> encode id

-- | Fetch a view; refetches whenever the path (or the extra key) changes.
useApi :: forall a k. Eq k => DecodeJson a => k -> String -> Hook (UseAff (Tuple k String) (Either String a)) (Maybe (Either String a))
useApi k path = useAff (Tuple k path) (get path)

-- | Run an action (a fetch of any shape) once per key: refetches when the key changes.
useFetch :: forall a k. Eq k => k -> Aff a -> Hook (UseAff k a) (Maybe a)
useFetch k action = useAff k action
