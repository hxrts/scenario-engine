-- | Calls to the read API, decoded with the shared codecs.
module Dashboard.Api (get, post, useApi) where

import Prelude

import Board.Decode (readJson)

import Control.Monad.Error.Class (try)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (message)
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

-- | Fetch a view; refetches whenever the path (or the extra key) changes.
useApi :: forall a k. Eq k => DecodeJson a => k -> String -> Hook (UseAff (Tuple k String) (Either String a)) (Maybe (Either String a))
useApi k path = useAff (Tuple k path) (get path)
