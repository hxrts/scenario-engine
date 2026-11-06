-- | Calls to the read API, decoded with the shared codecs.
module Dashboard.Api (get, post, useApi) where

import Prelude

import Board.Decode (readJson)

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks.Aff (UseAff, useAff)

foreign import fetchTextImpl :: String -> String -> String -> Effect (Promise String)

call :: forall a. DecodeJson a => String -> String -> String -> Aff (Either String a)
call method url body = readJson <$> toAffE (fetchTextImpl method url body)

get :: forall a. DecodeJson a => String -> Aff (Either String a)
get url = call "GET" url ""

post :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff (Either String b)
post url body = call "POST" url (stringify (encodeJson body))

-- | Fetch a view; refetches whenever the path (or the extra key) changes.
useApi :: forall a k. Eq k => DecodeJson a => k -> String -> Hook (UseAff (Tuple k String) (Either String a)) (Maybe (Either String a))
useApi k path = useAff (Tuple k path) (get path)
