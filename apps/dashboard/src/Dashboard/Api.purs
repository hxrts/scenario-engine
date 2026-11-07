-- | The low-level requests of the read and write API, decoded with the shared codecs, and the one
-- | fetch hook the pages use. The paths are not here: `Dashboard.Client` names every endpoint.
module Dashboard.Api (get, post, command, remove, encode, Loaded, useLoad) where

import Prelude

import Schema.Decode.Common (readJson)

import Control.Monad.Error.Class (try)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (message)
import JSURI (encodeURIComponent)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useSteppingAff)

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

-- | A load's state: the value shown, and whether it is the last loaded value while the current key
-- | loads (`busy`). A key that has not loaded yet shows nothing.
type Loaded a = { value :: Maybe a, busy :: Boolean }

-- | Load an action for a key. A new key supersedes the one in flight (its request is cancelled), and
-- | the last loaded value stays on screen, marked busy, until the new key's value arrives.
useLoad :: forall a k. Eq k => k -> Aff a -> Hook (UseAff k (Tuple k a)) (Loaded a)
useLoad k action = React.do
  loaded <- useSteppingAff k (Tuple k <$> action)
  pure case loaded of
    Nothing -> { value: Nothing, busy: true }
    Just (Tuple k' a) -> { value: Just a, busy: k' /= k }
