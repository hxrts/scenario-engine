-- | The dashboard's fetch device and the load hook the pages use. The device is the one FFI of the
-- | API client (`Api.js`): it sends a request and resolves to its reply, and `Handlers.Api.Client` does
-- | the rest. The operations are in `Dashboard.Client`.
module Dashboard.Api (fetch, Loaded, useLoad) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Handlers.Api.Client (Fetch)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useSteppingAff)
import Effects.Api (Reply)

foreign import fetchImpl :: String -> String -> String -> Effect (Promise Reply)

-- | The fetch device: a request's reply, from the browser's fetch.
fetch :: Fetch
fetch w = toAffE (fetchImpl w.method w.url w.body)

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
