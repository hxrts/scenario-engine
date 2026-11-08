-- | The client of the `API` row: answers each operation over a fetch device, text in and
-- | text out. The device is the composition root's (the dashboard's `fetch` FFI); this module names no
-- | URL except through `Handlers.Api.Route`, and its closure has no Node module, so it runs in a browser.
module Handlers.Api.Client
  ( Fetch
  , runApi
  , replyText
  ) where

import Prelude

import Core.Shape (Format(..), parse)
import Data.Either (Either(..), either)
import Effect.Aff (Aff, attempt)
import Effect.Exception (message)
import Effects.Api (API, ApiF(..), Reply, _apiCall)
import Handlers.Api.Route (Wire, errorShape)
import Handlers.Api.Route as Route
import Run (Run, case_, on, run)
import Type.Row (type (+))

-- | The fetch device: sends a request and resolves to its reply. A failed request resolves with status 0
-- | and the failure's message (the device never throws).
type Fetch = Wire -> Aff Reply

-- | Run API operations over a fetch device.
runApi :: forall a. Fetch -> Run (API + ()) a -> Aff a
runApi fetch = run (case_ # on _apiCall (\(Call req k) -> k <$> sendOver fetch req))
  where
  sendOver f req = do
    r <- attempt (f (Route.wire req))
    pure (either (\e -> { status: 0, body: message e }) identity r)

-- | The reply's body when it is a success (2xx); otherwise the error the reply carries: the `error` field
-- | of its JSON body, or its text, or its status.
replyText :: Reply -> Either String String
replyText r
  | r.status >= 200 && r.status < 300 = Right r.body
  | otherwise = Left (errorOf r)
      where
      errorOf reply = case parse Snapshot errorShape reply.body of
        Right { error } -> error
        Left _ -> if reply.body == "" then "HTTP " <> show reply.status else reply.body
