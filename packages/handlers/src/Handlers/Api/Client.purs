-- | The client of the `API` row: answers each operation over a fetch device, text in and
-- | text out. The device is the composition root's (the dashboard's `fetch` FFI); this module names no
-- | URL except through `Handlers.Api.Route`, and its closure has no Node module, so it runs in a browser.
module Handlers.Api.Client
  ( Fetch
  , runApi
  , replyText
  , send
  ) where

import Prelude

import Core.Shape (class Inject, Format(..), parse)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, attempt)
import Effect.Exception (message)
import Effects.Api (API, ApiF(..), ApiRequest, Reply, _apiCall, call)
import Handlers.Api.Contract (class EntryOf, Operations, request)
import Handlers.Api.Route (Wire, errorShape)
import Handlers.Api.Route as Route
import Prim.Row as Row
import Run (Run, case_, on, run)
import Schema.Decode (decodeError)
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

-- | Send the operation of a constructor's name; its reply decoded as the contract states.
send
  :: forall @name rep p op a e t
   . IsSymbol name
  => Generic ApiRequest rep
  => Inject name rep p
  => Row.Cons name op t Operations
  => EntryOf op a e
  => Fetch
  -> p
  -> Aff (Either String a)
send fetch p = (\reply -> replyText reply >>= decodeError <<< parse sent.op.format sent.op.reply) <$> runApi fetch (call sent.request)
  where
  sent = request @name p

-- | The reply's body when it is a success (2xx); otherwise the error the reply carries: the `error` field of its JSON
-- | body. A reply that is not the server's own (a proxy's HTML page when the server is down or restarting) is never
-- | shown as it came: it is named by its status, and a gateway status says the server is not answering.
replyText :: Reply -> Either String String
replyText r
  | r.status >= 200 && r.status < 300 = Right r.body
  | otherwise = Left (errorOf r)
      where
      errorOf reply = case parse Snapshot errorShape reply.body of
        Right { error } -> error
        Left _
          | reply.status == 502 || reply.status == 503 || reply.status == 504 -> "The server is not answering (HTTP " <> show reply.status <> "). It may be restarting: reload the page in a moment."
          | otherwise -> "The server's reply could not be read (HTTP " <> show reply.status <> ")."
