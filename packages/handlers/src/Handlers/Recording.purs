-- | The recording of typed model exchanges (docs/08_coverage.md). `Handlers.Llm` reports each call
-- | that completes as an `Exchange`: its effect, its typed request as JSON, and its typed reply as
-- | JSON. The capstone generator (`apps/cli`, `Transcript`, run by `just _scenarios`) keeps them and
-- | writes them as the recording file. Reading the file back and replaying it are test support.
module Handlers.Recording
  ( Exchange
  , Recording
  , encodeReply
  , encodeRecording
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | One model call: the effect it served, the encoded typed request, and the encoded typed reply.
type Exchange = { effect :: String, request :: Json, reply :: Json }

type Recording = Array Exchange

-- | The reply's encoding: an error text, or the typed value. Both keys are always written.
type ReplyJson = { error :: Maybe String, value :: Maybe Json }

encodeReply :: forall a. EncodeJson a => Either String a -> Json
encodeReply r = encodeJson $ case r of
  Left e -> { error: Just e, value: Nothing } :: ReplyJson
  Right a -> { error: Nothing, value: Just (encodeJson a) }

-- | The recording file's text: indented JSON, with a final newline.
encodeRecording :: Recording -> String
encodeRecording rec = stringifyWithIndent 1 (encodeJson rec) <> "\n"
