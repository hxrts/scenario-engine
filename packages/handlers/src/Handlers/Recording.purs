-- | The recording of typed model exchanges (docs/08_coverage.md). `Handlers.Llm` reports each call
-- | that completes as an `Exchange`: its effect, its typed request as JSON, and its typed reply as
-- | JSON. The capstone generator (`apps/cli`, `Transcript`, run by `just _premises`) keeps them and
-- | writes them as the recording file. Reading the file back and replaying it are test support.
module Handlers.Recording
  ( Exchange
  , Recording
  , encodeReply
  , encodeRecording
  ) where

import Prelude

import Core.Shape (Format(..), Shape, encodeWith)
import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | One model call: the effect it served, the round and the seats whose transcripts it carried (the
-- | key a replay matches on, never the prompt text), the encoded typed request, and the encoded typed
-- | reply. A call with no seat (the narrator's) has no seats.
type Exchange = { effect :: String, round :: Int, seats :: Array String, request :: Json, reply :: Json }

type Recording = Array Exchange

-- | The reply's encoding: an error text, or the typed value. Both keys are always written.
type ReplyJson = { error :: Maybe String, value :: Maybe Json }

-- | A reply's recorded form: the typed value encoded through its shape (the snapshot form).
encodeReply :: forall a. Shape a -> Either String a -> Json
encodeReply s r = encodeJson $ case r of
  Left e -> { error: Just e, value: Nothing } :: ReplyJson
  Right a -> { error: Nothing, value: Just (encodeWith Snapshot s a) }

-- | The recording file's text: indented JSON, with a final newline.
encodeRecording :: Recording -> String
encodeRecording rec = stringifyWithIndent 1 (encodeJson rec) <> "\n"
