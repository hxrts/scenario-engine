-- | The recording of typed model turns. `Handlers.Llm.*` reports each call
-- | that completes as an `Turn`: its effect, its typed request as JSON, and its typed reply as
-- | JSON. The capstone generator (`apps/cli`, `Transcript`, run by `just _premises`) keeps them and
-- | writes them as the recording file. Reading the file back and replaying it are test support.
module Handlers.Recording
  ( Turn
  , Recording
  , turnShape
  , encodeReply
  , encodeRecording
  ) where

import Prelude

import Core.Shape (Format(..), Shape, arrayOf, encodeWith, intShape, jsonShape, optionalOf, prettyJson, recordShape, stringShape)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | One model call: the effect it served, the round and the seats whose transcripts it carried (the
-- | key a replay matches on, never the prompt text), the encoded typed request, and the encoded typed
-- | reply.
type Turn = { effect :: String, round :: Int, seats :: Array String, request :: Json, reply :: Json }

turnShape :: Shape Turn
turnShape = recordShape "Turn" { effect: stringShape, round: intShape, seats: arrayOf stringShape, request: jsonShape, reply: jsonShape }

type Recording = Array Turn

-- | The reply's encoding: an error text, or the typed value. Both keys are always written.
type ReplyJson = { error :: Maybe String, value :: Maybe Json }

replyJsonShape :: Shape ReplyJson
replyJsonShape = recordShape "ReplyJson" { error: optionalOf stringShape, value: optionalOf jsonShape }

-- | A reply's recorded form: the typed value encoded through its shape (the snapshot form).
encodeReply :: forall a. Shape a -> Either String a -> Json
encodeReply s r = encodeWith Snapshot replyJsonShape $ case r of
  Left e -> { error: Just e, value: Nothing }
  Right a -> { error: Nothing, value: Just (encodeWith Snapshot s a) }

-- | The recording file's text: indented JSON, with a final newline.
encodeRecording :: Recording -> String
encodeRecording rec = prettyJson (encodeWith Snapshot (arrayOf turnShape) rec)
