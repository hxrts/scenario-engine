-- | One description of each model call: its effect name, prompt, request and reply shapes, and how a reply is
-- | read. The handlers, the recording, and the stub read the same description.
module Handlers.Llm.Call
  ( ModelCall
  , Key
  , seatText
  , interpretCall
  , narrateCall
  , speakCall
  , decodeWith
  ) where

import Prelude

import Core.Shape (Shape, stringShape)
import Core.Types (EntityId, Id(..))
import Data.Either (Either)
import Handlers.Llm.Prompt (interpretPrompt, narratePrompt, speakPrompt)
import Handlers.Llm.Wire (JointRequest, NarrateRequest, SpeakRequest, decodeReply, jointRequestShape, narrateRequestShape, speakRequestShape, textReplyShape)
import Schema.Joint (ProposedJoint, jointReplyShape)

-- | What a call concerns, for the recording: its round and the seats whose transcripts it carries.
type Key = { round :: Int, seats :: Array String }

seatText :: EntityId -> String
seatText (Id s) = s

-- | A model call: `effect` is its name in the evidence and the recording, `prompt` its system prompt,
-- | `request` its payload's shape, `wire` the reply the model writes, `recorded` the typed value the recording
-- | keeps, `project` and `rewire` between the two, and `key` what the call concerns.
type ModelCall p w r =
  { effect :: String
  , prompt :: String
  , request :: Shape p
  , wire :: Shape w
  , recorded :: Shape r
  , project :: w -> r
  , rewire :: r -> w
  , key :: p -> Key
  }

-- | The joint pass: the wire reply is what is recorded.
interpretCall :: ModelCall JointRequest { proposals :: Array ProposedJoint } { proposals :: Array ProposedJoint }
interpretCall =
  { effect: "interpret"
  , prompt: interpretPrompt
  , request: jointRequestShape
  , wire: jointReplyShape
  , recorded: jointReplyShape
  , project: identity
  , rewire: identity
  , key: \r -> { round: r.round, seats: map (\t -> seatText t.seat) r.transcripts }
  }

-- | A call whose reply is a `text`, recorded as that text.
textCall :: forall p. String -> String -> Shape p -> (p -> Key) -> ModelCall p { text :: String } String
textCall effect prompt request key =
  { effect, prompt, request, wire: textReplyShape, recorded: stringShape, project: _.text, rewire: \t -> { text: t }, key }

narrateCall :: ModelCall NarrateRequest { text :: String } String
narrateCall = textCall "narrate" narratePrompt narrateRequestShape \r -> { round: r.round, seats: [] }

speakCall :: ModelCall SpeakRequest { text :: String } String
speakCall = textCall "seat:speak" speakPrompt speakRequestShape \r -> { round: r.view.round, seats: [ seatText r.actor ] }

-- | A reply's text, read through the call's wire shape and projected to what is recorded.
decodeWith :: forall p w r. ModelCall p w r -> String -> Either String r
decodeWith c = map c.project <<< decodeReply c.wire
