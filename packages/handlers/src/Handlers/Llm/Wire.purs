-- | The model wire format (docs/06_simulator.md §14). Each request's payload and each reply is a shape
-- | (`Core.Shape`), so the JSON the model sees and writes is the description the setting and the game
-- | use. The prompts are the role briefing and framing prose, with the action list and the reply's
-- | schema rendered from those shapes (`prose`). A reply is one JSON object, the first balanced `{...}` in its
-- | text. Only this module and `Handlers.Llm.*` know these texts; the `pi` process that carries them is in
-- | `Adapters.Pi`.
module Handlers.Llm.Wire
  ( JointRequest
  , NarrateRequest
  , SpeakRequest
  , jointRequestShape
  , narrateRequestShape
  , speakRequestShape
  , textReplyShape
  , correctionShape
  , decodeReply
  , decodeJoint
  , decodeJointReply
  , decodeText
  ) where

import Prelude

import Schema.Joint (ProposedJoint, Transcript, jointReplyShape, transcriptShape)
import Core.Shape (Format(..), Shape, arrayOf, field, intShape, jsonShape, parse, recordOf, recordShape, requiredOf, stringShape)
import Core.Types (EntityId, MarketId, RoundIndex, idShape)
import Core.World (Event, View, eventShape, viewShape)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Array (drop, length, zip, (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, slice, toCharArray)
import Data.Tuple (Tuple(..))
import Schema.Decode (decodeError)

-- | The joint pass's payload: the round, its seat transcripts, and the Entities and Markets a
-- | Proposal may name.
type JointRequest = { round :: RoundIndex, transcripts :: Array Transcript, entities :: Array EntityId, markets :: Array MarketId }

jointRequestShape :: Shape JointRequest
jointRequestShape = recordShape "JointRequest" { round: intShape, transcripts: (arrayOf transcriptShape), entities: (arrayOf idShape), markets: (arrayOf idShape) }

-- | The narrator's payload: the round and its resolved events.
type NarrateRequest = { round :: RoundIndex, events :: Array Event }

narrateRequestShape :: Shape NarrateRequest
narrateRequestShape = recordShape "NarrateRequest" { round: intShape, events: (arrayOf eventShape) }

-- | An AI-agent seat's payload: the seat, its briefing, and what it can see.
type SpeakRequest = { actor :: EntityId, briefing :: String, view :: View }

speakRequestShape :: Shape SpeakRequest
speakRequestShape = recordShape "SpeakRequest" { actor: idShape, briefing: stringShape, view: viewShape }

-- | A narrator's or seat's reply: one object with a `text` string.
textReplyShape :: Shape { text :: String }
textReplyShape = recordOf "TextReply"
  ( field @"text" "the text" (requiredOf stringShape)
  )

-- | A correction's payload: the original request, the previous reply's text, and the decode error as
-- | the decoder reports it.
correctionShape :: Shape { request :: Json, previous_reply :: String, error :: String }
correctionShape = recordShape "Correction" { request: jsonShape, previous_reply: stringShape, error: stringShape }

-- | The first balanced {...} object in a reply (models sometimes wrap JSON in prose or fences).
-- | Braces inside JSON strings do not count. Text with no balanced object is returned whole.
firstObject :: String -> String
firstObject s = case indexOf (Pattern "{") s of
  Nothing -> s
  Just i -> maybe s (\end -> slice i (end + 1) s) (closing i)
  where
  chars = toCharArray s
  closing i = (foldl step { depth: 0, inString: false, escaped: false, at: Nothing } (zip (i .. (length chars - 1)) (drop i chars))).at
  step st (Tuple n c)
    | isJust st.at = st
    | st.escaped = st { escaped = false }
    | st.inString = case c of
        '\\' -> st { escaped = true }
        '"' -> st { inString = false }
        _ -> st
    | otherwise = case c of
        '"' -> st { inString = true }
        '{' -> st { depth = st.depth + 1 }
        '}' -> if st.depth == 1 then st { depth = 0, at = Just n } else st { depth = st.depth - 1 }
        _ -> st

-- | Decode a reply's text through a shape: its first object, read in the snapshot form.
decodeReply :: forall a. Shape a -> String -> Either String a
decodeReply s = decodeError <<< parse Snapshot s <<< firstObject

-- | A joint pass's reply text: its Proposals, as the model states them. Entries are not checked
-- | here; the handler drops what the engine cannot use.
decodeJoint :: String -> Either String (Array ProposedJoint)
decodeJoint = map _.proposals <<< decodeJointReply

-- | A joint pass's reply as the model wrote it, the form the recording keeps.
decodeJointReply :: String -> Either String { proposals :: Array ProposedJoint }
decodeJointReply = decodeReply jointReplyShape

-- | A narrator or seat reply's text.
decodeText :: String -> Either String String
decodeText text = map _.text (decodeReply textReplyShape text)

