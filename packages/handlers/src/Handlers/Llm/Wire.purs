-- | The model wire format (docs/06_simulator.md §14). Each request's payload and each reply is a shape
-- | (`Core.Shape`), so the JSON the model sees and writes is the description the premise and the game
-- | use. The prompts are the role briefing and framing prose, with the action list and the reply's
-- | schema rendered from those shapes (`prose`). A reply is one JSON object, the first balanced `{...}` in its
-- | text. Only this module and `Handlers.Llm` know these texts; the `pi` process that carries them is in
-- | `Adapters.Pi`.
module Handlers.Llm.Wire
  ( interpretPrompt
  , narratePrompt
  , speakPrompt
  , JointRequest
  , NarrateRequest
  , SpeakRequest
  , jointRequestShape
  , narrateRequestShape
  , speakRequestShape
  , jointReplyShape
  , proposedJointsShape
  , textReplyShape
  , correctionShape
  , decodeJoint
  , decodeJointReply
  , decodeText
  ) where

import Prelude

import Core.Num as Q
import Schema.Joint (ProposedJoint, Transcript, proposedJointShape, transcriptShape)
import Core.Shape (Format(..), Shape, arrayOf, encodeWith, fieldOf, intShape, object, parse, prim, prose, requiredOf, stringShape)
import Core.Types (EntityId, Id(..), Intent, IntentF(..), MarketId, RoundIndex, idShape, intentShape)
import Core.World (Event, View, eventShape, viewShape)
import Data.Argonaut.Core (Json, stringify)
import Data.Either (Either(..))
import Data.Array (drop, length, zip, (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, slice, toCharArray)
import Data.Tuple (Tuple(..))
import Schema.Decode.Common (decodeError)

-- | The joint pass's payload: the round, its seat transcripts, and the Entities and Markets a
-- | Proposal may name.
type JointRequest = { round :: RoundIndex, transcripts :: Array Transcript, entities :: Array EntityId, markets :: Array MarketId }

jointRequestShape :: Shape JointRequest
jointRequestShape = object "JointRequest"
  ( (\r t e m -> { round: r, transcripts: t, entities: e, markets: m })
      <$> fieldOf "round" "the round" intShape _.round
      <*> fieldOf "transcripts" "the seats' transcripts of the round" (arrayOf transcriptShape) _.transcripts
      <*> fieldOf "entities" "the Entities a Proposal may name" (arrayOf idShape) _.entities
      <*> fieldOf "markets" "the Markets a Proposal may name" (arrayOf idShape) _.markets
  )

-- | The narrator's payload: the round and its resolved events.
type NarrateRequest = { round :: RoundIndex, events :: Array Event }

narrateRequestShape :: Shape NarrateRequest
narrateRequestShape = object "NarrateRequest"
  ( (\r e -> { round: r, events: e })
      <$> fieldOf "round" "the round" intShape _.round
      <*> fieldOf "events" "the round's resolved events" (arrayOf eventShape) _.events
  )

-- | An AI-agent seat's payload: the seat, its briefing, and what it can see.
type SpeakRequest = { actor :: EntityId, briefing :: String, view :: View }

speakRequestShape :: Shape SpeakRequest
speakRequestShape = object "SpeakRequest"
  ( (\a b v -> { actor: a, briefing: b, view: v })
      <$> fieldOf "actor" "the seat" idShape _.actor
      <*> fieldOf "briefing" "the seat's role" stringShape _.briefing
      <*> fieldOf "view" "what the seat can see" viewShape _.view
  )

-- | The Proposals of a joint pass as the model states them: what its reply carries and what the
-- | recording keeps.
proposedJointsShape :: Shape (Array ProposedJoint)
proposedJointsShape = arrayOf proposedJointShape

-- | A joint pass's reply: one object whose `proposals` are the Proposals as the model states them.
jointReplyShape :: Shape { proposals :: Array ProposedJoint }
jointReplyShape = object "JointReply"
  ( (\p -> { proposals: p })
      <$> fieldOf "proposals" "the Proposals the transcripts describe" (requiredOf proposedJointsShape) _.proposals
  )

-- | A narrator's or seat's reply: one object with a `text` string.
textReplyShape :: Shape { text :: String }
textReplyShape = object "TextReply"
  ( (\t -> { text: t })
      <$> fieldOf "text" "the text" (requiredOf stringShape) _.text
  )

-- | A correction's payload: the original request, the previous reply's text, and the decode error as
-- | the decoder reports it.
correctionShape :: Shape { request :: Json, previous_reply :: String, error :: String }
correctionShape = object "Correction"
  ( (\r p e -> { request: r, previous_reply: p, error: e })
      <$> fieldOf "request" "the original request" (prim "Json" identity Right) _.request
      <*> fieldOf "previous_reply" "the reply that did not decode" stringShape _.previous_reply
      <*> fieldOf "error" "why it did not decode" stringShape _.error
  )

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Entities, Markets, or Resources that are not in the payload."

-- | The action list a Proposal is built from, rendered from the Intent shape.
actions :: String
actions = "\nActions (the Intents a Proposal may contain): " <> prose intentShape.desc

-- | One Intent in the wire form, rendered through the shape the reply decodes with.
intentExample :: String
intentExample = stringify (encodeWith Snapshot intentShape example)
  where
  example :: Intent
  example = Transfer { from: Id "US", to: Id "DE", resource: Id "M", quantity: Q.fromInt 10 }

-- | A reply's schema, rendered from the shape the reply decodes through.
replySchemaOf :: forall a. Shape a -> String
replySchemaOf s = "\nReply schema: " <> prose s.desc

-- | The system prompt of the joint pass.
interpretPrompt :: String
interpretPrompt = common <> "\nTask: read the round's `transcripts` together and reconcile them into Proposals. Each transcript is one seat's informal, spoken account of what it intends (`seat` is its id, `text` what it said). A transcript that describes a joint Proposal is that seat's agreement to it, and a seat that described a joint Proposal wanted it. Where transcripts describe the same joint Proposal, reconcile them into one Proposal, adjusting details slightly where the transcripts are loose and preferring Intents that match. Otherwise keep each Proposal in the spirit of its transcript. A joint Proposal between seats (a Proposal with a signer other than its author) bundles only Transfer and Transform Intents; a Trade, a Route, or a ChangeRule is one seat's own action, in a Proposal with no other signer. Every Intent is an object with a \"tag\" and a \"values\" array, for example " <> intentExample <> "; \"resource\" is a Resource id (for example E or M in Meridian), never an Entity id, and \"quantity\" is a decimal string. Never ask a question and never reply with a clarification; reconcile instead. `author` is a seat that spoke. `signers` are the other parties the Proposal needs. `agreed` lists the seats whose transcripts describe it. Use only ids from `entities` and `markets`. An empty list is allowed." <> actions <> replySchemaOf jointReplyShape

-- | The system prompt of the narrator.
narratePrompt :: String
narratePrompt = common <> "\nTask: write the round's front page: a short newspaper-style summary of `events`. Each event of kind Coupled is a settlement: its `legs` move a Resource from one endpoint to another, `owed` and `moved`, and a shortfall is `moved` below `owed`; `cause` says what produced it. Report only what the events state; add no facts, motives, or numbers not present. Reply with the page as `text`." <> replySchemaOf textReplyShape

-- | The system prompt of an AI-agent seat.
speakPrompt :: String
speakPrompt = common <> "\nTask: you play the seat `actor` according to `briefing`. Reply with `text`: a short transcript, one to three sentences, of what you intend to do this round in your own words, naming any joint Proposal you propose and the party it is with." <> replySchemaOf textReplyShape

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
