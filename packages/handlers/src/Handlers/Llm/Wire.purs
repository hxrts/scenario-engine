-- | The model wire format (docs/06_simulator.md §14). Each request's payload and each reply is a shape
-- | (`Core.Shape`), so the JSON the model sees and writes is the description the premise and the game
-- | use. The prompts are the role briefing and framing prose, with the action list and the reply's
-- | schema rendered from those shapes (`prose`). A reply is one JSON object, the first `{...}` in its
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
  , textReplyShape
  , decodeJoint
  , decodeText
  ) where

import Prelude

import Core.Proposal (ProposedDeal, Transcript, proposalBodyShape, proposedDealShape, transcriptShape)
import Core.Shape (Format(..), Shape, arrayOf, decodeJsonWith, field, intShape, prose, rec1, rec2, rec3, rec4, requiredOf, stringShape)
import Core.Types (EntityId, MarketId, RoundIndex, idShape, intentShape)
import Core.World (Event, View, eventShape, viewShape)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, lastIndexOf, slice)
import Schema.Decode.Common (readJsonWith)

-- | The joint pass's payload: the round, its seat transcripts, and the Entities and Markets a
-- | Proposal may name.
type JointRequest = { round :: RoundIndex, transcripts :: Array Transcript, entities :: Array EntityId, markets :: Array MarketId }

jointRequestShape :: Shape JointRequest
jointRequestShape = rec4 "JointRequest"
  (field "round" "round" "the round" intShape _.round)
  (field "transcripts" "transcripts" "the seats' transcripts of the round" (arrayOf transcriptShape) _.transcripts)
  (field "entities" "entities" "the Entities a Proposal may name" (arrayOf idShape) _.entities)
  (field "markets" "markets" "the Markets a Proposal may name" (arrayOf idShape) _.markets)
  (\r t e m -> { round: r, transcripts: t, entities: e, markets: m })

-- | The narrator's payload: the round and its resolved events.
type NarrateRequest = { round :: RoundIndex, events :: Array Event }

narrateRequestShape :: Shape NarrateRequest
narrateRequestShape = rec2 "NarrateRequest"
  (field "round" "round" "the round" intShape _.round)
  (field "events" "events" "the round's resolved events" (arrayOf eventShape) _.events)
  (\r e -> { round: r, events: e })

-- | An AI-agent seat's payload: the seat, its briefing, and what it can see.
type SpeakRequest = { actor :: EntityId, briefing :: String, view :: View }

speakRequestShape :: Shape SpeakRequest
speakRequestShape = rec3 "SpeakRequest"
  (field "actor" "actor" "the seat" idShape _.actor)
  (field "briefing" "briefing" "the seat's role" stringShape _.briefing)
  (field "view" "view" "what the seat can see" viewShape _.view)
  (\a b v -> { actor: a, briefing: b, view: v })

-- | A joint pass's reply: one object whose `proposals` are the Proposals as the model states them.
jointReplyShape :: Shape { proposals :: Array ProposedDeal }
jointReplyShape = rec1 "JointReply"
  (field "proposals" "proposals" "the Proposals the transcripts describe" (requiredOf (arrayOf proposedDealShape)) _.proposals)
  (\p -> { proposals: p })

-- | A narrator's or seat's reply: one object with a `text` string.
textReplyShape :: Shape { text :: String }
textReplyShape = rec1 "TextReply"
  (field "text" "text" "the text" (requiredOf stringShape) _.text)
  (\t -> { text: t })

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Entities, Markets, or Resources that are not in the payload."

-- | The action list a Proposal is built from, rendered from the Intent and Proposal shapes.
actions :: String
actions = "\nActions (the Intents a Proposal may contain; a Proposal has these fields): " <> prose intentShape.desc <> "\nProposal body: " <> prose proposalBodyShape.desc

-- | The reply schema, rendered from the shape the reply decodes through.
replySchema :: String
replySchema = "\nReply schema: " <> prose jointReplyShape.desc

-- | The system prompt of the joint pass.
interpretPrompt :: String
interpretPrompt = common <> "\nTask: read the round's `transcripts` together and reconcile them into Proposals. Each transcript is one seat's informal, spoken account of what it intends (`seat` is its id, `text` what it said). A transcript that describes a deal is that seat's agreement to it, and a seat that described a deal wanted it. Where transcripts describe the same deal, reconcile them into one Proposal, adjusting details slightly where the transcripts are loose and preferring Intents that match. Otherwise keep each Proposal in the spirit of its transcript. A deal between seats is Transfer intents; a Trade is one seat's own action. Every Intent is an object with a \"tag\" and a \"values\" array, for example {\"tag\":\"Transfer\",\"values\":[{\"from\":\"US\",\"to\":\"DE\",\"resource\":\"M\",\"quantity\":\"10\"}]}; \"resource\" is a Resource id (for example E or M in Meridian), never an Entity id, and \"quantity\" is a decimal string. Never ask a question and never reply with a clarification; reconcile instead. `author` is a seat that spoke. `signers` are the other parties the Proposal needs. `agreed` lists the seats whose transcripts describe it. Use only ids from `entities` and `markets`. An empty list is allowed." <> actions <> replySchema

-- | The system prompt of the narrator.
narratePrompt :: String
narratePrompt = common <> "\nTask: write the round's front page: a short newspaper-style summary of `events`. Report only what the events state; add no facts, motives, or numbers not present. Reply {\"text\":string}."

-- | The system prompt of an AI-agent seat.
speakPrompt :: String
speakPrompt = common <> "\nTask: you play the seat `actor` according to `briefing`. Reply {\"text\":string}: a short transcript, one to three sentences, of what you intend to do this round in your own words, naming any deal you propose and the party it is with."

-- | The first {...} object in a reply (models sometimes wrap JSON in prose or fences).
firstObject :: String -> String
firstObject s = case indexOf (Pattern "{") s, lastIndexOf (Pattern "}") s of
  Just i, Just j | j > i -> slice i (j + 1) s
  _, _ -> s

-- | Decode a reply's text through a shape: its first object, read in the snapshot form.
decodeReply :: forall a. Shape a -> String -> Either String a
decodeReply s = readJsonWith (decodeJsonWith Snapshot s) <<< firstObject

-- | A joint pass's reply text: its Proposals, as the model states them. Entries are not checked
-- | here; the handler drops what the engine cannot use.
decodeJoint :: String -> Either String (Array ProposedDeal)
decodeJoint text = map _.proposals (decodeReply jointReplyShape text)

-- | A narrator or seat reply's text.
decodeText :: String -> Either String String
decodeText text = map _.text (decodeReply textReplyShape text)
