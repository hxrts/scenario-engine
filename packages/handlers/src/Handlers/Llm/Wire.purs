-- | The model wire format (docs/06_simulator.md §14): the system prompts, the JSON payloads each
-- | request sends, and the decoding of each reply's text. A payload is the request's typed record
-- | encoded as JSON; a reply is one JSON object, the first `{...}` in its text. Only this module
-- | and `Handlers.Llm` know these texts; the `pi` process that carries them is in `Adapters.Pi`.
module Handlers.Llm.Wire
  ( interpretPrompt
  , narratePrompt
  , speakPrompt
  , JointRequest
  , ProposedDeal
  , NarrateRequest
  , SpeakRequest
  , decodeJoint
  , decodeText
  ) where

import Prelude

import Schema.Decode.Common (readJson)
import Core.Proposal (Transcript)
import Core.Types (EntityId, Intent, MarketId, RoundIndex)
import Core.World (Event, View)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Either (Either)
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, lastIndexOf, slice)
import Data.Maybe (Maybe(..))

-- | The joint pass's payload: the round, its seat transcripts, and the Entities and Markets a
-- | Proposal may name.
type JointRequest = { round :: RoundIndex, transcripts :: Array Transcript, entities :: Array EntityId, markets :: Array MarketId }

-- | One Proposal as the model states it, before the engine's checks: its author, Intents, the
-- | other signers the deal asks for, and the seats whose transcripts describe it.
type ProposedDeal = { author :: EntityId, intents :: Array Intent, signers :: Array EntityId, agreed :: Array EntityId }

-- | The narrator's payload: the round and its resolved events.
type NarrateRequest = { round :: RoundIndex, events :: Array Event }

-- | An AI-agent seat's payload: the seat, its briefing, and what it can see.
type SpeakRequest = { actor :: EntityId, briefing :: String, view :: View }

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Entities, Markets, or Resources that are not in the payload."

-- | The system prompt of the joint pass.
interpretPrompt :: String
interpretPrompt = common <> "\nTask: read the round's `transcripts` together and reconcile them into Proposals. Each transcript is one seat's informal, spoken account of what it intends (`seat` is its id, `text` what it said). A transcript that describes a deal is that seat's agreement to it, and a seat that described a deal wanted it. Where transcripts describe the same deal, reconcile them into one Proposal, adjusting details slightly where the transcripts are loose and preferring Intents that match. Otherwise keep each Proposal in the spirit of its transcript. A deal between seats is Transfer intents; a Trade is one seat's own action. Never ask a question and never reply with a clarification; reconcile instead. Reply {\"proposals\":[{\"author\":EntityId,\"intents\":[Intent],\"signers\":[EntityId],\"agreed\":[EntityId]}]}. `author` is a seat that spoke. `signers` are the other parties the Proposal needs. `agreed` lists the seats whose transcripts describe it. Use only ids from `entities` and `markets`. An empty list is allowed."

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

-- | Decode one JSON object out of a reply's text.
decodeAs :: forall a. DecodeJson a => String -> Either String a
decodeAs = readJson <<< firstObject

-- | A joint pass's reply text: one object whose `proposals` are the Proposals as the model states
-- | them. Entries are not checked here; the handler drops what the engine cannot use.
decodeJoint :: String -> Either String (Array ProposedDeal)
decodeJoint text = map _.proposals (decodeAs text :: Either String { proposals :: Array ProposedDeal })

-- | A narrator or seat reply's text: one object with a `text` string.
decodeText :: String -> Either String String
decodeText text = map _.text (decodeAs text :: Either String { text :: String })
