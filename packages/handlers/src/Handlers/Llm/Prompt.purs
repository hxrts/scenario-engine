-- | The three system prompts of the model calls.
module Handlers.Llm.Prompt
  ( actions
  , intentExample
  , replySchemaOf
  , common
  , interpretPrompt
  , speakPrompt
  ) where

import Prelude
import Core.Num as Q
import Schema.Joint (jointReplyShape)
import Core.Shape (Format(..), Shape, encodeWith, prose)
import Laws.Types (Id(..), Intent, IntentF(..), intentShape)
import Data.Argonaut.Core (stringify)
import Handlers.Llm.Wire (textReplyShape)

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Actors, Exchanges, or Resources that are not in the payload."

-- | The system prompt of the joint pass.
interpretPrompt :: String
interpretPrompt = common <> "\nTask: read the round's `transcripts` together and reconcile them into CandidateTransactions. Each transcript is one seat's informal, spoken account of what it intends (`seat` is its id, `text` what it said). A transcript that describes a joint CandidateTransaction is that seat's agreement to it, and a seat that described a joint CandidateTransaction wanted it. Where transcripts describe the same joint CandidateTransaction, reconcile them into one CandidateTransaction, adjusting details slightly where the transcripts are loose and preferring Intents that match. Otherwise keep each CandidateTransaction in the spirit of its transcript. A joint CandidateTransaction between seats (a CandidateTransaction with a signer other than its author) bundles only Transfer and Transform Intents; a Trade, a Route, or a ChangeRule is one seat's own action, in a CandidateTransaction with no other signer. Every Intent is an object with a \"tag\" and a \"values\" array, for example " <> intentExample <> "; \"resource\" is a Resource id (for example E or M in Meridian), never an Actor id, and \"quantity\" is a decimal string. Never ask a question and never reply with a clarification; reconcile instead. `author` is a seat that spoke. `signers` are the other parties the CandidateTransaction needs. `agreed` lists the seats whose transcripts describe it. Use only ids from `actors` and `exchanges`. An empty list is allowed." <> actions <> replySchemaOf jointReplyShape

-- | The system prompt of an AI-agent seat.
speakPrompt :: String
speakPrompt = common <> "\nTask: you play the seat `explicitActor` according to `briefing`. Reply with `text`: a short transcript, one to three sentences, of what you intend to do this round in your own words, naming any joint CandidateTransaction you propose and the party it is with." <> replySchemaOf textReplyShape

-- | The action list a candidate Transaction is built from, rendered from the Intent shape.
actions :: String
actions = "\nActions (the Intents a CandidateTransaction may contain): " <> prose intentShape.desc

-- | One Intent in the wire form, rendered through the shape the reply decodes with.
intentExample :: String
intentExample = stringify (encodeWith Snapshot intentShape example)
  where
  example :: Intent
  example = Transfer { from: Id "US", to: Id "DE", resource: Id "M", quantity: Q.fromInt 10 }

-- | A reply's schema, rendered from the shape the reply decodes through.
replySchemaOf :: forall a. Shape a -> String
replySchemaOf s = "\nReply schema: " <> prose s.desc
