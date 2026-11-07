-- | The language-model adapter (docs/01_overview.md §10): the implementation of `Handlers.Llm`'s
-- | typed `ModelPort`. It owns everything the handler does not see: the prompt templates, the JSON
-- | wire format of each request and reply, the `pi` process that answers one call, and the evidence
-- | of each call (digests of the template and payload, and the raw event stream by path). Each `pi`
-- | call is one ephemeral, tool-less process; the model ID is the only configuration. `fromCall`
-- | builds a port over any raw call: the capstone generator uses it with the DSL's replies, and
-- | the adapter tests use it to drive the wire format. `stub` answers typed replies, with no wire format.
module Adapters.Llm
  ( defaultModel
  , Evidence
  , Call
  , pi
  , fromCall
  , Replies
  , stub
  , interpretPrompt
  , narratePrompt
  , speakPrompt
  , decodeJoint
  , decodeText
  ) where

import Prelude

import Board.Decode.Common (readJson)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, lastIndexOf, slice)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Handlers.Llm (JointReply, JointRequest, ModelPort, NarrateReply, NarrateRequest, ProposedDeal, SpeakReply, SpeakRequest)

defaultModel :: String
defaultModel = "anthropic/claude-opus-5-5"

-- | Evidence by pointer, appended for every call: the effect, the model, digests of the prompt
-- | template and the JSON payload, the path of the raw event stream, and whether the reply decoded.
type Evidence = { effect :: String, model :: String, template :: String, input :: String, raw :: String, decoded :: String }

-- | One raw model call: the system prompt and the JSON payload in, the reply text and the path of
-- | its raw event stream out. A failed call is an Aff error.
type Call = String -> String -> Aff { text :: String, raw :: String }

-- | A short digest of a template or payload, for evidence. The same function names the raw files
-- | names use it too (Adapters/Llm.js).
foreign import sha256 :: String -> String

foreign import piImpl :: String -> String -> String -> String -> EffectFnAff { text :: String, raw :: String }

-- | The `pi` backend: one process per call, with its raw event stream saved under `rawDir`, and
-- | each call's evidence handed to `evidence`.
pi :: { model :: String, rawDir :: String, evidence :: Evidence -> Effect Unit } -> ModelPort
pi c = fromCall
  { model: c.model
  , evidence: c.evidence
  , call: \system input -> fromEffectFnAff (piImpl c.model c.rawDir system input) `catchError` backendFailed c.model
  }

-- | A backend failure (a missing `pi`, a non-zero exit, an unwritable raw directory) is fatal to
-- | the run: it is rethrown with the model and the cause named, and the run is marked failed.
backendFailed :: forall a. String -> Error -> Aff a
backendFailed model e = throwError (error ("language model backend failed (" <> model <> "): " <> message e))

-- | The port over a raw call: each request becomes one prompt and one JSON payload, and each reply
-- | text is decoded into the typed reply. A reply that does not decode is a `Left`.
fromCall :: { model :: String, call :: Call, evidence :: Evidence -> Effect Unit } -> ModelPort
fromCall c =
  { model: c.model
  , interpret: \r -> ask "interpret" interpretPrompt (encodeJson r) decodeJoint
  , narrate: \r -> ask "narrate" narratePrompt (encodeJson (r :: NarrateRequest)) decodeText
  , speak: \r -> ask "seat:speak" speakPrompt (encodeJson (r :: SpeakRequest)) decodeText
  }
  where
  ask :: forall a. String -> String -> Json -> (String -> Either String a) -> Aff (Either String a)
  ask effect template payload decode = do
    let input = stringify payload
    out <- c.call template input
    let decoded = decode out.text
    liftEffect $ c.evidence { effect, model: c.model, template: sha256 template, input: sha256 input, raw: out.raw, decoded: either ("error: " <> _) (const "ok") decoded }
    pure decoded

-- | Canned typed replies, for tests and for the stub model. No network, no `pi`.
type Replies = { interpret :: JointRequest -> JointReply, narrate :: NarrateRequest -> NarrateReply, speak :: SpeakRequest -> SpeakReply }

stub :: Replies -> ModelPort
stub r =
  { model: "stub"
  , interpret: \q -> pure (r.interpret q)
  , narrate: \q -> pure (r.narrate q)
  , speak: \q -> pure (r.speak q)
  }

-- The prompts ------------------------------------------------------------------------------

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

-- The replies -----------------------------------------------------------------------------

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
