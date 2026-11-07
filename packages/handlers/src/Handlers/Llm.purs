-- | The language-model handler (docs/04_instantiated_game.md §11, docs/01_overview.md §10): the joint
-- | pass, the narrator, and the AI-agent seats, interpreted over a typed `Backend`. A backend is the
-- | raw call (one prompt and one JSON payload in, the reply text out) plus the sinks for evidence and
-- | for the typed exchange. The handler owns the rest: the request payloads (their shapes), the prompt
-- | for each request (`Handlers.Llm.Wire`), decoding, the fallbacks, the joint pass's reconciliation,
-- | and the evidence digests. A reply that does not decode is a `Left` (the joint pass and the narrator
-- | fall back as documented below). A failed call is an Aff error, which the run treats as fatal.
-- | Model output is only a candidate: the engine checks authority, signatures, and balances when it
-- | resolves the round.
module Handlers.Llm
  ( Call
  , Evidence
  , Backend
  , interpreter
  , narrator
  , agents
  , listing
  ) where

import Prelude

import Core.Proposal (jointProposals, proposedDealShape)
import Core.Shape (Format(..), Shape, arrayOf, encodeWith, stringShape)
import Core.Types (EntityId, Id(..), MarketId)
import Core.World (RoundResult, View, WorldState)
import Data.Argonaut.Core (stringify)
import Data.Array (fromFoldable, null)
import Data.Either (Either, either)
import Data.Map as Map
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effects.Signatures (InterpretF(..), NarrateF(..))
import Handlers.Llm.Wire (JointRequest, NarrateRequest, SpeakRequest, decodeJoint, decodeText, interpretPrompt, jointRequestShape, narratePrompt, narrateRequestShape, speakPrompt, speakRequestShape)
import Handlers.Recording (Exchange, encodeReply)

foreign import sha256 :: String -> String

-- | One raw model call: the system prompt and the JSON payload in, the reply text and the path of
-- | its raw event stream out. A failed call is an Aff error.
type Call = String -> String -> Aff { text :: String, raw :: String }

-- | Evidence by pointer, appended for every call: the effect, the model, digests of the prompt
-- | template and the JSON payload, the path of the raw event stream, and whether the reply decoded.
type Evidence = { effect :: String, model :: String, template :: String, input :: String, raw :: String, decoded :: String }

-- | The model backend a handler interprets over: the raw call, the model name for evidence, and the
-- | sinks for each call's evidence and its typed exchange (the recording, when one is kept).
type Backend = { model :: String, call :: Call, evidence :: Evidence -> Effect Unit, exchange :: Exchange -> Effect Unit }

-- | What a call concerns, for the recording: its round and the seats whose transcripts it carries.
type Key = { round :: Int, seats :: Array String }

seatText :: EntityId -> String
seatText (Id s) = s

-- | One call: the payload is encoded through its shape, the prompt and the payload go to the backend,
-- | the reply decodes, and the call's evidence and typed exchange are kept. A failed call is not kept.
ask :: forall p a. Backend -> String -> Key -> String -> Shape p -> p -> Shape a -> (String -> Either String a) -> Aff (Either String a)
ask b effect key template requestShape payload replyShape decode = do
  let
    request = encodeWith Snapshot requestShape payload
    input = stringify request
  out <- b.call template input
  let decoded = decode out.text
  liftEffect do
    b.evidence { effect, model: b.model, template: sha256 template, input: sha256 input, raw: out.raw, decoded: either ("error: " <> _) (const "ok") decoded }
    b.exchange { effect, round: key.round, seats: key.seats, request, reply: encodeReply replyShape decoded }
  pure decoded

-- | The round's transcripts to Proposals, one joint call. A reply that does not decode is a
-- | `Left` (the caller tells the speakers and submits nothing). Entries the engine could not use
-- | are dropped by the joint pass rule (`Core.Proposal.jointProposals`).
interpreter :: Backend -> WorldState -> InterpretF ~> Aff
interpreter b w (Interpret round transcripts k) = k <$> do
  let
    request :: JointRequest
    request = { round, transcripts, entities: map _.id (fromFoldable (Map.values w.entities)), markets: markets w }
    key = { round, seats: map (\t -> seatText t.seat) transcripts }
  r <- ask b "interpret" key interpretPrompt jointRequestShape request (arrayOf proposedDealShape) decodeJoint
  pure (map (jointProposals (\s -> Map.member s w.entities) transcripts) r)

markets :: WorldState -> Array MarketId
markets w = map _.id (fromFoldable (Map.values w.markets))

-- | The round's front page. A round whose joint pass did not decode, and a model reply that does
-- | not decode, get the plain listing of the round's events, and a round whose joint pass did not
-- | decode makes no model call (docs/06 §6.1). Otherwise the model narrates from the round's record.
narrator :: Backend -> Boolean -> NarrateF ~> Aff
narrator b jointFailed (Narrate r k) = k <$>
  if jointFailed then pure (listing r)
  else do
    let
      request :: NarrateRequest
      request = { round: r.record.round, events: r.record.events }
      key = { round: r.record.round, seats: [] }
    either (const (listing r)) identity <$> ask b "narrate" key narratePrompt narrateRequestShape request stringShape decodeText

-- | The round's events, one per line, with no interpretation.
listing :: RoundResult -> String
listing r = joinWith "\n" ([ "Round " <> show r.record.round <> ":" ] <> bullets)
  where
  bullets = if null r.record.events then [ "- no events" ] else map (\e -> "- " <> show e) r.record.events

-- | AI-agent seats: one short transcript per round, from the role briefing and the authorized View.
-- | An undecodable reply is an empty transcript, so the seat submits nothing.
agents :: Backend -> (EntityId -> String) -> { speak :: EntityId -> View -> Aff String }
agents b briefing =
  { speak: \e v -> do
      let
        request :: SpeakRequest
        request = { actor: e, briefing: briefing e, view: v }
        key = { round: v.round, seats: [ seatText e ] }
      either (const "") identity <$> ask b "seat:speak" key speakPrompt speakRequestShape request stringShape decodeText
  }
