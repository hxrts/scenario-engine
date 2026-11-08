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
  , backend
  , interpreter
  , narrator
  , speak
  , listing
  ) where

import Prelude

import Core.Map (values)

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parallel, sequential)

import Core.Proposal (ProposedDeal, jointProposals)
import Core.Shape (Format(..), Shape, encodeWith, stringShape)
import Core.Types (EntityId, Id(..), MarketId)
import Core.World (RoundResult, View, WorldState)
import Data.Argonaut.Core (Json, stringify)
import Data.Array (null)
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Map as Map
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.Exception (Error, error, message)
import Effect.Class (liftEffect)
import Effects.Signatures (InterpretF(..), NarrateF(..))
import Handlers.Llm.Wire (JointRequest, NarrateRequest, SpeakRequest, correctionShape, decodeJointReply, decodeText, interpretPrompt, jointReplyShape, jointRequestShape, narratePrompt, narrateRequestShape, speakPrompt, speakRequestShape)
import Handlers.Recording (Exchange, encodeReply)

-- | One raw model call: the system prompt and the JSON payload in, the reply text and the path of
-- | its raw event stream out. A failed call is an Aff error.
type Call = String -> String -> Aff { text :: String, raw :: String }

-- | Evidence by pointer, appended for every call: the effect, the model, digests of the prompt
-- | template and the JSON payload, the path of the raw event stream, and whether the reply decoded.
type Evidence = { effect :: String, model :: String, template :: String, input :: String, raw :: String, decoded :: String }

-- | The model backend a handler interprets over: the raw call, the model name for evidence, the
-- | digest device that hashes a template or payload for evidence, and the sinks for each call's
-- | evidence and its typed exchange (the recording, when one is kept).
type Backend = { model :: String, call :: Call, digest :: String -> String, timeoutMs :: Int, evidence :: Evidence -> Effect Unit, exchange :: Exchange -> Effect Unit }

-- | A backend with the default timeout and sinks that keep nothing; a composition root overrides what it keeps.
backend :: { model :: String, call :: Call, digest :: String -> String } -> Backend
backend b = { model: b.model, call: b.call, digest: b.digest, timeoutMs: defaultTimeoutMs, evidence: const (pure unit), exchange: const (pure unit) }

-- | How long one model call may take before the run fails.
defaultTimeoutMs :: Int
defaultTimeoutMs = 300000

-- | What a call concerns, for the recording: its round and the seats whose transcripts it carries.
type Key = { round :: Int, seats :: Array String }

-- | The joint pass makes at most this many corrections after its first call (docs/04 §11).
maxCorrections :: Int
maxCorrections = 2

seatText :: EntityId -> String
seatText (Id s) = s

-- | One call with its payload already encoded: the payload goes to the backend, the reply decodes,
-- | and the call's evidence and typed exchange are kept. A failed call or one that times out leaves
-- | evidence naming why, keeps no exchange, and rethrows.
send :: forall a. Backend -> String -> Key -> String -> Json -> Shape a -> (String -> Either String a) -> Aff { text :: String, decoded :: Either String a }
send b effect key template request replyShape decode = do
  let input = stringify request
  let evidenceOf raw result = b.evidence { effect, model: b.model, template: b.digest template, input: b.digest input, raw, decoded: result }
  out <- within b (b.call template input) >>= case _ of
    Right r -> pure r
    Left err -> do
      liftEffect (evidenceOf "" ("error: the call failed: " <> message err))
      throwError err
  let decoded = decode out.text
  liftEffect do
    evidenceOf out.raw (either ("error: " <> _) (const "ok") decoded)
    b.exchange { effect, round: key.round, seats: key.seats, request, reply: encodeReply replyShape decoded }
  pure { text: out.text, decoded }

-- | Run a call under the backend's timeout: the first of its result and the timer. The loser is cancelled.
within :: forall a. Backend -> Aff a -> Aff (Either Error a)
within b call = sequential (parallel (attempt call) <|> parallel timer)
  where
  timer = delay (Milliseconds (toNumber b.timeoutMs)) $> Left (error ("the model call timed out after " <> show b.timeoutMs <> " ms"))

-- | One call: the payload is encoded through its shape, and the reply decodes.
ask :: forall p a. Backend -> String -> Key -> String -> Shape p -> p -> Shape a -> (String -> Either String a) -> Aff (Either String a)
ask b effect key template requestShape payload replyShape decode =
  _.decoded <$> send b effect key template (encodeWith Snapshot requestShape payload) replyShape decode

-- | A correction's payload: the original request, the previous reply's text, and the decode error as
-- | the decoder reports it. The system prompt is the same as the first call's.
correction :: Json -> String -> String -> Json
correction request previous err = encodeWith Snapshot correctionShape { request, previous_reply: previous, error: err }

-- | The joint pass: one call, then up to `maxCorrections` corrections while the reply does not decode.
-- | The first reply that decodes is the result. If the last correction does not decode either, the
-- | result is that decode error (the caller tells the speakers and submits nothing).
joint :: Backend -> Key -> JointRequest -> Aff (Either String (Array ProposedDeal))
joint b key request = go 0 original
  where
  original = encodeWith Snapshot jointRequestShape request
  go n payload = do
    out <- send b "interpret" key interpretPrompt payload jointReplyShape decodeJointReply
    case out.decoded of
      Right r -> pure (Right r.proposals)
      Left err
        | n < maxCorrections -> go (n + 1) (correction original out.text err)
        | otherwise -> pure (Left err)

-- | The round's transcripts to Proposals, one joint pass (`joint`). A reply that still does not
-- | decode after the corrections is a `Left` (the caller tells the speakers and submits nothing).
-- | Entries the engine could not use are dropped by the joint pass rule (`Core.Proposal.jointProposals`).
-- | `w` supplies only the Entity and Market ids, which no round changes, so the initial world serves.
interpreter :: Backend -> WorldState -> InterpretF ~> Aff
interpreter b w (Interpret round transcripts k) = k <$> do
  let
    request :: JointRequest
    request = { round, transcripts, entities: map _.id (values w.entities), markets: markets w }
    key = { round, seats: map (\t -> seatText t.seat) transcripts }
  r <- joint b key request
  pure (map (jointProposals (\s -> Map.member s w.entities) transcripts) r)

markets :: WorldState -> Array MarketId
markets w = map _.id (values w.markets)

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
speak :: Backend -> (EntityId -> String) -> EntityId -> View -> Aff String
speak b briefing e v = do
  let
    request :: SpeakRequest
    request = { actor: e, briefing: briefing e, view: v }
    key = { round: v.round, seats: [ seatText e ] }
  either (const "") identity <$> ask b "seat:speak" key speakPrompt speakRequestShape request stringShape decodeText
