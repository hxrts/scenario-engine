-- | The language-model handler (docs/04_instantiated_game.md §11, docs/01_overview.md §10): the joint
-- | pass, the narrator, and the AI-agent seats, interpreted over a typed `Backend`. A backend is the
-- | raw call (one prompt and one JSON payload in, the reply text out) plus the sinks for evidence and
-- | for the typed exchange. The handler owns the rest: the request payloads, the prompt for each
-- | request (`Handlers.Llm.Wire`), decoding, the fallbacks, the joint pass's reconciliation, and the
-- | evidence digests. A reply that does not decode is a `Left` (the joint pass and the narrator
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

import Core.Proposal (JointProposal, Transcript)
import Core.Types (EntityId, MarketId)
import Core.World (RoundResult, View, WorldState)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (elem, filter, fromFoldable, mapMaybe, nub, null)
import Data.Either (Either, either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effects.Signatures (InterpretF(..), NarrateF(..))
import Handlers.Llm.Wire (JointRequest, NarrateRequest, ProposedDeal, SpeakRequest, decodeJoint, decodeText, interpretPrompt, narratePrompt, speakPrompt)
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

-- | One call: the payload is encoded, the prompt and the payload go to the backend, the reply
-- | decodes, and the call's evidence and typed exchange are kept. A failed call is not kept.
ask :: forall a. EncodeJson a => Backend -> String -> String -> Json -> (String -> Either String a) -> Aff (Either String a)
ask b effect template payload decode = do
  let input = stringify payload
  out <- b.call template input
  let decoded = decode out.text
  liftEffect do
    b.evidence { effect, model: b.model, template: sha256 template, input: sha256 input, raw: out.raw, decoded: either ("error: " <> _) (const "ok") decoded }
    b.exchange { effect, request: payload, reply: encodeReply decoded }
  pure decoded

-- | The round's transcripts to Proposals, one joint call. A reply that does not decode is a
-- | `Left` (the caller tells the speakers and submits nothing). Entries the engine could not use
-- | are dropped by `jointProposals`.
interpreter :: Backend -> WorldState -> InterpretF ~> Aff
interpreter b w (Interpret round transcripts k) = k <$> do
  let
    request :: JointRequest
    request = { round, transcripts, entities: map _.id (fromFoldable (Map.values w.entities)), markets: markets w }
  r <- ask b "interpret" interpretPrompt (encodeJson request) decodeJoint
  pure (map (jointProposals w transcripts) r)

markets :: WorldState -> Array MarketId
markets w = map _.id (fromFoldable (Map.values w.markets))

-- | The Proposals the model reconciled, as the engine uses them. An author who did not speak, an
-- | empty Intent list, and an unknown signer are dropped. Only speakers can be recorded as agreeing.
jointProposals :: WorldState -> Array Transcript -> Array ProposedDeal -> Array JointProposal
jointProposals w transcripts = mapMaybe joint
  where
  speakers = map _.seat transcripts

  joint :: ProposedDeal -> Maybe JointProposal
  joint p
    | not (p.author `elem` speakers) = Nothing
    | null p.intents = Nothing
    | otherwise = Just
        { author: p.author
        , draft: { intents: p.intents, signers: filter (\s -> s /= p.author && Map.member s w.entities) p.signers, procedure: Nothing }
        , signed: nub ([ p.author ] <> filter (\s -> s `elem` speakers) p.agreed)
        }

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
    either (const (listing r)) identity <$> ask b "narrate" narratePrompt (encodeJson request) decodeText

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
      either (const "") identity <$> ask b "seat:speak" speakPrompt (encodeJson request) decodeText
  }
