-- | Language-model adapters (docs/04_instantiated_game.md §11, docs/01_overview.md §10). Each
-- | call is one ephemeral, tool-less `pi` process; the model ID is the only configuration.
-- | Every prompt asks for one JSON object, decoded with the shared codecs. A reply that fails
-- | to decode becomes a clarification (Interpret), an empty submission (seats), or a plain
-- | event listing (Narrate), and is never retried silently. Model output is only a candidate:
-- | it still goes through confirmation and screening.
module Adapters.Llm
  ( Backend
  , Evidence
  , defaultModel
  , pi
  , stub
  , interpreter
  , narrator
  , agents
  ) where

import Prelude

import Board.Decode.Common (readJson)

import Core.Engine (authorized, executor)
import Control.Monad.Error.Class (catchError, throwError)
import Core.Types (Entity, EntityId, ProposalId, proposalId)
import Core.World (View)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (all, concatMap, elem, filter, length, mapWithIndex)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Effects (InterpretF(..), Interpretation(..), NarrateF(..))
import Policy.Interpret (Draft)

-- | One model call: the system template and the JSON payload, to the reply text and the
-- | path of its raw event stream.
type Backend = { model :: String, call :: String -> String -> Aff { text :: String, raw :: String } }

-- | Evidence by pointer, appended for every call.
type Evidence = { effect :: String, model :: String, template :: String, input :: String, raw :: String, decoded :: String }

type Log = Evidence -> Effect Unit

defaultModel :: String
defaultModel = "anthropic/claude-opus-5-5"

foreign import piImpl :: String -> String -> String -> String -> EffectFnAff { text :: String, raw :: String }
foreign import sha256 :: String -> String
foreign import firstObject :: String -> String

pi :: { model :: String, rawDir :: String } -> Backend
pi c = { model: c.model, call: \system input -> fromEffectFnAff (piImpl c.model c.rawDir system input) `catchError` backendFailed c.model }

-- | A backend failure (a missing `pi`, a non-zero exit, an unwritable raw directory) is fatal to
-- | the run: it is rethrown with the model and the cause named, and the run is marked failed.
backendFailed :: forall a. String -> Error -> Aff a
backendFailed model e = throwError (error ("language model backend failed (" <> model <> "): " <> message e))

-- | Canned replies keyed by the call (the interpretation suite and CI; no network).
stub :: (String -> String -> String) -> Backend
stub reply = { model: "stub", call: \system input -> pure { text: reply system input, raw: "" } }

ask :: forall a. DecodeJson a => Backend -> Log -> String -> String -> Json -> Aff (Either String a)
ask b log effect template payload = do
  let input = stringify payload
  out <- b.call template input
  let decoded = readJson (firstObject out.text)
  liftEffect $ log { effect, model: b.model, template: sha256 template, input: sha256 input, raw: out.raw, decoded: either ("error: " <> _) (const "ok") decoded }
  pure decoded

type Reply = { kind :: String, proposals :: Maybe (Array Draft), sign :: Maybe (Array ProposalId), message :: Maybe String }

-- | Typed text to candidate Proposals and signatures. Admissibility is checked here against
-- | the Actor's own capabilities, not taken from the model.
interpreter :: Backend -> Log -> (EntityId -> Maybe Entity) -> InterpretF ~> Aff
interpreter b log entity (Interpret e v said k) = k <$> do
  r <- ask b log "interpret" interpretTemplate (encodeJson { actor: e, capabilities: map _.capabilities (entity e), view: v, text: said })
  pure case r of
    Left _ -> Clarify "I could not read that as an action. Please restate it."
    Right (x :: Reply) -> case x.kind of
      "understood" ->
        let
          ds = fromMaybe [] x.proposals
          caps = fromMaybe [] (map _.capabilities (entity e))
          ok = all identity (concatMap (\{ i, d } -> let p = { id: proposalId v.round e i, author: e, intents: d.intents, signers: d.signers, procedure: d.procedure } in map (\it -> executor p it /= e || authorized caps e it) d.intents) (mapWithIndex (\i d -> { i, d }) ds))
        in
          -- a sign list is a candidate too: keep only the ids the Actor may sign now
          if ok then Understood ds (filter (_ `elem` map _.id v.toSign) (fromMaybe [] x.sign)) else Inadmissible "the action needs a capability this Actor does not hold"
      "inadmissible" -> Inadmissible (fromMaybe "" x.message)
      _ -> Clarify (fromMaybe "Please clarify." x.message)

-- | Resolved events to front-page text; grounded only in the round's record.
narrator :: Backend -> Log -> NarrateF ~> Aff
narrator b log (Narrate r k) = k <$> do
  out <- ask b log "narrate" narrateTemplate (encodeJson { round: r.record.round, events: r.record.events })
  pure case out of
    Right ({ text } :: { text :: String }) -> text
    Left _ -> "Round " <> show r.record.round <> ": " <> show (length r.record.events) <> " events recorded."

-- | AI-agent seats: Proposals and signatures from the role briefing and the authorized View.
agents :: Backend -> Log -> (EntityId -> String) -> { propose :: EntityId -> View -> Aff (Array Draft), sign :: EntityId -> View -> Aff (Array ProposalId) }
agents b log briefing =
  { propose: \e v -> either (const []) (\(x :: Reply) -> fromMaybe [] x.proposals) <$> ask b log "seat:propose" seatTemplate (encodeJson { step: "propose", actor: e, briefing: briefing e, view: v })
  , sign: \e v -> either (const []) (\(x :: Reply) -> fromMaybe [] x.sign) <$> ask b log "seat:sign" seatTemplate (encodeJson { step: "sign", actor: e, briefing: briefing e, view: v })
  }

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents and Drafts use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Entities, Markets, or Resources that are not in the payload."

interpretTemplate :: String
interpretTemplate = common <> "\nTask: interpret a human player's typed text as game actions for `actor`, using only `capabilities` and `view`. Reply {\"kind\":\"understood\",\"proposals\":[Draft],\"sign\":[ProposalId]} when the action is clear (sign lists ids from view.toSign the player endorses); {\"kind\":\"clarify\",\"message\":question} when a quantity, counterparty, or market is materially ambiguous; {\"kind\":\"inadmissible\",\"message\":reason} when the player claims resources or authority they lack. A Draft is {\"intents\":[Intent],\"signers\":[EntityId],\"procedure\":null}."

narrateTemplate :: String
narrateTemplate = common <> "\nTask: write the round's front page: a short newspaper-style summary of `events`. Report only what the events state; add no facts, motives, or numbers not present. Reply {\"text\":string}."

seatTemplate :: String
seatTemplate = common <> "\nTask: you play the seat `actor` according to `briefing`. For step \"propose\" reply {\"kind\":\"understood\",\"proposals\":[Draft]} (an empty list is allowed); for step \"sign\" reply {\"kind\":\"understood\",\"sign\":[ProposalId]} choosing among view.toSign. A Draft is {\"intents\":[Intent],\"signers\":[EntityId],\"procedure\":null}."
