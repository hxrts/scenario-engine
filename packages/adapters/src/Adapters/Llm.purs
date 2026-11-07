-- | Language-model adapters (docs/04_instantiated_game.md §11, docs/01_overview.md §10). Each
-- | call is one ephemeral, tool-less `pi` process; the model ID is the only configuration.
-- | Every prompt asks for one JSON object, decoded with the shared codecs. A reply that fails
-- | to decode is an empty submission (the joint pass, or a seat's transcript) or a plain event
-- | listing (Narrate), and is never retried silently. Model output is only a candidate: the
-- | engine checks authority, signatures, and balances when it resolves the round.
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
import Control.Monad.Error.Class (catchError, throwError)
import Core.Types (EntityId, Intent, MarketId)
import Core.World (View, WorldState)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, filter, fromFoldable, length, mapMaybe, nub, null)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Effects.Signatures (InterpretF(..), JointProposal, NarrateF(..), Transcript)

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

-- | Canned replies keyed by the call (the test suites and CI; no network).
stub :: (String -> String -> String) -> Backend
stub reply = { model: "stub", call: \system input -> pure { text: reply system input, raw: "" } }

ask :: forall a. DecodeJson a => Backend -> Log -> String -> String -> Json -> Aff (Either String a)
ask b log effect template payload = do
  let input = stringify payload
  out <- b.call template input
  let decoded = readJson (firstObject out.text)
  liftEffect $ log { effect, model: b.model, template: sha256 template, input: sha256 input, raw: out.raw, decoded: either ("error: " <> _) (const "ok") decoded }
  pure decoded

-- | The joint pass's reply: one entry per reconciled Proposal.
type JointReply = { proposals :: Array { author :: EntityId, intents :: Array Intent, signers :: Array EntityId, agreed :: Array EntityId } }

-- | The round's transcripts to Proposals, one joint call. A reply that does not decode is a
-- | `Left` (the caller tells the speakers and submits nothing). Entries the engine could not
-- | use are dropped here: an author who did not speak, an empty Intent list, and unknown
-- | signers. Only speakers can be recorded as agreeing.
interpreter :: Backend -> Log -> WorldState -> InterpretF ~> Aff
interpreter b log w (Interpret round transcripts k) = k <$> do
  r <- ask b log "interpret" interpretTemplate
    (encodeJson { round, transcripts, entities: map _.id (fromFoldable (Map.values w.entities)), markets: markets })
  pure (map (\(x :: JointReply) -> mapMaybe joint x.proposals) r)
  where
  speakers = map _.seat transcripts
  markets :: Array MarketId
  markets = map _.id (fromFoldable (Map.values w.markets))
  joint p
    | not (p.author `elem` speakers) = Nothing
    | null p.intents = Nothing
    | otherwise = Just
        { author: p.author
        , draft: { intents: p.intents, signers: filter (\s -> s /= p.author && Map.member s w.entities) p.signers, procedure: Nothing }
        , signed: nub ([ p.author ] <> filter (\s -> s `elem` speakers) p.agreed)
        }

-- | Resolved events to front-page text; grounded only in the round's record.
narrator :: Backend -> Log -> NarrateF ~> Aff
narrator b log (Narrate r k) = k <$> do
  out <- ask b log "narrate" narrateTemplate (encodeJson { round: r.record.round, events: r.record.events })
  pure case out of
    Right ({ text } :: { text :: String }) -> text
    Left _ -> "Round " <> show r.record.round <> ": " <> show (length r.record.events) <> " events recorded."

-- | AI-agent seats: one short transcript per round, from the role briefing and the authorized View.
-- | An undecodable reply is an empty transcript, so the seat submits nothing.
agents :: Backend -> Log -> (EntityId -> String) -> { speak :: EntityId -> View -> Aff String }
agents b log briefing =
  { speak: \e v -> either (const "") (\(x :: { text :: String }) -> x.text) <$> ask b log "seat:speak" speakTemplate (encodeJson { actor: e, briefing: briefing e, view: v })
  }

common :: String
common = "You are a component of a scenario-planning game engine. The user message is a JSON payload. Reply with exactly one JSON object and nothing else. Intents use the engine's JSON encoding as it appears in the payload (tagged objects with \"tag\" and \"values\"). Never invent quantities, Entities, Markets, or Resources that are not in the payload."

interpretTemplate :: String
interpretTemplate = common <> "\nTask: read the round's `transcripts` together and reconcile them into Proposals. Each transcript is one seat's informal, spoken account of what it intends (`seat` is its id, `text` what it said). A transcript that describes a deal is that seat's agreement to it, and a seat that described a deal wanted it. Where transcripts describe the same deal, reconcile them into one Proposal, adjusting details slightly where the transcripts are loose and preferring Intents that match. Otherwise keep each Proposal in the spirit of its transcript. A deal between seats is Transfer intents; a Trade is one seat's own action. Never ask a question and never reply with a clarification; reconcile instead. Reply {\"proposals\":[{\"author\":EntityId,\"intents\":[Intent],\"signers\":[EntityId],\"agreed\":[EntityId]}]}. `author` is a seat that spoke. `signers` are the other parties the Proposal needs. `agreed` lists the seats whose transcripts describe it. Use only ids from `entities` and `markets`. An empty list is allowed."

narrateTemplate :: String
narrateTemplate = common <> "\nTask: write the round's front page: a short newspaper-style summary of `events`. Report only what the events state; add no facts, motives, or numbers not present. Reply {\"text\":string}."

speakTemplate :: String
speakTemplate = common <> "\nTask: you play the seat `actor` according to `briefing`. Reply {\"text\":string}: a short transcript, one to three sentences, of what you intend to do this round in your own words, naming any deal you propose and the party it is with."
