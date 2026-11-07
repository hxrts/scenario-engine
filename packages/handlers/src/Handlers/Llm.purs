-- | The language-model handler (docs/04_instantiated_game.md §11, docs/01_overview.md §10): the joint
-- | pass, the narrator, and the AI-agent seats, interpreted over a typed `ModelPort`. The port takes
-- | domain requests and answers domain replies: no prompt text, no JSON, and no digests cross it.
-- | Prompts, the JSON wire format, and the `pi` process belong to the adapter (`Adapters.Llm`), which
-- | builds each reply from the model's text. A reply that does not decode is a `Left`: an empty
-- | submission (the joint pass, or a seat's transcript) or a plain event listing (Narrate). A failed
-- | call is an Aff error, which the run treats as fatal. Model output is only a candidate: the engine
-- | checks authority, signatures, and balances when it resolves the round.
module Handlers.Llm
  ( ModelPort
  , JointRequest
  , ProposedDeal
  , JointReply
  , NarrateRequest
  , NarrateReply
  , SpeakRequest
  , SpeakReply
  , interpreter
  , narrator
  , listing
  , agents
  ) where

import Prelude

import Core.Types (EntityId, Intent, MarketId, RoundIndex)
import Core.World (Event, RoundResult, View, WorldState)
import Data.Array (elem, filter, fromFoldable, mapMaybe, nub, null)
import Data.Either (Either, either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effects.Signatures (InterpretF(..), JointProposal, NarrateF(..), Transcript)

-- | The joint pass's request: the round, its seat transcripts, and the Entities and Markets a
-- | Proposal may name.
type JointRequest = { round :: RoundIndex, transcripts :: Array Transcript, entities :: Array EntityId, markets :: Array MarketId }

-- | One Proposal as the model states it, before the engine's checks: its author, Intents, the
-- | other signers the deal asks for, and the seats whose transcripts describe it.
type ProposedDeal = { author :: EntityId, intents :: Array Intent, signers :: Array EntityId, agreed :: Array EntityId }

-- | The joint pass's reply: the Proposals the model reconciled, or why its reply did not decode.
type JointReply = Either String (Array ProposedDeal)

type NarrateRequest = { round :: RoundIndex, events :: Array Event }

-- | The front page text, or why the reply did not decode.
type NarrateReply = Either String String

-- | One AI-agent seat's turn of speech: the seat, its briefing, and what it can see.
type SpeakRequest = { actor :: EntityId, briefing :: String, view :: View }

-- | The seat's transcript for the round, or why the reply did not decode.
type SpeakReply = Either String String

-- | The language-model port, implemented by an adapter. A failed call is an Aff error; a reply the
-- | model gave that does not decode is a `Left` in the reply.
type ModelPort =
  { model :: String
  , interpret :: JointRequest -> Aff JointReply
  , narrate :: NarrateRequest -> Aff NarrateReply
  , speak :: SpeakRequest -> Aff SpeakReply
  }

-- | The round's transcripts to Proposals, one joint call. A reply that does not decode is a
-- | `Left` (the caller tells the speakers and submits nothing). Entries the engine could not
-- | use are dropped here: an author who did not speak, an empty Intent list, and unknown
-- | signers. Only speakers can be recorded as agreeing.
interpreter :: ModelPort -> WorldState -> InterpretF ~> Aff
interpreter b w (Interpret round transcripts k) = k <$> do
  r <- b.interpret { round, transcripts, entities: map _.id (fromFoldable (Map.values w.entities)), markets: markets }
  pure (map (mapMaybe joint) r)
  where
  speakers = map _.seat transcripts

  markets :: Array MarketId
  markets = map _.id (fromFoldable (Map.values w.markets))

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
narrator :: ModelPort -> Boolean -> NarrateF ~> Aff
narrator b jointFailed (Narrate r k) = k <$>
  if jointFailed then pure (listing r)
  else either (const (listing r)) identity <$> b.narrate { round: r.record.round, events: r.record.events }

-- | The round's events, one per line, with no interpretation.
listing :: RoundResult -> String
listing r = joinWith "\n" ([ "Round " <> show r.record.round <> ":" ] <> bullets)
  where
  bullets = if null r.record.events then [ "- no events" ] else map (\e -> "- " <> show e) r.record.events

-- | AI-agent seats: one short transcript per round, from the role briefing and the authorized View.
-- | An undecodable reply is an empty transcript, so the seat submits nothing.
agents :: ModelPort -> (EntityId -> String) -> { speak :: EntityId -> View -> Aff String }
agents b briefing =
  { speak: \e v -> either (const "") identity <$> b.speak { actor: e, briefing: briefing e, view: v }
  }
