-- | The SEAT_INPUT interpreter: one generic handler over a Game's seat bindings. A
-- | Strategy binding calls the pure `policy` interpreter; a Script answers from its rounds, with no
-- | eligibility filter; a Recorded binding answers from stored round records. Agent and Human bindings
-- | are answered by live play (`Handlers.Live`), never here: this handler gives them no answer, and the
-- | simulator rejects them before it runs.
module Handlers.Seats
  ( SeatEnv
  , seatEnv
  , proposeFor
  , signFor
  , recordedBindings
  , handleSeats
  , RecordSink
  , playPure
  , playWith
  ) where

import Prelude

import Core.Draw (Seed)
import Core.Num (Q)
import Core.Proposal (ProposalBody, submissionOrder)
import Core.Types (EntityId, ProposalId)
import Core.World (RoundRecord, RoundResult, View)
import Data.Array (concatMap, elem, filter, find, nub, sortWith)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effects.Signatures (INTERPRET, InterpretF(..), NARRATE, NarrateF(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Foreign.Object (Object)
import Game.Types (Bindings, Game, SeatBinding(..))
import Policy.Interpret (envFor)
import Policy.Interpret as Policy
import Core.Engine.Commit (EngineError)
import Core.Proposal (Transcript)
import Core.Strategy (InWorld)
import Core.World (WorldState)
import Effect (Effect)
import Effects.Round (runGame)
import Run (Run, case_, interpret, on, peel, run, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | What a machine seat's answer depends on besides its own binding: the player seed and the Gameboard's
-- | parameters.
type SeatEnv = { seed :: Seed, params :: Object Q }

-- | The seat environment of a game under a player seed.
seatEnv :: Seed -> Game -> SeatEnv
seatEnv seed g = { seed, params: g.parameters }

-- | The Proposal bodies a seat submits in a round. Agent and Human seats submit none here.
proposeFor :: SeatEnv -> Bindings -> EntityId -> View -> Array ProposalBody
proposeFor env bs e v = case Map.lookup e bs of
  Just (Strategy p) -> Policy.propose p (envFor { params: env.params, seed: env.seed, source: "seat" } e v)
  Just (Script rounds) -> maybe [] _.propose (find (\r -> r.round == v.round) rounds)
  Just (Recorded records) ->
    map (\s -> { intents: s.proposal.intents, signers: s.proposal.signers, procedure: s.proposal.procedure })
      (sortWith submissionOrder (filter (\s -> s.proposal.author == e) (inputsOf records v.round)))
  Just Agent -> []
  Just Human -> []
  Nothing -> []

-- | The Proposals a seat signs in a round. Agent and Human seats sign nothing here.
signFor :: SeatEnv -> Bindings -> EntityId -> View -> Array ProposalId
signFor env bs e v = case Map.lookup e bs of
  Just (Strategy p) -> Policy.sign p (envFor { params: env.params, seed: env.seed, source: "seat" } e v)
  Just (Script rounds) -> maybe [] _.sign (find (\r -> r.round == v.round) rounds)
  Just (Recorded records) -> map _.proposal.id (filter (\s -> e `elem` s.signatures) (inputsOf records v.round))
  Just Agent -> []
  Just Human -> []
  Nothing -> []

inputsOf :: forall t a r. Eq a => Array { inputs :: Array t, round :: a | r } -> a -> Array t
inputsOf records r = maybe [] _.inputs (find (\x -> x.round == r) records)

-- | The Recorded bindings of every Entity a set of round records mentions, as authors or signers.
recordedBindings :: Array RoundRecord -> Bindings
recordedBindings records = Map.fromFoldable (map (\e -> Tuple e (Recorded records)) mentioned)
  where
  mentioned = nub (concatMap (\r -> concatMap (\s -> [ s.proposal.author ] <> s.signatures) r.inputs) records)

-- | Interpret SEAT_INPUT over a game's bindings, leaving the other effects. A machine seat has no
-- | transcript, so Speak answers Nothing and the joint pass is never called: a simulated run makes no
-- | model call. Narration answers the empty text, since no front page is written without live play.
handleSeats :: forall r a. SeatEnv -> Bindings -> Run (SEAT_INPUT + INTERPRET + NARRATE + r) a -> Run r a
handleSeats env bs = interpret (on (Proxy :: _ "seatInput") handle (on (Proxy :: _ "interpret") silent (on (Proxy :: _ "narrate") quiet send)))
  where
  handle :: SeatInputF ~> Run r
  handle = case _ of
    Propose e v k -> pure (k (proposeFor env bs e v))
    Sign e v k -> pure (k (signFor env bs e v))
    Speak _ _ k -> pure (k Nothing)

  silent :: InterpretF ~> Run r
  silent (Interpret _ _ k) = pure (k (Right []))

  quiet :: NarrateF ~> Run r
  quiet (Narrate _ k) = pure (k "")

-- | Where a round goes once it is resolved: the round, with the transcripts read for it.
type RecordSink = RoundResult -> Array Transcript -> Effect Unit

-- | Play `n` rounds over machine bindings with records kept in memory. Pure. The simulator and the
-- | replay runner are this fold over their own bindings.
playPure :: SeatEnv -> Bindings -> InWorld -> Int -> WorldState -> { records :: Array RoundResult, value :: Either EngineError WorldState }
playPure env bs inWorld n w = collectRecords (handleSeats env bs (runGame inWorld n w))

-- | Play `n` rounds over machine bindings, each round going to `sink` as it completes.
playWith :: SeatEnv -> Bindings -> RecordSink -> InWorld -> Int -> WorldState -> Effect (Either EngineError WorldState)
playWith env bs sink inWorld n w =
  run (case_ # on (Proxy :: _ "record") (\(Record result transcripts next) -> sink result transcripts $> next)) (handleSeats env bs (runGame inWorld n w))

-- | Collect RECORD purely. The transcripts of a record are not kept: a machine-driven run has none.
collectRecords :: forall a. Run (RECORD + ()) a -> { records :: Array RoundResult, value :: a }
collectRecords = go []
  where
  go records r = case peel r of
    Right value -> { records, value }
    Left v -> v # (case_ # on (Proxy :: _ "record") (\(Record result _ next) -> go (records <> [ result ]) next))
