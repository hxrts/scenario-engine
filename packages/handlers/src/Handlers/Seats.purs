-- | Seat sources and the SEAT_INPUT handler they share. A source answers a seat's Propose
-- | and Sign steps: bound declarative programs (Test, Sim) or recorded input (Replay).
module Handlers.Seats (SeatSource, programs, replay, handleSeats, collectRecords) where

import Prelude

import Core.Draw (Seed)
import Core.Num (Q)
import Core.Types (EntityId, MarketId, ProposalId)
import Core.World (RoundRecord, RoundResult, View)
import Data.Array (elem, filter, find, sortWith)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Effects.Environment (envFor)
import Effects.Signatures (INTERPRET, InterpretF(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Foreign.Object (Object)
import Policy.Interpret (Draft)
import Policy.Interpret as Policy
import Policy.Program (Program)
import Run (Run, case_, interpret, on, peel, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type SeatSource =
  { propose :: EntityId -> View -> Array Draft
  , sign :: EntityId -> View -> Array ProposalId
  }

-- | Seats bound to declarative programs; their randomness draws from the player seed.
programs :: { bound :: Map EntityId Program, params :: Object Q, home :: EntityId -> Maybe MarketId, playerSeed :: Seed } -> SeatSource
programs b =
  { propose: \e v -> maybe [] (\p -> Policy.propose p (env e v)) (Map.lookup e b.bound)
  , sign: \e v -> maybe [] (\p -> Policy.sign p (env e v)) (Map.lookup e b.bound)
  }
  where
  env = envFor { params: b.params, home: b.home, seed: b.playerSeed, source: "seat" }

-- | Seats answered from recorded round records: what each seat proposed and signed.
replay :: Array RoundRecord -> SeatSource
replay records =
  { propose: \e v -> map (\s -> { intents: s.proposal.intents, signers: s.proposal.signers, procedure: s.proposal.procedure })
      (sortWith (_.proposal.id) (filter (\s -> s.proposal.author == e) (inputs v.round)))
  , sign: \e v -> map (_.proposal.id) (filter (\s -> e `elem` s.signatures) (inputs v.round))
  }
  where
  inputs r = maybe [] _.inputs (find (\x -> x.round == r) records)

-- | Interpret SEAT_INPUT with a source, leaving the other effects. A source has no transcripts,
-- | so Speak answers "" and the joint pass is never called: Sim and Replay runs make no model call.
handleSeats :: forall r a. SeatSource -> Run (SEAT_INPUT + INTERPRET + r) a -> Run r a
handleSeats source = interpret (on (Proxy :: _ "seatInput") handle (on (Proxy :: _ "interpret") silent send))
  where
  handle :: SeatInputF ~> Run r
  handle = case _ of
    Propose e v k -> pure (k (source.propose e v))
    Sign e v k -> pure (k (source.sign e v))
    Speak _ _ k -> pure (k "")
  silent :: InterpretF ~> Run r
  silent (Interpret _ _ k) = pure (k (Right []))

-- | Collect RECORD purely: the in-memory record store, shared by the replay and test runners.
collectRecords :: forall a. Run (RECORD + ()) a -> { records :: Array RoundResult, value :: a }
collectRecords = go []
  where
  go records r = case peel r of
    Right value -> { records, value }
    Left v -> v # (case_ # on (Proxy :: _ "record") (\(Record result next) -> go (records <> [ result ]) next))
