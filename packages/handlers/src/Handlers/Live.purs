-- | The Live handler (docs/04_instantiated_game.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats submit one transcript per round within the propose window; AI-agent seats speak
-- | through their own model call; the round's transcripts go to one joint pass (INTERPRET). Any
-- | other seat plays its bound program. There is no clarification and no confirmation.
module Handlers.Live (Live, Seat(..), Prompt, runLive) where

import Prelude

import Core.Types (EntityId)
import Core.World (RoundResult, View)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effects.Signatures (ClockF(..), Deadline(..), INTERPRET, InterpretF(..), JointProposal, RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Handlers.Seats (SeatSource)
import Run (Run, case_, on, run)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Seat = Human | Agent | Program

-- | A human seat's prompt for its transcript: the round and the window it is typed in.
type Prompt = { seat :: EntityId, round :: Int, window :: Deadline }

type Live =
  { seat :: EntityId -> Seat
  , ask :: Prompt -> Aff String -- a human seat's transcript for the round
  , notify :: EntityId -> String -> Aff Unit -- a message to the seat that needs no answer
  , interpret :: InterpretF ~> Aff -- the round's joint pass
  , clock :: ClockF ~> Aff
  , agents :: { speak :: EntityId -> View -> Aff String }
  , programs :: SeatSource
  , record :: RoundResult -> Aff Unit -- persist, then narrate
  }

runLive :: forall a. Live -> Run (SEAT_INPUT + INTERPRET + RECORD + ()) a -> Aff a
runLive live = run (case_ # on (Proxy :: _ "seatInput") seat # on (Proxy :: _ "interpret") joint # on (Proxy :: _ "record") record)
  where
  record (Record r next) = live.record r *> pure next
  seat = case _ of
    Speak e v k -> k <$> case live.seat e of
      Human -> do
        text <- live.ask { seat: e, round: v.round, window: ProposeDeadline }
        live.clock (Await v.round ProposeDeadline unit)
        pure text
      Agent -> live.agents.speak e v
      Program -> pure ""
    Propose e v k -> k <$> case live.seat e of
      Program -> pure (live.programs.propose e v)
      _ -> pure []
    Sign e v k -> k <$> case live.seat e of
      Human -> live.clock (Await v.round SignDeadline unit) $> []
      Agent -> pure []
      Program -> pure (live.programs.sign e v)

  -- A transcript that does not decode submits nothing for the round, and each human speaker is
  -- told so. Nothing is asked back.
  joint :: forall b. InterpretF b -> Aff b
  joint (Interpret round transcripts k) = k <$> do
    out <- live.interpret (Interpret round transcripts identity)
    case out of
      Right (js :: Array JointProposal) -> pure js
      Left why -> do
        for_ transcripts \t -> case live.seat t.seat of
          Human -> live.notify t.seat ("Round " <> show round <> ": the transcripts could not be read (" <> why <> "), so nothing was submitted")
          _ -> pure unit
        pure []
