-- | The Live handler (docs/04_instantiated_game.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats submit one transcript per round within the propose window; AI-agent seats speak
-- | through their own model call; the round's transcripts go to one joint pass (INTERPRET). Any
-- | other seat plays its bound program. There is no clarification and no confirmation.
module Handlers.Live (Live, Seat(..), Prompt, runLive) where

import Prelude

import Core.Types (EntityId)
import Core.World (RoundResult, View)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (ClockF(..), Deadline(..), INTERPRET, InterpretF(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
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
  , record :: RoundResult -> Boolean -> Aff Unit -- persist, then narrate; the Boolean is whether the round's joint pass did not decode
  }

runLive :: forall a. Live -> Run (SEAT_INPUT + INTERPRET + RECORD + ()) a -> Aff a
runLive live prog = do
  -- set by the round's joint pass when its reply does not decode; read and cleared when the round is recorded
  failed <- liftEffect (Ref.new false)
  run (case_ # on (Proxy :: _ "seatInput") seat # on (Proxy :: _ "interpret") (joint failed) # on (Proxy :: _ "record") (record failed)) prog
  where
  record :: Ref Boolean -> RecordF ~> Aff
  record failed (Record r next) = do
    f <- liftEffect (Ref.read failed <* Ref.write false failed)
    live.record r f *> pure next
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
  -- told so. Nothing is asked back. The round is marked, so its front page is a plain listing.
  joint :: forall b. Ref Boolean -> InterpretF b -> Aff b
  joint failed (Interpret round transcripts k) = do
    out <- live.interpret (Interpret round transcripts identity)
    liftEffect (Ref.write (isLeft out) failed)
    case out of
      Left why -> for_ transcripts \t -> case live.seat t.seat of
        Human -> live.notify t.seat ("Round " <> show round <> ": the transcripts could not be read (" <> why <> "), so nothing was submitted")
        _ -> pure unit
      Right _ -> pure unit
    pure (k out)
