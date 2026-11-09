-- | The Live handler (docs/04_run.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats type one transcript per round at their prompt, within the propose window; AI-agent
-- | seats speak through their own model call; the round's transcripts go to one joint pass (INTERPRET).
-- | Each round is recorded with those transcripts. This module answers only
-- | what needs a person or a model; `Handlers.Seats.handleSeats` answers every other seat beneath it.
-- | There is no clarification and no confirmation.
module Handlers.Live (Live, runLive) where

import Prelude

import Core.Draw (Seed)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parallel, sequential)
import Laws.Types (ActorId, RoundIndex)
import Schema.Joint (JointCandidateTransaction, Transcript)
import Laws.World (View, WorldState)
import Laws.Engine.Commit (EngineError)
import Data.Map as Map
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, attempt, forkAff, joinFiber, killFiber)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (INTERPRET, InterpretF(..), RecordF(..), SEAT_INPUT, SeatInputF(..), _interpret, _record, _seatInput)
import Assembly.Types (PreparedRun, SeatBinding(..))
import Handlers.Seats (RecordSink, handleSeats, seatEnv)
import Effects.Round (runGame)
import Run (AFF, Run, case_, interpret, lift, liftAff, on, runBaseAff)
import Type.Row (type (+))

-- | The live session's devices and callbacks, supplied by the composition root: the player seed (the
-- | machine seats draw from it), a line of input, the propose window's timer (`Nothing` for no deadline),
-- | output, the model calls, and the record write. Which seats are human or agent is the game's: each
-- | seat's SeatBinding.
type Live =
  { seed :: Seed
  , input :: ActorId -> RoundIndex -> String -> Aff String -- a human seat's text for a round; the last argument is the prompt to show
  , window :: Maybe (Aff Unit) -- the propose window: completes when the window has elapsed
  , say :: String -> Effect Unit -- a line for the facilitator
  , notify :: ActorId -> String -> Aff Unit -- a message to the seat that needs no answer
  , interpret :: RoundIndex -> Array Transcript -> Aff (Either String (Array JointCandidateTransaction)) -- the round's joint pass
  , speak :: ActorId -> View -> Aff String -- an AI-agent seat's transcript for the round (its model call)
  , record :: RecordSink -- persist the round
  }

-- | The round whose propose window is running, and whether it has closed.
type Window = { round :: RoundIndex, timer :: Fiber Unit, closed :: Ref Boolean }

-- | A seat's words as the round reads them: an empty transcript is silence (Nothing).
spokenText :: String -> Maybe String
spokenText "" = Nothing
spokenText s = Just s

-- | The prompt a human seat types its transcript at.
prompt :: ActorId -> RoundIndex -> String
prompt seat round = "[round " <> show round <> ", propose window] " <> show seat <> "> "

-- | What the live layer leaves for `handleSeats`: every seat operation it does not answer.
type Beneath = SEAT_INPUT + INTERPRET + AFF + ()

-- | Play a game live: the round program runs on the game's world and seats. What a Human or Agent seat
-- | says is answered here; everything else falls through to `handleSeats`. Every human is prompted in
-- | turn under one propose window, opened at the round's first prompt. A human who has not answered
-- | when it closes says nothing that round.
runLive :: Live -> PreparedRun -> Aff (Either EngineError WorldState)
runLive live game = do
  window <- liftEffect (Ref.new Nothing)
  runBaseAff
    ( handleSeats (seatEnv live.seed game) game.seats
        ( interpret
            ( case_
                # on _seatInput (seat window)
                # on _interpret (\x -> liftAff (joint x))
                # on _record (\x -> liftAff (record window x))
            )
            (runGame game.inWorld game.rounds game.world)
        )
    )
  where
  binding e = Map.lookup e game.seats

  -- humans give no input after their transcript, and agents' agreement is read in the joint pass,
  -- so Propose and Sign fall through: `handleSeats` answers them with nothing for these seats
  seat :: Ref (Maybe Window) -> SeatInputF ~> Run Beneath
  seat window op = case op of
    Speak e v k -> case binding e of
      Just Human -> liftAff (k <<< spokenText <$> human window e v.round)
      Just Agent -> liftAff (k <<< spokenText <$> live.speak e v)
      _ -> lift _seatInput op
    _ -> lift _seatInput op

  human :: Ref (Maybe Window) -> ActorId -> RoundIndex -> Aff String
  human window e round = case live.window of
    Nothing -> live.input e round (prompt e round)
    Just timer -> do
      w <- opened window timer round
      over <- liftEffect (Ref.read w.closed)
      if over then liftEffect (live.say (show e <> " was not reached before the window closed")) $> ""
      else do
        answer <- sequential (parallel (attempt (live.input e round (prompt e round))) <|> parallel (joinFiber w.timer $> Right ""))
        either throwError pure answer

  -- the round's window, started at its first prompt
  opened :: Ref (Maybe Window) -> Aff Unit -> RoundIndex -> Aff Window
  opened window timer round = liftEffect (Ref.read window) >>= case _ of
    Just w | w.round == round -> pure w
    _ -> do
      closed <- liftEffect (Ref.new false)
      fiber <- forkAff do
        timer
        liftEffect (Ref.write true closed *> live.say ("— round " <> show round <> " propose window closed —"))
      let w = { round, timer: fiber, closed }
      liftEffect (Ref.write (Just w) window)
      pure w

  -- a round that is over needs no timer running
  record :: Ref (Maybe Window) -> RecordF ~> Aff
  record window (Record r transcripts next) = do
    running <- liftEffect (Ref.read window)
    for_ running \w -> killFiber (error "the round is over") w.timer
    liftEffect (Ref.write Nothing window)
    liftEffect (live.record r transcripts) $> next

  -- A transcript that does not decode submits nothing for the round, and each human speaker is
  -- told so. Nothing is asked back.
  joint :: forall b. InterpretF b -> Aff b
  joint (Interpret round transcripts k) = do
    out <- live.interpret round transcripts
    case out of
      Left why -> for_ transcripts \t -> case binding t.seat of
        Just Human -> live.notify t.seat ("Round " <> show round <> ": the transcripts could not be read (" <> why <> "), so nothing was submitted")
        _ -> pure unit
      Right _ -> pure unit
    pure (k out)
