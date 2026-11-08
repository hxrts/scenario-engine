-- | The Live handler (docs/04_instantiated_game.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats type one transcript per round at their prompt, within the propose window; AI-agent
-- | seats speak through their own model call; the round's transcripts go to one joint pass (INTERPRET).
-- | Each round is recorded with those transcripts, then narrated (NARRATE). Any other seat plays its
-- | bound strategy. There is no clarification and no confirmation.
module Handlers.Live (Live, runLive) where

import Prelude

import Core.Draw (Seed)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parallel, sequential)
import Core.Types (EntityId, RoundIndex)
import Core.World (RoundResult, View, WorldState)
import Core.Engine.Commit (EngineError)
import Data.Map as Map
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, attempt, forkAff, joinFiber, killFiber)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (InterpretF(..), NarrateF(..), RecordF(..), SeatInputF(..))
import Game.Types (Game, SeatBinding(..))
import Handlers.Seats (RecordSink, proposeFor, seatEnv, signFor)
import Effects.Round (runGame)
import Run (case_, on, run)
import Type.Proxy (Proxy(..))

-- | The live session's devices and callbacks, supplied by the composition root: the player seed (the
-- | machine seats draw from it), a line of input, the propose window's timer (`Nothing` for no deadline),
-- | output, the model calls, and the record write. Which seats are human or agent is the game's: each
-- | seat's SeatBinding.
type Live =
  { seed :: Seed
  , input :: String -> Aff String -- a human seat's line, after the prompt text
  , window :: Maybe (Aff Unit) -- the propose window: completes when the window has elapsed
  , say :: String -> Effect Unit -- a line for the facilitator
  , notify :: EntityId -> String -> Aff Unit -- a message to the seat that needs no answer
  , interpret :: InterpretF ~> Aff -- the round's joint pass
  , narrate :: Boolean -> RoundResult -> Aff String -- the round's front page; the Boolean is whether its joint pass did not decode
  , speak :: EntityId -> View -> Aff String -- an AI-agent seat's transcript for the round (its model call)
  , record :: RecordSink -- persist the round
  }

-- | The round whose propose window is running, and whether it has closed.
type Window = { round :: RoundIndex, timer :: Fiber Unit, closed :: Ref Boolean }

-- | A seat's words as the round reads them: an empty transcript is silence (Nothing).
spokenText :: String -> Maybe String
spokenText "" = Nothing
spokenText s = Just s

-- | The prompt a human seat types its transcript at.
prompt :: EntityId -> RoundIndex -> String
prompt seat round = "[round " <> show round <> ", propose window] " <> show seat <> "> "

-- | Play a game live: the round program runs on the game's world and seats. Human and Agent seats are
-- | answered here; every other binding is answered by `Handlers.Seats`. Every human is prompted in
-- | turn under one propose window, opened at the round's first prompt. A human who has not answered
-- | when it closes says nothing that round.
runLive :: Live -> Game -> Aff (Either EngineError WorldState)
runLive live game = do
  -- set by the round's joint pass when its reply does not decode; read and cleared when the round is narrated
  failed <- liftEffect (Ref.new false)
  window <- liftEffect (Ref.new Nothing)
  run (case_ # on (Proxy :: _ "seatInput") (seat window) # on (Proxy :: _ "interpret") (joint failed) # on (Proxy :: _ "record") (record window) # on (Proxy :: _ "narrate") (narrate failed)) (runGame game.inWorld game.rounds game.world)
  where
  env = seatEnv live.seed game
  binding e = Map.lookup e game.seats

  seat :: Ref (Maybe Window) -> SeatInputF ~> Aff
  seat window = case _ of
    Speak e v k -> k <<< spokenText <$> case binding e of
      Just Human -> human window e v.round
      Just Agent -> live.speak e v
      _ -> pure ""
    Propose e v k -> k <$> case binding e of
      Just Human -> pure []
      Just Agent -> pure []
      _ -> pure (proposeFor env game.seats e v)
    -- humans give no input after their transcript, and agents' agreement is read in the joint pass
    Sign e v k -> k <$> case binding e of
      Just Human -> pure []
      Just Agent -> pure []
      _ -> pure (signFor env game.seats e v)

  human :: Ref (Maybe Window) -> EntityId -> RoundIndex -> Aff String
  human window e round = case live.window of
    Nothing -> live.input (prompt e round)
    Just timer -> do
      w <- opened window timer round
      over <- liftEffect (Ref.read w.closed)
      if over then liftEffect (live.say (show e <> " was not reached before the window closed")) $> ""
      else do
        answer <- sequential (parallel (attempt (live.input (prompt e round))) <|> parallel (joinFiber w.timer $> Right ""))
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

  narrate :: Ref Boolean -> NarrateF ~> Aff
  narrate failed (Narrate r k) = do
    f <- liftEffect (Ref.read failed <* Ref.write false failed)
    k <$> live.narrate f r

  -- A transcript that does not decode submits nothing for the round, and each human speaker is
  -- told so. Nothing is asked back. The round is marked, so its front page is a plain listing.
  joint :: forall b. Ref Boolean -> InterpretF b -> Aff b
  joint failed (Interpret round transcripts k) = do
    out <- live.interpret (Interpret round transcripts identity)
    liftEffect (Ref.write (isLeft out) failed)
    case out of
      Left why -> for_ transcripts \t -> case binding t.seat of
        Just Human -> live.notify t.seat ("Round " <> show round <> ": the transcripts could not be read (" <> why <> "), so nothing was submitted")
        _ -> pure unit
      Right _ -> pure unit
    pure (k out)
