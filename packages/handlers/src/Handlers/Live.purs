-- | The Live handler (docs/04_instantiated_game.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats type one transcript per round at their prompt, within the propose window; AI-agent
-- | seats speak through their own model call; the round's transcripts go to one joint pass (INTERPRET).
-- | Each round is recorded with those transcripts, then narrated (NARRATE). Any other seat plays its
-- | bound strategy. Each deadline's window is waited out once. There is no clarification and no
-- | confirmation.
module Handlers.Live (Live, runLive) where

import Prelude

import Core.Draw (Seed)
import Core.Proposal (Transcript)
import Core.Types (EntityId, RoundIndex)
import Core.World (RoundResult, View, WorldState)
import Core.Engine.Commit (EngineError)
import Data.Map as Map
import Data.Array (elem)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (Deadline(..), InterpretF(..), NarrateF(..), RecordF(..), SeatInputF(..))
import Game.Types (Game, SeatBinding(..))
import Handlers.Seats (proposeFor, seatEnv, signFor)
import Effects.Round (runGame)
import Run (case_, on, run)
import Type.Proxy (Proxy(..))

-- | The live session's devices and callbacks, supplied by the composition root: the player seed (the
-- | machine seats draw from it), a line of input, the window's wait, output, the model calls, and the
-- | record write. Which seats are human or agent is the game's: each seat's SeatBinding.
type Live =
  { seed :: Seed
  , input :: String -> Aff String -- a human seat's line, after the prompt text
  , window :: Aff Unit -- one deadline's window of wall-clock time
  , say :: String -> Effect Unit -- a line for the facilitator
  , notify :: EntityId -> String -> Aff Unit -- a message to the seat that needs no answer
  , interpret :: InterpretF ~> Aff -- the round's joint pass
  , narrate :: Boolean -> RoundResult -> Aff String -- the round's front page; the Boolean is whether its joint pass did not decode
  , agents :: { speak :: EntityId -> View -> Aff String }
  , record :: RoundResult -> Array Transcript -> Aff Unit -- persist the round
  }

-- | A seat's words as the round reads them: an empty transcript is silence (Nothing).
spokenText :: String -> Maybe String
spokenText "" = Nothing
spokenText s = Just s

-- | The prompt a human seat types its transcript at: the round and the window it is typed in.
prompt :: EntityId -> RoundIndex -> Deadline -> String
prompt seat round window = "[round " <> show round <> ", " <> show window <> " window] " <> show seat <> "> "

-- | Play a game live: the round program runs on the game's world and seats. Human and Agent seats are
-- | answered here; every other binding is answered by `Handlers.Seats`.
runLive :: Live -> Game -> Aff (Either EngineError WorldState)
runLive live game = do
  -- set by the round's joint pass when its reply does not decode; read and cleared when the round is narrated
  failed <- liftEffect (Ref.new false)
  -- the deadlines whose windows have closed, so each window is waited out once
  closed <- liftEffect (Ref.new [])
  run (case_ # on (Proxy :: _ "seatInput") (seat closed) # on (Proxy :: _ "interpret") (joint failed) # on (Proxy :: _ "record") record # on (Proxy :: _ "narrate") (narrate failed)) (runGame game.inWorld game.rounds game.world)
  where
  env = seatEnv live.seed game
  binding e = Map.lookup e game.seats

  seat :: Ref (Array (Tuple RoundIndex Deadline)) -> SeatInputF ~> Aff
  seat closed = case _ of
    Speak e v k -> k <<< spokenText <$> case binding e of
      Just Human -> do
        text <- live.input (prompt e v.round ProposeDeadline)
        await closed v.round ProposeDeadline
        pure text
      Just Agent -> live.agents.speak e v
      _ -> pure ""
    Propose e v k -> k <$> case binding e of
      Just Human -> pure []
      Just Agent -> pure []
      _ -> pure (proposeFor env game.seats e v)
    Sign e v k -> k <$> case binding e of
      Just Human -> await closed v.round SignDeadline $> []
      Just Agent -> pure []
      _ -> pure (signFor env game.seats e v)

  await :: Ref (Array (Tuple RoundIndex Deadline)) -> RoundIndex -> Deadline -> Aff Unit
  await closed round d = do
    seen <- liftEffect (Ref.read closed)
    unless (Tuple round d `elem` seen) do
      live.window
      liftEffect (Ref.modify_ (_ <> [ Tuple round d ]) closed *> live.say ("— round " <> show round <> " " <> show d <> " window closed —"))

  record :: RecordF ~> Aff
  record (Record r transcripts next) = live.record r transcripts *> pure next

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
