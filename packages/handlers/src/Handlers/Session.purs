-- | The live handler's interpreter: one live run over the live handler, its model calls, and its run in the store. A failure
-- | of any device marks the run failed with its cause.
-- |
-- | The lifecycle controls a live run is driven by come in as devices: `paused` stops the propose window's clock
-- | while it answers true, `ending` completes the run after the round being played when it answers true, and
-- | `enteredBy` names who typed a seat's text for a round when that was not the seat (a facilitator), which the
-- | stored transcript records.
module Handlers.Session (SessionDevices, interpretLive, pausableDelay) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, throwException)
import Effects.Signatures (LiveF(..))
import Effects.Store (setStatus)
import Handlers.Live (runLive)
import Handlers.Llm.Backend (Backend)
import Handlers.Llm.Joint (interpreter)
import Handlers.Llm.Speak (speak)
import Handlers.Store (Metric, StoreBackend, recorder, runStore, settle)
import Laws.Types (ActorId, RoundIndex)
import Laws.World (RoundResult)
import Schema.Run (RunStatus(..))

type SessionDevices =
  { store :: StoreBackend
  , metricsOf :: RoundResult -> Array Metric
  , input :: ActorId -> RoundIndex -> String -> Aff String
  , say :: String -> Effect Unit
  , llm :: Maybe String -> Backend -- the model backend: a seat's own model, or the run's default (Nothing)
  , paused :: Effect Boolean
  , ending :: Effect Boolean
  , enteredBy :: ActorId -> RoundIndex -> Effect (Maybe String)
  }

-- | What the round loop is stopped with when the run is ended after a round: not a failure.
endedMark :: String
endedMark = "the live run was ended after this round"

interpretLive :: SessionDevices -> LiveF ~> Aff
interpretLive d (PlayLive r k) = do
  save <- liftEffect (recorder d.store d.metricsOf r.id r.game.world)
  out <- attempt $ runLive
    { seed: r.seed
    , input: d.input
    , window: if r.window > 0 then Just (pausableDelay d.paused (r.window * 1000)) else Nothing
    , say: d.say
    , notify: \_ text -> liftEffect (d.say ("  " <> text))
    , interpret: interpreter (d.llm Nothing) r.game.world
    , speak: \model -> speak (d.llm model) r.briefing
    , record: \result transcripts -> do
        marked <- traverse (\t -> (\by -> t { enteredBy = by <|> t.enteredBy }) <$> d.enteredBy t.seat result.record.round) transcripts
        save result marked
        for_ (r.report result) d.say
        stop <- d.ending
        when stop (throwException (error endedMark))
    }
    r.game
  case out of
    Left ex | message ex == endedMark -> liftEffect (runStore d.store (setStatus r.id Completed Nothing))
    _ -> liftEffect (settle d.store r.id "" out)
  pure $ k case out of
    Left ex | message ex == endedMark -> Right unit
    Left ex -> Left (message ex)
    Right (Left e) -> Left (show e)
    Right (Right _) -> Right unit

-- | A delay of the milliseconds given whose clock stands still while `paused` answers true.
pausableDelay :: Effect Boolean -> Int -> Aff Unit
pausableDelay paused = go
  where
  step = 100
  go remaining
    | remaining <= 0 = pure unit
    | otherwise = do
        delay (Milliseconds (toNumber step))
        p <- liftEffect paused
        go (if p then remaining else remaining - step)
