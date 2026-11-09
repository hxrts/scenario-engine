-- | The `LIVE` interpreter: one live session over the live handler, its model calls, and its run in the
-- | store. A failure of any device marks the run failed with its cause.
module Handlers.Session (SessionDevices, interpretLive) where

import Prelude

import Rules.World (RoundResult)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effects.Signatures (LiveF(..))
import Handlers.Live (runLive)
import Handlers.Llm.Backend (Backend)
import Handlers.Llm.Joint (interpreter)
import Handlers.Llm.Narrate (narrator)
import Handlers.Llm.Speak (speak)
import Handlers.Store (Metric, StoreBackend, recorder, settle)

type SessionDevices =
  { store :: StoreBackend
  , metricsOf :: RoundResult -> Array Metric
  , input :: String -> Aff String
  , say :: String -> Effect Unit
  , llm :: Backend
  }

interpretLive :: SessionDevices -> LiveF ~> Aff
interpretLive d (PlayLive r k) = do
  save <- liftEffect (recorder d.store d.metricsOf r.id r.game.world)
  out <- attempt $ runLive
    { seed: r.seed
    , input: d.input
    , window: if r.window > 0 then Just (delay (Milliseconds (toNumber (r.window * 1000)))) else Nothing
    , say: d.say
    , notify: \_ text -> liftEffect (d.say ("  " <> text))
    , interpret: interpreter d.llm r.game.world
    , narrate: \failed result -> do
        front <- narrator d.llm failed result
        liftEffect (for_ (r.report result front) d.say)
        pure front
    , speak: speak d.llm r.briefing
    , record: save
    }
    r.game
  liftEffect (settle d.store r.id "" out)
  pure $ k case out of
    Left ex -> Left (message ex)
    Right (Left e) -> Left (show e)
    Right (Right _) -> Right unit
