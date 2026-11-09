-- | The `PLAY` interpreter: the lifecycle of a stored play. Whatever the kind of play, a thrown error (a store
-- | or simulation fault) marks the run failed with its cause, and the error goes on.
module Handlers.Play (interpretPlay) where

import Prelude

import Core.Engine.Commit (EngineError)
import Core.World (RoundResult, WorldState)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException, try)
import Effects.Signatures (NewRun, PlayF(..), PlayOutcome, PlayRequest(..))
import Effects.Store (setSpec, setStatus)
import Handlers.Seats (RecordSink)
import Handlers.Simulate (resumeWith, simulateWith)
import Handlers.Store (Metric, StoreBackend, openRun, recorder, runStore, settle)
import Schema.Run (RunStatus(..))

interpretPlay :: StoreBackend -> (RoundResult -> Array Metric) -> PlayF ~> Effect
interpretPlay store metricsOf (Play request k) = k <$> case request of
  Simulated r -> played r.run.id $ simulateWith r.seed r.game \_ -> opened r.run r.game.world
  Replayed r -> played r.run.id do
    save <- opened r.run r.game.world
    traverse_ (\result -> save result []) r.records
    pure (Right r.value :: Either String _)
  Continued r -> played r.id $ resumeWith r.seed r.game { world: r.world, done: r.done } \_ -> do
    runStore store (setStatus r.id Running Nothing)
    runStore store (setSpec r.id r.spec)
    recorder store metricsOf r.id r.world
  where
  opened :: NewRun -> WorldState -> Effect RecordSink
  opened run initial = openRun store metricsOf run initial >>= either (throwException <<< error) pure

  -- a game that does not start is a `Left`, with nothing stored; a game that played is the engine's result
  played :: forall e a. Show e => String -> Effect (Either e (Either EngineError a)) -> Effect PlayOutcome
  played id game = try game >>= case _ of
    Left ex -> settle store id exceptionPrefix (Left ex) *> throwException ex
    Right (Left e) -> pure (Left (show e))
    Right (Right r) -> settle store id exceptionPrefix (Right r) $> Right (void (lmap show r))

  exceptionPrefix = "exception: "
