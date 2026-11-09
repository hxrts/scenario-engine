-- | The `PLAY` interpreter: the lifecycle of a stored play. Whatever the kind of play, a thrown error (a store
-- | or simulation fault) marks the run failed with its cause, and the error goes on.
module Handlers.Play (interpretPlay) where

import Prelude

import Laws.Engine.Commit (EngineError)
import Laws.World (RoundResult, WorldState)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException, try)
import Effects.Signatures (NewRun, PlayF(..), PlayOutcome, PlayRequest(..))
import Effects.Store (setSpec, setStatus)
import Assembly.Error (AssemblyError)
import Assembly.Types (PreparedRun)
import Core.Draw (Seed)
import Handlers.Seats (RecordSink, playWith, seatEnv)
import Handlers.Simulate (fromCheckpoint, machineOnly)
import Handlers.Store (Metric, StoreBackend, openRun, recorder, runStore, settle)
import Schema.Run (RunStatus(..))

interpretPlay :: StoreBackend -> (RoundResult -> Array Metric) -> PlayF ~> Effect
interpretPlay store metricsOf (Play request k) = k <$> case request of
  Simulated r -> played r.run.id $ persisting r.seed r.game (opened r.run r.game.world)
  Replayed r -> played r.run.id do
    save <- opened r.run r.game.world
    traverse_ (\result -> save result []) r.records
    pure (Right r.value :: Either String _)
  Continued r -> played r.id $ persisting r.seed (fromCheckpoint r.game { world: r.world, done: r.done }) do
    runStore store (setStatus r.id Running Nothing)
    runStore store (setSpec r.id r.spec)
    recorder store metricsOf r.id r.world
  where
  -- the sink opens only once the game is machine-playable, so a rejected game leaves the store untouched
  persisting :: Seed -> PreparedRun -> Effect RecordSink -> Effect (Either AssemblyError (Either EngineError WorldState))
  persisting seed g sink = case machineOnly g of
    Left e -> pure (Left e)
    Right _ -> do
      persist <- sink
      Right <$> playWith (seatEnv seed g) g.seats persist g.inWorld g.rounds g.world

  opened :: NewRun -> WorldState -> Effect RecordSink
  opened run initial = openRun store metricsOf run initial >>= either (throwException <<< error) pure

  -- a game that does not start is a `Left`, with nothing stored; a game that played is the engine's result
  played :: forall e a. Show e => String -> Effect (Either e (Either EngineError a)) -> Effect PlayOutcome
  played id game = try game >>= case _ of
    Left ex -> settle store id exceptionPrefix (Left ex) *> throwException ex
    Right (Left e) -> pure (Left (show e))
    Right (Right r) -> settle store id exceptionPrefix (Right r) $> Right (void (lmap show r))

  exceptionPrefix = "exception: "
