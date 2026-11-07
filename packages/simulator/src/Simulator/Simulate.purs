-- | The simulator (docs/06_simulator.md §3–§6): the one fold over a `Game`. `simulate` plays a game with
-- | machine seats (Strategy, Script, Recorded) and is pure; `resumeFrom` continues a snapshot; the
-- | `…With` variants persist each round through a store as it completes. The simulator imports no
-- | premise, board, or policy module: a Game is all it sees.
module Simulator.Simulate
  ( SimRun
  , Snapshot
  , simulate
  , resumeFrom
  , simulateWith
  , resumeWith
  ) where

import Prelude

import Core.Draw (Seed)
import Core.Engine.Commit (EngineError)
import Core.Types (EntityId)
import Core.World (Manifest, RoundResult, WorldState)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effects.Round (runGame)
import Game.Error (GameError(..))
import Game.Types (Game, SeatBinding(..))
import Handlers.Seats (collectRecords, handleSeats, seatEnv)
import Handlers.Sim (runSim)

-- | One simulated run: its manifest, the player seed it was played with, its round records, and the
-- | final World State (or the engine error that stopped it).
type SimRun = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }

-- | A saved World State after `done` completed rounds (docs/06 §8).
type Snapshot = { world :: WorldState, done :: Int }

-- | A game with a human or agent seat cannot be simulated: its answers come from live play.
machineOnly :: Game -> Either GameError Unit
machineOnly g = traverse_ check (Map.toUnfoldable g.seats :: Array (Tuple EntityId SeatBinding))
  where
  check (Tuple e b) = case b of
    Human -> Left (NotSimulable e)
    Agent -> Left (NotSimulable e)
    _ -> Right unit

-- | Play a game to completion with records kept in memory. Pure.
simulate :: Seed -> Game -> Either GameError SimRun
simulate seed g = do
  machineOnly g
  let out = collectRecords (handleSeats (seatEnv seed g) g.seats (runGame g.inWorld g.rounds g.world))
  pure { manifest: g.manifest, playerSeed: seed, records: out.records, final: out.value }

-- | Continue a game from a saved snapshot. `rounds` is the game's total, so the snapshot's completed
-- | rounds are subtracted. Seat randomness is keyed by round, so a resumed run matches the uninterrupted one.
resumeFrom :: Seed -> Game -> Snapshot -> Either GameError SimRun
resumeFrom seed g snap = simulate seed (fromSnapshot g snap)

fromSnapshot :: Game -> Snapshot -> Game
fromSnapshot g snap = g { world = snap.world, rounds = g.rounds - snap.done }

-- | Play a game under the Sim runner, persisting each round through `store` as it completes. `store` runs
-- | only once the game is machine-playable, so a rejected game leaves the caller's store untouched.
simulateWith :: Seed -> Game -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either GameError (Either EngineError WorldState))
simulateWith seed g store = case machineOnly g of
  Left e -> pure (Left e)
  Right _ -> do
    persist <- store g.manifest
    Right <$> runSim (seatEnv seed g) g.seats persist (runGame g.inWorld g.rounds g.world)

-- | Continue a stored run from its snapshot under the Sim runner, persisting the new rounds through `store`.
resumeWith :: Seed -> Game -> Snapshot -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either GameError (Either EngineError WorldState))
resumeWith seed g snap store = simulateWith seed (fromSnapshot g snap) store
