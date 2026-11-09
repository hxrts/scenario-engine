-- | The simulator (docs/06_simulator.md §3–§6): the one fold over a `Game`. `simulate` plays a game with
-- | machine seats (StrategyRules, Script) and is pure; `fromCheckpoint` continues a snapshot;
-- | persisting each round is `PLAY`'s (`Handlers.Play`). The simulator imports no
-- | premise, board, or strategy module: a Game is all it sees.
module Handlers.Simulate
  ( SimRun
  , Checkpoint
  , simulate
  , fromCheckpoint
  , replayGame
  , machineOnly
  ) where

import Prelude

import Core.Map (entries)

import Core.Draw (Seed)
import Laws.Engine.Commit (EngineError)
import Laws.World (Manifest, RoundRecord, RoundResult, WorldState)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Assembly.Error (GameError(..))
import Assembly.Types (Game, SeatBinding(..))
import Handlers.Replay (runReplay)
import Handlers.Seats (playPure, seatEnv)

-- | One simulated run: its manifest, the player seed it was played with, its round records, and the
-- | final World State (or the engine error that stopped it).
type SimRun = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }

-- | A saved World State after `done` completed rounds (docs/06 §8).
type Checkpoint = { world :: WorldState, done :: Int }

-- | A game with a human or agent seat cannot be simulated: its answers come from live play.
machineOnly :: Game -> Either GameError Unit
machineOnly g = traverse_ check (entries g.seats)
  where
  check (Tuple e b) = case b of
    Human -> Left (NotSimulable e)
    Agent -> Left (NotSimulable e)
    _ -> Right unit

-- | Play a game to completion with records kept in memory. Pure.
simulate :: Seed -> Game -> Either GameError SimRun
simulate seed g = do
  machineOnly g
  let out = playPure (seatEnv seed g) g.seats g.inWorld g.rounds g.world
  pure { manifest: g.manifest, playerSeed: seed, records: out.records, final: out.value }

-- | A game continued from a saved snapshot. `rounds` is the game's total, so the snapshot's completed
-- | rounds are subtracted. Seat randomness is keyed by round, so a resumed run matches the uninterrupted one.
fromCheckpoint :: Game -> Checkpoint -> Game
fromCheckpoint g snap = g { world = snap.world, rounds = g.rounds - snap.done }

-- | A game played from its round records, its seats answered by what was recorded (`Handlers.Replay`).
replayGame :: Seed -> Game -> Array RoundRecord -> Either String { records :: Array RoundResult, value :: Either EngineError WorldState }
replayGame seed g records = runReplay (seatEnv seed g) g.rounds records g

