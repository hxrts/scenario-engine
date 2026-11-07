-- | The in-memory test runner: a game's seats answered by `Handlers.Seats`, records kept in memory. Used
-- | only by the handler tests; it is not a runtime handler. Also the Meridian game the suites start from.
module Test.Handlers.Run (meridian, runTest) where

import Prelude

import Core.Draw (Seed)
import Core.World (RoundResult)
import Effect.Aff (Aff)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, SEAT_INPUT)
import Game.Types (Game)
import Handlers.Seats (collectRecords, handleSeats, seatEnv)
import Testkit.Meridian (unseated)
import Run (Run)
import Type.Row (type (+))

-- | The shipped Meridian premise assembled with no seats bound.
meridian :: Aff Game
meridian = unseated

-- | The game's seats answered under a player seed, with the records kept.
runTest :: forall a. Seed -> Game -> Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) a -> { records :: Array RoundResult, value :: a }
runTest seed g = collectRecords <<< handleSeats (seatEnv seed g) g.seats
