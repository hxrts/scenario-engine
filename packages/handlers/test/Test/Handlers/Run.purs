-- | The in-memory test runner: a game's seats answered by `Handlers.Seats`, records kept in memory. Used
-- | only by the handler tests; it is not a runtime handler. Also the Meridian game the suites start from.
module Test.Handlers.Run (meridian, runTest) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed)
import Core.World (RoundResult)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, SEAT_INPUT)
import Game.Assemble (assemble)
import Game.Premise (Premise(..))
import Game.Types (Game)
import Handlers.Seats (collectRecords, handleSeats, seatEnv)
import Data.Map as Map
import Testkit.Meridian (unseated)
import Run (Run)
import Type.Row (type (+))

-- | The shipped Meridian premise assembled with no seats bound.
meridian :: Aff Game
meridian = unseated

-- | The game's seats answered under a player seed, with the records kept.
runTest :: forall a. Seed -> Game -> Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) a -> { records :: Array RoundResult, value :: a }
runTest seed g = collectRecords <<< handleSeats (seatEnv seed g) g.seats
