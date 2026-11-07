-- | The in-memory test runner: seats bound to declarative strategies, records kept in memory.
-- | Used only by the handler tests; it is not a runtime handler.
module Test.Handlers.Run (SeatBindings, runTest) where

import Prelude

import Core.Draw (Seed)
import Core.Num (Q)
import Core.Types (EntityId, MarketId)
import Core.World (RoundResult)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, SEAT_INPUT)
import Foreign.Object (Object)
import Handlers.Seats (collectRecords, handleSeats, strategies)
import Core.Strategy (Strategy)
import Run (Run)
import Type.Row (type (+))

type SeatBindings =
  { strategies :: Map EntityId Strategy -- a seat without a strategy submits nothing
  , params :: Object Q
  , home :: EntityId -> Maybe MarketId
  , playerSeed :: Seed
  }

runTest :: forall a. SeatBindings -> Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) a -> { records :: Array RoundResult, value :: a }
runTest b = collectRecords <<< handleSeats (strategies { bound: b.strategies, params: b.params, home: b.home, playerSeed: b.playerSeed })
