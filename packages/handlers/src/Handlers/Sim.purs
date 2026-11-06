-- | The Sim handler: seats from a source, each round's result persisted as it completes.
module Handlers.Sim (runSim) where

import Prelude

import Core.World (RoundResult)
import Data.Either (Either(..))
import Effect (Effect)
import Effects (RECORD, RecordF(..), SEAT_INPUT)
import Handlers.Seats (SeatSource, handleSeats)
import Run (Run, case_, on, peel)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

runSim :: forall a. SeatSource -> (RoundResult -> Effect Unit) -> Run (SEAT_INPUT + RECORD + ()) a -> Effect a
runSim source persist = go <<< handleSeats source
  where
  go r = case peel r of
    Right a -> pure a
    Left v -> v # (case_ # on (Proxy :: _ "record") (\(Record result next) -> persist result *> go next))
