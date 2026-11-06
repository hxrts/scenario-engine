-- | The Replay handler: seats answered from recorded Proposals and signatures.
module Handlers.Replay (runReplay) where

import Prelude

import Core.World (RoundRecord, RoundResult)
import Effects (RECORD, SEAT_INPUT)
import Handlers.Seats (collectRecords, handleSeats, replay)
import Run (Run)
import Type.Row (type (+))

runReplay :: forall a. Array RoundRecord -> Run (SEAT_INPUT + RECORD + ()) a -> { records :: Array RoundResult, value :: a }
runReplay records = collectRecords <<< handleSeats (replay records)
