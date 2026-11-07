-- | The Sim handler: seat bindings answered by `Handlers.Seats`, each round's result persisted as it completes.
module Handlers.Sim (runSim) where

import Prelude

import Core.World (RoundResult)
import Data.Either (Either(..))
import Effect (Effect)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, RecordF(..), SEAT_INPUT)
import Handlers.Seats (SeatEnv, handleSeats)
import Game.Types (Bindings)
import Run (Run, case_, on, peel)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

runSim :: forall a. SeatEnv -> Bindings -> (RoundResult -> Effect Unit) -> Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) a -> Effect a
runSim env bs persist = go <<< handleSeats env bs
  where
  go r = case peel r of
    Right a -> pure a
    Left v -> v # (case_ # on (Proxy :: _ "record") (\(Record result _ next) -> persist result *> go next))
