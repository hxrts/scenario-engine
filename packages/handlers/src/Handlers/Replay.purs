-- | The Replay handler: seats answered from recorded Proposals and signatures.
module Handlers.Replay (runReplay, missingRounds) where

import Prelude

import Core.World (RoundRecord, RoundResult)
import Data.Array (any, filter, range)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, SEAT_INPUT)
import Handlers.Seats (SeatEnv, collectRecords, handleSeats, recordedBindings)
import Run (Run)
import Type.Row (type (+))

-- | The rounds 1..n that have no record. A record may have empty inputs; the round must exist.
missingRounds :: Int -> Array RoundRecord -> Array Int
missingRounds n records = filter (\r -> not (any (\x -> x.round == r) records)) (range 1 n)

-- | Replay `n` rounds from their records. Every round 1..n must have a record: a truncated
-- | record set would otherwise replay as a different history, so it is an error.
runReplay :: forall a. SeatEnv -> Int -> Array RoundRecord -> Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) a -> Either String { records :: Array RoundResult, value :: a }
runReplay env n records game = case missingRounds n records of
  [] -> Right (collectRecords (handleSeats env (recordedBindings records) game))
  gaps -> Left ("replay: no record for round(s) " <> intercalate ", " (map show gaps) <> " of " <> show n)
