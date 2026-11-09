-- | The Replay handler: seats answered from recorded Proposals and signatures.
module Handlers.Replay
  ( runReplay
  ) where

import Prelude

import Laws.World (RoundRecord, RoundResult, WorldState)
import Data.Array (any, filter, range)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Laws.Engine.Commit (EngineError)
import Game.Types (Game)
import Game.Bindings (scriptBindings)
import Handlers.Seats (SeatEnv, playPure)

-- | The rounds 1..n that have no record. A record may have empty inputs; the round must exist.
missingRounds :: Int -> Array RoundRecord -> Array Int
missingRounds n records = filter (\r -> not (any (\x -> x.round == r) records)) (range 1 n)

-- | Replay `n` rounds from their records. Every round 1..n must have a record: a truncated
-- | record set would otherwise replay as a different history, so it is an error.
runReplay :: SeatEnv -> Int -> Array RoundRecord -> Game -> Either String { records :: Array RoundResult, value :: Either EngineError WorldState }
runReplay env n records g = case missingRounds n records of
  [] -> Right (playPure env (scriptBindings records) g.inWorld n g.world)
  gaps -> Left ("replay: no record for round(s) " <> intercalate ", " (map show gaps) <> " of " <> show n)
