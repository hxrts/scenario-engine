-- | The pure Test handler: seats are bound to declarative programs, records accumulate in
-- | memory, and seat randomness draws from the player seed.
module Handlers.Test (SeatBindings, runTest) where

import Prelude

import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Types (EntityId, MarketId)
import Core.World (RoundResult)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Effects (RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Foreign.Object (Object)
import Policy.Interpret as Policy
import Policy.Program (Program)
import Run (Run, case_, on, peel)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type SeatBindings =
  { programs :: Map EntityId Program -- a seat without a program submits nothing
  , params :: Object Q
  , home :: EntityId -> MarketId
  , playerSeed :: Seed
  }

runTest :: forall a. SeatBindings -> Run (SEAT_INPUT + RECORD + ()) a -> { records :: Array RoundResult, value :: a }
runTest seats = go []
  where
  go records r = case peel r of
    Right value -> { records, value }
    Left v -> v #
      ( case_
          # on (Proxy :: _ "seatInput")
              ( case _ of
                  Propose e view k -> go records (k (maybe [] (\p -> Policy.propose p (env e view)) (Map.lookup e seats.programs)))
                  Sign e view k -> go records (k (maybe [] (\p -> Policy.sign p (env e view)) (Map.lookup e seats.programs)))
              )
          # on (Proxy :: _ "record") (\(Record result next) -> go (records <> [ result ]) next)
      )
  env e view =
    { view
    , params: seats.params
    , home: seats.home e
    , random: \i -> draw seats.playerSeed { round: view.round, source: "seat", entity: unwrap e, category: "choice", index: i }
    }
