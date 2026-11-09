-- | What one round produced, for a component that reports on it (docs/06_simulator.md §8): the round's
-- | events, each Exchange's price and its movement, each Actor's holdings and their change, the
-- | shortfalls, the Rules that took effect, and what was turned down. Pure.
module Views.Report
  ( RoundReport
  , roundReport
  ) where

import Prelude

import Core.Map (entries)
import Core.Num as Q
import Laws.Query (holding)
import Laws.Slice (event, select, shortfalls)
import Laws.Types (idText)
import Laws.World (Event, RoundResult, WorldState, renderReason)
import Core.Exchange (exchangePrice)
import Data.Array (concatMap, filter, findIndex, index)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Views.Common (StoredRun, quantity, worlds)

type RoundReport =
  { round :: Int
  , events :: Array Event
  , prices :: Array { exchange :: String, price :: String, change :: String }
  , balances :: Array { actor :: String, resource :: String, holding :: String, change :: String }
  , shortfalls :: Array { actor :: String, resource :: String, short :: String }
  , enacted :: Array String
  , repealed :: Array String
  , rejected :: Array { candidateTransaction :: String, reason :: String }
  , lapsed :: Array String
  }

-- | The report of a round the Run has completed, and nothing for one it has not.
roundReport :: StoredRun -> Int -> Maybe RoundReport
roundReport r round = do
  position <- findIndex (\x -> x.record.round == round) r.rounds
  result <- index r.rounds position
  before <- index (worlds r) position
  pure (report round result before)

report :: Int -> RoundResult -> WorldState -> RoundReport
report round result before =
  { round
  , events
  , prices: map price (entries after.exchanges)
  , balances: concatMap balance (Map.toUnfoldable after.actors)
  , shortfalls: map (\x -> { actor: idText x.actor, resource: idText x.resource, short: quantity x.quantity }) (select shortfalls events)
  , enacted: map (idText <<< _.id) (select (event @"Enacted") events)
  , repealed: map (idText <<< _.id) (select (event @"Repealed") events)
  , rejected: map (\x -> { candidateTransaction: idText x.candidateTransaction, reason: renderReason x.reason }) (select (event @"Rejected") events)
  , lapsed: map (idText <<< _.candidateTransaction) (select (event @"Lapsed") events)
  }
  where
  after = result.world
  events = result.record.events
  price (Tuple id m) =
    let
      now = exchangePrice m
      was = fromMaybe now (exchangePrice <$> Map.lookup id before.exchanges)
    in
      { exchange: idText id, price: quantity now, change: quantity (Q.sub now was) }
  balance (Tuple id a) =
    let
      resources = Set.toUnfoldable (Set.union (Map.keys a.accounts) (maybe' (Map.lookup id before.actors))) :: Array _
      row res = { actor: idText id, resource: idText res, holding: quantity (holding after id res), change: quantity (Q.sub (holding after id res) (holding before id res)) }
    in
      map row (filter (\res -> holding after id res /= Q.zero || holding before id res /= Q.zero) resources)
  maybe' = case _ of
    Just a -> Map.keys a.accounts
    Nothing -> Set.empty

