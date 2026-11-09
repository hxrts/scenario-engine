-- | Player standings (docs/06_simulator.md §9.2): each Actor's net worth in M at each round's
-- | marketable prices, its change from the start, its rank, and the net-worth chart. This module is
-- | the one definition of net worth; the balance measures and the seed variation read it. Pure.
module Views.Standings
  ( netWorthStat
  , netWorth
  , netWorths
  , Standing
  , standingsOf
  , standings
  , netWorthChart
  , columns
  ) where

import Prelude

import Core.Map (entries)

import Core.Map as CoreMap
import Core.Exchange (exchangePrice)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Laws.Query (holding)
import Laws.Types (ActorId)
import Laws.World (WorldState)
import Data.Array (filter, length, nub, sortBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Views.Common (Chart, Column, Field, Statistic, StoredRun, Table, cellOf, col, column, columnsOf, completed, final, players, quantity, series, tableOf, unitOf)

netWorthStat :: Statistic
netWorthStat =
  { name: "Net worth"
  , question: "How well are players doing relative to one another?"
  , definition: "The numeraire held plus each held base Resource valued at its Exchange's marginal price."
  , formula: "NW(e, r) = holding(e, numeraire) + sum over Resources b traded in an Exchange quoted in the numeraire of holding(e, b) × P(b, r), where P(b, r) is the mean of the marginal prices, marginal(cost, position) after round r, of the numeraire-quoted Exchanges whose base is b. Resources with no such Exchange are not counted. Actors only."
  }

-- | An Actor's net worth in a World State: the numeraire it holds, and each Resource that an Exchange quotes in
-- | the numeraire valued at that Resource's price. Where several Exchanges trade one Resource (the
-- | regional Exchanges), the price is the mean of their marginal prices, so a holding counts once.
netWorth :: WorldState -> ActorId -> Q
netWorth w e = Q.sumQ ([ holding w e w.numeraire ] <> map valued bases)
  where
  priced = filter (\m -> m.quote == w.numeraire) (CoreMap.values w.exchanges)
  bases = nub (map _.base priced)
  valued b = Q.mul (holding w e b) (priceOf b)
  priceOf b =
    let
      prices = map exchangePrice (filter (\m -> m.base == b) priced)
    in
      Q.div (Q.sumQ prices) (Q.fromInt (length prices))

-- | Every Actor's net worth in a World State, keyed by Actor, in Actor order.
netWorths :: WorldState -> Map ActorId Q
netWorths w = Map.fromFoldable (map (\e -> Tuple e (netWorth w e)) (players w))

-- | One Actor's standing after the final round: its net worth, its change from the start of the run,
-- | and its rank.
type Standing = { player :: ActorId, netWorth :: Q, change :: Q, rank :: Int }

-- | The standings after the final round, highest net worth first.
standingsOf :: StoredRun -> Array Standing
standingsOf r = map standing ranked
  where
  start = netWorths r.manifest.initial
  finals = netWorths (final r)
  ranked = sortBy (\(Tuple _ a) (Tuple _ b) -> compare b a) (entries finals)
  rankOf v = 1 + length (filter (\(Tuple _ u) -> u > v) ranked)
  standing (Tuple e v) = { player: e, netWorth: v, change: Q.sub v (fromMaybe Q.zero (Map.lookup e start)), rank: rankOf v }

netWorthColumn :: Column
netWorthColumn = column "Net worth" "The numeraire held plus each held Resource valued at its Exchange's marginal price, after the final round."

changeColumn :: Column
changeColumn = column "Change from start" "Net worth after the final round minus net worth at the start of the run."

rankColumn :: Column
rankColumn = column "Rank" "The player's place by final net worth, 1 being highest."

-- | The columns this module defines.
columns :: Array Column
columns = columnsOf standingFields

-- | The standings table: the net worth, its change from the start, and the rank of each Actor.
standings :: StoredRun -> Table
standings r = tableOf ("Standings after round " <> show (completed r)) standingFields (standingsOf r)

-- | The standings table's columns and cells.
standingFields :: Array (Field Standing)
standingFields =
  [ cellOf col.player (show <<< _.player)
  , cellOf netWorthColumn (quantity <<< _.netWorth)
  , cellOf changeColumn (quantity <<< _.change)
  , cellOf rankColumn (show <<< _.rank)
  ]

-- | Every Actor's net worth over the run, one line per Actor, from round 0 (the start) to the final round.
netWorthChart :: StoredRun -> Chart
netWorthChart r =
  { title: "Net worth by player"
  , unit: unitOf r r.manifest.initial.numeraire
  , kind: Quantity
  , series: series r (\w -> map (\(Tuple e v) -> Tuple (show e) v) (Map.toUnfoldable (netWorths w)))
  }
