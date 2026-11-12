-- | Player standings (docs/07_simulator.md §9.2): each Actor's net worth in M at each round's
-- | marketable prices, its change from the start, its rank, and the net-worth chart. This module is
-- | the one definition of net worth; the balance measures and the seed variation read it. Pure.
module Views.Standings
  ( netWorthStat
  , netWorth
  , netWorths
  , valueIn
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
import Laws.Types (ActorId, ResourceId)
import Laws.World (WorldState)
import Control.Alt ((<|>))
import Data.Array (filter, length, mapMaybe, nub, null, sortBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Views.Common (Chart, Column, Field, Statistic, StoredRun, Table, cellOf, col, column, columnsOf, completed, final, players, quantity, series, tableOf, unitOf)

netWorthStat :: Statistic
netWorthStat =
  { name: "Net worth"
  , question: "How well are players doing relative to one another?"
  , definition: "The numeraire held plus each other held Resource valued in the numeraire at its Exchanges' marginal prices, a second currency at its currency Exchange's."
  , formula: "NW(e, r) = sum over Resources b of holding(e, b) × V(b, r). V(numeraire) = 1. V(b) is the mean marginal price, marginal(cost, position) after round r, of the Exchanges quoting b in the numeraire; else, for a currency c in which an Exchange quotes the numeraire, 1 / the mean marginal price of those Exchanges; else the mean of P(b in c) × V(c) over the Exchanges quoting b in such a currency c. Resources with none of these are not counted. Actors only."
  }

-- | An Actor's net worth in a World State: the numeraire it holds, and every other Resource it holds valued in the
-- | numeraire (`valueIn`). A Resource with no price in the numeraire is not counted.
netWorth :: WorldState -> ActorId -> Q
netWorth w e = Q.sumQ (map (\b -> Q.mul (holding w e b) (valueIn w b)) (nub ([ w.numeraire ] <> map _.base exchanges <> map _.quote exchanges)))
  where
  exchanges = CoreMap.values w.exchanges

-- | What a unit of a Resource is worth in the numeraire. The numeraire is worth 1. A Resource that Exchanges quote in
-- | the numeraire is worth the mean of their marginal prices, so one held in several regional Exchanges counts once. A
-- | Resource in which an Exchange quotes the numeraire, a second currency, is worth one over that Exchange's mean
-- | price. And a Resource quoted only in such a currency is worth its mean price there, times what the currency is
-- | worth. Anything else is worth nothing here.
valueIn :: WorldState -> ResourceId -> Q
valueIn w b
  | b == w.numeraire = Q.one
  | otherwise = fromMaybe Q.zero (direct b <|> currency b <|> viaCurrency)
      where
      exchanges = CoreMap.values w.exchanges
      mean ps = if null ps then Nothing else Just (Q.div (Q.sumQ ps) (Q.fromInt (length ps)))
      direct x = mean (map exchangePrice (filter (\m -> m.base == x && m.quote == w.numeraire) exchanges))
      currency x = mean (map exchangePrice (filter (\m -> m.base == w.numeraire && m.quote == x) exchanges)) >>= \p -> Q.divMaybe Q.one p
      viaCurrency = mean (mapMaybe (\m -> if m.base == b then (\c -> Q.mul (exchangePrice m) c) <$> currency m.quote else Nothing) exchanges)

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
netWorthColumn = column "Net worth" "The numeraire held plus each other held Resource valued in the numeraire at its Exchanges' marginal prices, after the final round."

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
