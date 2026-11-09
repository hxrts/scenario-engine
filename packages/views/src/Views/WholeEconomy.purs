-- | The whole-economy measures (docs/06_simulator.md §11), by round and Market: demand and unfilled demand, the
-- | concentration of net worth, and the distribution of Money. Pure.
module Views.WholeEconomy (wholeEconomy, herfindahl, unfilled, columns) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Rules.Query (holding, tradesOf)
import Rules.Slice (exchanges, read, runSlice, total, whereEq, whereGt)
import Rules.World (WorldState)
import Data.Array (concatMap, filter, fromFoldable, zip)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Views.Balance (gini)
import Views.Common (Column, StoredRun, Table, col, column, names, players, quantity, ratio, records, snapshots)
import Views.Standings (netWorth)

demandColumn :: Column
demandColumn = column "Demand" "Demand(r, m): the sum of every positive Trade quantity submitted on Market m in round r."

unfilledColumn :: Column
unfilledColumn = column "Unfilled demand" "Unfilled(r, m) = max(0, Demand(r, m) − bought(r, m)): what was asked for on Market m in round r and not bought."

concentrationColumn :: Column
concentrationColumn = column "Concentration" "HHI(r) = sum(share²) of the players' positive net worth after round r: 1/n when equal, 1 when one player holds all."

moneyColumn :: Column
moneyColumn = column "Money distribution" "The Gini coefficient of the players' holdings of the numeraire after round r. n/a when none is held or a holding is negative."

columns :: Array Column
columns = [ demandColumn, unfilledColumn, concentrationColumn, moneyColumn ]

-- | The sum of squared shares of the positive values. Zero when none is positive.
herfindahl :: Array Q -> Q
herfindahl xs = Q.sumQ (map (\x -> maybe Q.zero (\s -> Q.mul s s) (Q.divMaybe x whole)) positive)
  where
  positive = filter (_ > Q.zero) xs
  whole = Q.sumQ positive

-- | What was asked for and not bought; a round never owes a negative amount.
unfilled :: Q -> Q -> Q
unfilled demand bought = max Q.zero (Q.sub demand bought)

wholeEconomy :: StoredRun -> Table
wholeEconomy r =
  { title: "Whole-economy measures"
  , columns: names ([ col.round, marketColumn ] <> columns)
  , rows: concatMap rowsOf (zip (records r) (snapshots r))
  }
  where
  marketColumn = column "Market" "The Market the row's demand is on."
  rowsOf (Tuple rec w) = map (rowOf rec w) (fromFoldable (Map.keys w.markets))
  rowOf rec w m =
    let
      asked = demandOn m rec
      bought = runSlice (total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"market" m exchanges)))) rec.events
    in
      [ show rec.round
      , show m
      , quantity asked
      , quantity (unfilled asked bought)
      , ratio (herfindahl (netWorths w))
      , maybe "n/a" ratio (gini (moneyOf w))
      ]
  demandOn m rec = Q.sumQ (concatMap (\s -> map _.quantity (filter (\o -> o.market == m && o.quantity > Q.zero) (tradesOf s.proposal.intents))) rec.inputs)
  netWorths w = map (netWorth w) (players w)

  moneyOf :: WorldState -> Array Q
  moneyOf w = map (\e -> holding w e w.numeraire) (players w)

