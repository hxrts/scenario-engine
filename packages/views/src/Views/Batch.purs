-- | A batch's summary (docs/06_simulator.md §11): the final measures across a set of runs, with their mean, their
-- | spread, and how many runs met each explicit Actor's objective. Pure: every value is read from the runs.
module Views.Batch (batchSummary, measureRow, objectiveRow, columns) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap, filter, length, mapMaybe, nub)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Schema.PossibleWorld (PossibleWorld)
import Views.Balance (completionOf, measuresOf, spread)
import Views.Common (Column, StoredRun, Table, column, final, names, price, quantity)
import Views.Exchanges (exchangePrices)

runsColumn :: Column
runsColumn = column "Runs" "How many runs of the batch give this measure a value."

meanColumn :: Column
meanColumn = column "Mean" "The measure's mean across the runs."

spreadColumn :: Column
spreadColumn = column "Spread across runs" "The highest value across the runs less the lowest."

metCountColumn :: Column
metCountColumn = column "Runs meeting it" "How many runs meet the Actor's objective in full."

measureName :: Column
measureName = column "Batch measure" "A final measure of each run, or an Actor's objective."

columns :: Array Column
columns = [ measureName, runsColumn, meanColumn, spreadColumn, metCountColumn ]

-- | One measure across the runs that give it a value.
measureRow :: (Q -> String) -> String -> Array Q -> Array String
measureRow format name xs =
  [ name
  , show (length xs)
  , maybe "n/a" format (Q.divMaybe (Q.sumQ xs) (Q.fromInt (length xs)))
  , maybe "n/a" format (spread xs)
  , ""
  ]

-- | One objective across the runs: whether each met it in full.
objectiveRow :: String -> Array Boolean -> Array String
objectiveRow name met = [ name, show (length met), "", "", show (length (filter identity met)) ]

batchSummary :: (StoredRun -> Maybe PossibleWorld) -> Array StoredRun -> Table
batchSummary boardOf runs =
  { title: "Batch summary"
  , columns: names columns
  , rows:
      [ measureRow quantity "Net worth spread" (mapMaybe (_.spread <<< measuresOf) runs)
      , measureRow quantity "Net worth Gini" (mapMaybe (_.gini <<< measuresOf) runs)
      ]
        <> map exchangeRow exchangesOf
        <> map explicitActorRow explicitActorsOf
  }
  where
  prices r = exchangePrices (final r)
  exchangesOf = nub (concatMap (map (\m -> show m.exchange) <<< prices) runs)
  exchangeRow m = measureRow price ("Final price, " <> m) (concatMap (map _.price <<< filter (\p -> show p.exchange == m) <<< prices) runs)
  completions r = fromMaybe [] (flip completionOf r <$> boardOf r)
  explicitActorsOf = nub (concatMap (map (\c -> show c.explicitActor) <<< completions) runs)
  explicitActorRow a = objectiveRow ("Objective met, " <> a) (concatMap (map (\c -> c.met == c.criteria) <<< filter (\c -> show c.explicitActor == a) <<< completions) runs)
