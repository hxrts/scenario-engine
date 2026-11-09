-- | Each failed assertion of a run (docs/06_simulator.md §7.1): its round and invariant, the expected and observed
-- | values when the check compares two, and the round's events that concern it. Pure.
module Views.Assertions (assertions, columns) where

import Prelude

import Core.Invariants (Failure)
import Core.World (RoundRecord)
import Data.Array (all, concat, filter, index, mapWithIndex, null)
import Data.Maybe (maybe)
import Data.String (Pattern(..), contains, joinWith)
import Views.Common (Column, Field, StoredRun, Table, cellOf, col, column, records, tableOf)

columns :: Array Column
columns = [ invariant, detail, expected, observed, relevant ]

invariant :: Column
invariant = column "Invariant" "The invariant the assertion checks."

detail :: Column
detail = column "What failed" "The assertion's own account of the failure."

expected :: Column
expected = column "Expected" "The value the assertion expected, when it compares two values."

observed :: Column
observed = column "Observed" "The value the round produced, when the assertion compares two values."

relevant :: Column
relevant = column "Relevant events" "The round's events that name what the failure concerns."

type FailedRow = { round :: Int, failure :: Failure, events :: String }

assertions :: StoredRun -> Table
assertions r = tableOf "Failed assertions" fields (concat (mapWithIndex rowsOf r.assertions))
  where
  recs = records r
  rowsOf i failures = map (\f -> { round: i + 1, failure: f, events: maybe "" (eventsAbout f) (index recs i) }) failures

-- | The round's events naming everything the failure concerns. A failure about nothing in particular has none.
eventsAbout :: Failure -> RoundRecord -> String
eventsAbout f rec
  | null f.about = ""
  | otherwise = joinWith "; " (filter (\e -> all (\name -> contains (Pattern name) e) f.about) (map show rec.events))

fields :: Array (Field FailedRow)
fields =
  [ cellOf col.round (show <<< _.round)
  , cellOf invariant _.failure.invariant
  , cellOf detail _.failure.detail
  , cellOf expected _.failure.expected
  , cellOf observed _.failure.observed
  , cellOf relevant _.events
  ]
