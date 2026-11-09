-- | Drivers of gameplay (docs/06_simulator.md §9.2): the rules enacted and repealed, the collective
-- | procedure outcomes, and the shortfalls, by round, on the same round axis as the price chart. This
-- | is the one definition of a shortfall total. Pure.
module Views.Drivers
  ( driversStat
  , shortfallTotal
  , drivers
  , columns
  , shortfallsColumn
  ) where

import Prelude
import Laws.World (Focus, RoundRecord, WorldState)
import Laws.Types (idText, FlowEffect(..), EntityId, ResourceId, Rule, RuleF(..), ruleKind)

import Core.Num (Q)
import Core.Num as Q
import Laws.Slice (Slice, describeSlice, foldEvents, groupBy, ignore, read, runSlice, shortfalls, total, whereEq)
import Data.Array (concatMap, length, nub)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Views.Common (Column, Procedure, Statistic, StoredRun, Table, asProcedure, col, column, names, outcomeLabel, quantity, ratio, records, supportThreshold)

driversStat :: Statistic
driversStat =
  { name: "Drivers"
  , question: "What forces are driving gameplay (what moves prices and outcomes)?"
  , definition: "Rules enacted and repealed, collective procedure outcomes, and shortfalls, by round."
  , formula: "Shortfall(r, resource) = " <> describeSlice shortfallsByResource <> ": per Resource in round r, the sum of quantity and the number of distinct Entities short; a procedure's support and threshold are the ratios its ProcedureOutcome records."
  }

-- | Every Shortfall, grouped by Resource: what `Shortfall(r, resource)` totals.
shortfallsByResource :: Slice (Array (Tuple ResourceId (Array { round :: Int, entity :: EntityId, resource :: ResourceId, quantity :: Q })))
shortfallsByResource = groupBy @"resource" shortfalls

-- | The Shortfall quantities of one Resource, summed.
shortfallSum :: ResourceId -> Slice Q
shortfallSum res = total (read @"quantity" (whereEq @"resource" res shortfalls))

-- | The round's shortfall of one Resource: an amount owed but not delivered.
shortfallTotal :: ResourceId -> RoundRecord -> Q
shortfallTotal res rec = runSlice (shortfallSum res) rec.events

-- | Shortfalls in a round, per Resource: the total quantity and the number of Entities short.
shortfallsOf :: RoundRecord -> Array { resource :: ResourceId, quantity :: Q, entities :: Int }
shortfallsOf rec = map perResource (runSlice shortfallsByResource rec.events)
  where
  perResource (Tuple res mine) = { resource: res, quantity: Q.sumQ (map _.quantity mine), entities: length (nub (map _.entity mine)) }

-- | A Rule as its declared kind writes it.
ruleText :: WorldState -> Rule -> String
ruleText w r = case Map.lookup (ruleKind r) w.ruleKinds of
  Nothing -> show (ruleKind r)
  Just d -> fill "amount" amount (scopeFill d.text)
  where
  fill key value = replaceAll (Pattern ("{" <> key <> "}")) (Replacement value)
  amount = case r of
    FlowRule { effect: Levy l } -> ratio l.rate
    FlowRule { effect: Block } -> ""
    HoldingRule x -> quantity x.reduce
  scopeFill t = case r of
    FlowRule x -> fill "governor" (show x.scope.governor) (fill "counterparty" (show x.scope.counterparty) t)
    HoldingRule x -> fill "resource" (show x.resource) t

-- | A procedure's row in the drivers table: its outcome and the support against the threshold.
procedureRow :: Procedure -> Array String
procedureRow p = [ show p.round, "Collective procedure", show p.proposal, "", outcomeLabel p.executed <> ": " <> supportThreshold p.support p.threshold ]

driverColumn :: Column
driverColumn = column "Driver" "The kind of force at work: a Rule enacted or repealed, a collective procedure, or a shortfall."

subjectColumn :: Column
subjectColumn = column "Subject" "What the driver is about: the Rule, the Proposal or the Resource."

amountColumn :: Column
amountColumn = column "Amount" "The size of the driver, where it has one, in its own unit."

-- | The focus Resource's shortfall of a round; also a column of the pressure table.
shortfallsColumn :: Focus -> Column
shortfallsColumn f = column ("Shortfalls (" <> idText f.resource <> ")") (f.label <> " wanted but not supplied in the round (a Shortfall). 0 means every request was filled. Computed as: " <> describeSlice (shortfallSum f.resource) <> ".")

-- | The columns this module defines.
columns :: Focus -> Array Column
columns f = [ driverColumn, subjectColumn, amountColumn, shortfallsColumn f ]

-- | The drivers table: each round's enacted and repealed rules and procedure outcomes in the order the
-- | engine recorded them, then that round's shortfalls per Resource.
drivers :: StoredRun -> Table
drivers r =
  { title: "Drivers by round"
  , columns: names [ col.round, driverColumn, subjectColumn, amountColumn, col.outcome ]
  , rows: concatMap roundRows (records r)
  }
  where
  roundRows rec = eventRows rec <> map (shortRow rec) (shortfallsOf rec)
  -- the events a round's drivers show, in the order the engine recorded them; a kind with no row is `ignore`d
  eventRows rec = foldEvents
    { "Lapsed": ignore
    , "Rejected": ignore
    , "OrderRemoved": ignore
    , "Coupled": ignore
    , "Enacted": \e acc -> acc <> [ [ show rec.round, "Rule enacted", show e.id, "", ruleText r.manifest.initial e.rule ] ]
    , "Repealed": \e acc -> acc <> [ [ show rec.round, "Rule repealed", show e.id, "", "withdrawn" ] ]
    , "Estimated": ignore
    , "ProcedureOutcome": \o acc -> acc <> [ procedureRow (asProcedure rec o) ]
    }
    []
    rec.events
  shortRow rec s = [ show rec.round, "Shortfall", show s.resource, quantity s.quantity, show s.entities <> " entities short" ]
