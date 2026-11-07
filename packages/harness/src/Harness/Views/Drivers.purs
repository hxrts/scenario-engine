-- | Drivers of gameplay (docs/06_simulator.md §9.2): the rules enacted and repealed, the collective
-- | procedure outcomes, and the shortfalls, by round, on the same round axis as the price chart. This
-- | is the one definition of a shortfall total. Pure.
module Harness.Views.Drivers
  ( driversStat
  , shortfallTotal
  , drivers
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, ResourceId, Rule(..))
import Core.World (Event(..), RoundRecord)
import Data.Array (concatMap, filter, length, nub)
import Harness.Views.Common (Statistic, StoredRun, Table, quantity, ratio)

driversStat :: Statistic
driversStat =
  { name: "Drivers"
  , question: "What forces are driving gameplay (what moves prices and outcomes)?"
  , definition: "Rules enacted and repealed, collective procedure outcomes, and shortfalls, by round."
  , formula: "Shortfall(r, resource) = sum of Shortfall quantities in round r for that Resource, with the number of Entities short; a procedure's support and threshold are the ratios its ProcedureOutcome records."
  }

-- | The round's Shortfall events, in the order recorded.
shortfalls :: RoundRecord -> Array { resource :: ResourceId, entity :: EntityId, quantity :: Q }
shortfalls rec = concatMap
  ( \ev -> case ev of
      Shortfall s -> [ { resource: s.resource, entity: s.entity, quantity: s.quantity } ]
      _ -> []
  )
  rec.events

-- | The round's shortfalls summed over all Resources: an amount owed but not delivered.
shortfallTotal :: RoundRecord -> Q
shortfallTotal rec = Q.sumQ (map _.quantity (shortfalls rec))

-- | Shortfalls in a round, per Resource: the total quantity and the number of Entities short.
shortfallsOf :: RoundRecord -> Array { resource :: ResourceId, quantity :: Q, entities :: Int }
shortfallsOf rec = map perResource resources
  where
  shorts = shortfalls rec
  resources = nub (map _.resource shorts)
  perResource res =
    let
      mine = filter (\s -> s.resource == res) shorts
    in
      { resource: res, quantity: Q.sumQ (map _.quantity mine), entities: length (nub (map _.entity mine)) }

ruleText :: Rule -> String
ruleText = case _ of
  Tariff t -> "tariff on " <> show t.origin <> " goods into " <> show t.jurisdiction <> ", rate " <> ratio t.rate
  AccessBan b -> "access ban: " <> show b.jurisdiction <> " bars " <> show b.target
  Withholding w -> "withholding of " <> quantity w.units <> " from " <> show w.capacity

-- | The drivers table: each round's enacted and repealed rules and procedure outcomes in the order the
-- | engine recorded them, then that round's shortfalls per Resource.
drivers :: StoredRun -> Table
drivers r =
  { title: "Drivers by round"
  , columns: [ "Round", "Driver", "Subject", "Amount", "Outcome" ]
  , rows: concatMap roundRows r.records
  }
  where
  roundRows rec =
    concatMap (eventRow rec) rec.events <> map (shortRow rec) (shortfallsOf rec)
  eventRow rec = case _ of
    Enacted e -> [ [ show rec.round, "Rule enacted", show e.id, "", ruleText e.rule ] ]
    Repealed e -> [ [ show rec.round, "Rule repealed", show e.id, "", "withdrawn" ] ]
    ProcedureOutcome o ->
      [ [ show rec.round
        , "Collective procedure"
        , show o.proposal
        , ""
        , (if o.executed then "executed" else "lapsed") <> ": support " <> ratio o.support <> ", threshold " <> ratio o.threshold
        ]
      ]
    _ -> []
  shortRow rec s = [ show rec.round, "Shortfall", show s.resource, quantity s.quantity, show s.entities <> " entities short" ]

