-- | Dependence (docs/07_simulator.md §9.2): for each round, each Actor and each Resource it has a fixed
-- | requirement of, what its own capacity covers, what it must import, what it traded, what it received for its
-- | sales, what it went short, the Rules in force that block that Resource reaching it, and the capacity on
-- | order for it. Derived from the World and the round's record only. Pure.
module Views.Dependence (dependence, columns, coverage, exposure, requirementsOf, demandsOf, capacitiesOf, finalCoverage, cumulativeShortfall, cumulativeReceipts, cumulativePurchases) where

import Prelude

import Core.Num (Q)
import Core.Map (values)
import Core.Num as Q
import Data.Array (concatMap, elem, filter, length, mapMaybe, zip)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Laws.Engine.Compile (deliveredOf)
import Laws.Query (holding, tradesOf)
import Laws.Slice (exchangeFills, select, shortfalls)
import Laws.Types (ActorId, FlowEffect(..), ResourceId, RuleF(..))
import Laws.World (RecurringAmount(..), RecurringDestination(..), RecurringSource(..), RoundRecord, WorldState)
import Schema.Recurring (kinds)
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, ratio, records, snapshots)

requirementColumn :: Column
requirementColumn = column "Requirement" "What the Actor's fixed recurring requirement consumes of the Resource each round."

capacityColumn :: Column
capacityColumn = column "Capacity" "Capacity(a, g): what the Actor's own capacity produces of the Resource each round, after the round."

coverageColumn :: Column
coverageColumn = column "Coverage" "Coverage = min(1, Capacity / Requirement): how much of the requirement domestic capacity can meet."

exposureColumn :: Column
exposureColumn = column "Exposure" "Exposure = max(0, Requirement − Capacity): what the Actor must import, or hold, to meet its requirement."

importsColumn :: Column
importsColumn = column "Imports" "What the Actor bought of the Resource in the round's Exchange fills."

exportsColumn :: Column
exportsColumn = column "Exports" "What the Actor sold of the Resource in the round's Exchange fills."

receiptsColumn :: Column
receiptsColumn = column "Receipts" "What the Actor was paid for its sales of the Resource in the round: price × quantity sold."

shortfallColumn :: Column
shortfallColumn = column "Shortfall" "What the requirement was owed and not consumed in the round."

blockedColumn :: Column
blockedColumn = column "Bans against" "The Rules in force after the round that block the Resource reaching the Actor."

orderedColumn :: Column
orderedColumn = column "Capacity on order" "Capacity for the Resource that scheduled Flows will still deliver to the Actor."

unsoldColumn :: Column
unsoldColumn = column "Unsold" "What the Actor offered of the Resource on the Exchanges in the round and no buyer took."

columns :: Array Column
columns = [ requirementColumn, capacityColumn, coverageColumn, exposureColumn, importsColumn, exportsColumn, receiptsColumn, shortfallColumn, blockedColumn, orderedColumn, unsoldColumn ]

coverage :: Q -> Q -> Q
coverage requirement capacity = min Q.one (fromMaybe Q.one (Q.divMaybe capacity requirement))

exposure :: Q -> Q -> Q
exposure requirement capacity = max Q.zero (Q.sub requirement capacity)

dependence :: StoredRun -> Table
dependence r =
  { title: "Dependence"
  , columns: names ([ col.round, actorColumn, resourceColumn ] <> columns)
  , rows: concatMap rowsOf (zip (records r) (snapshots r))
  }
  where
  actorColumn = column "Actor" "The Actor with the requirement."
  resourceColumn = column "Resource" "The Resource it requires."
  -- a fixed requirement, then a demand: an Actor whose requirement is what it buys, whose purchases are its
  -- requirement and which has no capacity to cover it
  rowsOf (Tuple rec w) = map (rowOf rec w) (map (\q -> q { amount = Just q.amount }) (requirementsOf w) <> map (\q -> { actor: q.actor, resource: q.resource, amount: Nothing }) (demandsOf w))

  rowOf :: RoundRecord -> WorldState -> { actor :: ActorId, resource :: ResourceId, amount :: Maybe Q } -> Array String
  rowOf rec w q =
    let
      caps = capacitiesOf w q.actor q.resource
      capacity = Q.sumQ (map (holding w q.actor) caps)
      fills = filter (\f -> f.actor == q.actor && f.resource == q.resource) (select exchangeFills rec.events)
      bought = Q.sumQ (map _.quantity (filter (\f -> f.quantity > Q.zero) fills))
      sold = Q.neg (Q.sumQ (map _.quantity (filter (\f -> f.quantity < Q.zero) fills)))
      paid = Q.sumQ (map (\f -> Q.neg (Q.mul f.price f.quantity)) (filter (\f -> f.quantity < Q.zero) fills))
      short = Q.sumQ (map _.quantity (filter (\f -> f.actor == q.actor && f.resource == q.resource) (select shortfalls rec.events)))
      ordered = Q.sumQ (map _.quantity (filter (\d -> d.actor == q.actor && d.resource `elem` caps) (mapMaybe deliveredOf w.pending)))
      offered = Q.sumQ (concatMap (\s -> if s.candidateTransaction.author == q.actor then map (\o -> Q.neg o.quantity) (filter (\o -> o.quantity < Q.zero && baseOf o.exchange == Just q.resource) (tradesOf s.candidateTransaction.intents)) else []) rec.inputs)
      baseOf x = _.base <$> Map.lookup x w.exchanges
      amount = fromMaybe bought q.amount
    in
      [ show rec.round
      , show q.actor
      , show q.resource
      , quantity amount
      , quantity capacity
      , maybe "n/a" (\a -> ratio (coverage a capacity)) q.amount
      , maybe "n/a" (\a -> quantity (exposure a capacity)) q.amount
      , quantity bought
      , quantity sold
      , quantity paid
      , quantity short
      , show (length (filter (blocks q.actor q.resource) (values w.rules)))
      , quantity ordered
      , quantity (max Q.zero (Q.sub offered sold))
      ]

-- | What a requirement's capacity covers after a World: min(1, capacity / requirement).
finalCoverage :: WorldState -> { actor :: ActorId, resource :: ResourceId, amount :: Q } -> Q
finalCoverage w q = coverage q.amount (Q.sumQ (map (holding w q.actor) (capacitiesOf w q.actor q.resource)))

-- | An Actor's shortfall of a Resource summed over the run's rounds.
cumulativeShortfall :: StoredRun -> ActorId -> ResourceId -> Q
cumulativeShortfall r a g = Q.sumQ (concatMap (\rec -> map _.quantity (filter (\f -> f.actor == a && f.resource == g) (select shortfalls rec.events))) (records r))

-- | What an Actor was paid for its Exchange sales over the run.
cumulativeReceipts :: StoredRun -> ActorId -> Q
cumulativeReceipts r a = Q.sumQ (concatMap (\rec -> map (\f -> Q.neg (Q.mul f.price f.quantity)) (filter (\f -> f.actor == a && f.quantity < Q.zero) (select exchangeFills rec.events))) (records r))

-- | Every fixed requirement of the recurring cycle.
requirementsOf :: WorldState -> Array { actor :: ActorId, resource :: ResourceId, amount :: Q }
requirementsOf w = mapMaybe one w.recurring
  where
  one x = case x.source, x.amount of
    FromActor a, FixedAmount amount | x.kind == kinds.requirement -> Just { actor: a, resource: x.resource, amount }
    _, _ -> Nothing

-- | Every requirement measured by what its Actor buys: the Actors whose demand is their purchases.
demandsOf :: WorldState -> Array { actor :: ActorId, resource :: ResourceId }
demandsOf w = mapMaybe one w.recurring
  where
  one x = case x.source, x.amount of
    FromActor a, Measured _ | x.kind == kinds.requirement -> Just { actor: a, resource: x.resource }
    _, _ -> Nothing

-- | The capacities whose production delivers a Resource to an Actor.
capacitiesOf :: WorldState -> ActorId -> ResourceId -> Array ResourceId
capacitiesOf w a g = mapMaybe one w.recurring
  where
  one x = case x.destination, x.amount of
    ToActor to, HeldAmount cap | x.kind == kinds.production && to == a && x.resource == g -> Just cap
    _, _ -> Nothing

-- | A block in force on a Resource coming to an Actor from its governor's side.
blocks :: forall v. ActorId -> ResourceId -> RuleF v -> Boolean
blocks a g = case _ of
  FlowRule x -> isBlock x.effect && x.scope.counterparty == a && g `elem` x.scope.over
  HoldingRule _ -> false
  where
  isBlock = case _ of
    Block -> true
    Levy _ -> false

-- | What an Actor bought of a Resource on the Exchanges over the run.
cumulativePurchases :: StoredRun -> ActorId -> ResourceId -> Q
cumulativePurchases r a g = Q.sumQ (concatMap (\rec -> map _.quantity (filter (\f -> f.actor == a && f.resource == g && f.quantity > Q.zero) (select exchangeFills rec.events))) (records r))
