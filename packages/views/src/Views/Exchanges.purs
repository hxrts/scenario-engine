-- | The exchange and economy views (docs/06_simulator.md §9.2): Exchange prices, reserves and traded
-- | volume, transformation orders, the world economy's totals and numeraire circulation, the per-round
-- | pressure indicators, and an Exchange's price trajectory across runs. Pure.
module Views.Exchanges
  ( exchangePrices
  , roundPressure
  , exchanges
  , transformations
  , economy
  , trajectories
  , exchangeCharts
  , exchangeUnit
  , columns
  ) where

import Prelude
import Laws.World (Focus, RoundRecord, WorldState)
import Laws.Types (ActorId, TransformationId, idText, Exchange, ExchangeId, ResourceId)
import Views.Common (listedHolding, Chart, Column, Field, StoredRun, Table, cellOf, col, column, columnsOf, exact, focusOf, kindOf, pointsOf, quantity, recordPoints, records, series, tableOf, unitOf)

import Core.Map (entries, sumInto)

import Core.Exchange (exchangePrice)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Laws.Query (lookupQ, priceOf, totalHolding)
import Laws.Slice (Slice, transformationFills, count, describeSlice, event, exchangeFills, read, runSlice, select, sunk, total, transfers, whereEq, whereGt)
import Schema.Recurring (kinds)
import Data.Array (concatMap, filter, foldl, fromFoldable, index, mapMaybe)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..), fst, snd)
import Views.Actions (rejectionCount, rejections)
import Views.Drivers (shortfallTotal, shortfallsColumn)

-- | An Exchange's price after a World State, as the views show it.
type ExchangePrice = { exchange :: ExchangeId, price :: Q }

exchangePrices :: WorldState -> Array ExchangePrice
exchangePrices w = map (\(Tuple id m) -> { exchange: id, price: exchangePrice m }) (Map.toUnfoldable w.exchanges)

exchanges :: StoredRun -> Array Chart
exchanges r =
  [ { title: "Marginal price", unit: unitOf r r.manifest.initial.numeraire <> "/" <> unitOf r focus.resource, kind: Price, series: series r (\w -> map (\p -> Tuple (show p.exchange) p.price) (exchangePrices w)) }
  , { title: focus.label <> " reserves", unit: unitOf r focus.resource, kind: Quantity, series: series r (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.exchanges)) }
  , { title: "Traded volume", unit: unitOf r focus.resource, kind: Quantity, series: volume }
  ]
  where
  focus = focusOf r
  volume = map
    ( \(Tuple id _) ->
        { name: show id
        , points: recordPoints r \rec -> exact (runSlice (boughtVolume id) rec.events)
        }
    )
    (entries r.manifest.initial.exchanges)

transformations :: StoredRun -> Table
transformations r = tableOf "Transformations" transformationFields
  (concatMap (\rec -> map (\t -> { round: rec.round, transformation: t.transformation, by: t.by, kind: kindOf r.manifest.initial t.by, quantity: t.quantity, paid: t.paid }) (select transformationFills rec.events)) (records r))

-- | One order of the transformations table.
type TransformationRow = { round :: Int, transformation :: TransformationId, by :: ActorId, kind :: String, quantity :: Q, paid :: Q }

transformationFields :: Array (Field TransformationRow)
transformationFields =
  [ cellOf col.round (show <<< _.round)
  , cellOf transformationColumn (show <<< _.transformation)
  , cellOf orderedBy (show <<< _.by)
  , cellOf col.kind _.kind
  , cellOf quantityColumn (quantity <<< _.quantity)
  , cellOf paidColumn (quantity <<< _.paid)
  ]

economy :: StoredRun -> { totals :: Chart, flows :: Table, pressure :: Table }
economy r = { totals: resourceTotals r, flows: moneyFlows r, pressure: pressure r }

resourceTotals :: StoredRun -> Chart
resourceTotals r =
  { title: "Resource totals across all accounts"
  , unit: ""
  , kind: Quantity
  , series: series r (\w -> map (\res -> Tuple (show res) (totalHolding w res)) (filter (listedHolding w) (fromFoldable (Map.keys w.resources))))
  }

-- | The numeraire moved between categories of Actor (Actor or Aggregate) over the run.
moneyFlows :: StoredRun -> Table
moneyFlows r = tableOf "Numeraire circulation (from → to category)" moneyFlowFields (Map.toUnfoldable (foldl add Map.empty (select numeraireMoves (concatMap _.events (records r)))))
  where
  w0 = r.manifest.initial
  numeraireMoves = whereEq @"resource" w0.numeraire transfers
  add acc t = sumInto (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc

-- | One round's pressure: shortfalls, consumption and decay, and the candidate Transactions rejected or lapsed.
type Pressure = { shortfall :: Q, consumed :: Q, decayed :: Q, rejected :: Int, lapsed :: Int }

-- | The focus Resource consumed in the round.
consumedTotal :: ResourceId -> Slice Q
consumedTotal res = total (read @"quantity" (whereEq @"resource" res (sunk kinds.requirement)))

decayedTotal :: Slice Q
decayedTotal = total (read @"quantity" (sunk kinds.decay))

lapsedCount :: Slice Int
lapsedCount = count (event @"Lapsed")

boughtVolume :: ExchangeId -> Slice Q
boughtVolume id = total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"exchange" id exchangeFills)))

-- | One round's pressure on the focus Resource (shortfall and consumption are that Resource's).
roundPressure :: ResourceId -> RoundRecord -> Pressure
roundPressure res rec =
  { shortfall: shortfallTotal res rec
  , consumed: runSlice (consumedTotal res) rec.events
  , decayed: runSlice decayedTotal rec.events
  , rejected: rejectionCount rec
  , lapsed: runSlice lapsedCount rec.events
  }

transformationColumn :: Column
transformationColumn = column "Transformation" "The transformation that filled the order."

orderedBy :: Column
orderedBy = column "Ordered by" "The Actor that placed the order."

quantityColumn :: Column
quantityColumn = column "Quantity" "The quantity of the transformation's output that the order bought."

paidColumn :: Column
paidColumn = column "Paid" "The numeraire paid for the order."

fromColumn :: Column
fromColumn = column "From" "The category of the paying Actor: Actor, or Aggregate (residual)."

toColumn :: Column
toColumn = column "To" "The category of the receiving Actor: Actor, or Aggregate (residual)."

moneyColumn :: Column
moneyColumn = column "Numeraire moved" "Total numeraire moved from one category to the other over the run (Circulation between Actor categories)."

rejectedColumn :: Column
rejectedColumn = column "Rejected" ("candidate Transactions the engine rejected in the round (rejected candidate Transactions). Computed as: " <> describeSlice rejections <> ".")

lapsedColumn :: Column
lapsedColumn = column "Lapsed" ("candidate Transactions whose signature rule was not met in the round, so they lapsed without executing. Computed as: " <> describeSlice lapsedCount <> ".")

consumedColumn :: Focus -> Column
consumedColumn f = column ("Consumed (" <> idText f.resource <> ")") (f.label <> " consumed in the round. Computed as: " <> describeSlice (consumedTotal f.resource) <> ".")

decayedColumn :: Column
decayedColumn = column "Decayed" ("Quantity that decayed in the round. Computed as: " <> describeSlice decayedTotal <> ".")

-- | The columns this module defines.
columns :: Focus -> Array Column
columns f = columnsOf transformationFields <> columnsOf moneyFlowFields <> columnsOf (pressureFields f)

-- | Per round: shortfalls, rejected and lapsed candidate Transactions, consumption, and decay.
pressure :: StoredRun -> Table
pressure r = tableOf "Pressure" (pressureFields (focusOf r)) (map (\rec -> Tuple rec.round (roundPressure (focusOf r).resource rec)) (records r))

pressureFields :: Focus -> Array (Field (Tuple Int Pressure))
pressureFields f =
  [ cellOf col.round (show <<< fst)
  , cellOf (shortfallsColumn f) (quantity <<< _.shortfall <<< snd)
  , cellOf rejectedColumn (show <<< _.rejected <<< snd)
  , cellOf lapsedColumn (show <<< _.lapsed <<< snd)
  , cellOf (consumedColumn f) (quantity <<< _.consumed <<< snd)
  , cellOf decayedColumn (quantity <<< _.decayed <<< snd)
  ]

moneyFlowFields :: Array (Field (Tuple (Tuple String String) Q))
moneyFlowFields =
  [ cellOf fromColumn (\(Tuple (Tuple a _) _) -> a)
  , cellOf toColumn (\(Tuple (Tuple _ b) _) -> b)
  , cellOf moneyColumn (\(Tuple _ m) -> quantity m)
  ]

-- | The Exchange a name stands for, checked once against every run: a run without it is an error that
-- | names the run. Everything after this takes the typed id.
exchangeIn :: Array StoredRun -> String -> Either String ExchangeId
exchangeIn runs name = do
  let m = wrap name
  for_ runs \r -> void (requireExchange m r)
  pure m

-- | The Exchange in a run's manifest, or the error naming the run that lacks it.
requireExchange :: ExchangeId -> StoredRun -> Either String Exchange
requireExchange m r = maybe (Left ("exchange " <> show m <> " is not in run " <> r.id)) Right (Map.lookup m r.manifest.initial.exchanges)

-- | One exchange's price across runs, aligned by round. The unit is the Exchange's quote per base unit,
-- | read from the Resources' units in the run's manifest. A run without the Exchange is an error.
trajectories :: Array StoredRun -> String -> Either String Chart
trajectories runs name = do
  m <- exchangeIn runs name
  trajectory runs m

trajectory :: Array StoredRun -> ExchangeId -> Either String Chart
trajectory runs m = do
  unit <- maybe (Right "") (exchangeUnit m) (index runs 0)
  pure
    { title: "Price, " <> show m
    , unit
    , kind: Price
    , series: map (\r -> { name: r.id, points: pointsOf r (Just <<< exact <<< priceAt) }) runs
    }
  where
  priceAt w = fromMaybe Q.zero (priceOf w m)

-- | Every Exchange the first run has, as one price chart per Exchange over the runs that have it, so the
-- | runs are compared on the same axes. An Exchange a run lacks leaves that run out of its chart.
exchangeCharts :: Array StoredRun -> Array Chart
exchangeCharts runs = case index runs 0 of
  Nothing -> []
  Just base -> mapMaybe (\m -> hush (trajectory (filter (hasExchange m) runs) m)) (fromFoldable (Map.keys base.manifest.initial.exchanges))
  where
  hasExchange m r = Map.member m r.manifest.initial.exchanges

-- | An Exchange's unit, the quote per base unit, from the run's Resources. A run without the Exchange is an error.
exchangeUnit :: ExchangeId -> StoredRun -> Either String String
exchangeUnit m r = do
  exchange <- requireExchange m r
  base <- resourceUnit exchange.base
  quoteUnit <- resourceUnit exchange.quote
  pure (quoteUnit <> "/" <> base)
  where
  resourceUnit :: ResourceId -> Either String String
  resourceUnit res = maybe (Left ("resource " <> show res <> " is not in run " <> r.id)) (Right <<< _.unit) (Map.lookup res r.manifest.initial.resources)
