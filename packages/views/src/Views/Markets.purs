-- | The market and economy views (docs/06_simulator.md §9.2): Market prices, reserves and traded
-- | volume, facility orders, the world economy's totals and numeraire circulation, the per-round
-- | pressure indicators, and a Market's price trajectory across runs. Pure.
module Views.Markets
  ( marketPrices
  , roundPressure
  , markets
  , facilities
  , economy
  , trajectories
  , marketCharts
  , marketUnit
  , columns
  ) where

import Prelude
import Core.World (Focus, RoundRecord, WorldState)
import Core.Types (EntityId, TransformationId, idText, Market, MarketId, ResourceId)
import Views.Common (Chart, Column, Field, StoredRun, Table, cellOf, col, column, columnsOf, exact, focusOf, kindOf, pointsOf, quantity, recordPoints, records, series, tableOf, unitOf)

import Core.Map (entries, sumInto)

import Core.Market (marketPrice)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Core.Query (lookupQ, priceOf, totalHolding)
import Core.Slice (Slice, conversions, count, describeSlice, event, exchanges, read, runSlice, select, sunk, total, transfers, whereEq, whereGt)
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

-- | A Market's price after a World State, as the views show it.
type MarketPrice = { market :: MarketId, price :: Q }

marketPrices :: WorldState -> Array MarketPrice
marketPrices w = map (\(Tuple id m) -> { market: id, price: marketPrice m }) (Map.toUnfoldable w.markets)

markets :: StoredRun -> Array Chart
markets r =
  [ { title: "Marginal price", unit: unitOf r r.manifest.initial.numeraire <> "/" <> unitOf r focus.resource, kind: Price, series: series r (\w -> map (\p -> Tuple (show p.market) p.price) (marketPrices w)) }
  , { title: focus.label <> " reserves", unit: unitOf r focus.resource, kind: Quantity, series: series r (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.markets)) }
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
    (entries r.manifest.initial.markets)

facilities :: StoredRun -> Table
facilities r = tableOf "Facilities" facilityFields
  (concatMap (\rec -> map (\t -> { round: rec.round, transformation: t.transformation, by: t.by, kind: kindOf r.manifest.initial t.by, quantity: t.quantity, paid: t.paid }) (select conversions rec.events)) (records r))

-- | One order of the facilities table.
type FacilityRow = { round :: Int, transformation :: TransformationId, by :: EntityId, kind :: String, quantity :: Q, paid :: Q }

facilityFields :: Array (Field FacilityRow)
facilityFields =
  [ cellOf col.round (show <<< _.round)
  , cellOf facilityColumn (show <<< _.transformation)
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
  , series: series r (\w -> map (\res -> Tuple (show res) (totalHolding w res)) (fromFoldable (Map.keys w.resources)))
  }

-- | The numeraire moved between categories of Entity (Actor or Aggregate) over the run.
moneyFlows :: StoredRun -> Table
moneyFlows r = tableOf "Numeraire circulation (from → to category)" moneyFlowFields (Map.toUnfoldable (foldl add Map.empty (select numeraireMoves (concatMap _.events (records r)))))
  where
  w0 = r.manifest.initial
  numeraireMoves = whereEq @"resource" w0.numeraire transfers
  add acc t = sumInto (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc

-- | One round's pressure: shortfalls, consumption and decay, and the Proposals rejected or lapsed.
type Pressure = { shortfall :: Q, consumed :: Q, decayed :: Q, rejected :: Int, lapsed :: Int }

-- | The focus Resource consumed in the round.
consumedTotal :: ResourceId -> Slice Q
consumedTotal res = total (read @"quantity" (whereEq @"resource" res (sunk kinds.requirement)))

decayedTotal :: Slice Q
decayedTotal = total (read @"quantity" (sunk kinds.decay))

lapsedCount :: Slice Int
lapsedCount = count (event @"Lapsed")

boughtVolume :: MarketId -> Slice Q
boughtVolume id = total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"market" id exchanges)))

-- | One round's pressure on the focus Resource (shortfall and consumption are that Resource's).
roundPressure :: ResourceId -> RoundRecord -> Pressure
roundPressure res rec =
  { shortfall: shortfallTotal res rec
  , consumed: runSlice (consumedTotal res) rec.events
  , decayed: runSlice decayedTotal rec.events
  , rejected: rejectionCount rec
  , lapsed: runSlice lapsedCount rec.events
  }

facilityColumn :: Column
facilityColumn = column "Facility" "The facility that filled the order."

orderedBy :: Column
orderedBy = column "Ordered by" "The Entity that placed the order."

quantityColumn :: Column
quantityColumn = column "Quantity" "The quantity of the facility's output that the order bought."

paidColumn :: Column
paidColumn = column "Paid" "The numeraire paid for the order."

fromColumn :: Column
fromColumn = column "From" "The category of the paying Entity: Actor, or Aggregate (residual)."

toColumn :: Column
toColumn = column "To" "The category of the receiving Entity: Actor, or Aggregate (residual)."

moneyColumn :: Column
moneyColumn = column "Numeraire moved" "Total numeraire moved from one category to the other over the run (Circulation between Entity categories)."

rejectedColumn :: Column
rejectedColumn = column "Rejected" ("Proposals the engine rejected in the round (rejected Proposals). Computed as: " <> describeSlice rejections <> ".")

lapsedColumn :: Column
lapsedColumn = column "Lapsed" ("Proposals whose signature rule was not met in the round, so they lapsed without executing. Computed as: " <> describeSlice lapsedCount <> ".")

consumedColumn :: Focus -> Column
consumedColumn f = column ("Consumed (" <> idText f.resource <> ")") (f.label <> " consumed in the round. Computed as: " <> describeSlice (consumedTotal f.resource) <> ".")

decayedColumn :: Column
decayedColumn = column "Decayed" ("Quantity that decayed in the round. Computed as: " <> describeSlice decayedTotal <> ".")

-- | The columns this module defines.
columns :: Focus -> Array Column
columns f = columnsOf facilityFields <> columnsOf moneyFlowFields <> columnsOf (pressureFields f)

-- | Per round: shortfalls, rejected and lapsed Proposals, consumption, and decay.
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

-- | The Market a name stands for, checked once against every run: a run without it is an error that
-- | names the run. Everything after this takes the typed id.
marketIn :: Array StoredRun -> String -> Either String MarketId
marketIn runs name = do
  let m = wrap name
  for_ runs \r -> void (requireMarket m r)
  pure m

-- | The Market in a run's manifest, or the error naming the run that lacks it.
requireMarket :: MarketId -> StoredRun -> Either String Market
requireMarket m r = maybe (Left ("market " <> show m <> " is not in run " <> r.id)) Right (Map.lookup m r.manifest.initial.markets)

-- | One market's price across runs, aligned by round. The unit is the Market's quote per base unit,
-- | read from the Resources' units in the run's manifest. A run without the Market is an error.
trajectories :: Array StoredRun -> String -> Either String Chart
trajectories runs name = do
  m <- marketIn runs name
  trajectory runs m

trajectory :: Array StoredRun -> MarketId -> Either String Chart
trajectory runs m = do
  unit <- maybe (Right "") (marketUnit m) (index runs 0)
  pure
    { title: "Price, " <> show m
    , unit
    , kind: Price
    , series: map (\r -> { name: r.id, points: pointsOf r (Just <<< exact <<< priceAt) }) runs
    }
  where
  priceAt w = fromMaybe Q.zero (priceOf w m)

-- | Every Market the first run has, as one price chart per Market over the runs that have it, so the
-- | runs are compared on the same axes. A Market a run lacks leaves that run out of its chart.
marketCharts :: Array StoredRun -> Array Chart
marketCharts runs = case index runs 0 of
  Nothing -> []
  Just base -> mapMaybe (\m -> hush (trajectory (filter (hasMarket m) runs) m)) (fromFoldable (Map.keys base.manifest.initial.markets))
  where
  hasMarket m r = Map.member m r.manifest.initial.markets

-- | A Market's unit, the quote per base unit, from the run's Resources. A run without the Market is an error.
marketUnit :: MarketId -> StoredRun -> Either String String
marketUnit m r = do
  market <- requireMarket m r
  base <- resourceUnit market.base
  quoteUnit <- resourceUnit market.quote
  pure (quoteUnit <> "/" <> base)
  where
  resourceUnit :: ResourceId -> Either String String
  resourceUnit res = maybe (Left ("resource " <> show res <> " is not in run " <> r.id)) (Right <<< _.unit) (Map.lookup res r.manifest.initial.resources)
