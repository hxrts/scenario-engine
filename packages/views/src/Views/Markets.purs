-- | The market and economy views (docs/06_simulator.md §9.2): Market prices, reserves and traded
-- | volume, facility orders, the world economy's totals and Money circulation, the per-round
-- | pressure indicators, and a Market's price trajectory across runs. Pure.
module Views.Markets
  ( marketPrices
  , consumedTotal
  , decayedTotal
  , lapsedCount
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
import Core.World (Focus)
import Core.Types (idText)
import Views.Common (focusOf)

import Core.Map (sumInto)

import Core.Market (marketPrice)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Core.Query (lookupQ, priceOf, totalHolding)
import Core.Slice (Slice, conversions, count, describeSlice, event, exchanges, read, runSlice, select, sunk, total, transfers, whereEq, whereGt)
import Schema.Recurring (kinds)
import Core.Types (Id(..), Market, MarketId, ResourceId)
import Core.World (RoundRecord, WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable, index, mapMaybe)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Views.Actions (rejectionCount, rejections)
import Views.Common (Chart, Column, StoredRun, Table, col, column, exact, kindOf, names, pointsOf, quantity, recordPoints, records, series, unitOf)
import Views.Drivers (shortfallTotal, shortfallsColumn)

-- | A Market's price after a World State, as the views show it.
type MarketPrice = { market :: MarketId, price :: Q }

marketPrices :: WorldState -> Array MarketPrice
marketPrices w = map (\(Tuple id m) -> { market: id, price: marketPrice m }) (Map.toUnfoldable w.markets)

markets :: StoredRun -> Array Chart
markets r =
  [ { title: "Marginal price", unit: unitOf r r.manifest.initial.numeraire <> "/" <> unitOf r focus.resource, kind: Price, series: series r (\w -> map (\p -> Tuple (show p.market) p.price) (marketPrices w)) exact }
  , { title: focus.label <> " reserves", unit: unitOf r focus.resource, kind: Quantity, series: series r (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.markets)) exact }
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
    (Map.toUnfoldable r.manifest.initial.markets :: Array _)

facilities :: StoredRun -> Table
facilities r =
  { title: "Facilities"
  , columns: names [ col.round, facilityColumn, orderedBy, col.kind, quantityColumn, paidColumn ]
  , rows: concatMap
      ( \rec -> map (\t -> [ show rec.round, show t.transformation, show t.by, kindOf r.manifest.initial t.by, quantity t.quantity, quantity t.paid ])
          (select conversions rec.events)
      )
      (records r)
  }

economy :: StoredRun -> { totals :: Chart, flows :: Table, pressure :: Table }
economy r = { totals: resourceTotals r, flows: moneyFlows r, pressure: pressure r }

resourceTotals :: StoredRun -> Chart
resourceTotals r =
  { title: "Resource totals across all accounts"
  , unit: ""
  , kind: Quantity
  , series: series r (\w -> map (\res -> Tuple (show res) (totalHolding w res)) (fromFoldable (Map.keys w.resources))) exact
  }

-- | Money moved between categories of Entity (Actor or Aggregate) over the run.
moneyFlows :: StoredRun -> Table
moneyFlows r =
  { title: "Money circulation (from → to category)"
  , columns: names [ fromColumn, toColumn, moneyColumn ]
  , rows: map (\(Tuple (Tuple a b) m) -> [ a, b, quantity m ]) (Map.toUnfoldable (foldl add Map.empty (select numeraireMoves (concatMap _.events (records r)))))
  }
  where
  w0 = r.manifest.initial
  numeraireMoves = whereEq @"resource" w0.numeraire transfers
  add acc t = sumInto (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc

-- | One round's pressure: shortfalls, consumption and decay, and the Proposals rejected or lapsed.
type Pressure = { shortfall :: Q, consumed :: Q, decayed :: Q, rejected :: Int, lapsed :: Int }

consumedTotal :: Slice Q
consumedTotal = total (read @"quantity" (sunk kinds.requirement))

decayedTotal :: Slice Q
decayedTotal = total (read @"quantity" (sunk kinds.decay))

lapsedCount :: Slice Int
lapsedCount = count (event @"Lapsed")

boughtVolume :: MarketId -> Slice Q
boughtVolume id = total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"market" id exchanges)))

roundPressure :: RoundRecord -> Pressure
roundPressure rec =
  { shortfall: shortfallTotal rec
  , consumed: runSlice consumedTotal rec.events
  , decayed: runSlice decayedTotal rec.events
  , rejected: rejectionCount rec
  , lapsed: runSlice lapsedCount rec.events
  }

facilityColumn :: Column
facilityColumn = column "Facility" "The facility that filled the order: defense procurement, public attention or energy shipping."

orderedBy :: Column
orderedBy = column "Ordered by" "The Entity that placed the order."

quantityColumn :: Column
quantityColumn = column "Quantity" "The quantity of the facility's output that the order bought."

paidColumn :: Column
paidColumn = column "Paid" "The Money paid for the order."

fromColumn :: Column
fromColumn = column "From" "The category of the paying Entity: Actor, or Aggregate (residual)."

toColumn :: Column
toColumn = column "To" "The category of the receiving Entity: Actor, or Aggregate (residual)."

moneyColumn :: Column
moneyColumn = column "Money" "Total Money moved from one category to the other over the run (Circulation between Entity categories)."

rejectedColumn :: Column
rejectedColumn = column "Rejected" ("Proposals the engine rejected in the round (rejected Proposals). Computed as: " <> describeSlice rejections <> ".")

lapsedColumn :: Column
lapsedColumn = column "Lapsed" ("Proposals whose signature rule was not met in the round, so they lapsed without executing. Computed as: " <> describeSlice lapsedCount <> ".")

consumedColumn :: Focus -> Column
consumedColumn f = column ("Consumed (" <> idText f.resource <> ")") (f.label <> " consumed in the round. Computed as: " <> describeSlice consumedTotal <> ".")

decayedColumn :: Column
decayedColumn = column "Decayed" ("Quantity that decayed in the round. Computed as: " <> describeSlice decayedTotal <> ".")

-- | The columns this module defines.
columns :: Focus -> Array Column
columns f = [ facilityColumn, orderedBy, quantityColumn, paidColumn, fromColumn, toColumn, moneyColumn, rejectedColumn, lapsedColumn, consumedColumn f, decayedColumn ]

-- | Per round: shortfalls, rejected and lapsed Proposals, consumption, and decay.
pressure :: StoredRun -> Table
pressure r =
  { title: "Pressure"
  , columns: names [ col.round, shortfallsColumn (focusOf r), rejectedColumn, lapsedColumn, consumedColumn (focusOf r), decayedColumn ]
  , rows: map (\rec -> let p = roundPressure rec in [ show rec.round, quantity p.shortfall, show p.rejected, show p.lapsed, quantity p.consumed, quantity p.decayed ]) (records r)
  }

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
