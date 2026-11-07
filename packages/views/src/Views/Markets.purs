-- | The market and economy views (docs/06_simulator.md §9.2): Market prices, reserves and traded
-- | volume, facility orders, the world economy's totals and Money circulation, the per-round
-- | pressure indicators, and a Market's price trajectory across runs. Pure.
module Views.Markets
  ( MarketPrice
  , Pressure
  , marketPrices
  , roundPressure
  , markets
  , facilities
  , economy
  , trajectories
  , marketCharts
  , marketUnit
  ) where

import Prelude

import Core.Market (marginal)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Core.Query (countEvents, eventsOf, lookupQ, priceOf, totalHolding)
import Core.Types (Id(..), Market, MarketId, ResourceId)
import Core.World (Event(..), RoundRecord, WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable, index, mapMaybe, mapWithIndex)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Views.Actions (rejectionCount)
import Views.Common (Chart, StoredRun, Table, exact, kindOf, quantity, series, unitOf)
import Views.Drivers (shortfallTotal)

-- | A Market's price after a World State, as the views show it.
type MarketPrice = { market :: MarketId, price :: Q }

marketPrices :: WorldState -> Array MarketPrice
marketPrices w = map (\(Tuple id m) -> { market: id, price: marginal m.cost m.position }) (Map.toUnfoldable w.markets)

-- | The Energy Resource and the money numeraire the Energy Markets quote in; their units are the
-- | manifest's, so the chart units are not spelled out here.
energy :: ResourceId
energy = Id "E"

markets :: StoredRun -> Array Chart
markets r =
  [ { title: "Marginal price", unit: unitOf r r.manifest.initial.numeraire <> "/" <> unitOf r energy, kind: Price, series: series r (\w -> map (\(Tuple id m) -> Tuple (show id) (marginal m.cost m.position)) (Map.toUnfoldable w.markets)) exact }
  , { title: "Energy reserves", unit: unitOf r energy, kind: Quantity, series: series r (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.markets)) exact }
  , { title: "Traded volume", unit: unitOf r energy, kind: Quantity, series: volume }
  ]
  where
  volume = map
    ( \(Tuple id _) ->
        { name: show id
        , points: mapWithIndex
            ( \i rec ->
                { round: i + 1
                , value: exact
                    ( Q.sumQ
                        ( eventsOf
                            ( case _ of
                                Traded t | t.market == id && t.quantity > Q.zero -> Just t.quantity
                                _ -> Nothing
                            )
                            rec.events
                        )
                    )
                }
            )
            r.records
        }
    )
    (Map.toUnfoldable r.manifest.initial.markets :: Array _)

facilities :: StoredRun -> Table
facilities r =
  { title: "Facilities"
  , columns: [ "Round", "Facility", "Ordered by", "Kind", "Quantity", "Paid" ]
  , rows: concatMap
      ( \rec -> map (\t -> [ show rec.round, show t.transformation, show t.by, kindOf r.manifest.initial t.by, quantity t.quantity, quantity t.paid ])
          (eventsOf transformed rec.events)
      )
      r.records
  }
  where
  transformed = case _ of
    Transformed t -> Just t
    _ -> Nothing

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
  , columns: [ "From", "To", "Money" ]
  , rows: map (\(Tuple (Tuple a b) m) -> [ a, b, quantity m ]) (Map.toUnfoldable (foldl add Map.empty (concatMap _.events r.records)))
  }
  where
  w0 = r.manifest.initial
  add acc = case _ of
    Transferred t | t.resource == w0.numeraire -> Map.insertWith Q.add (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc
    _ -> acc

-- | One round's pressure: shortfalls, consumption and decay, and the Proposals rejected or lapsed.
type Pressure = { shortfall :: Q, consumed :: Q, decayed :: Q, rejected :: Int, lapsed :: Int }

roundPressure :: RoundRecord -> Pressure
roundPressure rec =
  { shortfall: shortfallTotal rec
  , consumed: Q.sumQ (eventsOf consumedOf rec.events)
  , decayed: Q.sumQ (eventsOf decayedOf rec.events)
  , rejected: rejectionCount rec
  , lapsed: countEvents isLapsed rec.events
  }
  where
  consumedOf = case _ of
    Consumed c -> Just c.quantity
    _ -> Nothing
  decayedOf = case _ of
    Decayed d -> Just d.quantity
    _ -> Nothing
  isLapsed = case _ of
    Lapsed _ -> true
    _ -> false

-- | Per round: shortfalls, rejected and lapsed Proposals, consumption, and decay.
pressure :: StoredRun -> Table
pressure r =
  { title: "Pressure"
  , columns: [ "Round", "Shortfalls (E)", "Rejected", "Lapsed", "Consumed (E)", "Decayed" ]
  , rows: map (\rec -> let p = roundPressure rec in [ show rec.round, quantity p.shortfall, show p.rejected, show p.lapsed, quantity p.consumed, quantity p.decayed ]) r.records
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
    , series: map (\r -> { name: r.id, points: mapWithIndex (\i w -> { round: i, value: exact (priceAt w) }) ([ r.manifest.initial ] <> r.snapshots) }) runs
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
