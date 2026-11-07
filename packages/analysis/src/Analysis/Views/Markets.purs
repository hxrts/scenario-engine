-- | The market and economy views (docs/06_simulator.md §9.2): Market prices, reserves and traded
-- | volume, facility orders, the world economy's totals and Money circulation, the per-round
-- | pressure indicators, and a Market's price trajectory across runs. Pure.
module Analysis.Views.Markets
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
import Core.Query (lookupQ, priceOf, totalHolding)
import Core.Types (Id, MarketTag, ResourceId)
import Core.World (Event(..), RoundRecord, WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable, index, length, mapMaybe, mapWithIndex)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Analysis.Views.Actions (rejectionCount)
import Analysis.Views.Common (Chart, StoredRun, Table, exact, kindOf, price, quantity, series)
import Analysis.Views.Drivers (shortfallTotal)

-- | A Market's price after a World State, as the views show it.
type MarketPrice = { market :: String, price :: String }

marketPrices :: WorldState -> Array MarketPrice
marketPrices w = map (\(Tuple id m) -> { market: show id, price: price (marginal m.cost m.position) }) (Map.toUnfoldable w.markets)

markets :: StoredRun -> Array Chart
markets r =
  [ { title: "Marginal price", unit: "M/E", kind: Price, series: series r "price" (\w -> map (\(Tuple id m) -> Tuple (show id) (marginal m.cost m.position)) (Map.toUnfoldable w.markets)) exact }
  , { title: "Energy reserves", unit: "E", kind: Quantity, series: series r "reserves" (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.markets)) exact }
  , { title: "Traded volume", unit: "E", kind: Quantity, series: volume }
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
                        ( concatMap
                            ( case _ of
                                Traded t | t.market == id && t.quantity > Q.zero -> [ t.quantity ]
                                _ -> []
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
      ( \rec -> concatMap
          ( case _ of
              Transformed t -> [ [ show rec.round, show t.transformation, show t.by, kindOf r.manifest.initial t.by, quantity t.quantity, quantity t.paid ] ]
              _ -> []
          )
          rec.events
      )
      r.records
  }

economy :: StoredRun -> { totals :: Chart, flows :: Table, pressure :: Table }
economy r = { totals: resourceTotals r, flows: moneyFlows r, pressure: pressure r }

resourceTotals :: StoredRun -> Chart
resourceTotals r =
  { title: "Resource totals across all accounts"
  , unit: ""
  , kind: Quantity
  , series: series r "totals" (\w -> map (\res -> Tuple (show res) (totalHolding w res)) (fromFoldable (Map.keys w.resources))) exact
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
  , consumed: sum' consumedOf
  , decayed: sum' decayedOf
  , rejected: rejectionCount rec
  , lapsed: count isLapsed
  }
  where
  sum' f = Q.sumQ (concatMap f rec.events)
  count f = length (filter f rec.events)
  consumedOf = case _ of
    Consumed c -> [ c.quantity ]
    _ -> []
  decayedOf = case _ of
    Decayed d -> [ d.quantity ]
    _ -> []
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

-- | One market's price across runs, aligned by round. The unit is the Market's quote per base unit,
-- | read from the Resources' units in the run's manifest. A run without the Market is an error.
trajectories :: Array StoredRun -> String -> Either String Chart
trajectories runs market = do
  for_ runs \r -> maybe (Left ("market " <> market <> " is not in run " <> r.id)) Right (Map.lookup mid r.manifest.initial.markets)
  unit <- maybe (Right "") (marketUnit market) (index runs 0)
  pure
    { title: "Price, " <> market
    , unit
    , kind: Price
    , series: map (\r -> { name: r.id, points: mapWithIndex (\i w -> { round: i, value: exact (priceAt w) }) ([ r.manifest.initial ] <> r.snapshots) }) runs
    }
  where
  mid :: Id MarketTag
  mid = wrap market

  priceAt w = fromMaybe Q.zero (priceOf w mid)

-- | Every Market the first run has, as one price chart per Market over the runs that have it, so the
-- | runs are compared on the same axes. A Market a run lacks leaves that run out of its chart.
marketCharts :: Array StoredRun -> Array Chart
marketCharts runs = case index runs 0 of
  Nothing -> []
  Just base -> mapMaybe (\m -> hush (trajectories (filter (hasMarket m) runs) m)) (map show (fromFoldable (Map.keys base.manifest.initial.markets) :: Array (Id MarketTag)))
  where
  hasMarket m r = Map.member (wrap m) r.manifest.initial.markets

-- | A Market's unit, the quote per base unit, from the run's Resources.
marketUnit :: String -> StoredRun -> Either String String
marketUnit market r = do
  m <- maybe (Left ("market " <> market <> " is not in run " <> r.id)) Right (Map.lookup mid r.manifest.initial.markets)
  base <- resourceUnit m.base
  quoteUnit <- resourceUnit m.quote
  pure (quoteUnit <> "/" <> base)
  where
  mid :: Id MarketTag
  mid = wrap market

  resourceUnit :: ResourceId -> Either String String
  resourceUnit res = maybe (Left ("resource " <> show res <> " is not in run " <> r.id)) (Right <<< _.unit) (Map.lookup res r.manifest.initial.resources)
