-- | Player standings (docs/06_simulator.md §9.2): each Actor's net worth in M at each round's
-- | marketable prices, its change from the start, its rank, and the net-worth chart. This module is
-- | the one definition of net worth; the balance measures and the seed variation read it. Pure.
module Views.Standings
  ( netWorthStat
  , rankStat
  , netWorth
  , netWorths
  , Standing
  , standingsOf
  , standings
  , netWorthChart
  ) where

import Prelude

import Core.Market (marginal)
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (EntityId)
import Core.World (WorldState)
import Data.Array (filter, fromFoldable, length, nub, sortBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Views.Common (Chart, Statistic, StoredRun, Table, exact, final, players, quantity, series, unitOf)

netWorthStat :: Statistic
netWorthStat =
  { name: "Net worth"
  , question: "How well are players doing relative to one another?"
  , definition: "Money plus each held base Resource valued at its Market's marginal price, in M."
  , formula: "NW(e, r) = holding(e, M) + sum over Resources b traded in a Market quoted in M of holding(e, b) × P(b, r), where P(b, r) is the mean of the marginal prices, marginal(cost, position) after round r, of the M-quoted Markets whose base is b. Military is valued at the defence Market; Soft Power and Resources with no Market are not counted. Actors only."
  }

rankStat :: Statistic
rankStat =
  { name: "Rank"
  , question: "How well are players doing relative to one another?"
  , definition: "Position by final net worth, 1 the highest; players with equal net worth share a rank."
  , formula: "rank(e) = 1 + the number of Actors whose final net worth is strictly higher than NW(e)."
  }

-- | An Actor's net worth in a World State, in M: its Money, and each Resource that a Market quotes in
-- | the numeraire valued at that Resource's price. Where several Markets trade one Resource (the
-- | regional Energy Markets), the price is the mean of their marginal prices, so a holding counts once.
netWorth :: WorldState -> EntityId -> Q
netWorth w e = Q.sumQ ([ holding w e w.numeraire ] <> map valued bases)
  where
  priced = filter (\m -> m.quote == w.numeraire) (fromFoldable (Map.values w.markets))
  bases = nub (map _.base priced)
  valued b = Q.mul (holding w e b) (priceOf b)
  priceOf b =
    let
      prices = map (\m -> marginal m.cost m.position) (filter (\m -> m.base == b) priced)
    in
      Q.div (Q.sumQ prices) (Q.fromInt (length prices))

-- | Every Actor's net worth in a World State, keyed by Actor, in Entity order.
netWorths :: WorldState -> Map EntityId Q
netWorths w = Map.fromFoldable (map (\e -> Tuple e (netWorth w e)) (players w))

-- | One Actor's standing after the final round: its net worth, its change from the start of the run,
-- | and its rank.
type Standing = { player :: EntityId, netWorth :: Q, change :: Q, rank :: Int }

-- | The standings after the final round, highest net worth first.
standingsOf :: StoredRun -> Array Standing
standingsOf r = map standing ranked
  where
  start = netWorths r.manifest.initial
  finals = netWorths (final r)
  ranked = sortBy (\(Tuple _ a) (Tuple _ b) -> compare b a) (Map.toUnfoldable finals :: Array (Tuple EntityId Q))
  rankOf v = 1 + length (filter (\(Tuple _ u) -> u > v) ranked)
  standing (Tuple e v) = { player: e, netWorth: v, change: Q.sub v (fromMaybe Q.zero (Map.lookup e start)), rank: rankOf v }

-- | The standings table: the net worth, its change from the start, and the rank of each Actor.
standings :: StoredRun -> Table
standings r =
  { title: "Standings after round " <> show (length r.records)
  , columns: [ "Player", "Net worth (M)", "Change from start (M)", "Rank" ]
  , rows: map (\s -> [ show s.player, quantity s.netWorth, quantity s.change, show s.rank ]) (standingsOf r)
  }

-- | Every Actor's net worth over the run, one line per Actor, from round 0 (the start) to the final round.
netWorthChart :: StoredRun -> Chart
netWorthChart r =
  { title: "Net worth by player"
  , unit: unitOf r r.manifest.initial.numeraire
  , kind: Quantity
  , series: series r (\w -> map (\(Tuple e v) -> Tuple (show e) v) (Map.toUnfoldable (netWorths w))) exact
  }
