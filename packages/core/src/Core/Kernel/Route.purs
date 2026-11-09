-- | Best execution (docs/02_type_system.md §5.2): plans how each routed order is split across the Markets it
-- | may use. Planned slices are ordinary orders, which each Market then clears as usual, so this module
-- | decides only how much of each order each Market receives. Orders are planned in canonical order and
-- | candidates in merit order, ties broken by MarketId, so the result is deterministic.
module Core.Kernel.Route
  ( Slice
  , Routed
  , bounds
  , plan
  ) where

import Prelude

import Core.Map (sumInto, lookupQ)

import Core.Market (averagePrice, marketPrice)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, Market, MarketId, Price)
import Data.Array (drop, mapMaybe, snoc, sortWith, zipWith)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

-- | A routed order after funding: its signed quantity (+ buys, − sells), its
-- | limit, and the Markets it may use in MarketId order.
type Routed pid =
  { entity :: EntityId
  , pid :: pid
  , index :: Int
  , quantity :: Q
  , limitPrice :: Price
  , candidates :: Array MarketId
  }

-- | One Market's share of a routed order, in the shape of an explicit Market order.
type Slice pid =
  { market :: MarketId
  , entity :: EntityId
  , pid :: pid
  , position :: Int
  , quantity :: Q
  , limitPrice :: Maybe Price
  }

-- | A Market's bounds on the net change of its position in one batch: its cost domain and its
-- | base reserves (§5.5).
bounds :: Market -> { minNet :: Q, maxNet :: Q }
bounds m =
  { minNet: Q.sub m.cost.domain.lo m.position
  , maxNet: min (Q.sub m.cost.domain.hi m.position) (lookupQ m.base m.reserves)
  }

-- | The slices of every routed order. `explicit` is each Market's net change from its explicit
-- | orders, which the routed slices are added to. Routed orders are planned in canonical order
-- | (Entity, source, position), so the plan does not depend on submission order.
plan :: forall pid. Ord pid => Map MarketId Market -> Map MarketId Q -> Array (Routed pid) -> Array (Slice pid)
plan markets explicit routed = (foldl (route markets) { net: explicit, slices: [] } (sortWith key routed)).slices
  where
  key r = Tuple r.entity (Tuple r.pid r.index)

type State pid = { net :: Map MarketId Q, slices :: Array (Slice pid) }

-- | Walk one routed order's candidates in merit order: the lowest opening marginal price first for
-- | a buy, the highest for a sell, ties by MarketId. Each Market takes as much as its batch price
-- | allows, up to the next candidate's opening price (or the limit, for the last candidate); the
-- | remainder goes on to the next Market.
route :: forall pid. Map MarketId Market -> State pid -> Routed pid -> State pid
route markets st r = { net: done.net, slices: done.slices }
  where
  done = foldl visit { net: st.net, slices: st.slices, left: Q.abs r.quantity } (zipWith Tuple ranked nexts)
  buy = r.quantity > Q.zero
  candidates = mapMaybe (\id -> (\m -> { id, market: m, price: marketPrice m }) <$> Map.lookup id markets) r.candidates
  ranked = sortWith (\c -> Tuple (if buy then c.price else Q.neg c.price) c.id) candidates
  nexts = map Just (drop 1 ranked) <> [ Nothing ]
  visit acc (Tuple c next) =
    let
      cap = case next of
        Nothing -> r.limitPrice
        Just n
          | buy -> min r.limitPrice n.price
          | otherwise -> max r.limitPrice n.price
      n0 = lookupQ c.id acc.net
      x = fillOf c.market n0 buy cap acc.left
    in
      if x == Q.zero then acc
      else
        let
          signed = if buy then x else Q.neg x
        in
          { net: sumInto c.id signed acc.net
          , slices: snoc acc.slices { market: c.id, entity: r.entity, pid: r.pid, position: r.index, quantity: signed, limitPrice: Just r.limitPrice }
          , left: Q.sub acc.left x
          }

-- | The largest quantity, on the 0.0001 grid, that a buy (or a sell) can add to a Market whose
-- | wanted net change is `n`, without the Market's batch price (`averagePrice`, the rule
-- | `Core.Market.clear` uses) passing `cap`, within the Market's bounds and `left`. The price is a
-- | monotone function of the net, so bisection finds the largest quantity that fits.
fillOf :: Market -> Q -> Boolean -> Price -> Q -> Q
fillOf m n buy cap left =
  let
    b = bounds m
    room = if buy then Q.sub b.maxNet n else Q.sub n b.minNet
    top = min left room
    fits x =
      let
        p = averagePrice m.cost m.position (if buy then Q.add n x else Q.sub n x)
      in
        if buy then p <= cap else p >= cap
    ulp = Q.ulp
    -- `lo` fits and `up` does not
    search lo up
      | Q.sub up lo <= ulp = lo
      | otherwise =
          let
            mid = fromMaybe lo (Q.divFloor (Q.add lo up) (Q.fromInt 2))
          in
            if fits mid then search mid up else search lo mid
  in
    if top <= Q.zero then Q.zero
    else if fits top then top
    else if not (fits Q.zero) then Q.zero
    else search Q.zero top
