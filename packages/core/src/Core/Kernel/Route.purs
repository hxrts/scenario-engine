-- | Best execution (docs/02_kernel.md §5.2): plans how each routed order is split across the Exchanges it
-- | may use. Planned slices are ordinary orders, which each Exchange then clears as usual, so this module
-- | decides only how much of each order each Exchange receives. Orders are planned in canonical order and
-- | candidates in merit order, ties broken by ExchangeId, so the result is deterministic.
module Core.Kernel.Route
  ( Slice
  , Routed
  , bounds
  , plan
  ) where

import Prelude

import Core.Map (sumInto, lookupQ)

import Core.Exchange (averagePrice, exchangePrice)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (ActorId, Exchange, ExchangeId, Price)
import Data.Array (drop, mapMaybe, snoc, sortWith, zipWith)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

-- | A routed order after funding: its signed quantity (+ buys, − sells), its
-- | limit, and the Exchanges it may use in ExchangeId order.
type Routed pid =
  { actor :: ActorId
  , pid :: pid
  , index :: Int
  , quantity :: Q
  , limitPrice :: Price
  , candidates :: Array ExchangeId
  }

-- | One Exchange's share of a routed order, in the shape of an explicit Exchange order.
type Slice pid =
  { exchange :: ExchangeId
  , actor :: ActorId
  , pid :: pid
  , position :: Int
  , quantity :: Q
  , limitPrice :: Maybe Price
  }

-- | An Exchange's bounds on the net change of its position in one batch: its cost domain and its
-- | base reserves (§5.5).
bounds :: Exchange -> { minNet :: Q, maxNet :: Q }
bounds m =
  { minNet: Q.sub m.cost.domain.lo m.position
  , maxNet: min (Q.sub m.cost.domain.hi m.position) (lookupQ m.base m.reserves)
  }

-- | The slices of every routed order. `explicit` is each Exchange's net change from its explicit
-- | orders, which the routed slices are added to. Routed orders are planned in canonical order
-- | (Actor, source, position), so the plan does not depend on submission order.
plan :: forall pid. Ord pid => Map ExchangeId Exchange -> Map ExchangeId Q -> Array (Routed pid) -> Array (Slice pid)
plan exchanges explicit routed = (foldl (route exchanges) { net: explicit, slices: [] } (sortWith key routed)).slices
  where
  key r = Tuple r.actor (Tuple r.pid r.index)

type State pid = { net :: Map ExchangeId Q, slices :: Array (Slice pid) }

-- | Walk one routed order's candidates in merit order: the lowest opening marginal price first for
-- | a buy, the highest for a sell, ties by ExchangeId. Each Exchange takes as much as its batch price
-- | allows, up to the next candidate's opening price (or the limit, for the last candidate); the
-- | remainder goes on to the next Exchange.
route :: forall pid. Map ExchangeId Exchange -> State pid -> Routed pid -> State pid
route exchanges st r = { net: done.net, slices: done.slices }
  where
  done = foldl visit { net: st.net, slices: st.slices, left: Q.abs r.quantity } (zipWith Tuple ranked nexts)
  buy = r.quantity > Q.zero
  candidates = mapMaybe (\id -> (\m -> { id, exchange: m, price: exchangePrice m }) <$> Map.lookup id exchanges) r.candidates
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
      x = fillOf c.exchange n0 buy cap acc.left
    in
      if x == Q.zero then acc
      else
        let
          signed = if buy then x else Q.neg x
        in
          { net: sumInto c.id signed acc.net
          , slices: snoc acc.slices { exchange: c.id, actor: r.actor, pid: r.pid, position: r.index, quantity: signed, limitPrice: Just r.limitPrice }
          , left: Q.sub acc.left x
          }

-- | The largest quantity, on the 0.0001 grid, that a buy (or a sell) can add to an Exchange whose
-- | wanted net change is `n`, without the Exchange's batch price (`averagePrice`, the rule
-- | `Core.Exchange.clear` uses) passing `cap`, within the Exchange's bounds and `left`. The price is a
-- | monotone function of the net, so bisection finds the largest quantity that fits.
fillOf :: Exchange -> Q -> Boolean -> Price -> Q -> Q
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
