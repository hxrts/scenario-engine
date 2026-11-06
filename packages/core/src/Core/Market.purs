-- | Cost functions and uniform batch clearing (docs/02_type_system.md §5.1, §5.5).
module Core.Market
  ( marginal
  , cost
  , averagePrice
  , Order
  , ClearResult
  , clear
  ) where

import Prelude

import Core.Num as Q
import Core.Types (CostFunction, Price, Quantity)
import Data.Array (filter, null, partition, zipWith)
import Data.Maybe (Maybe, maybe)

-- | p(q) = base + slope·q
marginal :: CostFunction -> Quantity -> Price
marginal c q = Q.add c.base (Q.mul c.slope q)

-- | C(q) = base·q + slope·q²/2
cost :: CostFunction -> Quantity -> Quantity
cost c q = Q.add (Q.mul c.base q) (Q.div (Q.mul c.slope (Q.mul q q)) (Q.fromInt 2))

-- | Average price of moving the position from `x0` by `d`; the marginal price when `d` is zero.
averagePrice :: CostFunction -> Quantity -> Quantity -> Price
averagePrice c x0 d
  | d == Q.zero = marginal c x0
  | otherwise = Q.div (Q.sub (cost c (Q.add x0 d)) (cost c x0)) d

type Order r = { quantity :: Quantity, limitPrice :: Maybe Price | r }

type ClearResult r =
  { price :: Price
  , net :: Quantity
  , fills :: Array { order :: Order r, quantity :: Quantity }
  , rejected :: Array (Order r)
  }

-- | Clear one Market's batch at a single uniform price. Buys are positive, sells negative.
-- | `minNet` and `maxNet` bound the net position change (domain and reserves). Orders whose
-- | limit is violated at the computed price are removed and the price recomputed; removed
-- | orders stay removed, so the result depends only on the set of orders. Orders must be in
-- | canonical order.
clear :: forall r. CostFunction -> Quantity -> { minNet :: Quantity, maxNet :: Quantity } -> Array (Order r) -> ClearResult r
clear c x0 bounds = go []
  where
  go removed orders =
    let
      result = settle orders
      ok o = maybe true (\l -> if o.quantity > Q.zero then result.price <= l else result.price >= l) o.limitPrice
      { yes: keep, no: violated } = partition ok orders
    in
      if null violated then result { rejected = removed }
      else go (removed <> violated) keep

  settle orders =
    let
      buys = filter (\o -> o.quantity > Q.zero) orders
      sells = filter (\o -> o.quantity < Q.zero) orders
      b = Q.sumQ (map _.quantity buys)
      s = Q.sub Q.zero (Q.sumQ (map _.quantity sells))
      wanted = Q.sub b s
      net = max bounds.minNet (min bounds.maxNet wanted)
      -- the binding side is filled pro rata
      buyFills = if net == wanted then map _.quantity buys else Q.proRata (Q.add s net) (map _.quantity buys)
      sellFills =
        if net == wanted then map _.quantity sells
        else map (Q.sub Q.zero) (Q.proRata (Q.sub b net) (map (Q.sub Q.zero <<< _.quantity) sells))
      pair os qs = filter ((_ /= Q.zero) <<< _.quantity) (zipWith (\order quantity -> { order, quantity }) os qs)
    in
      { price: averagePrice c x0 net, net, fills: pair buys buyFills <> pair sells sellFills, rejected: [] }
