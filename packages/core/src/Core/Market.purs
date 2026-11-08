-- | Cost functions and uniform batch clearing (docs/02_type_system.md §5.1, §5.5).
module Core.Market
  ( marginal
  , marketPrice
  , averagePrice
  , clear
  ) where

import Prelude

import Core.Num as Q
import Core.Types (Market, CostFunction, Price, Quantity)
import Data.Array (filter, null, partition, zipWith)
import Data.Maybe (Maybe, maybe)

-- | p(q) = base + slope·q
marginal :: CostFunction -> Quantity -> Price
marginal c q = Q.add c.base (Q.mul c.slope q)

-- | C(q) = base·q + slope·q²/2
-- | A Market's current marginal price.
marketPrice :: Market -> Price
marketPrice m = marginal m.cost m.position

-- | Average price of moving the position from `x0` by `d`; the marginal price when `d` is zero.
-- | This is (C(x0 + d) − C(x0)) / d, which for a linear marginal price is exactly the marginal
-- | price at the midpoint, base + slope·(2·x0 + d)/2. Computing it that way rounds twice, not
-- | once per integral, so the error stays within one step however small `d` is.
averagePrice :: CostFunction -> Quantity -> Quantity -> Price
averagePrice c x0 d = Q.add c.base (Q.div (Q.mul c.slope (Q.add (Q.mul (Q.fromInt 2) x0) d)) (Q.fromInt 2))

type BatchOrder r = { quantity :: Quantity, limitPrice :: Maybe Price | r }

type ClearResult r =
  { price :: Price
  , net :: Quantity
  , fills :: Array { order :: BatchOrder r, quantity :: Quantity }
  , rejected :: Array (BatchOrder r)
  }

-- | Clear one Market's batch at a single uniform price. Buys are positive, sells negative.
-- | `minNet` and `maxNet` bound the net position change (domain and reserves). Orders whose
-- | limit is violated at the computed price are removed and the price recomputed; removed
-- | orders stay removed, so the result depends only on the set of orders. Orders must be in
-- | canonical order.
clear :: forall r. CostFunction -> Quantity -> { minNet :: Quantity, maxNet :: Quantity } -> Array (BatchOrder r) -> ClearResult r
clear c x0 bounds = go []
  where
  go removed orders =
    let
      result = settle orders
      ok o = o.quantity == Q.zero || maybe true (\l -> if o.quantity > Q.zero then result.price <= l else result.price >= l) o.limitPrice
      { yes: keep, no: violated } = partition ok orders
    in
      if null violated then result { rejected = removed }
      else go (removed <> violated) keep

  settle orders =
    let
      buys = filter (\o -> o.quantity > Q.zero) orders
      sells = filter (\o -> o.quantity < Q.zero) orders
      b = Q.sumQ (map _.quantity buys)
      s = Q.neg (Q.sumQ (map _.quantity sells))
      wanted = Q.sub b s
      net = max bounds.minNet (min bounds.maxNet wanted)
      -- the binding side is rationed pro rata; the other side fills in full. When the net
      -- position is held below the wanted demand, buys are rationed to s + net; when it is
      -- held above, sells are rationed to b - net.
      buyFills = if net < wanted then Q.proRata (Q.add s net) (map _.quantity buys) else map _.quantity buys
      sellFills =
        if net > wanted then map Q.neg (Q.proRata (Q.sub b net) (map (Q.neg <<< _.quantity) sells))
        else map _.quantity sells
      pair os qs = filter ((_ /= Q.zero) <<< _.quantity) (zipWith (\order quantity -> { order, quantity }) os qs)
    in
      { price: averagePrice c x0 net, net, fills: pair buys buyFills <> pair sells sellFills, rejected: [] }
