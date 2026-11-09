-- | Clearing (docs/02_kernel.md §5.5): one Exchange's batch at one uniform price. An Exchange pays its
-- | sellers in quote from its reserves. If the batch would overdraw them, the sell side is removed and
-- | the batch clears on its buys alone, which pay out nothing.
module Core.Kernel.Clear
  ( Cleared
  , Order
  , clearSolvent
  ) where

import Prelude

import Core.Kernel.Route (bounds)
import Core.Exchange as Exchange
import Core.Num (Q)
import Core.Num as Q
import Core.Map (lookupQ)
import Core.Types (Exchange, Price, Quantity)
import Data.Array (filter)
import Data.Maybe (Maybe)

type Order r = { quantity :: Quantity, limitPrice :: Maybe Price | r }

type Cleared r =
  { price :: Price
  , net :: Q
  , fills :: Array { order :: Order r, quantity :: Quantity }
  , rejected :: Array (Order r)
  , unpaid :: Array (Order r)
  }

-- | Clear an Exchange's batch, given in canonical order.
clearSolvent :: forall r. Exchange -> Array (Order r) -> Cleared r
clearSolvent m orders =
  { price: result.price, net: result.net, fills: result.fills, rejected: result.rejected, unpaid }
  where
  limits = bounds m
  first = Exchange.clear m.cost m.position limits orders
  quoteIn = Q.sumQ (map (\f -> Q.mul first.price f.quantity) first.fills)
  solvent = Q.add (lookupQ m.quote m.reserves) quoteIn >= Q.zero
  result = if solvent then first else Exchange.clear m.cost m.position limits (filter (\o -> o.quantity > Q.zero) orders)
  unpaid = if solvent then [] else filter (\o -> o.quantity < Q.zero) orders
