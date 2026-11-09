-- | Clearing (docs/02_type_system.md §5.5): one Market's batch at one uniform price. A Market pays its
-- | sellers in quote from its reserves. If the batch would overdraw them, the sell side is removed and
-- | the batch clears on its buys alone, which pay out nothing.
module Core.Kernel.Clear
  ( Cleared
  , Order
  , clearSolvent
  ) where

import Prelude

import Core.Kernel.Route (bounds)
import Core.Market as Market
import Core.Num (Q)
import Core.Num as Q
import Core.Query (lookupQ)
import Core.Types (Market, Price, Quantity)
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

-- | Clear a Market's batch, given in canonical order.
clearSolvent :: forall r. Market -> Array (Order r) -> Cleared r
clearSolvent m orders =
  { price: result.price, net: result.net, fills: result.fills, rejected: result.rejected, unpaid }
  where
  limits = bounds m
  first = Market.clear m.cost m.position limits orders
  quoteIn = Q.sumQ (map (\f -> Q.mul first.price f.quantity) first.fills)
  solvent = Q.add (lookupQ m.quote m.reserves) quoteIn >= Q.zero
  result = if solvent then first else Market.clear m.cost m.position limits (filter (\o -> o.quantity > Q.zero) orders)
  unpaid = if solvent then [] else filter (\o -> o.quantity < Q.zero) orders
