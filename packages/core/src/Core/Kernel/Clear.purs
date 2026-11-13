-- | Clearing (docs/02_kernel.md §5.5): one Exchange's batch at one uniform price, and the pairs it settles as.
-- | An Exchange pays its sellers in quote from its reserves. If the batch would overdraw them, the sell side is
-- | removed and the batch clears on its buys alone, which pay out nothing.
-- |
-- | A cleared batch settles as pairs: each pair is a quantity of the base moving from one side to the other at
-- | the batch price. A side is an order or the Exchange's reserve, which takes the batch's net: it sells what
-- | the buyers take beyond the sellers, and buys what the sellers give beyond the buyers. An Exchange with no
-- | reserves and a domain that starts at its position has a net of zero, so its pairs are only between orders.
module Core.Kernel.Clear
  ( Cleared
  , Order
  , Side(..)
  , Pair
  , clearSolvent
  ) where

import Prelude

import Core.Kernel.Route (bounds)
import Core.Exchange as Exchange
import Core.Num (Q)
import Core.Num as Q
import Core.Map (lookupQ)
import Core.Types (Exchange, Price, Quantity)
import Data.Array (filter, foldl, snoc, zip, zipWith)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

type Order r = { quantity :: Quantity, limitPrice :: Maybe Price | r }

-- | One side of a pair: an order of the batch, or the Exchange's reserve.
data Side r = Party (Order r) | Reserve

-- | A quantity of the base moving from the seller to the buyer at the batch price.
type Pair r = { buyer :: Side r, seller :: Side r, quantity :: Quantity }

type Cleared r =
  { price :: Price
  , net :: Q
  , fills :: Array { order :: Order r, quantity :: Quantity }
  , pairs :: Array (Pair r)
  , rejected :: Array (Order r)
  , unpaid :: Array (Order r)
  -- what the batch's orders asked to buy and to sell, before any limit or reserve: the gap a price adjusts by
  , demand :: Quantity
  , supply :: Quantity
  }

-- | Clear an Exchange's batch, given in canonical order.
clearSolvent :: forall r. Exchange -> Array (Order r) -> Cleared r
clearSolvent m orders =
  { price: result.price, net: result.net, fills: result.fills, pairs: pairsOf result.net result.fills, rejected: result.rejected, unpaid, demand: Q.sumQ (map _.quantity (filter (\o -> o.quantity > Q.zero) orders)), supply: Q.neg (Q.sumQ (map _.quantity (filter (\o -> o.quantity < Q.zero) orders))) }
  where
  limits = bounds m
  first = Exchange.clear m.cost m.position limits orders
  quoteIn = Q.sumQ (map (\f -> Q.mul first.price f.quantity) first.fills)
  solvent = Q.add (lookupQ m.quote m.reserves) quoteIn >= Q.zero
  result = if solvent then first else Exchange.clear m.cost m.position limits (filter (\o -> o.quantity > Q.zero) orders)
  unpaid = if solvent then [] else filter (\o -> o.quantity < Q.zero) orders

-- | The pairs of a cleared batch. The buyers (the buy fills, then the reserve when the net is a sale to the
-- | batch) take from the sellers (the sell fills, then the reserve when the net is a purchase from it), in
-- | canonical order: each buyer takes pro rata from what each seller has left, in whole grid units by largest
-- | remainder, and the last takes exactly what is left. So every fill is the sum of its pairs, and the pairs do
-- | not depend on the order the batch was submitted in.
pairsOf :: forall r. Q -> Array { order :: Order r, quantity :: Quantity } -> Array (Pair r)
pairsOf net fills = (foldl take { left: map _.quantity sellers, pairs: [] } buyers).pairs
  where
  buyers = map (\f -> { side: Party f.order, quantity: f.quantity }) (filter (\f -> f.quantity > Q.zero) fills) <> reserveIf (net < Q.zero) (Q.neg net)
  sellers = map (\f -> { side: Party f.order, quantity: Q.neg f.quantity }) (filter (\f -> f.quantity < Q.zero) fills) <> reserveIf (net > Q.zero) net
  reserveIf yes q = if yes then [ { side: Reserve, quantity: q } ] else []
  take acc b =
    let
      shares = if b.quantity == Q.sumQ acc.left then acc.left else Q.proRata b.quantity acc.left
    in
      { left: zipWith Q.sub acc.left shares
      , pairs: foldl (\ps (Tuple s share) -> if share > Q.zero then snoc ps { buyer: b.side, seller: s.side, quantity: share } else ps) acc.pairs (zip sellers shares)
      }
