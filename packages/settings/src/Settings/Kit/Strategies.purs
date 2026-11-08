-- | Strategy pieces a Setting shares.
module Settings.Kit.Strategies
  ( banded
  , SupplierNames
  , Supplier
  , supplier
  , supplierOffers
  ) where

import Prelude
import Core.Num (Q)
import Core.Types (MarketId)
import Core.Strategy (Clause)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Schema.Build.Base ((:=))
import Schema.Build.Decision (eqQ, ge, gt, le, lit, max', named, not', orderAt, param, priceAt, roundQ, rule, subQ)

-- | A price band: trade `below` while the Market's price is at or below `threshold`, and `above`
-- | while it is above.
banded :: MarketId -> Int -> Int -> Int -> Array Clause
banded m threshold below above =
  [ rule [ le (priceAt (named m)) (lit threshold) ] [ orderAt (named m) (lit below) Nothing ]
  , rule [ gt (priceAt (named m)) (lit threshold) ] [ orderAt (named m) (lit above) Nothing ]
  ]

-- | The names of one Market's supplier parameters: the offer below the high price, the offer at or
-- | above it, and the disruption cut.
type SupplierNames = { offer :: String, high :: String, cut :: String }

-- | A Market's supplier parameters: their names, and their entries with the values given.
type Supplier = { names :: SupplierNames, entries :: Array (Tuple String Q) }

-- | The supplier parameters of the Market with this suffix: `supplier-offer-<suffix>`,
-- | `supplier-offer-<suffix>-high`, and `disruption-cut-<suffix>`.
supplier :: String -> { offer :: Int, high :: Int, cut :: Int } -> Supplier
supplier suffix v = { names, entries: [ names.offer := v.offer, names.high := v.high, names.cut := v.cut ] }
  where
  names =
    { offer: "supplier-offer-" <> suffix
    , high: "supplier-offer-" <> suffix <> "-high"
    , cut: "disruption-cut-" <> suffix
    }

-- | Suppliers' offers in one Market. Below the parameter `highPrice` they offer the base volume; at or
-- | above it, the high volume. In the round the parameter `disruptionRound` names, the offer is cut by
-- | the disruption amount and never below zero: a cut larger than the offer removes it, and never
-- | turns it into a purchase.
supplierOffers :: { disruptionRound :: String, highPrice :: String } -> MarketId -> SupplierNames -> Array Clause
supplierOffers ps m names =
  [ rule [ notDisruption, low ] [ offer (subQ (lit 0) (param names.offer)) ]
  , rule [ notDisruption, high ] [ offer (subQ (lit 0) (param names.high)) ]
  , rule [ disruption, low ] [ offer (subQ (lit 0) (max' (lit 0) (subQ (param names.offer) (param names.cut)))) ]
  , rule [ disruption, high ] [ offer (subQ (lit 0) (max' (lit 0) (subQ (param names.high) (param names.cut)))) ]
  ]
  where
  disruption = eqQ roundQ (param ps.disruptionRound)
  notDisruption = not' disruption
  low = not' (ge (priceAt (named m)) (param ps.highPrice))
  high = ge (priceAt (named m)) (param ps.highPrice)
  offer quantity = orderAt (named m) quantity Nothing
