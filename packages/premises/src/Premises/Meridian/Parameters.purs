-- | Meridian's premise parameters (docs/05_test_instance.md §11): the named values the Policies
-- | read and the premise's overrides set. A regional supplier's parameters are built by one
-- | function from its Market's suffix, which returns both the names and the entries, so a name is
-- | spelled once. `Premises.Meridian` lists them in `game-rules.json`.
module Premises.Meridian.Parameters
  ( supplierAtlantic
  , supplierAsia
  , majorActionThreshold
  , euMatchingTariff
  , supplierHighPrice
  , disruptionRound
  , parameters
  ) where

import Prelude

import Core.Num (Q)
import Schema.Build.Base (Keyed, keyed, (:=))
import Data.Tuple (Tuple)

-- | The names of one regional Market's supplier parameters: the offer below the high price, the
-- | offer at or above it, and the disruption cut.
type SupplierNames = { offer :: String, high :: String, cut :: String }

-- | A regional Market's supplier parameters: their names, and their entries with the values given.
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

supplierAtlantic :: Supplier
supplierAtlantic = supplier "atlantic" { offer: 24, high: 30, cut: 12 }

supplierAsia :: Supplier
supplierAsia = supplier "asia" { offer: 16, high: 20, cut: 8 }

-- | The major-action threshold: the units of a major military action that trigger China's response.
majorActionThreshold :: String
majorActionThreshold = "major-action-threshold"

-- | The EU's matching tariff on a rival's restriction.
euMatchingTariff :: String
euMatchingTariff = "eu-matching-tariff"

-- | The Energy price at which suppliers switch from their base offer to their high offer.
supplierHighPrice :: String
supplierHighPrice = "supplier-high-price"

-- | The round of the logistics disruption (docs/05 §13.6).
disruptionRound :: String
disruptionRound = "disruption-round"

-- | Every premise parameter with its value.
parameters :: Keyed Q
parameters = keyed
  ( [ euMatchingTariff := "0.5"
    , majorActionThreshold := 12
    , supplierHighPrice := "2.6"
    , disruptionRound := 3
    ]
      <> supplierAtlantic.entries
      <> supplierAsia.entries
  )
