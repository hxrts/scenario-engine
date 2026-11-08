-- | Meridian's setting parameters (docs/05_test_instance.md §11): the named values the Policies
-- | read and the setting's overrides set. A regional supplier's parameters are built by one
-- | function from its Market's suffix, which returns both the names and the entries, so a name is
-- | spelled once. `Settings.Meridian` lists them in `game-rules.json`.
module Settings.Meridian.Parameters
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
import Foreign.Object as Object
import Settings.Kit (Supplier, supplier)
import Schema.Build.Base ((:=))

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

-- | Every setting parameter with its value.
parameters :: Object.Object Q
parameters = Object.fromFoldable
  ( [ euMatchingTariff := "0.5"
    , majorActionThreshold := 12
    , supplierHighPrice := "2.6"
    , disruptionRound := 3
    ]
      <> supplierAtlantic.entries
      <> supplierAsia.entries
  )
