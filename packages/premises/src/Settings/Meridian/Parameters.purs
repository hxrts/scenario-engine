-- | Meridian's setting parameters (docs/05_test_instance.md §11): the named values the Policies
-- | read and the setting's overrides set. A regional supplier's parameters are built by one
-- | function from its Market's suffix, which returns both the names and the entries, so a name is
-- | spelled once. `Settings.Meridian` lists them in `game-rules.json`.
module Settings.Meridian.Parameters
  ( smeSample
  , schemes
  , majorShock
  , supplierAtlantic
  , supplierAsia
  , majorActionThreshold
  , euMatchingTariff
  , supplierHighPrice
  , disruptionRound
  , parameters
  ) where

import Prelude (identity, map, negate, not, show, (+), (<>))
import Core.World (FlowGate)
import Settings.Meridian.Names (n, seatActors)
import Core.Num (Q)
import Data.Array (elem, mapWithIndex)
import Data.Either (Either(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Schema.Build.Base (keyed, (:=), payee, sales)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, noFacility, noResource, resourcesOf)
import Settings.Compose (Setting)
import Settings.Kit.Markets (MarketTerms, market)
import Settings.Kit.Compose (Premise, assemble, emptyParts, premise)
import Settings.Kit.Seats (flowsOf, requirementsOf, (~>))
import Settings.Kit.Markets as Kit
import Settings.Kit.Presets (PresetBinding, standardThresholds)
import Schema.Build.Decision (lit)
import Schema.Gameboard (Gameboard, InstantiationConfig, Scheme)
import Core.Draw (Seed(..))
import Core.Types (CostFunction, EntityId, Estimator, Holdings, Id(..), Market, MarketId, Payment, ResourceDefinition, ResourceId, Transferability(..), Transformation, TransformationId)
import Data.Map (Map)
import Schema.Recurring (Circulation, Decay, Flow, Production, Requirement, Share, compileIn)
import Prelude

import Foreign.Object as Object
import Settings.Kit.Strategies (Supplier, supplier)

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

-- | The SME Representation (docs/05 §12): Global SMEs' Energy demand, estimated from SME's
-- | sample. Its scheme and the fixtures' Representations share these values.
smeSample
  :: { id :: String
     , actor :: String
     , aggregate :: String
     , domain :: String
     , estimator :: Q -> Estimator
     }
smeSample =
  { id: "sme-energy-sample"
  , actor: n.sme
  , aggregate: n.globalSmes
  , domain: n.energy
  , estimator: \noise -> { reference: q 5, shrinkage: q "0.25", deviationBound: Just { lo: q "-0.5", hi: q "0.5" }, noiseSpread: noise }
  }

schemes :: Array Scheme
schemes =
  [ { id: smeSample.id, eligibleActors: [ Id smeSample.actor ], target: Id smeSample.aggregate, domain: Id smeSample.domain, observes: Id smeSample.domain, estimator: smeSample.estimator (q "0.1") }
  ]

-- | The major shock (docs/05 §13.6): a designed test case, not the reference game. Round 1's
-- | Prepare phase removes both regions' supplier offers entirely (a cut at or above the offer
-- | removes it, and never turns it into a purchase). It is the shock that flips the most preset
-- | behaviours without any round of the reference disruption: Accumulator, Trader, Opportunistic and
-- | Deployer change their Atlantic actions as the price crosses 3.0. Run it as overrides of the
-- | reference game; the simulator test "Major shock" pins what it flips.
majorShock :: Array (Tuple String String)
majorShock =
  [ Tuple disruptionRound "1"
  , Tuple supplierAtlantic.names.cut "60"
  , Tuple supplierAsia.names.cut "40"
  ]
