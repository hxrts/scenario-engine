-- | Meridian's recurring flows.
module Settings.Meridian.Circulation
  ( productionCapacity
  , flows
  , shares
  , requirements
  , circulation
  ) where

import Prelude (map)
import Settings.Meridian.Names (n)
import Settings.Meridian.Holdings (capacityHolders)
import Schema.Build.Base (payee, sales, (:=))
import Schema.Build.Quantity (q)
import Settings.Kit.Seats (flowsOf, requirementsOf, (~>))
import Core.Types (Id(..))
import Schema.Recurring (Circulation, Flow, Production, Requirement, Share)

-- | The Entities that produce Energy: every Entity that holds an energy-production capacity
-- | (Meridian.Holdings), in catalog order.
productionCapacity :: Array Production
productionCapacity = map (\entity -> { entity: Id entity, resource: Id n.energy, capacity: Id n.energyProduction }) (capacityHolders n.energyProduction)

flows :: Array Flow
flows = flowsOf (Id n.money)
  [ n.usWorkers ~> n.us := 15
  , n.globalSmes ~> n.us := 5
  , n.cnPrivate ~> n.cn := 20
  , n.eu ~> n.de := 10
  , n.worldHouseholds ~> n.india := 10
  , n.worldHouseholds ~> n.jp := 10
  , n.worldHouseholds ~> n.uk := 8
  , n.worldHouseholds ~> n.br := 6
  , n.worldHouseholds ~> n.ng := 4
  , n.us ~> n.usWorkers := 12
  , n.cn ~> n.cnPrivate := 18
  , n.de ~> n.eu := 8
  , n.india ~> n.worldHouseholds := 8
  , n.jp ~> n.worldHouseholds := 8
  , n.uk ~> n.worldHouseholds := 6
  , n.br ~> n.worldHouseholds := 5
  , n.ng ~> n.worldHouseholds := 3
  , n.enco ~> n.usWorkers := 6
  , n.def ~> n.usWorkers := 6
  , n.chip ~> n.worldHouseholds := 6
  , n.ship ~> n.worldHouseholds := 5
  , n.globalSmes ~> n.worldHouseholds := 10
  , n.globalSmes ~> n.usWorkers := 6
  , n.usWorkers ~> n.globalSmes := 12
  , n.worldHouseholds ~> n.globalSmes := 12
  , n.usWorkers ~> n.sme := 3
  , n.worldHouseholds ~> n.ship := 4
  , n.usWorkers ~> n.labor := 3
  , n.eu ~> n.aid := 3
  , n.def ~> n.chip := 6
  ]

shares :: Array Share
shares =
  [ { from: Id n.energySuppliers, to: Id n.worldHouseholds, share: q "0.5", source: sales n.energy }
  , { from: Id n.def, to: Id n.usWorkers, share: q "0.25", source: payee }
  ]

requirements :: Array Requirement
requirements = requirementsOf { resource: Id n.energy, penalty: q 2 }
  [ n.us := 10
  , n.cn := 10
  , n.de := 8
  , n.india := 8
  , n.jp := 8
  , n.eu := 8
  , n.uk := 6
  , n.chip := 6
  , n.ship := 4
  ]
  [ n.cnPrivate
  , n.usWorkers
  , n.worldHouseholds
  , n.globalSmes
  ]

circulation :: Circulation
circulation =
  { production: productionCapacity
  , flows: flows
  , shares: shares
  , requirements: requirements
  }
