-- | The Intent builders, one per kind (docs/02_type_system.md §3). `trade`, `route`, `transfer`,
-- | `enactWithholding` and `repeal` are polymorphic in their filling: a fixture's Intent takes `Q`
-- | quantities, a Strategy's template takes `Expr` and is lifted with `Schema.Build.Decision.asTemplate`.
-- | `transform`, `enactTariff` and `enactAccessBan` name their actor, which the premise format of a
-- | template cannot write, so they take `Q` only; a Strategy uses `Decision.selfTransform`,
-- | `selfTariff` and `selfAccessBan` (a template naming another actor is a compile error, not a crash).
module Schema.Build.Intent
  ( trade
  , route
  , transfer
  , transform
  , enactTariff
  , enactAccessBan
  , enactWithholding
  , repeal
  ) where

import Core.Num (Q)
import Core.Types (Id(..), Intent, IntentF(..), RuleChangeF(..), RuleF(..))
import Data.Maybe (Maybe)

-- | An order on a Market; `limit` is the optional limit price.
trade :: forall v. { market :: String, quantity :: v, limit :: Maybe v } -> IntentF v
trade r = Trade { market: Id r.market, quantity: r.quantity, limitPrice: r.limit }

-- | A routed order: no Market is named, so `base` and `quote` give the pair.
route :: forall v. { base :: String, quote :: String, quantity :: v, limit :: Maybe v } -> IntentF v
route r = Route { pair: { base: Id r.base, quote: Id r.quote }, quantity: r.quantity, limitPrice: r.limit }

-- | The `from` Entity gives `quantity` of `resource` to `to`.
transfer :: forall v. { from :: String, to :: String, resource :: String, quantity :: v } -> IntentF v
transfer r = Transfer { from: Id r.from, to: Id r.to, resource: Id r.resource, quantity: r.quantity }

transform :: { by :: String, transformation :: String, quantity :: Q } -> Intent
transform r = Transform { by: Id r.by, transformation: Id r.transformation, quantity: r.quantity }

enactTariff :: { jurisdiction :: String, origin :: String, rate :: Q } -> Intent
enactTariff r = ChangeRule (Enact (Tariff { jurisdiction: Id r.jurisdiction, origin: Id r.origin, rate: r.rate }))

enactAccessBan :: { jurisdiction :: String, target :: String } -> Intent
enactAccessBan r = ChangeRule (Enact (AccessBan { jurisdiction: Id r.jurisdiction, target: Id r.target }))

enactWithholding :: forall v. { capacity :: String, units :: v } -> IntentF v
enactWithholding r = ChangeRule (Enact (Withholding { capacity: Id r.capacity, units: r.units }))

-- | Repeal a Rule by the id the engine assigned it.
repeal :: forall v. { rule :: String } -> IntentF v
repeal r = ChangeRule (Repeal (Id r.rule))
