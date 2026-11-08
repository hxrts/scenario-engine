-- | The Intent builders, one per kind (docs/02_type_system.md §3). Each is polymorphic in its
-- | filling: a fixture's Intent takes `Q` quantities, a Strategy's template takes `Expr`. A Strategy
-- | lifts one into its template with `Schema.Build.Decision.intent`.
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

import Core.Types (Id(..), IntentF(..), MarketId, RuleChangeF(..), RuleF(..))
import Data.Maybe (Maybe)

-- | An order on a Market, by id; `limit` is the optional limit price. A Strategy that picks its Market
-- | by selector writes `Schema.Build.Decision.tradeAt`.
trade :: forall v. { market :: String, quantity :: v, limit :: Maybe v } -> IntentF MarketId v
trade r = Trade { market: Id r.market, quantity: r.quantity, limitPrice: r.limit }

-- | A routed order: no Market is named, so `base` and `quote` give the pair.
route :: forall m v. { base :: String, quote :: String, quantity :: v, limit :: Maybe v } -> IntentF m v
route r = Route { pair: { base: Id r.base, quote: Id r.quote }, quantity: r.quantity, limitPrice: r.limit }

-- | The `from` Entity gives `quantity` of `resource` to `to`.
transfer :: forall m v. { from :: String, to :: String, resource :: String, quantity :: v } -> IntentF m v
transfer r = Transfer { from: Id r.from, to: Id r.to, resource: Id r.resource, quantity: r.quantity }

transform :: forall m v. { by :: String, transformation :: String, quantity :: v } -> IntentF m v
transform r = Transform { by: Id r.by, transformation: Id r.transformation, quantity: r.quantity }

enactTariff :: forall m v. { jurisdiction :: String, origin :: String, rate :: v } -> IntentF m v
enactTariff r = ChangeRule (Enact (Tariff { jurisdiction: Id r.jurisdiction, origin: Id r.origin, rate: r.rate }))

enactAccessBan :: forall m v. { jurisdiction :: String, target :: String } -> IntentF m v
enactAccessBan r = ChangeRule (Enact (AccessBan { jurisdiction: Id r.jurisdiction, target: Id r.target }))

enactWithholding :: forall m v. { capacity :: String, units :: v } -> IntentF m v
enactWithholding r = ChangeRule (Enact (Withholding { capacity: Id r.capacity, units: r.units }))

-- | Repeal a Rule by the id the engine assigned it.
repeal :: forall m v. { rule :: String } -> IntentF m v
repeal r = ChangeRule (Repeal (Id r.rule))
