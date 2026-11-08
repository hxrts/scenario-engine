-- | The Intent builders, one per kind (docs/02_type_system.md §3). `trade`, `route`, `transfer`,
-- | `enactWithholding` and `repeal` are polymorphic in their filling: a fixture's Intent takes `Q`
-- | quantities, a Strategy's template takes `Expr` and is lifted with `Schema.Build.Decision.asTemplate`.
-- | `transform`, `enactTariff` and `enactAccessBan` name their actor, which the setting format of a
-- | template cannot write, so they take `Q` only; a Strategy uses `Decision.selfTransform`,
-- | `selfTariff` and `selfAccessBan` (a template naming another actor is a compile error, not a crash).
module Schema.Build.Intent
  ( trade
  , route
  , transfer
  , transform
  , enactRule
  , repeal
  ) where

import Core.Num (Q)
import Core.Types (Id(..), Intent, IntentF(..), MarketId, RuleChangeF(..), RuleF)
import Data.Maybe (Maybe(..))

-- | An order on a Market, by id; `limit` is the optional limit price. A Strategy that picks its Market
-- | by selector writes `Schema.Build.Decision.tradeAt`.
trade :: forall v. { market :: String, quantity :: v, limit :: Maybe v } -> IntentF MarketId v
trade r = Trade { market: Id r.market, quantity: r.quantity, limitPrice: r.limit }

-- | A routed order: no Market is named, so `base` and `quote` give the pair.
route :: forall m v. { base :: String, quote :: String, quantity :: v, limit :: v } -> IntentF m v
route r = Route { pair: { base: Id r.base, quote: Id r.quote }, quantity: r.quantity, limitPrice: Just r.limit }

-- | The `from` Entity gives `quantity` of `resource` to `to`.
transfer :: forall m v. { from :: String, to :: String, resource :: String, quantity :: v } -> IntentF m v
transfer r = Transfer { from: Id r.from, to: Id r.to, resource: Id r.resource, quantity: r.quantity }

transform :: { by :: String, transformation :: String, quantity :: Q } -> Intent
transform r = Transform { by: Id r.by, transformation: Id r.transformation, quantity: r.quantity }

-- | Enact a Rule of a declared kind.
enactRule :: forall m v. RuleF v -> IntentF m v
enactRule r = ChangeRule (Enact r)

-- | Repeal a Rule by the id the engine assigned it.
repeal :: forall m v. { rule :: String } -> IntentF m v
repeal r = ChangeRule (Repeal (Id r.rule))
