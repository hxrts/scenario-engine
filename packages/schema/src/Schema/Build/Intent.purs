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
import Core.Types (EntityTag, Intent, IntentF(..), MarketId, MarketTag, ResourceTag, RuleChangeF(..), RuleF, RuleTag, TransformationTag)
import Schema.Build.Named (class Named, named)
import Data.Maybe (Maybe(..))

-- | An order on a Market, by id; `limit` is the optional limit price. A Strategy that picks its Market
-- | by selector writes `Schema.Build.Decision.tradeAt`.
trade :: forall a v. Named a MarketTag => { market :: a, quantity :: v, limit :: Maybe v } -> IntentF MarketId v
trade r = Trade { market: named r.market, quantity: r.quantity, limitPrice: r.limit }

-- | A routed order: no Market is named, so `base` and `quote` give the pair.
route :: forall b q m v. Named b ResourceTag => Named q ResourceTag => { base :: b, quote :: q, quantity :: v, limit :: v } -> IntentF m v
route r = Route { pair: { base: named r.base, quote: named r.quote }, quantity: r.quantity, limitPrice: Just r.limit }

-- | The `from` Entity gives `quantity` of `resource` to `to`.
transfer :: forall f t r m v. Named f EntityTag => Named t EntityTag => Named r ResourceTag => { from :: f, to :: t, resource :: r, quantity :: v } -> IntentF m v
transfer r = Transfer { from: named r.from, to: named r.to, resource: named r.resource, quantity: r.quantity }

transform :: forall b t. Named b EntityTag => Named t TransformationTag => { by :: b, transformation :: t, quantity :: Q } -> Intent
transform r = Transform { by: named r.by, transformation: named r.transformation, quantity: r.quantity }

-- | Enact a Rule of a declared kind.
enactRule :: forall m v. RuleF v -> IntentF m v
enactRule r = ChangeRule (Enact r)

-- | Repeal a Rule by the id the engine assigned it.
repeal :: forall a m v. Named a RuleTag => { rule :: a } -> IntentF m v
repeal r = ChangeRule (Repeal (named r.rule))
