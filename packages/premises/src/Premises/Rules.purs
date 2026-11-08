-- | The kinds of Rule the Meridian premises declare (docs/03_gameboard.md): a tariff is a levy on inbound
-- | Transfers governed by the destination, an access ban blocks either way, and a withholding reduces a
-- | capacity. The mechanism is the engine's; these names and their combinations are this premise's.
module Premises.Rules
  ( ruleKinds
  , mayEnactTariff
  , mayEnactAccessBan
  , mayEnactWithholding
  , tariffRule
  , accessBanRule
  , withholdingRule
  , enactTariff
  , enactAccessBan
  , enactWithholding
  , selfTariff
  , selfAccessBan
  ) where

import Core.Num (Q)
import Core.Strategy (Expr, IntentTemplate, selfName)
import Core.Types (Capability, Direction(..), EffectF(..), EffectKind(..), Id(..), Intent, IntentF(..), RuleChangeF(..), RuleF, RuleKindDecl, Scope(..), ScopeKind(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Base (Keyed, keyed, mayEnact)
import Schema.Build.Decision (selfEnact)

tariffKind :: String
tariffKind = "tariff"

accessBanKind :: String
accessBanKind = "accessBan"

withholdingKind :: String
withholdingKind = "withholding"

-- | The declarations.
ruleKinds :: Keyed RuleKindDecl
ruleKinds = keyed
  [ Tuple tariffKind { effect: LevyEffect, scope: InboundFlows, text: "tariff on {counterparty} goods into {governor}, rate {amount}" }
  , Tuple accessBanKind { effect: BlockEffect, scope: EitherWayFlows, text: "access ban: {governor} bars {counterparty}" }
  , Tuple withholdingKind { effect: ReduceEffect, scope: CapacityScope, text: "withholding of {amount} from {capacity}" }
  ]

mayEnactTariff :: Capability
mayEnactTariff = mayEnact tariffKind

mayEnactAccessBan :: Capability
mayEnactAccessBan = mayEnact accessBanKind

mayEnactWithholding :: Capability
mayEnactWithholding = mayEnact withholdingKind

tariffOf :: forall v. String -> String -> v -> RuleF v
tariffOf jurisdiction origin rate = { kind: Id tariffKind, scope: Flows { governor: Id jurisdiction, counterparty: Id origin, direction: Inbound }, effect: Levy rate }

banOf :: forall v. String -> String -> RuleF v
banOf jurisdiction target = { kind: Id accessBanKind, scope: Flows { governor: Id jurisdiction, counterparty: Id target, direction: EitherWay }, effect: Block }

withholdingOf :: forall v. String -> v -> RuleF v
withholdingOf capacity units = { kind: Id withholdingKind, scope: OnCapacity (Id capacity), effect: Reduce units }

tariffRule :: { jurisdiction :: String, origin :: String, rate :: Q } -> RuleF Q
tariffRule r = tariffOf r.jurisdiction r.origin r.rate

accessBanRule :: { jurisdiction :: String, target :: String } -> RuleF Q
accessBanRule r = banOf r.jurisdiction r.target

withholdingRule :: { capacity :: String, units :: Q } -> RuleF Q
withholdingRule r = withholdingOf r.capacity r.units

enactTariff :: { jurisdiction :: String, origin :: String, rate :: Q } -> Intent
enactTariff r = ChangeRule (Enact (tariffRule r))

enactAccessBan :: { jurisdiction :: String, target :: String } -> Intent
enactAccessBan r = ChangeRule (Enact (accessBanRule r))

enactWithholding :: forall m v. { capacity :: String, units :: v } -> IntentF m v
enactWithholding r = ChangeRule (Enact (withholdingOf r.capacity r.units))

-- | Enact a tariff as the acting Entity.
selfTariff :: { origin :: String, rate :: Expr } -> IntentTemplate
selfTariff r = selfEnact { kind: tariffKind, scope: Flows { governor: Id selfName, counterparty: Id r.origin, direction: Inbound }, effect: Levy r.rate }

-- | Enact an access ban as the acting Entity.
selfAccessBan :: { target :: String } -> IntentTemplate
selfAccessBan r = selfEnact { kind: accessBanKind, scope: Flows { governor: Id selfName, counterparty: Id r.target, direction: EitherWay }, effect: Block }
