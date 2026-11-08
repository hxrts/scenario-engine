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
import Core.Types (Capability, Direction(..), FlowEffect(..), FlowGovernance(..), LevyBasis(..), LevyRecipient(..), Id(..), Intent, IntentF(..), RuleChangeF(..), RuleF(..), ResourceId, RuleForm(..), RuleKindDecl)
import Data.Tuple (Tuple(..))
import Premises.Names as N
import Schema.Build.Base (Keyed, keyed, mayEnact)
import Schema.Build.Decision (selfEnact)

-- | What Meridian's flows Rules cover: the gated Resource.
covered :: Array ResourceId
covered = [ Id N.energy ]

tariffKind :: String
tariffKind = "tariff"

accessBanKind :: String
accessBanKind = "accessBan"

withholdingKind :: String
withholdingKind = "withholding"

-- | The declarations.
ruleKinds :: Keyed RuleKindDecl
ruleKinds = keyed
  [ Tuple tariffKind { form: LevyFlows GovernedByDestination, text: "tariff on {counterparty} goods into {governor}, rate {amount}" }
  , Tuple accessBanKind { form: BlockFlows GovernedByEither, text: "access ban: {governor} bars {counterparty}" }
  , Tuple withholdingKind { form: ReduceCapacity, text: "withholding of {amount} from {capacity}" }
  ]

mayEnactTariff :: Capability
mayEnactTariff = mayEnact tariffKind

mayEnactAccessBan :: Capability
mayEnactAccessBan = mayEnact accessBanKind

mayEnactWithholding :: Capability
mayEnactWithholding = mayEnact withholdingKind

tariffOf :: forall v. String -> String -> v -> RuleF v
tariffOf jurisdiction origin rate = FlowRule { kind: Id tariffKind, scope: { governor: Id jurisdiction, counterparty: Id origin, direction: Inbound, over: covered }, effect: Levy { rate, basis: PerUnit, in: Id N.money, to: LevyToDestination } }

banOf :: forall v. String -> String -> RuleF v
banOf jurisdiction target = FlowRule { kind: Id accessBanKind, scope: { governor: Id jurisdiction, counterparty: Id target, direction: EitherWay, over: covered }, effect: Block }

withholdingOf :: forall v. String -> v -> RuleF v
withholdingOf capacity units = CapacityRule { kind: Id withholdingKind, capacity: Id capacity, reduce: units }

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
selfTariff r = selfEnact (tariffOf selfName r.origin r.rate)

-- | Enact an access ban as the acting Entity.
selfAccessBan :: { target :: String } -> IntentTemplate
selfAccessBan r = selfEnact (banOf selfName r.target)
