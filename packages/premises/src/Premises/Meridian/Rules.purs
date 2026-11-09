-- | The kinds of Rule Meridian declares (docs/03_premise.md): a tariff is a levy on inbound Transfers
-- | governed by the destination, an access ban blocks either way, and a withholding reduces a capacity.
-- | The mechanism is the engine's; these names and their combinations are this premise's.
module Premises.Meridian.Rules
  ( ruleKinds
  , tariff
  , accessBan
  , withholding
  ) where

import Laws.Types (FlowGovernance(..), ResourceId, RuleForm(..), RuleKindDecl, RuleKindId)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Premises.Kit.RuleKinds (BlockKit, LevyKit, ReduceKit, blockRule, levyRule, reduceRule)
import Premises.Meridian.Names (n)
import Schema.Build.Base (keyed)

-- | What Meridian's flows Rules cover: the gated Resource.
covered :: Array ResourceId
covered = [ n.energy ]

tariffKind :: String
tariffKind = "tariff"

accessBanKind :: String
accessBanKind = "accessBan"

withholdingKind :: String
withholdingKind = "withholding"

-- | The declarations.
ruleKinds :: Map RuleKindId RuleKindDecl
ruleKinds = keyed
  [ Tuple tariffKind { form: LevyFlows GovernedByDestination, text: "tariff on {counterparty} goods into {governor}, rate {amount}" }
  , Tuple accessBanKind { form: BlockFlows GovernedByEither, text: "access ban: {governor} bars {counterparty}" }
  , Tuple withholdingKind { form: ReduceHolding, text: "withholding of {amount} from {resource}" }
  ]

tariff :: LevyKit
tariff = levyRule { kind: tariffKind, covered, paidIn: n.money }

accessBan :: BlockKit
accessBan = blockRule { kind: accessBanKind, covered }

withholding :: ReduceKit
withholding = reduceRule { kind: withholdingKind }
