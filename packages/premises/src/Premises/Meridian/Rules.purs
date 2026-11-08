-- | The kinds of Rule Meridian declares (docs/03_gameboard.md): a tariff is a levy on inbound Transfers
-- | governed by the destination, an access ban blocks either way, and a withholding reduces a capacity.
-- | The mechanism is the engine's; these names and their combinations are this premise's.
module Premises.Meridian.Rules
  ( ruleKinds
  , tariff
  , accessBan
  , withholding
  ) where

import Core.Types (FlowGovernance(..), Id(..), ResourceId, RuleForm(..), RuleKindDecl, RuleKindId)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Premises.Kit (BlockKit, LevyKit, ReduceKit, blockRule, levyRule, reduceRule)
import Premises.Meridian.Names as N
import Schema.Build.Base (keyed)

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
ruleKinds :: Map RuleKindId RuleKindDecl
ruleKinds = keyed
  [ Tuple tariffKind { form: LevyFlows GovernedByDestination, text: "tariff on {counterparty} goods into {governor}, rate {amount}" }
  , Tuple accessBanKind { form: BlockFlows GovernedByEither, text: "access ban: {governor} bars {counterparty}" }
  , Tuple withholdingKind { form: ReduceCapacity, text: "withholding of {amount} from {capacity}" }
  ]

tariff :: LevyKit
tariff = levyRule { kind: tariffKind, covered, paidIn: Id N.money }

accessBan :: BlockKit
accessBan = blockRule { kind: accessBanKind, covered }

withholding :: ReduceKit
withholding = reduceRule { kind: withholdingKind }
