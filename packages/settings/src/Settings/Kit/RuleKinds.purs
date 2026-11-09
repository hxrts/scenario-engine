-- | The Rule kinds a Setting can declare: a levy, a block, a reduction.
module Settings.Kit.RuleKinds
  ( LevyKit
  , levyRule
  , BlockKit
  , blockRule
  , ReduceKit
  , reduceRule
  ) where

import Core.Num (Q)
import Laws.Strategy (Expr, IntentTemplate, selfName)
import Laws.Types (Capability, Direction(..), EntityId, FlowEffect(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleChangeF(..), RuleF(..))
import Schema.Build.Base (mayEnact)
import Schema.Build.Decision (selfEnact)

-- | A levy on inbound flows of the covered Resources, per unit, paid in a Resource to the destination.
type LevyKit =
  { may :: Capability
  , rule :: { jurisdiction :: EntityId, origin :: EntityId, rate :: Q } -> RuleF Q
  , enact :: { jurisdiction :: EntityId, origin :: EntityId, rate :: Q } -> Intent
  , self :: { origin :: EntityId, rate :: Expr } -> IntentTemplate
  }

levyRule :: { kind :: String, covered :: Array ResourceId, paidIn :: ResourceId } -> LevyKit
levyRule d =
  { may: mayEnact d.kind
  , rule: \r -> levyOf r.jurisdiction r.origin r.rate
  , enact: \r -> ChangeRule (Enact (levyOf r.jurisdiction r.origin r.rate))
  , self: \r -> selfEnact (levyOf (Id selfName) r.origin r.rate)
  }
  where
  levyOf :: forall v. EntityId -> EntityId -> v -> RuleF v
  levyOf jurisdiction origin rate = FlowRule { kind: Id d.kind, scope: { governor: jurisdiction, counterparty: origin, direction: Inbound, over: d.covered }, effect: Levy { rate, basis: PerUnit, in: d.paidIn, to: LevyToDestination } }

-- | A block of flows of the covered Resources, either way.
type BlockKit =
  { may :: Capability
  , rule :: { jurisdiction :: EntityId, target :: EntityId } -> RuleF Q
  , enact :: { jurisdiction :: EntityId, target :: EntityId } -> Intent
  , self :: { target :: EntityId } -> IntentTemplate
  }

blockRule :: { kind :: String, covered :: Array ResourceId } -> BlockKit
blockRule d =
  { may: mayEnact d.kind
  , rule: \r -> blockOf r.jurisdiction r.target
  , enact: \r -> ChangeRule (Enact (blockOf r.jurisdiction r.target))
  , self: \r -> selfEnact (blockOf (Id selfName) r.target)
  }
  where
  blockOf :: forall v. EntityId -> EntityId -> RuleF v
  blockOf jurisdiction target = FlowRule { kind: Id d.kind, scope: { governor: jurisdiction, counterparty: target, direction: EitherWay, over: d.covered }, effect: Block }

-- | A reduction of a capacity. Its enactment is polymorphic in its filling: a fixture's takes `Q`, a
-- | Strategy's an expression.
type ReduceKit =
  { may :: Capability
  , rule :: { capacity :: ResourceId, units :: Q } -> RuleF Q
  , enact :: forall m v. { capacity :: ResourceId, units :: v } -> IntentF m v
  }

reduceRule :: { kind :: String } -> ReduceKit
reduceRule d =
  { may: mayEnact d.kind
  , rule: \r -> reduceOf r.capacity r.units
  , enact: \r -> ChangeRule (Enact (reduceOf r.capacity r.units))
  }
  where
  reduceOf :: forall v. ResourceId -> v -> RuleF v
  reduceOf capacity units = HoldingRule { kind: Id d.kind, resource: capacity, reduce: units }
