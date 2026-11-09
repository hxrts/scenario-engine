-- | The Rule kinds a Premise can declare: a levy, a block, a reduction.
module Premises.Kit.RuleKinds
  ( LevyKit
  , levyRule
  , BlockKit
  , blockRule
  , ReduceKit
  , reduceRule
  ) where

import Core.Num (Q)
import Laws.StrategyRules (Expr, IntentTemplate, selfName)
import Laws.Types (Capability, Direction(..), ActorId, FlowEffect(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleChangeF(..), RuleF(..))
import Schema.Build.Base (mayEnact)
import Schema.Build.Decision (selfEnact)

-- | A levy on inbound flows of the covered Resources, per unit, paid in a Resource to the destination.
type LevyKit =
  { may :: Capability
  , rule :: { jurisdiction :: ActorId, origin :: ActorId, rate :: Q } -> RuleF Q
  , enact :: { jurisdiction :: ActorId, origin :: ActorId, rate :: Q } -> Intent
  , self :: { origin :: ActorId, rate :: Expr } -> IntentTemplate
  }

levyRule :: { kind :: String, covered :: Array ResourceId, paidIn :: ResourceId } -> LevyKit
levyRule d =
  { may: mayEnact d.kind
  , rule: \r -> levyOf r.jurisdiction r.origin r.rate
  , enact: \r -> ChangeRule (Enact (levyOf r.jurisdiction r.origin r.rate))
  , self: \r -> selfEnact (levyOf (Id selfName) r.origin r.rate)
  }
  where
  levyOf :: forall v. ActorId -> ActorId -> v -> RuleF v
  levyOf jurisdiction origin rate = FlowRule { kind: Id d.kind, scope: { governor: jurisdiction, counterparty: origin, direction: Inbound, over: d.covered }, effect: Levy { rate, basis: PerUnit, in: d.paidIn, to: LevyToDestination } }

-- | A block of flows of the covered Resources, either way.
type BlockKit =
  { may :: Capability
  , rule :: { jurisdiction :: ActorId, target :: ActorId } -> RuleF Q
  , enact :: { jurisdiction :: ActorId, target :: ActorId } -> Intent
  , self :: { target :: ActorId } -> IntentTemplate
  }

blockRule :: { kind :: String, covered :: Array ResourceId } -> BlockKit
blockRule d =
  { may: mayEnact d.kind
  , rule: \r -> blockOf r.jurisdiction r.target
  , enact: \r -> ChangeRule (Enact (blockOf r.jurisdiction r.target))
  , self: \r -> selfEnact (blockOf (Id selfName) r.target)
  }
  where
  blockOf :: forall v. ActorId -> ActorId -> RuleF v
  blockOf jurisdiction target = FlowRule { kind: Id d.kind, scope: { governor: jurisdiction, counterparty: target, direction: EitherWay, over: d.covered }, effect: Block }

-- | A reduction of a capacity. Its enactment is polymorphic in its filling: a fixture's takes `Q`, a
-- | StrategyRules's an expression.
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
