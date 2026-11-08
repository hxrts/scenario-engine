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
import Core.Strategy (Expr, IntentTemplate, selfName)
import Core.Types (Capability, Direction(..), FlowEffect(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleChangeF(..), RuleF(..))
import Schema.Build.Base (mayEnact)
import Schema.Build.Decision (selfEnact)

-- | A levy on inbound flows of the covered Resources, per unit, paid in a Resource to the destination.
type LevyKit =
  { may :: Capability
  , rule :: { jurisdiction :: String, origin :: String, rate :: Q } -> RuleF Q
  , enact :: { jurisdiction :: String, origin :: String, rate :: Q } -> Intent
  , self :: { origin :: String, rate :: Expr } -> IntentTemplate
  }

levyRule :: { kind :: String, covered :: Array ResourceId, paidIn :: ResourceId } -> LevyKit
levyRule d =
  { may: mayEnact d.kind
  , rule: \r -> levyOf r.jurisdiction r.origin r.rate
  , enact: \r -> ChangeRule (Enact (levyOf r.jurisdiction r.origin r.rate))
  , self: \r -> selfEnact (levyOf selfName r.origin r.rate)
  }
  where
  levyOf :: forall v. String -> String -> v -> RuleF v
  levyOf jurisdiction origin rate = FlowRule { kind: Id d.kind, scope: { governor: Id jurisdiction, counterparty: Id origin, direction: Inbound, over: d.covered }, effect: Levy { rate, basis: PerUnit, in: d.paidIn, to: LevyToDestination } }

-- | A block of flows of the covered Resources, either way.
type BlockKit =
  { may :: Capability
  , rule :: { jurisdiction :: String, target :: String } -> RuleF Q
  , enact :: { jurisdiction :: String, target :: String } -> Intent
  , self :: { target :: String } -> IntentTemplate
  }

blockRule :: { kind :: String, covered :: Array ResourceId } -> BlockKit
blockRule d =
  { may: mayEnact d.kind
  , rule: \r -> blockOf r.jurisdiction r.target
  , enact: \r -> ChangeRule (Enact (blockOf r.jurisdiction r.target))
  , self: \r -> selfEnact (blockOf selfName r.target)
  }
  where
  blockOf :: forall v. String -> String -> RuleF v
  blockOf jurisdiction target = FlowRule { kind: Id d.kind, scope: { governor: Id jurisdiction, counterparty: Id target, direction: EitherWay, over: d.covered }, effect: Block }

-- | A reduction of a capacity. Its enactment is polymorphic in its filling: a fixture's takes `Q`, a
-- | Strategy's an expression.
type ReduceKit =
  { may :: Capability
  , rule :: { capacity :: String, units :: Q } -> RuleF Q
  , enact :: forall m v. { capacity :: String, units :: v } -> IntentF m v
  }

reduceRule :: { kind :: String } -> ReduceKit
reduceRule d =
  { may: mayEnact d.kind
  , rule: \r -> reduceOf r.capacity r.units
  , enact: \r -> ChangeRule (Enact (reduceOf r.capacity r.units))
  }
  where
  reduceOf :: forall v. String -> v -> RuleF v
  reduceOf capacity units = CapacityRule { kind: Id d.kind, capacity: Id capacity, reduce: units }
