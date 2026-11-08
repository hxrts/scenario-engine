-- | The recurring-flow forms a premise authors (docs/03_gameboard.md §9), compiled to the one
-- | `RecurringFlow` rule the engine reads. The kinds are names a premise can use: a premise that
-- | declares none has none of them. `compile` assigns ids and puts the rules in the order the engine
-- | evaluates them: production, then shares, requirements, fixed flows in recipient order, then
-- | decay. `normalize` restores that order after coverage merges Entities.
module Schema.Recurring
  ( kinds
  , Circulated
  , ShareSource(..)
  , shareBasis
  , compile
  , normalize
  , productionRule
  , flowRule
  , shareRule
  , requirementRule
  , decayRule
  , isRequirement
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, Id(..), RecurringFlowId, RecurringKindId, ResourceId, CapacityRef)
import Core.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShareBasis(..), ShortfallEffect(..))
import Data.Generic.Rep (class Generic)
import Data.Array (mapWithIndex, sortWith)
import Data.Maybe (Maybe(..), maybe)

-- | The kinds a premise compiles to; a view or a test reads these names.
kinds :: { production :: RecurringKindId, share :: RecurringKindId, requirement :: RecurringKindId, flow :: RecurringKindId, decay :: RecurringKindId }
kinds =
  { production: Id "production"
  , share: Id "share"
  , requirement: Id "requirement"
  , flow: Id "flow"
  , decay: Id "decay"
  }

-- | What an authored share is a fraction of.
data ShareSource
  = SalesOf ResourceId -- net Money revenue from selling this Resource on Markets
  | PayeeReceipts -- Money received as a conversion's payee

derive instance Generic ShareSource _
derive instance Eq ShareSource

-- | The authored form of a share's basis, as the engine reads it.
shareBasis :: ShareSource -> ShareBasis
shareBasis = case _ of
  SalesOf r -> NetSales r
  PayeeReceipts -> PaymentsReceived

type Circulated =
  { production :: Array { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef }
  , flows :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Q }
  , shares :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, share :: Q, basis :: ShareBasis }
  , requirements :: Array { entity :: EntityId, resource :: ResourceId, amount :: Maybe Q, penalty :: Q }
  , decays :: Array { resource :: ResourceId, rate :: Q }
  }

-- | A rule of a kind, its id set by `compile`.
rule :: RecurringKindId -> RecurringSource -> RecurringDestination -> ResourceId -> RecurringAmount -> RecurringFlow
rule kind source destination resource amount =
  { id: Id ""
  , kind
  , source
  , destination
  , resource
  , amount
  , condition: Nothing
  , onShortfall: NoEffect
  , offsetable: false
  , delay: 0
  }

-- | Production: capacity delivered next round.
productionRule :: { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef } -> RecurringFlow
productionRule p = (rule kinds.production Created (ToEntity p.entity) p.resource (CapacityHeld p.capacity)) { delay = 1 }

-- | A fixed flow, reduced by the recipient's accrued offset.
flowRule :: { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Q } -> RecurringFlow
flowRule f = (rule kinds.flow (FromEntity f.from) (ToEntity f.to) f.resource (FixedAmount f.amount)) { offsetable = true }

-- | A variable share of what the payer received.
shareRule :: { from :: EntityId, to :: EntityId, resource :: ResourceId, share :: Q, basis :: ShareBasis } -> RecurringFlow
shareRule s = rule kinds.share (FromEntity s.from) (ToEntity s.to) s.resource (ShareOf { ratio: s.share, basis: s.basis })

-- | An operating requirement: consumed from what is held; a shortfall offsets incoming flows.
requirementRule :: { entity :: EntityId, resource :: ResourceId, amount :: Maybe Q, penalty :: Q } -> RecurringFlow
requirementRule r = (rule kinds.requirement (FromEntity r.entity) ToSink r.resource (maybe PurchasesThisRound FixedAmount r.amount))
  { onShortfall = if r.penalty == Q.zero then NoEffect else OffsetIncoming r.penalty }

-- | Decay: every holder loses a fraction of a Resource.
decayRule :: { resource :: ResourceId, rate :: Q } -> RecurringFlow
decayRule d = rule kinds.decay FromEachHolder ToSink d.resource (FractionHeld d.rate)

-- | Whether a rule is an operating requirement: the form coverage merges by Entity and Resource.
isRequirement :: RecurringFlow -> Boolean
isRequirement r = r.kind == kinds.requirement

-- | Every authored form as rules, ids assigned per kind, in evaluation order.
compile :: Circulated -> Array RecurringFlow
compile c = normalize
  ( withIds kinds.production (map productionRule c.production)
      <> withIds kinds.share (map shareRule c.shares)
      <> withIds kinds.requirement (map requirementRule c.requirements)
      <> withIds kinds.flow (map flowRule c.flows)
      <> withIds kinds.decay (map decayRule c.decays)
  )

withIds :: RecurringKindId -> Array RecurringFlow -> Array RecurringFlow
withIds (Id k) = mapWithIndex (\i r -> r { id = recurringId k i })

recurringId :: String -> Int -> RecurringFlowId
recurringId k i = Id (k <> "-" <> show i)

-- | The engine's evaluation order: kinds in sequence, fixed flows by recipient (stable).
normalize :: Array RecurringFlow -> Array RecurringFlow
normalize = sortWith key
  where
  key r = { rank: rank r.kind, recipient: if r.kind == kinds.flow then recipient r else "" }
  rank k
    | k == kinds.production = 0
    | k == kinds.share = 1
    | k == kinds.requirement = 2
    | k == kinds.flow = 3
    | k == kinds.decay = 4
    | otherwise = 5
  recipient r = case r.destination of
    ToEntity (Id to) -> to
    ToSink -> ""
