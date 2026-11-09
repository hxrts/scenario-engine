-- | The recurring-flow forms a premise authors (docs/03_premise.md §9), compiled to the one
-- | `RecurringFlow` rule the engine reads. The kinds are names a premise can use: a premise that
-- | declares none has none of them. `compileIn` assigns ids and puts the rules in the order the engine
-- | evaluates them: production, then shares, requirements, fixed flows in recipient order, then
-- | decay. `normalize` restores that order after coverage merges Actors.
module Schema.Recurring
  ( kinds
  , Circulation
  , Production
  , Flow
  , Share
  , Requirement
  , Decay
  , Authored
  , compileIn
  , salesOf
  , payeeReceipts
  , normalize
  , productionRule
  , flowRule
  , shareRule
  , requirementRule
  , isRequirement
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Laws.Types (ActorId, Id(..), RecurringFlowId, RecurringKindId, ResourceId)
import Laws.World (CausePattern(..), LegAggregate(..), LegQuery, RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), ShortfallEffect(..), Side(..), Window(..))
import Schema.Premise.Query (ShareMeasure, atParty)
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

-- | What an authored share is a share of: this round's net sales of a Resource on Exchanges, or what it
-- | received as a transformation's payee.
salesOf :: ResourceId -> ShareMeasure
salesOf r = { window: ThisRoundSoFar, side: NetOutgoing, resource: Just r, cause: CurveBase, aggregate: SumValue }

payeeReceipts :: ShareMeasure
payeeReceipts = { window: ThisRoundSoFar, side: Incoming, resource: Nothing, cause: TransformationPayment, aggregate: SumMoved }

-- | What the party bought of a Resource on Exchanges this round.
purchases :: ActorId -> ResourceId -> LegQuery
purchases party r = { party, window: ThisRoundSoFar, side: Incoming, resource: Just r, cause: CurveBase, aggregate: SumMoved }

type Production = { actor :: ActorId, resource :: ResourceId, capacity :: ResourceId }

type Flow = { from :: ActorId, to :: ActorId, resource :: ResourceId, amount :: Q }

type Share = { from :: ActorId, to :: ActorId, share :: Q, source :: ShareMeasure }

type Requirement = { actor :: ActorId, resource :: ResourceId, amount :: Maybe Q, penalty :: Q }

-- | The recurring cycle as a premise authors it.
type Circulation =
  { production :: Array Production
  , flows :: Array Flow
  , shares :: Array Share
  , requirements :: Array Requirement
  }

-- | A Resource's decay: the share every holder loses each round.
type Decay = { resource :: ResourceId, rate :: Q }

-- | What a premise authors that Core has no field for: the cycle it compiles to rules, what a transformation may
-- | reserve, and each Resource's decay. Compiled premise files are written from this and the World.
type Authored = { circulation :: Circulation, reservable :: Array ResourceId, decays :: Array Decay }

-- | A rule of a kind, its id set by `compile`.
rule :: RecurringKindId -> RecurringSource -> RecurringDestination -> ResourceId -> RecurringAmount -> RecurringFlow
rule kind source destination resource amount =
  { id: Id ""
  , kind
  , source
  , destination
  , resource
  , amount
  , onShortfall: NoEffect
  , offsetable: false
  , delay: 0
  }

-- | Production: the holding of a capacity, delivered as the Resource next round.
productionRule :: Production -> RecurringFlow
productionRule p = (rule kinds.production Created (ToActor p.actor) p.resource (HeldAmount p.capacity)) { delay = 1 }

-- | A fixed flow, reduced by the recipient's accrued offset.
flowRule :: { from :: ActorId, to :: ActorId, resource :: ResourceId, amount :: Q } -> RecurringFlow
flowRule f = (rule kinds.flow (FromActor f.from) (ToActor f.to) f.resource (FixedAmount f.amount)) { offsetable = true }

-- | A variable share of what the payer received.
shareRule :: ResourceId -> Share -> RecurringFlow
shareRule resource s = rule kinds.share (FromActor s.from) (ToActor s.to) resource (Measured { ratio: s.share, query: atParty s.from s.source })

-- | An operating requirement: consumed from what is held; a shortfall offsets incoming flows.
requirementRule :: { actor :: ActorId, resource :: ResourceId, amount :: Maybe Q, penalty :: Q } -> RecurringFlow
requirementRule r = (rule kinds.requirement (FromActor r.actor) ToSink r.resource (maybe (Measured { ratio: Q.one, query: purchases r.actor r.resource }) FixedAmount r.amount))
  { onShortfall = if r.penalty == Q.zero then NoEffect else OffsetIncoming r.penalty }

-- | Decay: every holder loses a fraction of a Resource.
decayRule :: Decay -> RecurringFlow
decayRule d = rule kinds.decay FromEachHolder ToSink d.resource (FractionHeld d.rate)

-- | Whether a rule is an operating requirement: the form coverage merges by Actor and Resource.
isRequirement :: RecurringFlow -> Boolean
isRequirement r = r.kind == kinds.requirement

-- | Every authored form as rules, ids assigned per kind, in evaluation order. A share is paid in the
-- | premise's numeraire (the DSL fills it in).
compileIn :: ResourceId -> Circulation -> Array Decay -> Array RecurringFlow
compileIn numeraire c decays = normalize
  ( withIds kinds.production (map productionRule c.production)
      <> withIds kinds.share (map (shareRule numeraire) c.shares)
      <> withIds kinds.requirement (map requirementRule c.requirements)
      <> withIds kinds.flow (map flowRule c.flows)
      <> withIds kinds.decay (map decayRule decays)
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
    ToActor (Id to) -> to
    ToSink -> ""
