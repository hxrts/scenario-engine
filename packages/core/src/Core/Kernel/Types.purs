-- | The kernel's vocabulary (docs/02_kernel.md): stocks, flows, laws over them, and transactions of
-- | conditional obligations. Nothing here names an agreement, an institution or a schedule: those are
-- | values an authoring layer writes. `cap` is what a party may be required to hold, `reason` what a
-- | failed condition reports, and `cause` what produced a settlement; the kernel only carries them.
module Core.Kernel.Types
  ( Param
  , TemplateId
  , ObligationId
  , ObligationIndex
  , Phase
  , Tier
  , Party(..)
  , Account(..)
  , StockRef
  , Amount(..)
  , FlowOwed
  , Bond(..)
  , Short(..)
  , Limit(..)
  , Condition(..)
  , Guarded
  , Effect(..)
  , LawChange(..)
  , Spawn
  , Next
  , noNext
  , Obligation
  , Hold
  , Solver(..)
  , Ration
  , Transaction
  , Template
  , Pending
  , StockScope
  , FlowScope
  , ActionScope
  , Subject(..)
  , Trigger(..)
  , Operation(..)
  , Law
  , Endpoint(..)
  , Flow
  , endpointShape
  , flowShape
  , KernelShapes
  , dynamicShape
  , amountShape
  , transactionShape
  , pendingShape
  , templateShape
  ) where

import Prelude

import Core.Num (Q, qShape)
import Core.Shape (Format(..), Shape, decodeJsonWith, encodeWith, arrayOf, atTagged, boolShape, genericEnum, genericSum, intShape, mapOf, optionalOf, recordShape, recursive, stringShape, unitShape)
import Core.Types (ActorId, ExchangeId, Quantity, ResourceId, RoundOffset, RoundIndex, idShape)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe)

type Param = String
type TemplateId = String
type ObligationId = String
type ObligationIndex = Int

-- | Where in a round's fixed sequence a transaction or a law settles. Lower runs first.
type Phase = Int

-- | Rationing priority: lower fills first.
type Tier = Int

-- | Where one settled settled Flow moved from or to, as the ledger records it.
data Endpoint = Held ActorId | InExchange ExchangeId | Source | Sink

derive instance Eq Endpoint
derive instance Ord Endpoint
derive instance Generic Endpoint _
instance Show Endpoint where
  show a = genericShow a

instance EncodeJson Endpoint where
  encodeJson = encodeWith Snapshot endpointShape

instance DecodeJson Endpoint where
  decodeJson = decodeJsonWith Snapshot endpointShape

endpointShape :: Shape Endpoint
endpointShape = genericSum "Endpoint"
  { "Held": atTagged "an Actor's account" idShape
  , "InExchange": atTagged "an Exchange's reserves" idShape
  , "Source": atTagged "where a Resource is created" unitShape
  , "Sink": atTagged "where a Resource ends" unitShape
  }

-- | One settled settled Flow: what was owed, and what moved.
type Flow = { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Quantity, moved :: Quantity }

flowShape :: Shape Flow
flowShape = recordShape "Flow" { from: endpointShape, to: endpointShape, resource: idShape, owed: qShape, moved: qShape }

-- | Who an obligation binds: a named party, or one a template binds from its parent.
data Party = Named ActorId | Bound Param

-- | Where a quantity sits or moves to. `Created` and `Destroyed` are the economy's boundary.
data Account
  = HeldBy Party
  | Reserve ExchangeId
  | Created
  | Destroyed

type StockRef = { at :: Account, resource :: ResourceId }

-- | A quantity computed when its obligation is consumed. Arguments are frozen values. `query` is a
-- | read of the record, opaque to the kernel.
data Amount query
  = Exactly Q
  | Arg Param
  | Stock StockRef
  | Times { ratio :: Q, amount :: Amount query }
  | Plus { left :: Amount query, right :: Amount query }
  | Minus { left :: Amount query, right :: Amount query }
  | Least { left :: Amount query, right :: Amount query }
  | ShareOf { limit :: Amount query, among :: Array (Amount query), index :: Int }
  | Read query
  | Moved -- what the consumed obligation moved; only a spawn's arguments read it
  | Remainder -- what it owed and did not move

type FlowOwed query = { from :: Account, to :: Account, resource :: ResourceId, owed :: Amount query }

-- | How a dependency on another obligation binds: all or nothing, or in proportion at a rate.
data Bond = Whole | Proportional Q

-- | What a shortfall does: the obligation is not performed, or it moves what is held.
data Short = Reject | Cap

data Limit = AtMost Q | AtLeast Q

data Condition cap query
  = Performed { index :: ObligationIndex, bond :: Bond }
  | StockAtLeast { stock :: StockRef, amount :: Amount query }
  | Consent (Array ActorId)
  | WeightedConsent { weights :: Map ActorId Q, residual :: Q, threshold :: Q }
  | PriceWithin Limit
  | QuantityAtLeast (Amount query)
  | Holds { party :: Party, capability :: cap }

-- | A condition and what its failure reports. No reason is a silent removal.
type Guarded cap query reason = { condition :: Condition cap query, reason :: Maybe reason }

-- | A change to the laws in force, applied when its obligation is consumed.
data LawChange law = SetLaw { id :: String, law :: law } | RemoveLaw String

data Effect law query
  = Move (FlowOwed query)
  | Change (LawChange law)

-- | A successor, as data: what to instantiate, how many rounds on (at least 1), and its frozen arguments.
type Spawn query =
  { template :: TemplateId
  , due :: RoundOffset
  , args :: Map Param (Amount query)
  , parties :: Map Param Party
  }

-- | What each way of being consumed spawns.
type Next query = { performed :: Array (Spawn query), short :: Array (Spawn query), lapsed :: Array (Spawn query) }

noNext :: forall query. Next query
noNext = { performed: [], short: [], lapsed: [] }

type Obligation cap law query reason =
  { id :: ObligationId
  , obligor :: Party
  , effect :: Effect law query
  , when :: Array (Guarded cap query reason)
  , onShort :: Short
  , next :: Next query
  }

-- | A quantity set aside for the round and released at commit. It moves nothing.
type Hold query = { party :: Party, resource :: ResourceId, amount :: Amount query }

-- | How one limit is divided among many: the closed set of solvers.
data Solver
  = Clearing { exchange :: ExchangeId }
  | Routing { base :: ResourceId, quote :: ResourceId, limit :: Q, candidates :: Array ExchangeId }
  | Pool { resource :: Maybe ResourceId, perUnit :: Q }

-- | Who shares a shortage: the tier that fills first, whether a transaction is whole or scaled, and the
-- | group whose claims scale together.
type Ration = { tier :: Tier, whole :: Boolean, scaleGroup :: Maybe ActorId }

type Transaction cap law query reason cause =
  { obligations :: Array (Obligation cap law query reason)
  , holds :: Array (Hold query)
  , solver :: Maybe Solver
  , ration :: Ration
  , phase :: Phase
  , cause :: Maybe cause -- Nothing inherits the parent's
  , consented :: Array ActorId -- who has signed, carried to its descendants
  }

type Template cap law query reason cause =
  { params :: Array Param
  , parties :: Array Param
  , transaction :: Transaction cap law query reason cause
  }

type Pending cap law query reason cause = { due :: RoundIndex, transaction :: Transaction cap law query reason cause }

type StockScope = { holders :: Maybe (Array ActorId), resource :: ResourceId }
type FlowScope = { from :: Array ActorId, to :: Array ActorId, resources :: Array ResourceId }
type ActionScope = { obligor :: Maybe ActorId, cause :: Maybe String }

data Subject = OfStock StockScope | OfFlow FlowScope

data Trigger = EachRound | OnAction ActionScope

data Operation query
  = Emit (FlowOwed query)
  | Refuse
  | Withhold (Amount query)

type Law cap query reason cause =
  { subject :: Subject
  , trigger :: Trigger
  , operation :: Operation query
  , guard :: Array (Guarded cap query reason)
  , phase :: Phase
  , cause :: cause
  }

derive instance Eq Party
derive instance Generic Party _
instance Show Party where
  show = genericShow

derive instance Eq Account
derive instance Generic Account _
instance Show Account where
  show = genericShow

derive instance Eq query => Eq (Amount query)
derive instance Generic (Amount query) _
instance Show query => Show (Amount query) where
  show a = genericShow a

derive instance Eq Bond
derive instance Generic Bond _
instance Show Bond where
  show a = genericShow a

derive instance Eq Short
derive instance Generic Short _
instance Show Short where
  show a = genericShow a

derive instance Eq Limit
derive instance Generic Limit _
instance Show Limit where
  show a = genericShow a

derive instance (Eq cap, Eq query) => Eq (Condition cap query)
derive instance Generic (Condition cap query) _
instance (Show cap, Show query) => Show (Condition cap query) where
  show a = genericShow a

derive instance Eq law => Eq (LawChange law)
derive instance Generic (LawChange law) _
instance Show law => Show (LawChange law) where
  show a = genericShow a

derive instance (Eq law, Eq query) => Eq (Effect law query)
derive instance Generic (Effect law query) _
instance (Show law, Show query) => Show (Effect law query) where
  show a = genericShow a

derive instance Eq Solver
derive instance Generic Solver _
instance Show Solver where
  show a = genericShow a

-- | The codecs of what the kernel only carries.
type KernelShapes cap law query reason cause =
  { cap :: Shape cap, law :: Shape law, query :: Shape query, reason :: Shape reason, cause :: Shape cause }

partyShape :: Shape Party
partyShape = genericSum "Party"
  { "Named": atTagged "this Actor" idShape
  , "Bound": atTagged "the party a template binds under this name" stringShape
  }

accountShape :: Shape Account
accountShape = genericSum "Account"
  { "HeldBy": atTagged "what a party holds" partyShape
  , "Reserve": atTagged "an Exchange's reserve" idShape
  , "Created": atTagged "from outside the economy" unitShape
  , "Destroyed": atTagged "out of the economy" unitShape
  }

stockRefShape :: Shape StockRef
stockRefShape = recordShape "StockRef" { at: accountShape, resource: idShape }

amountShape :: forall query. Shape query -> Shape (Amount query)
amountShape query = recursive "Amount" \self ->
  let
    pair = recordShape "AmountPair" { left: self, right: self }
  in
    genericSum "Amount"
      { "Exactly": atTagged "this quantity" qShape
      , "Arg": atTagged "a frozen argument" stringShape
      , "Stock": atTagged "what a stock holds" stockRefShape
      , "Times": atTagged "a ratio of an amount" (recordShape "Times" { ratio: qShape, amount: self })
      , "Plus": atTagged "the sum" pair
      , "Minus": atTagged "the difference" pair
      , "Least": atTagged "the smaller" pair
      , "ShareOf": atTagged "a pro-rata share of a limit" (recordShape "ShareOf" { limit: self, among: arrayOf self, index: intShape })
      , "Read": atTagged "a read of the record" query
      , "Moved": atTagged "what the consumed obligation moved" unitShape
      , "Remainder": atTagged "what it owed and did not move" unitShape
      }

flowOwedShape :: forall query. Shape query -> Shape (FlowOwed query)
flowOwedShape query = recordShape "FlowOwed" { from: accountShape, to: accountShape, resource: idShape, owed: amountShape query }

bondShape :: Shape Bond
bondShape = genericSum "Bond"
  { "Whole": atTagged "all or nothing" unitShape
  , "Proportional": atTagged "in proportion, at this rate" qShape
  }

shortShape :: Shape Short
shortShape = genericEnum "Short"

limitShape :: Shape Limit
limitShape = genericSum "Limit"
  { "AtMost": atTagged "no higher than" qShape
  , "AtLeast": atTagged "no lower than" qShape
  }

conditionShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Condition cap query)
conditionShape k = genericSum "Condition"
  { "Performed": atTagged "another obligation stands" (recordShape "PerformedOn" { index: intShape, bond: bondShape })
  , "StockAtLeast": atTagged "a stock holds at least" (recordShape "StockAtLeast" { stock: stockRefShape, amount: amountShape k.query })
  , "Consent": atTagged "these parties signed" (arrayOf idShape)
  , "WeightedConsent": atTagged "signed weight exceeds a threshold" (recordShape "WeightedConsent" { weights: mapOf idShape qShape, residual: qShape, threshold: qShape })
  , "PriceWithin": atTagged "the cleared price is within a limit" limitShape
  , "QuantityAtLeast": atTagged "the quantity is at least" (amountShape k.query)
  , "Holds": atTagged "a party holds a capability" (recordShape "HoldsCapability" { party: partyShape, capability: k.cap })
  }

guardedShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Guarded cap query reason)
guardedShape k = recordShape "Guarded" { condition: conditionShape k, reason: optionalOf k.reason }

dynamicChangeShape :: forall law. Shape law -> Shape (LawChange law)
dynamicChangeShape law = genericSum "LawChange"
  { "SetLaw": atTagged "put a law in force" (recordShape "SetLaw" { id: stringShape, law })
  , "RemoveLaw": atTagged "remove a law" stringShape
  }

effectShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Effect law query)
effectShape k = genericSum "Effect"
  { "Move": atTagged "a leg" (flowOwedShape k.query)
  , "Change": atTagged "a change to the laws" (dynamicChangeShape k.law)
  }

spawnShape :: forall query. Shape query -> Shape (Spawn query)
spawnShape query = recordShape "Spawn" { template: stringShape, due: intShape, args: mapOf stringShape (amountShape query), parties: mapOf stringShape partyShape }

nextShape :: forall query. Shape query -> Shape (Next query)
nextShape query = recordShape "Next" { performed: arrayOf (spawnShape query), short: arrayOf (spawnShape query), lapsed: arrayOf (spawnShape query) }

obligationShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Obligation cap law query reason)
obligationShape k = recordShape "Obligation" { id: stringShape, obligor: partyShape, effect: effectShape k, when: arrayOf (guardedShape k), onShort: shortShape, next: nextShape k.query }

holdShape :: forall query. Shape query -> Shape (Hold query)
holdShape query = recordShape "Hold" { party: partyShape, resource: idShape, amount: amountShape query }

solverShape :: Shape Solver
solverShape = genericSum "Solver"
  { "Clearing": atTagged "clear on an Exchange's curve" (recordShape "Clearing" { exchange: idShape })
  , "Routing": atTagged "split across Exchanges" (recordShape "Routing" { base: idShape, quote: idShape, limit: qShape, candidates: arrayOf idShape })
  , "Pool": atTagged "share what can be drawn" (recordShape "Pool" { resource: optionalOf idShape, perUnit: qShape })
  }

rationShape :: Shape Ration
rationShape = recordShape "Ration" { tier: intShape, whole: boolShape, scaleGroup: optionalOf idShape }

transactionShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Transaction cap law query reason cause)
transactionShape k = recordShape "Transaction"
  { obligations: arrayOf (obligationShape k)
  , holds: arrayOf (holdShape k.query)
  , solver: optionalOf solverShape
  , ration: rationShape
  , phase: intShape
  , cause: optionalOf k.cause
  , consented: arrayOf idShape
  }

templateShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Template cap law query reason cause)
templateShape k = recordShape "Template" { params: arrayOf stringShape, parties: arrayOf stringShape, transaction: transactionShape k }

pendingShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Pending cap law query reason cause)
pendingShape k = recordShape "Pending" { due: intShape, transaction: transactionShape k }

derive instance Eq Subject
derive instance Generic Subject _
instance Show Subject where
  show a = genericShow a

derive instance Eq Trigger
derive instance Generic Trigger _
instance Show Trigger where
  show a = genericShow a

derive instance Eq query => Eq (Operation query)
derive instance Generic (Operation query) _
instance Show query => Show (Operation query) where
  show a = genericShow a

subjectShape :: Shape Subject
subjectShape = genericSum "Subject"
  { "OfStock": atTagged "a stock" (recordShape "StockScope" { holders: optionalOf (arrayOf idShape), resource: idShape })
  , "OfFlow": atTagged "a flow" (recordShape "FlowScope" { from: arrayOf idShape, to: arrayOf idShape, resources: arrayOf idShape })
  }

triggerShape :: Shape Trigger
triggerShape = genericSum "Trigger"
  { "EachRound": atTagged "every round" unitShape
  , "OnAction": atTagged "when an action settles" (recordShape "ActionScope" { obligor: optionalOf idShape, cause: optionalOf stringShape })
  }

operationShape :: forall query. Shape query -> Shape (Operation query)
operationShape query = genericSum "Operation"
  { "Emit": atTagged "emit a leg" (flowOwedShape query)
  , "Refuse": atTagged "refuse" unitShape
  , "Withhold": atTagged "hold back from being drawn" (amountShape query)
  }

dynamicShape :: forall cap law query reason cause. KernelShapes cap law query reason cause -> Shape (Law cap query reason cause)
dynamicShape k = recordShape "Law" { subject: subjectShape, trigger: triggerShape, operation: operationShape k.query, guard: arrayOf (guardedShape k), phase: intShape, cause: k.cause }
