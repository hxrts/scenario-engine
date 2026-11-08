-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed(..), drawVersion)
import Core.Num (Q, qShape)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Shape (Format(..), Shape, arrayOf, atTagged, boolShape, decodeJsonWith, encodeWith, field, genericEnum, genericSum, intShape, iso, mapOf, optionalOf, pairOf, recordOf, recordShape, stringShape, unitShape, (&.))
import Core.Types (BehaviorDomain, CapacityEffect, CapacityRef, CostFunction, Entity, EntityId, Estimator, Id, Interval, Market, MarketId, Payment, Price, Proposal, ProposalId, Quantity, Ratio, RecurringFlowId, RecurringKindId, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleId, RuleKindDecl, RuleKindId, SignedProposal, Transferability, Transformation, TransformationId, entityShape, idShape, proposalShape, ruleKind, ruleKindDeclShape, ruleShape, signedProposalShape, transferabilityShape)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)

-- | The condition on a Transfer of one Resource: whether parties who share a Market for it are exempt,
-- | and the Transformation a sender's orders of which must carry it. A Transfer that is exempt is also
-- | outside every flows Rule.
type FlowGate = { resource :: ResourceId, exemptSharedMarket :: Boolean, bundled :: Maybe TransformationId }

-- | The World State's fields, with the seed as the row `r`. A World State has a seed only once it
-- | is instantiated (`seeded`); a decoded Gameboard's world is `UnseededWorld`.
type WorldFields r =
  ( round :: RoundIndex
  , entities :: Map EntityId Entity
  , resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , representations :: Array Representation
  , numeraire :: ResourceId -- the setting's unit of account; only the views read it
  , focus :: Maybe Focus -- the Resource the setting's market views centre on; only the views read it
  , flowGates :: Array FlowGate -- the conditions on Transfers, per Resource
  , groups :: Map EntityId EntityId -- the group an Entity counts as, when not itself
  , ruleKinds :: Map RuleKindId RuleKindDecl -- the kinds of Rule the setting declares
  , rules :: Map RuleId Rule -- Rules in force
  , scheduled :: Map RuleId (Maybe Rule) -- take effect at the next round boundary; Nothing repeals
  , deliveries :: Array Delivery -- due at Prepare
  , factors :: Map EntityId (Map BehaviorDomain Ratio) -- stored Representation adjustment factors
  , recurring :: Array RecurringFlow -- the recurring cycle, in the order the engine evaluates it
  , events :: Array Event -- public history
  | r
  )

type WorldState = Record (WorldFields (seed :: Seed)) -- seed: the game seed

-- | A World State before instantiation: every field but the seed.
type UnseededWorld = Record (WorldFields ())

-- | The World State with its game seed. Instantiation and fixtures are the only callers.
seeded :: Seed -> UnseededWorld -> WorldState
seeded seed w = Record.insert (Proxy :: Proxy "seed") seed w

-- | One step of the recurring cycle (docs/02_type_system.md §6): a Resource moving from a source to a
-- | destination each round. The DSL compiles a setting's recurring forms into these; `kind` is the
-- | name it declares for the form, and Core reads none of them.
type RecurringFlow =
  { id :: RecurringFlowId
  , kind :: RecurringKindId
  , source :: RecurringSource
  , destination :: RecurringDestination
  , resource :: ResourceId
  , amount :: RecurringAmount
  , onShortfall :: ShortfallEffect -- what a shortfall on this rule does
  , offsetable :: Boolean -- reduced pro rata by the recipient's accrued offset
  , delay :: Int -- rounds until a created Resource is delivered; 0 delivers in this round
  }

-- | Where a recurring flow comes from: one Entity, each Entity that holds the Resource, or nowhere.
data RecurringSource = FromEntity EntityId | FromEachHolder | Created

data RecurringDestination = ToEntity EntityId | ToSink

-- | How much a recurring flow moves.
data RecurringAmount
  = FixedAmount Quantity
  | CapacityHeld CapacityRef -- the destination's holding of this capacity
  | FractionHeld Ratio -- a fraction of what the source holds
  | Measured { ratio :: Ratio, query :: LegQuery } -- this fraction of what a query of the record reads

-- | Which round a query reads: this round's settlement so far, or the round before.
data Window = ThisRoundSoFar | LastRound

-- | Which of the party's legs a query reads: those into it, those out of it, or out minus in, at least zero.
data Side = Incoming | Outgoing | NetOutgoing

-- | Which settlements a query reads: a Market fill's base leg, a conversion's payment leg, or conversions of a
-- | Transformation.
data CausePattern = CurveBase | ConversionPayment | ConversionOf TransformationId

-- | What a query computes: the sum moved, the sum of rate times moved, or the largest single conversion's units.
data LegAggregate = SumMoved | SumValue | LargestUnits

-- | A question about the record (docs/02 §6): a window, a party, a side, an optional Resource, a cause
-- | pattern, and an aggregate. Stored in a setting as data.
type LegQuery =
  { window :: Window
  , party :: EntityId
  , side :: Side
  , resource :: Maybe ResourceId
  , cause :: CausePattern
  , aggregate :: LegAggregate
  }

-- | What the engine does when a recurring flow cannot move all it owes.
data ShortfallEffect
  = NoEffect
  | OffsetIncoming Quantity -- accrue this much per unit short against the offsetable flows the Entity receives

-- | A scheduled change to a holding or a capacity, applied at Prepare. `cause` is what the settlement
-- | that scheduled it was caused by.
type Delivery =
  { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity, cause :: Cause }

-- | Where a leg of a settlement starts or ends: an Entity, a Market, nowhere, or nothing.
data Endpoint = Held EntityId | InMarket MarketId | Source | Sink

derive instance Eq Endpoint
derive instance Ord Endpoint
derive instance Generic Endpoint _
instance Show Endpoint where
  show = genericShow

-- | One leg: a quantity of a Resource owed from one endpoint to another, and how much of it moved.
-- | A shortfall is `moved < owed`.
type Flow = { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Quantity, moved :: Quantity }

-- | How an Intent's quantities were settled: along a Market's curve, as a conversion under a capacity,
-- | or moved directly.
data Mechanism
  = Curve MarketId
  | Conversion { by :: EntityId, transformation :: TransformationId, units :: Quantity }
  | Direct

derive instance Eq Mechanism
derive instance Generic Mechanism _
instance Show Mechanism where
  show = genericShow

-- | What produced a settlement: an Intent of a Proposal, a declared Rule kind, or a recurring-flow rule.
-- | It points at the thing; a setting's name for it is on the thing.
data Cause
  = OfIntent { proposal :: ProposalId, intent :: Int, mechanism :: Mechanism }
  | OfRule { kind :: RuleKindId }
  | OfRecurring { rule :: RecurringFlowId, kind :: RecurringKindId }

derive instance Eq Cause
derive instance Generic Cause _
instance Show Cause where
  show = genericShow

derive instance Eq RecurringSource
derive instance Generic RecurringSource _
instance Show RecurringSource where
  show = genericShow

derive instance Eq RecurringDestination
derive instance Generic RecurringDestination _
instance Show RecurringDestination where
  show = genericShow

derive instance Eq RecurringAmount
derive instance Generic RecurringAmount _
instance Show RecurringAmount where
  show = genericShow

derive instance Eq Window
derive instance Generic Window _
instance Show Window where
  show = genericShow

derive instance Eq Side
derive instance Generic Side _
instance Show Side where
  show = genericShow

derive instance Eq CausePattern
derive instance Generic CausePattern _
instance Show CausePattern where
  show = genericShow

derive instance Eq LegAggregate
derive instance Generic LegAggregate _
instance Show LegAggregate where
  show = genericShow

derive instance Eq ShortfallEffect
derive instance Generic ShortfallEffect _
instance Show ShortfallEffect where
  show = genericShow

instance EncodeJson Endpoint where
  encodeJson = encodeWith Snapshot endpointShape

instance DecodeJson Endpoint where
  decodeJson = decodeJsonWith Snapshot endpointShape

instance EncodeJson Mechanism where
  encodeJson = encodeWith Snapshot mechanismShape

instance DecodeJson Mechanism where
  decodeJson = decodeJsonWith Snapshot mechanismShape

instance EncodeJson Cause where
  encodeJson = encodeWith Snapshot causeShape

instance DecodeJson Cause where
  decodeJson = decodeJsonWith Snapshot causeShape

instance EncodeJson RecurringSource where
  encodeJson = encodeWith Snapshot recurringSourceShape

instance DecodeJson RecurringSource where
  decodeJson = decodeJsonWith Snapshot recurringSourceShape

instance EncodeJson RecurringDestination where
  encodeJson = encodeWith Snapshot recurringDestinationShape

instance DecodeJson RecurringDestination where
  decodeJson = decodeJsonWith Snapshot recurringDestinationShape

instance EncodeJson RecurringAmount where
  encodeJson = encodeWith Snapshot recurringAmountShape

instance DecodeJson RecurringAmount where
  decodeJson = decodeJsonWith Snapshot recurringAmountShape

instance EncodeJson Window where
  encodeJson = encodeWith Snapshot windowShape

instance DecodeJson Window where
  decodeJson = decodeJsonWith Snapshot windowShape

instance EncodeJson Side where
  encodeJson = encodeWith Snapshot sideShape

instance DecodeJson Side where
  decodeJson = decodeJsonWith Snapshot sideShape

instance EncodeJson CausePattern where
  encodeJson = encodeWith Snapshot causePatternShape

instance DecodeJson CausePattern where
  decodeJson = decodeJsonWith Snapshot causePatternShape

instance EncodeJson LegAggregate where
  encodeJson = encodeWith Snapshot legAggregateShape

instance DecodeJson LegAggregate where
  decodeJson = decodeJsonWith Snapshot legAggregateShape

instance EncodeJson ShortfallEffect where
  encodeJson = encodeWith Snapshot shortfallEffectShape

instance DecodeJson ShortfallEffect where
  decodeJson = decodeJsonWith Snapshot shortfallEffectShape

-- | A settlement of the given legs.
coupled :: RoundIndex -> Array Flow -> Maybe Ratio -> Cause -> Event
coupled round legs rate cause = Coupled { round, legs, rate, cause, delivered: false }

data Target = Holding ResourceId | CapacityOf CapacityRef

-- | What a Policy sees (INV-16).
type View =
  { round :: RoundIndex
  , self :: Entity
  , prices :: Map MarketId Price
  , rules :: Map RuleId Rule -- the Rules in force, by the id a repeal names
  , transferability :: Map ResourceId Transferability
  , events :: Array Event
  , factors :: Map BehaviorDomain Ratio
  , toSign :: Array Proposal
  , markets :: Array Venue -- the Markets the Entity has access to, in id order
  , entities :: Array EntityId -- the known Entities, in id order
  , transformations :: Map TransformationId Bounds -- the known Transformations and their order bounds
  , ownedCapacities :: Array CapacityRef -- the capacities the Entity owns
  , ruleKinds :: Map RuleKindId RuleKindDecl -- the kinds of Rule declared
  }

-- | A Transformation's order bounds, as a View shows them.
type Bounds = { minimum :: Quantity, maximum :: Maybe Quantity }

-- | A Market as a Policy sees it: where a Resource (the base) is traded for the quote.
type Venue = { id :: MarketId, base :: ResourceId, quote :: ResourceId }

-- | What a Venue shows of a Market.
venueOf :: Market -> Venue
venueOf m = { id: m.id, base: m.base, quote: m.quote }

-- | What a View shows of a Transformation.
boundsOf :: Transformation -> Bounds
boundsOf t = { minimum: t.minimum, maximum: t.maximum }

-- | Why the engine rejected a Proposal. Each constructor names one check, so a test or a view
-- | matches on the specific failure. The record stores the value through `rejectReasonShape`;
-- | `renderReason` is the one place its display text is written.
data RejectReason
  = CapacityExceeded
  | LimitPriceNotMet
  | QuoteReservesShort
  | InputsNotHeld TransformationId
  | PartyCannotFund
  | JointBundlesOnlyTransfers
  | DuplicateProposalId
  | OutsideProcedureAuthority
  | UnknownProcedure
  | UnknownEntity EntityId
  | MustSign EntityId
  | NotAuthorized EntityId
  | UnknownMarket MarketId
  | UnknownResource ResourceId
  | UnknownTransformation TransformationId
  | UnknownRule RuleId
  | RouteNeedsLimit
  | NoMarketAccess EntityId MarketId
  | NoRouteAccess EntityId ResourceId ResourceId
  | NotTransferable ResourceId
  | OutOfScope Rule
  | OnlyGoverningRepeal
  | BelowMinimum TransformationId
  | AboveMaximum TransformationId
  | OwnerMustSign TransformationId
  | MissingBundledOrder
  | Blocked RuleKindId

derive instance Generic RejectReason _
derive instance Eq RejectReason

instance Show RejectReason where
  show = renderReason

-- | The text of a rejection reason.
renderReason :: RejectReason -> String
renderReason = case _ of
  CapacityExceeded -> "capacity exceeded"
  LimitPriceNotMet -> "limit price not met"
  QuoteReservesShort -> "the market cannot pay its sellers"
  InputsNotHeld t -> show t <> ": inputs are not held"
  PartyCannotFund -> "a party cannot fund the joint Proposal"
  JointBundlesOnlyTransfers -> "joint Proposals bundle only Transfers and Transforms"
  DuplicateProposalId -> "duplicate proposal id"
  OutsideProcedureAuthority -> "outside the procedure's authority"
  UnknownProcedure -> "unknown procedure"
  UnknownEntity e -> "unknown entity " <> show e
  MustSign e -> show e <> " must sign"
  NotAuthorized e -> show e <> " is not authorized"
  UnknownMarket m -> "unknown market " <> show m
  UnknownResource r -> "unknown resource " <> show r
  UnknownTransformation t -> "unknown transformation " <> show t
  UnknownRule r -> "unknown rule " <> show r
  RouteNeedsLimit -> "a routed order needs a limit price"
  NoMarketAccess e m -> show e <> " has no access to " <> show m
  NoRouteAccess e base quote -> show e <> " has no accessible market for " <> show base <> "/" <> show quote
  NotTransferable r -> show r <> " is not transferable"
  OutOfScope r -> "a Rule of kind " <> show (ruleKind r) <> " applies only to its governing Entity or owner, and only in its declared form"
  OnlyGoverningRepeal -> "only the governing authority may repeal a Rule"
  BelowMinimum t -> show t <> " order below its minimum"
  AboveMaximum t -> show t <> " order above its maximum"
  OwnerMustSign t -> "the owner of " <> show t <> "'s capacity must sign"
  MissingBundledOrder -> "this Transfer needs a matching bundled order by the sender"
  Blocked k -> "blocked by a Rule of kind " <> show k

instance EncodeJson RejectReason where
  encodeJson = encodeWith Snapshot rejectReasonShape

instance DecodeJson RejectReason where
  decodeJson = decodeJsonWith Snapshot rejectReasonShape

-- | Append events to whatever carries the record being built.
addEvents :: forall r. Array Event -> { events :: Array Event | r } -> { events :: Array Event | r }
addEvents es x = x { events = x.events <> es }

-- | A Rejected event for a Proposal the engine turned down in `round`.
rejection :: RoundIndex -> ProposalId -> RejectReason -> Event
rejection round proposal reason = Rejected { round, proposal, reason }

-- | A Lapsed event for a Proposal whose signature rule was not met in `round`.
lapse :: RoundIndex -> ProposalId -> Event
lapse round proposal = Lapsed { round, proposal }

-- | An OrderRemoved event for one order of a Proposal dropped at settlement in `round`.
orderRemoved :: RoundIndex -> ProposalId -> RejectReason -> Event
orderRemoved round proposal reason = OrderRemoved { round, proposal, reason }

-- | The Resource a setting's market views centre on, and what they call it.
type Focus = { resource :: ResourceId, label :: String }

focusShape :: Shape Focus
focusShape = recordShape "Focus" { resource: idShape, label: stringShape }

-- | Everything the engine records. The round record is the list of a round's events.
data Event
  = Lapsed { round :: RoundIndex, proposal :: ProposalId }
  | Rejected { round :: RoundIndex, proposal :: ProposalId, reason :: RejectReason }
  -- one order of a Proposal removed at settlement; the Proposal's other Intents still settle
  | OrderRemoved { round :: RoundIndex, proposal :: ProposalId, reason :: RejectReason }
  -- one settlement: its legs settle together, at its rate when it has one. `delivered` marks a delivery of
  -- something settled in an earlier round, applied at Prepare
  | Coupled { round :: RoundIndex, legs :: Array Flow, rate :: Maybe Ratio, cause :: Cause, delivered :: Boolean }
  | CapacityChanged { round :: RoundIndex, entity :: EntityId, capacity :: CapacityRef, quantity :: Quantity }
  | Enacted { round :: RoundIndex, id :: RuleId, rule :: Rule }
  | Repealed { round :: RoundIndex, id :: RuleId }
  | Estimated { round :: RoundIndex, aggregate :: EntityId, domain :: BehaviorDomain, factor :: Ratio }
  | ProcedureOutcome { round :: RoundIndex, proposal :: ProposalId, support :: Ratio, threshold :: Ratio, executed :: Boolean }

derive instance Generic Target _
derive instance Eq Target
instance Show Target where
  show = genericShow

derive instance Generic Event _
derive instance Eq Event
instance Show Event where
  show = genericShow

type RoundRecord =
  { round :: RoundIndex
  , inputs :: Array SignedProposal
  , events :: Array Event
  }

type RoundResult = { world :: WorldState, record :: RoundRecord }

-- | The immutable record of a game's resolved configuration and initial World State.
type Manifest =
  { gameboard :: String
  , engine :: String
  , draw :: String
  , initial :: WorldState
  }

-- | The engine's version, recorded in every Manifest beside `drawVersion`.
engineVersion :: String
engineVersion = "sp-engine/v1"

-- | The Manifest of a game whose configuration is named `gameboard` and which starts at `initial`.
manifestOf :: String -> WorldState -> Manifest
manifestOf gameboard initial = { gameboard, engine: engineVersion, draw: drawVersion, initial }

-- Shapes. The event, target and View shapes carry the closure the model and the Run Store encode.

rejectReasonShape :: Shape RejectReason
rejectReasonShape = genericSum "RejectReason"
  { "CapacityExceeded": atTagged "a capacity was exceeded" unitShape
  , "LimitPriceNotMet": atTagged "the limit price was not met" unitShape
  , "QuoteReservesShort": atTagged "the Market cannot pay its sellers" unitShape
  , "InputsNotHeld": atTagged "the Transformation's inputs are not held" idShape
  , "PartyCannotFund": atTagged "a party cannot fund the joint Proposal" unitShape
  , "JointBundlesOnlyTransfers": atTagged "a joint Proposal bundles something else" unitShape
  , "DuplicateProposalId": atTagged "the Proposal's id is already used" unitShape
  , "OutsideProcedureAuthority": atTagged "outside the procedure's authority" unitShape
  , "UnknownProcedure": atTagged "no such procedure" unitShape
  , "UnknownEntity": atTagged "no such Entity" idShape
  , "MustSign": atTagged "the Entity's signature is missing" idShape
  , "NotAuthorized": atTagged "the Entity is not authorized" idShape
  , "UnknownMarket": atTagged "no such Market" idShape
  , "UnknownResource": atTagged "no such Resource" idShape
  , "UnknownTransformation": atTagged "no such Transformation" idShape
  , "UnknownRule": atTagged "no such Rule" idShape
  , "RouteNeedsLimit": atTagged "a routed order has no limit price" unitShape
  , "NoMarketAccess": atTagged "the Entity has no access to the Market" (pairOf idShape idShape)
  , "NoRouteAccess": atTagged "the Entity has no accessible Market for the pair" (pairOf idShape (pairOf idShape idShape))
  , "NotTransferable": atTagged "the Resource is not transferable" idShape
  , "OutOfScope": atTagged "the Rule is outside its governor's scope or declared form" ruleShape
  , "OnlyGoverningRepeal": atTagged "only the governing authority may repeal" unitShape
  , "BelowMinimum": atTagged "the order is below the Transformation's minimum" idShape
  , "AboveMaximum": atTagged "the order is above the Transformation's maximum" idShape
  , "OwnerMustSign": atTagged "the capacity's owner has not signed" idShape
  , "MissingBundledOrder": atTagged "the Transfer has no matching bundled order" unitShape
  , "Blocked": atTagged "blocked by a Rule of this kind" idShape
  }

targetShape :: Shape Target
targetShape = genericSum "Target"
  { "Holding": atTagged "a Resource held" idShape
  , "CapacityOf": atTagged "a capacity of a Transformation" idShape
  }

endpointShape :: Shape Endpoint
endpointShape = genericSum "Endpoint"
  { "Held": atTagged "an Entity's account" idShape
  , "InMarket": atTagged "a Market's reserves" idShape
  , "Source": atTagged "where a Resource is created" unitShape
  , "Sink": atTagged "where a Resource ends" unitShape
  }

flowShape :: Shape Flow
flowShape = recordShape "Flow" { from: endpointShape, to: endpointShape, resource: idShape, owed: qShape, moved: qShape }

mechanismShape :: Shape Mechanism
mechanismShape = genericSum "Mechanism"
  { "Curve": atTagged "along a Market's curve" idShape
  , "Conversion": atTagged "a conversion under a capacity" (recordShape "Conversion" { by: idShape, transformation: idShape, units: qShape })
  , "Direct": atTagged "moved directly" unitShape
  }

causeShape :: Shape Cause
causeShape = genericSum "Cause"
  { "OfIntent": atTagged "an Intent of a Proposal" (recordShape "OfIntent" { proposal: idShape, intent: intShape, mechanism: mechanismShape })
  , "OfRule": atTagged "a declared Rule kind" (recordShape "OfRule" { kind: idShape })
  , "OfRecurring": atTagged "a recurring-flow rule" (recordShape "OfRecurring" { rule: idShape, kind: idShape })
  }

-- | Everything the engine records, as the snapshot writes it: a tag and the record of its payload.
eventShape :: Shape Event
eventShape = genericSum "Event"
  { "Lapsed": atTagged "a Proposal that lapsed" (recordShape "Lapsed" { round: intShape, proposal: idShape })
  , "Rejected": atTagged "a Proposal the engine turned down" (recordShape "Rejected" { round: intShape, proposal: idShape, reason: rejectReasonShape })
  , "OrderRemoved": atTagged "one order of a Proposal removed at settlement" (recordShape "OrderRemoved" { round: intShape, proposal: idShape, reason: rejectReasonShape })
  , "Coupled": atTagged "one settlement: its legs, its rate, and what caused it" (recordShape "Coupled" { round: intShape, legs: arrayOf flowShape, rate: optionalOf qShape, cause: causeShape, delivered: boolShape })
  , "CapacityChanged": atTagged "a capacity changed" (recordShape "CapacityChanged" { round: intShape, entity: idShape, capacity: idShape, quantity: qShape })
  , "Enacted": atTagged "a Rule enacted" (recordShape "Enacted" { round: intShape, id: idShape, rule: ruleShape })
  , "Repealed": atTagged "a Rule repealed" (recordShape "Repealed" { round: intShape, id: idShape })
  , "Estimated": atTagged "a representation's factor estimated" (recordShape "Estimated" { round: intShape, aggregate: idShape, domain: idShape, factor: qShape })
  , "ProcedureOutcome": atTagged "a collective procedure's outcome" (recordShape "ProcedureOutcome" { round: intShape, proposal: idShape, support: qShape, threshold: qShape, executed: boolShape })
  }

-- | A View: what a Policy or a seat sees (INV-16).
viewShape :: Shape View
viewShape = recordOf "View"
  ( field @"round" "the round" intShape
      &. field @"self" "the Entity seeing it" entityShape
      &. field @"prices" "the price of each Market" (mapOf idShape qShape)
      &. field @"rules" "the Rules in force, by id" (mapOf idShape ruleShape)
      &. field @"transferability" "how each Resource may move" (mapOf idShape transferabilityShape)
      &. field @"events" "the public history" (arrayOf eventShape)
      &. field @"factors" "the factors of each behaviour domain" (mapOf idShape qShape)
      &. field @"toSign" "the Proposals the Entity is asked to sign" (arrayOf proposalShape)
      &. field @"markets" "the Markets the Entity has access to" (arrayOf venueShape)
      &. field @"entities" "the known Entities" (arrayOf idShape)
      &. field @"transformations" "the known Transformations and their order bounds" (mapOf idShape boundsShape)
      &. field @"ownedCapacities" "the capacities the Entity owns" (arrayOf idShape)
      &. field @"ruleKinds" "the kinds of Rule declared" (mapOf idShape ruleKindDeclShape)
  )

boundsShape :: Shape Bounds
boundsShape = recordOf "Bounds"
  ( field @"minimum" "the smallest order accepted" qShape
      &. field @"maximum" "the largest order accepted, if any" (optionalOf qShape)
  )

venueShape :: Shape Venue
venueShape = recordOf "Venue"
  ( field @"id" "the Market" idShape
      &. field @"base" "the Resource traded" idShape
      &. field @"quote" "the Resource it is priced in" idShape
  )

-- | A round's record as the Run Store holds it.
roundRecordShape :: Shape RoundRecord
roundRecordShape = recordOf "RoundRecord"
  ( field @"round" "the round" intShape
      &. field @"inputs" "every Proposal submitted in the round, with its signatures" (arrayOf signedProposalShape)
      &. field @"events" "the round's events" (arrayOf eventShape)
  )

instance EncodeJson Target where
  encodeJson = encodeWith Snapshot targetShape

instance DecodeJson Target where
  decodeJson = decodeJsonWith Snapshot targetShape

instance EncodeJson Event where
  encodeJson = encodeWith Snapshot eventShape

instance DecodeJson Event where
  decodeJson = decodeJsonWith Snapshot eventShape

-- The stored run ------------------------------------------------------------------------------
--
-- A Run Store row holds these shapes' snapshot form: its bytes are what argonaut's generic record
-- encoder writes (test: "stored records encode the same"), so stored rows read back unchanged.

intervalShape :: Shape Interval
intervalShape = recordShape "Interval" { lo: qShape, hi: qShape }

costFunctionShape :: Shape CostFunction
costFunctionShape = recordShape "CostFunction" { intercept: qShape, slope: qShape, domain: intervalShape }

holdingsShape :: forall t. Shape (Map (Id t) Q)
holdingsShape = mapOf idShape qShape

marketShape :: Shape Market
marketShape = recordShape "Market" { id: idShape, base: idShape, quote: idShape, cost: costFunctionShape, position: qShape, reserves: holdingsShape, access: (arrayOf idShape) }

capacityEffectShape :: Shape CapacityEffect
capacityEffectShape = recordShape "CapacityEffect" { capacity: idShape, holder: (optionalOf idShape), perUnit: qShape, rounds: (optionalOf intShape) }

paymentShape :: Shape Payment
paymentShape = recordShape "Payment" { curve: costFunctionShape, resource: idShape, to: idShape }

transformationShape :: Shape Transformation
transformationShape = recordShape "Transformation" { id: idShape, inputs: holdingsShape, outputs: holdingsShape, escrowPerUnit: holdingsShape, payment: (optionalOf paymentShape), capacity: (optionalOf idShape), delay: intShape, minimum: qShape, maximum: (optionalOf qShape), capacityUse: qShape, ownerSigns: boolShape, effects: (arrayOf capacityEffectShape) }

resourceDefinitionShape :: Shape ResourceDefinition
resourceDefinitionShape = recordShape "ResourceDefinition" { id: idShape, unit: stringShape, transferability: transferabilityShape }

estimatorShape :: Shape Estimator
estimatorShape = recordShape "Estimator" { reference: qShape, shrinkage: qShape, deviationBound: (optionalOf intervalShape), noiseSpread: qShape }

representationShape :: Shape Representation
representationShape = recordShape "Representation" { id: idShape, actor: idShape, aggregate: idShape, domain: idShape, observes: idShape, weight: qShape, estimator: estimatorShape }

deliveryShape :: Shape Delivery
deliveryShape = recordShape "Delivery" { round: intShape, entity: idShape, target: targetShape, quantity: qShape, cause: causeShape }

recurringFlowShape :: Shape RecurringFlow
recurringFlowShape = recordShape "RecurringFlow"
  { id: idShape
  , kind: idShape
  , source: recurringSourceShape
  , destination: recurringDestinationShape
  , resource: idShape
  , amount: recurringAmountShape
  , onShortfall: shortfallEffectShape
  , offsetable: boolShape
  , delay: intShape
  }

recurringSourceShape :: Shape RecurringSource
recurringSourceShape = genericSum "RecurringSource"
  { "FromEntity": atTagged "one Entity" idShape
  , "FromEachHolder": atTagged "each Entity that holds the Resource" unitShape
  , "Created": atTagged "created in the destination" unitShape
  }

recurringDestinationShape :: Shape RecurringDestination
recurringDestinationShape = genericSum "RecurringDestination"
  { "ToEntity": atTagged "one Entity" idShape
  , "ToSink": atTagged "nothing receives it" unitShape
  }

recurringAmountShape :: Shape RecurringAmount
recurringAmountShape = genericSum "RecurringAmount"
  { "FixedAmount": atTagged "a fixed quantity" qShape
  , "CapacityHeld": atTagged "what the destination holds of this capacity" idShape
  , "FractionHeld": atTagged "a fraction of what the source holds" qShape
  , "Measured": atTagged "a fraction of what a query of the record reads" (recordShape "Measured" { ratio: qShape, query: legQueryShape })
  }

windowShape :: Shape Window
windowShape = genericEnum "Window"

sideShape :: Shape Side
sideShape = genericEnum "Side"

causePatternShape :: Shape CausePattern
causePatternShape = genericSum "CausePattern"
  { "CurveBase": atTagged "the base leg of a Market fill" unitShape
  , "ConversionPayment": atTagged "a conversion's payment leg" unitShape
  , "ConversionOf": atTagged "conversions of this Transformation" idShape
  }

legAggregateShape :: Shape LegAggregate
legAggregateShape = genericEnum "LegAggregate"

legQueryShape :: Shape LegQuery
legQueryShape = recordShape "LegQuery" { window: windowShape, party: idShape, side: sideShape, resource: optionalOf idShape, cause: causePatternShape, aggregate: legAggregateShape }

shortfallEffectShape :: Shape ShortfallEffect
shortfallEffectShape = genericSum "ShortfallEffect"
  { "NoEffect": atTagged "nothing" unitShape
  , "OffsetIncoming": atTagged "accrue against the flows the Entity receives, per unit short" qShape
  }

flowGateShape :: Shape FlowGate
flowGateShape = recordShape "FlowGate" { resource: idShape, exemptSharedMarket: boolShape, bundled: optionalOf idShape }

worldStateShape :: Shape WorldState
worldStateShape = recordShape "WorldState" { round: intShape, seed: (iso Seed unwrap intShape), entities: (mapOf idShape entityShape), resources: (mapOf idShape resourceDefinitionShape), markets: (mapOf idShape marketShape), transformations: (mapOf idShape transformationShape), representations: (arrayOf representationShape), numeraire: idShape, focus: optionalOf focusShape, flowGates: (arrayOf flowGateShape), groups: (mapOf idShape idShape), ruleKinds: (mapOf idShape ruleKindDeclShape), rules: (mapOf idShape ruleShape), scheduled: (mapOf idShape (optionalOf ruleShape)), deliveries: (arrayOf deliveryShape), factors: (mapOf idShape holdingsShape), recurring: (arrayOf recurringFlowShape), events: (arrayOf eventShape) }

manifestShape :: Shape Manifest
manifestShape = recordShape "Manifest" { gameboard: stringShape, engine: stringShape, draw: stringShape, initial: worldStateShape }
