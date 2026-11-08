-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed(..), drawVersion)
import Core.Num (Q, qShape)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Shape (atTagged, genericSum, recordShape, Format(..), Shape, arrayOf, boolShape, class Described, decodeJsonWith, encodeJsonWith, fieldOf, intShape, iso, mapOf, object, optionalOf, prim, stringShape, unitShape)
import Core.Types (RecurringFlowId, RecurringKindId, RuleKindDecl, RuleKindId, ruleKindDeclShape, Transferability, CapacityEffect, CostFunction, Estimator, Id, Interval, BehaviorDomain, Entity, EntityId, Market, MarketId, Price, Proposal, ProposalId, Quantity, Ratio, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, SignedProposal, Payment, Transformation, TransformationId, CapacityRef, entityShape, idShape, transferabilityShape, proposalShape, ruleKind, ruleShape, signedProposalShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Either (note)
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
  , numeraire :: ResourceId -- the premise's unit of account; only the views read it
  , flowGates :: Array FlowGate -- the conditions on Transfers, per Resource
  , groups :: Map EntityId EntityId -- the group an Entity counts as, when not itself
  , ruleKinds :: Map RuleKindId RuleKindDecl -- the kinds of Rule the premise declares
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
-- | destination each round. The DSL compiles a premise's recurring forms into these; `kind` is the
-- | name it declares for the form, and Core reads none of them.
type RecurringFlow =
  { id :: RecurringFlowId
  , kind :: RecurringKindId
  , source :: RecurringSource
  , destination :: RecurringDestination
  , resource :: ResourceId
  , amount :: RecurringAmount
  , condition :: Maybe { holder :: EntityId, resource :: ResourceId, atLeast :: Quantity } -- the rule applies only while the holder holds at least this much
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
  | ShareOf { ratio :: Ratio, basis :: ShareBasis } -- a fraction of what the source received this round
  | PurchasesThisRound -- what the source bought of the Resource on Markets this round
  | FractionHeld Ratio -- a fraction of what the source holds

-- | What a share is a fraction of, from the source's settlement this round.
data ShareBasis
  = NetSales ResourceId -- net receipts from selling this Resource on Markets
  | PaymentsReceived -- what it received as the payee of a conversion

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
-- | It points at the thing; a premise's name for it is on the thing.
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

derive instance Eq ShareBasis
derive instance Generic ShareBasis _
instance Show ShareBasis where
  show = genericShow

derive instance Eq ShortfallEffect
derive instance Generic ShortfallEffect _
instance Show ShortfallEffect where
  show = genericShow

instance EncodeJson Endpoint where
  encodeJson = encodeJsonWith Snapshot endpointShape

instance DecodeJson Endpoint where
  decodeJson = decodeJsonWith Snapshot endpointShape

instance EncodeJson Mechanism where
  encodeJson = encodeJsonWith Snapshot mechanismShape

instance DecodeJson Mechanism where
  decodeJson = decodeJsonWith Snapshot mechanismShape

instance EncodeJson Cause where
  encodeJson = encodeJsonWith Snapshot causeShape

instance DecodeJson Cause where
  decodeJson = decodeJsonWith Snapshot causeShape

instance EncodeJson RecurringSource where
  encodeJson = encodeJsonWith Snapshot recurringSourceShape

instance DecodeJson RecurringSource where
  decodeJson = decodeJsonWith Snapshot recurringSourceShape

instance EncodeJson RecurringDestination where
  encodeJson = encodeJsonWith Snapshot recurringDestinationShape

instance DecodeJson RecurringDestination where
  decodeJson = decodeJsonWith Snapshot recurringDestinationShape

instance EncodeJson RecurringAmount where
  encodeJson = encodeJsonWith Snapshot recurringAmountShape

instance DecodeJson RecurringAmount where
  decodeJson = decodeJsonWith Snapshot recurringAmountShape

instance EncodeJson ShareBasis where
  encodeJson = encodeJsonWith Snapshot shareBasisShape

instance DecodeJson ShareBasis where
  decodeJson = decodeJsonWith Snapshot shareBasisShape

instance EncodeJson ShortfallEffect where
  encodeJson = encodeJsonWith Snapshot shortfallEffectShape

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

-- | Why the engine rejected a Proposal. Each constructor names one check, so a test or a view
-- | matches on the specific failure. `renderReason` gives the text the record stores and the
-- | dashboard shows; it is the only place that text is written.
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
  | Recorded String -- a reason read back from the store: the text is all the record keeps

-- | Stored as text by decision (docs/02 §9, "Rejection reasons in records").
-- | Two reasons are equal when their texts are: a reason read back from the store is its text, so
-- | a stored event compares equal to the event that was recorded.
instance Eq RejectReason where
  eq a b = renderReason a == renderReason b

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
  Recorded text -> text

-- | The only place a rejection reason is stored: its rendered text, so the record is unchanged.
instance EncodeJson RejectReason where
  encodeJson = encodeJsonWith Snapshot rejectReasonShape

instance DecodeJson RejectReason where
  decodeJson = decodeJsonWith Snapshot rejectReasonShape

-- | A Rejected event for a Proposal the engine turned down in `round`.
rejection :: RoundIndex -> ProposalId -> RejectReason -> Event
rejection round proposal reason = Rejected { round, proposal, reason }

-- | A Lapsed event for a Proposal whose signature rule was not met in `round`.
lapse :: RoundIndex -> ProposalId -> Event
lapse round proposal = Lapsed { round, proposal }

-- | An OrderRemoved event for one order of a Proposal dropped at settlement in `round`.
orderRemoved :: RoundIndex -> ProposalId -> RejectReason -> Event
orderRemoved round proposal reason = OrderRemoved { round, proposal, reason }

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

-- Shapes. The rejection reason is stored as its text; the event, target and View
-- shapes carry the closure the model and the Run Store encode.

rejectReasonShape :: Shape RejectReason
rejectReasonShape = prim "RejectReason" (fromString <<< renderReason) (\j -> map Recorded (note "expected a rejection reason text" (toString j)))

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
viewShape = object "View"
  ( (\r s p rs tr e f t m es ts oc rk -> { round: r, self: s, prices: p, rules: rs, transferability: tr, events: e, factors: f, toSign: t, markets: m, entities: es, transformations: ts, ownedCapacities: oc, ruleKinds: rk })
      <$> fieldOf "round" "the round" intShape _.round
      <*> fieldOf "self" "the Entity seeing it" entityShape _.self
      <*> fieldOf "prices" "the price of each Market" (mapOf idShape qShape) _.prices
      <*> fieldOf "rules" "the Rules in force, by id" (mapOf idShape ruleShape) _.rules
      <*> fieldOf "transferability" "how each Resource may move" (mapOf idShape transferabilityShape) _.transferability
      <*> fieldOf "events" "the public history" (arrayOf eventShape) _.events
      <*> fieldOf "factors" "the factors of each behaviour domain" (mapOf idShape qShape) _.factors
      <*> fieldOf "toSign" "the Proposals the Entity is asked to sign" (arrayOf proposalShape) _.toSign
      <*> fieldOf "markets" "the Markets the Entity has access to" (arrayOf venueShape) _.markets
      <*> fieldOf "entities" "the known Entities" (arrayOf idShape) _.entities
      <*> fieldOf "transformations" "the known Transformations and their order bounds" (mapOf idShape boundsShape) _.transformations
      <*> fieldOf "ownedCapacities" "the capacities the Entity owns" (arrayOf idShape) _.ownedCapacities
      <*> fieldOf "ruleKinds" "the kinds of Rule declared" (mapOf idShape ruleKindDeclShape) _.ruleKinds
  )

boundsShape :: Shape Bounds
boundsShape = object "Bounds"
  ( (\a b -> { minimum: a, maximum: b })
      <$> fieldOf "minimum" "the smallest order accepted" qShape _.minimum
      <*> fieldOf "maximum" "the largest order accepted, if any" (optionalOf qShape) _.maximum
  )

venueShape :: Shape Venue
venueShape = object "Venue"
  ( (\i b q -> { id: i, base: b, quote: q })
      <$> fieldOf "id" "the Market" idShape _.id
      <*> fieldOf "base" "the Resource traded" idShape _.base
      <*> fieldOf "quote" "the Resource it is priced in" idShape _.quote
  )

-- | A round's record as the Run Store holds it.
roundRecordShape :: Shape RoundRecord
roundRecordShape = object "RoundRecord"
  ( (\r i e -> { round: r, inputs: i, events: e })
      <$> fieldOf "round" "the round" intShape _.round
      <*> fieldOf "inputs" "every Proposal submitted in the round, with its signatures" (arrayOf signedProposalShape) _.inputs
      <*> fieldOf "events" "the round's events" (arrayOf eventShape) _.events
  )

instance Described Target where
  shape = targetShape

instance Described Event where
  shape = eventShape

instance EncodeJson Target where
  encodeJson = encodeJsonWith Snapshot targetShape

instance DecodeJson Target where
  decodeJson = decodeJsonWith Snapshot targetShape

instance EncodeJson Event where
  encodeJson = encodeJsonWith Snapshot eventShape

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
resourceDefinitionShape = recordShape "ResourceDefinition" { id: idShape, unit: stringShape, transferability: transferabilityShape, reservable: boolShape }

estimatorShape :: Shape Estimator
estimatorShape = recordShape "Estimator" { reference: qShape, shrinkage: qShape, deviationBound: (optionalOf intervalShape), noiseSpread: qShape }

representationShape :: Shape Representation
representationShape = recordShape "Representation" { id: idShape, actor: idShape, aggregate: idShape, domain: idShape, weight: qShape, estimator: estimatorShape }

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
  , condition: optionalOf (recordShape "Condition" { holder: idShape, resource: idShape, atLeast: qShape })
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
  , "ShareOf": atTagged "a share of what the source received" (recordShape "ShareOf" { ratio: qShape, basis: shareBasisShape })
  , "PurchasesThisRound": atTagged "this round's Market purchases of the Resource" unitShape
  , "FractionHeld": atTagged "a fraction of what the source holds" qShape
  }

shareBasisShape :: Shape ShareBasis
shareBasisShape = genericSum "ShareBasis"
  { "NetSales": atTagged "net receipts from selling this Resource" idShape
  , "PaymentsReceived": atTagged "payments received as a payee" unitShape
  }

shortfallEffectShape :: Shape ShortfallEffect
shortfallEffectShape = genericSum "ShortfallEffect"
  { "NoEffect": atTagged "nothing" unitShape
  , "OffsetIncoming": atTagged "accrue against the flows the Entity receives, per unit short" qShape
  }

flowGateShape :: Shape FlowGate
flowGateShape = recordShape "FlowGate" { resource: idShape, exemptSharedMarket: boolShape, bundled: optionalOf idShape }

worldStateShape :: Shape WorldState
worldStateShape = recordShape "WorldState" { round: intShape, seed: (iso Seed unwrap intShape), entities: (mapOf idShape entityShape), resources: (mapOf idShape resourceDefinitionShape), markets: (mapOf idShape marketShape), transformations: (mapOf idShape transformationShape), representations: (arrayOf representationShape), numeraire: idShape, flowGates: (arrayOf flowGateShape), groups: (mapOf idShape idShape), ruleKinds: (mapOf idShape ruleKindDeclShape), rules: (mapOf idShape ruleShape), scheduled: (mapOf idShape (optionalOf ruleShape)), deliveries: (arrayOf deliveryShape), factors: (mapOf idShape holdingsShape), recurring: (arrayOf recurringFlowShape), events: (arrayOf eventShape) }

manifestShape :: Shape Manifest
manifestShape = recordShape "Manifest" { gameboard: stringShape, engine: stringShape, draw: stringShape, initial: worldStateShape }
