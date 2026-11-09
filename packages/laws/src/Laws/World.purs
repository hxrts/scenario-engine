-- | World State, Views, events, and round records
-- | (docs/02_kernel.md §4, §8; docs/04_run.md §9).
module Laws.World (module Laws.World, module Core.Kernel.Types) where

import Prelude

import Core.Kernel.Types as K
import Core.Map (values)
import Data.Array as Array
import Laws.Types (RefKind(..), idText, capabilityShape, BehaviorDomain, Capability(..), CostFunction, DelayedFlow, Draw, Actor, ActorId, ActorKind(..), Estimator, FlowEffect(..), Holdings, Id, Interval, LevyBasis(..), Exchange, ExchangeId, Payment, Price, CandidateTransaction, CandidateTransactionId, Quantity, Ratio, RecurringFlowId, RecurringKindId, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, RuleKindDecl, RuleKindId, SignedCandidateTransaction, Transferability, Transformation, TransformationId, drawSourceShape, actorShape, idShape, candidateTransactionShape, ruleKind, ruleKindDeclShape, ruleShape, signedCandidateTransactionShape, transferabilityShape)
import Data.Maybe (Maybe(..))

import Core.Draw (Seed(..), drawVersion)
import Core.Kernel.Types (Law, Endpoint(..), Flow, KernelShapes, Pending, dynamicShape, Template, TemplateId, endpointShape, flowShape, pendingShape, templateShape)
import Core.Num (Q, qShape)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Shape (Format(..), Shape, arrayOf, atTagged, boolShape, decodeJsonWith, encodeWith, field, genericEnum, genericSum, intShape, iso, mapOf, optionalOf, pairOf, recordOf, recordShape, stringShape, unitShape, (&.))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Const (Const(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, un, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Show.Generic (genericShow)

-- | A spawned transaction and a template, over what this World carries.
newtype PendingTransaction = PendingTransaction (Pending Capability Rule LegQuery RejectReason Cause)

newtype AuthoredLaw = AuthoredLaw (Law Capability LegQuery RejectReason Cause)

derive instance Newtype AuthoredLaw _
derive instance Eq AuthoredLaw
derive newtype instance Show AuthoredLaw

instance EncodeJson AuthoredLaw where
  encodeJson = encodeWith Snapshot authoredLawShape

instance DecodeJson AuthoredLaw where
  decodeJson = decodeJsonWith Snapshot authoredLawShape

authoredLawShape :: Shape AuthoredLaw
authoredLawShape = iso AuthoredLaw unwrap (dynamicShape worldKernelShapes)

newtype WorldTemplate = WorldTemplate (Template Capability Rule LegQuery RejectReason Cause)

derive instance Newtype PendingTransaction _
derive instance Newtype WorldTemplate _
derive instance Eq PendingTransaction
derive instance Eq WorldTemplate

derive newtype instance Show PendingTransaction

derive newtype instance Show WorldTemplate

instance EncodeJson PendingTransaction where
  encodeJson = encodeWith Snapshot pendingTransactionShape

instance DecodeJson PendingTransaction where
  decodeJson = decodeJsonWith Snapshot pendingTransactionShape

instance EncodeJson WorldTemplate where
  encodeJson = encodeWith Snapshot worldTemplateShape

instance DecodeJson WorldTemplate where
  decodeJson = decodeJsonWith Snapshot worldTemplateShape

worldKernelShapes :: KernelShapes Capability Rule LegQuery RejectReason Cause
worldKernelShapes = { cap: capabilityShape, law: ruleShape, query: legQueryShape, reason: rejectReasonShape, cause: causeShape }

pendingTransactionShape :: Shape PendingTransaction
pendingTransactionShape = iso PendingTransaction unwrap (pendingShape worldKernelShapes)

worldTemplateShape :: Shape WorldTemplate
worldTemplateShape = iso WorldTemplate unwrap (templateShape worldKernelShapes)

-- | The condition on a Transfer of one Resource: whether parties who share an Exchange for it are exempt,
-- | and the Transformation a sender's orders of which must carry it. A Transfer that is exempt is also
-- | outside every flows Rule.
type FlowGate = { resource :: ResourceId, exemptSharedExchange :: Boolean, bundled :: Maybe TransformationId }

-- | The World State's fields, with the seed as the row `r`. A World State has a seed only once it
-- | is instantiated (`seeded`); a decoded possible World's world is `UnseededWorld`.
type WorldFields r =
  ( round :: RoundIndex
  , actors :: Map ActorId Actor
  , resources :: Map ResourceId ResourceDefinition
  , exchanges :: Map ExchangeId Exchange
  , transformations :: Map TransformationId Transformation
  , representations :: Array Representation
  , numeraire :: ResourceId -- the premise's unit of account; only the views read it
  , focus :: Maybe Focus -- the Resource the premise's exchange views centre on; only the views read it
  , flowGates :: Array FlowGate -- the conditions on Transfers, per Resource
  , groups :: Map ActorId ActorId -- the group an Actor counts as, when not itself
  , ruleKinds :: Map RuleKindId RuleKindDecl -- the kinds of Rule the premise declares
  , rules :: Map RuleId Rule -- Rules in force
  , pending :: Array PendingTransaction -- spawned transactions, each due in a later round
  , laws :: Array AuthoredLaw -- laws the premise writes directly, run when their action settles
  , templates :: Map TemplateId WorldTemplate -- what a spawn may instantiate
  , factors :: Map ActorId (Map BehaviorDomain Ratio) -- stored Representation adjustment factors
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

-- | One step of the recurring cycle (docs/02_kernel.md §6): a Resource moving from a source to a
-- | destination each round. The DSL compiles a premise's recurring forms into these; `kind` is the
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

-- | Where a recurring flow comes from: one Actor, each Actor that holds the Resource, or nowhere.
data RecurringSource = FromActor ActorId | FromEachHolder | Created

data RecurringDestination = ToActor ActorId | ToSink

-- | How much a recurring flow moves.
data RecurringAmount
  = FixedAmount Quantity
  | HeldAmount ResourceId -- the destination's holding of this Resource
  | FractionHeld Ratio -- a fraction of what the source holds
  | Measured { ratio :: Ratio, query :: LegQuery } -- this fraction of what a query of the record reads

-- | Which round a query reads: this round's settlement so far, or the round before.
data Window = ThisRoundSoFar | LastRound

-- | Which of the party's settled Flows a query reads: those into it, those out of it, or out minus in, at least zero.
data Side = Incoming | Outgoing | NetOutgoing

-- | Which settlements a query reads: an Exchange fill's base settled Flow, a transformation's payment settled Flow, or transformationFills of a
-- | Transformation.
data CausePattern = CurveBase | TransformationPayment | TransformationOf TransformationId

-- | What a query computes: the sum moved, the sum of rate times moved, or the largest single transformation's units.
data LegAggregate = SumMoved | SumValue | LargestUnits

-- | A question about the record (docs/02 §6): a window, a party, a side, an optional Resource, a cause
-- | pattern, and an aggregate. Stored in a premise as data.
type LegQuery =
  { window :: Window
  , party :: ActorId
  , side :: Side
  , resource :: Maybe ResourceId
  , cause :: CausePattern
  , aggregate :: LegAggregate
  }

-- | What the engine does when a recurring flow cannot move all it owes.
data ShortfallEffect
  = NoEffect
  | OffsetIncoming Quantity -- accrue this much per unit short against the offsetable flows the Actor receives

-- | Where a settled Flow of a settlement starts or ends: an Actor, an Exchange, nowhere, or nothing.
-- | One settled Flow: a quantity of a Resource owed from one endpoint to another, and how much of it moved.
-- | A shortfall is `moved < owed`.
-- | How an Intent's quantities were settled: along an Exchange's curve, as a transformation, or moved directly.
data Mechanism
  = Curve ExchangeId
  | Transformation { by :: ActorId, transformation :: TransformationId, units :: Quantity }
  | Direct

derive instance Eq Mechanism
derive instance Generic Mechanism _
instance Show Mechanism where
  show = genericShow

-- | What produced a settlement: an Intent of a candidate Transaction, a declared Rule kind, or a recurring-flow rule.
-- | It points at the thing; a premise's name for it is on the thing.
data Cause
  = OfIntent { candidateTransaction :: CandidateTransactionId, intent :: Int, mechanism :: Mechanism }
  | OfRule { kind :: RuleKindId }
  | OfRecurring { rule :: RecurringFlowId, kind :: RecurringKindId }
  | OfAgreement { template :: TemplateId }

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

-- | A settlement of the given settled Flows.
settledEvent :: RoundIndex -> Array Flow -> Maybe Ratio -> Cause -> Event
settledEvent round legs rate cause = Settled { round, legs, rate, cause, delivered: false }

-- | What a Strategy sees.
type View =
  { round :: RoundIndex
  , self :: Actor
  , prices :: Map ExchangeId Price
  , rules :: Map RuleId Rule -- the Rules in force, by the id a repeal names
  , transferability :: Map ResourceId Transferability
  , events :: Array Event
  , factors :: Map BehaviorDomain Ratio
  , toSign :: Array CandidateTransaction
  , pending :: Array PendingTransaction -- the spawned transactions this Actor owes or is owed
  , exchanges :: Array Venue -- the Exchanges the Actor has access to, in id order
  , actors :: Array ActorId -- the known Actors, in id order
  , transformations :: Map TransformationId Bounds -- the known Transformations and their order bounds
  , soleHoldings :: Array ResourceId -- the Resources whose holder the Actor is (`holderOf`)
  , ruleKinds :: Map RuleKindId RuleKindDecl -- the kinds of Rule declared
  , templates :: Map TemplateId TemplateTerms -- the agreements that can be entered
  }

-- | What entering an agreement must supply.
type TemplateTerms = { params :: Array String, parties :: Array String }

-- | A Transformation's order bounds, as a View shows them.
type Bounds = { minimum :: Quantity, maximum :: Maybe Quantity }

-- | An Exchange as a Strategy sees it: where a Resource (the base) is traded for the quote.
type Venue = { id :: ExchangeId, base :: ResourceId, quote :: ResourceId }

-- | What a Venue shows of an Exchange.
venueOf :: Exchange -> Venue
venueOf m = { id: m.id, base: m.base, quote: m.quote }

-- | What a View shows of a Transformation.
boundsOf :: Transformation -> Bounds
boundsOf t = { minimum: t.minimum, maximum: t.maximum }

-- | Why the engine rejected a candidate Transaction. Each constructor names one check, so a test or a view
-- | matches on the specific failure. The record stores the value through `rejectReasonShape`;
-- | `renderReason` is the one place its display text is written.
data RejectReason
  = PoolExceeded
  | LimitPriceNotMet
  | QuoteReservesShort
  | InputsNotHeld TransformationId
  | PartyCannotFund
  | JointBundlesOnlyTransfers
  | DuplicateCandidateTransactionId
  | OutsideProcedureAuthority
  | UnknownProcedure
  | UnknownActor ActorId
  | MustSign ActorId
  | NotAuthorized ActorId
  | UnknownExchange ExchangeId
  | UnknownResource ResourceId
  | UnknownTransformation TransformationId
  | UnknownRule RuleId
  | RouteNeedsLimit
  | NoExchangeAccess ActorId ExchangeId
  | NoRouteAccess ActorId ResourceId ResourceId
  | NotTransferable ResourceId
  | OutOfScope Rule
  | OnlyGoverningRepeal
  | BelowMinimum TransformationId
  | AboveMaximum TransformationId
  | OwnerMustSign TransformationId
  | MissingBundledOrder
  | Blocked RuleKindId
  | UnknownAgreement String
  | MissingArgument { template :: String, name :: String }
  | MissingParty { template :: String, name :: String }

derive instance Generic RejectReason _
derive instance Eq RejectReason

instance Show RejectReason where
  show = renderReason

-- | The text of a rejection reason.
renderReason :: RejectReason -> String
renderReason = case _ of
  PoolExceeded -> "the pool cannot fill it"
  LimitPriceNotMet -> "limit price not met"
  QuoteReservesShort -> "the exchange cannot pay its sellers"
  InputsNotHeld t -> show t <> ": inputs are not held"
  PartyCannotFund -> "a party cannot fund the joint CandidateTransaction"
  JointBundlesOnlyTransfers -> "joint CandidateTransactions bundle only Transfers and Transforms"
  DuplicateCandidateTransactionId -> "duplicate candidateTransaction id"
  OutsideProcedureAuthority -> "outside the procedure's authority"
  UnknownProcedure -> "unknown procedure"
  UnknownActor e -> "unknown actor " <> show e
  MustSign e -> show e <> " must sign"
  NotAuthorized e -> show e <> " is not authorized"
  UnknownExchange m -> "unknown exchange " <> show m
  UnknownResource r -> "unknown resource " <> show r
  UnknownTransformation t -> "unknown transformation " <> show t
  UnknownRule r -> "unknown rule " <> show r
  RouteNeedsLimit -> "a routed order needs a limit price"
  NoExchangeAccess e m -> show e <> " has no access to " <> show m
  NoRouteAccess e base quote -> show e <> " has no accessible exchange for " <> show base <> "/" <> show quote
  NotTransferable r -> show r <> " is not transferable"
  OutOfScope r -> "a Rule of kind " <> show (ruleKind r) <> " applies only to its governing Actor or owner, and only in its declared form"
  OnlyGoverningRepeal -> "only the governing authority may repeal a Rule"
  BelowMinimum t -> show t <> " order below its minimum"
  AboveMaximum t -> show t <> " order above its maximum"
  OwnerMustSign t -> "the holder of what " <> show t <> " draws must sign"
  MissingBundledOrder -> "this Transfer needs a matching bundled order by the sender"
  Blocked k -> "blocked by a Rule of kind " <> show k
  UnknownAgreement t -> "no agreement template " <> t
  MissingArgument m -> m.template <> " needs the argument " <> m.name
  MissingParty m -> m.template <> " needs the party " <> m.name

instance EncodeJson RejectReason where
  encodeJson = encodeWith Snapshot rejectReasonShape

instance DecodeJson RejectReason where
  decodeJson = decodeJsonWith Snapshot rejectReasonShape

-- | Append events to whatever carries the record being built.
addEvents :: forall r. Array Event -> { events :: Array Event | r } -> { events :: Array Event | r }
addEvents es x = x { events = x.events <> es }

-- | A Rejected event for a candidate Transaction the engine turned down in `round`.
rejection :: RoundIndex -> CandidateTransactionId -> RejectReason -> Event
rejection round candidateTransaction reason = Rejected { round, candidateTransaction, reason }

-- | A Lapsed event for a candidate Transaction whose signature rule was not met in `round`.
lapse :: RoundIndex -> CandidateTransactionId -> Event
lapse round candidateTransaction = Lapsed { round, candidateTransaction }

-- | An OrderRemoved event for one order of a candidate Transaction dropped at settlement in `round`.
orderRemoved :: RoundIndex -> CandidateTransactionId -> RejectReason -> Event
orderRemoved round candidateTransaction reason = OrderRemoved { round, candidateTransaction, reason }

-- | The Resource a premise's exchange views centre on, and what they call it.
type Focus = { resource :: ResourceId, label :: String }

focusShape :: Shape Focus
focusShape = recordShape "Focus" { resource: idShape, label: stringShape }

-- | Everything the engine records. The round record is the list of a round's events.
data Event
  = Lapsed { round :: RoundIndex, candidateTransaction :: CandidateTransactionId }
  | Rejected { round :: RoundIndex, candidateTransaction :: CandidateTransactionId, reason :: RejectReason }
  -- one order of a candidate Transaction removed at settlement; the candidate Transaction's other Intents still settle
  | OrderRemoved { round :: RoundIndex, candidateTransaction :: CandidateTransactionId, reason :: RejectReason }
  -- one settlement: its settled Flows settle together, at its rate when it has one. `delivered` marks a delivery of
  -- something settled in an earlier round, applied at Prepare
  | Settled { round :: RoundIndex, legs :: Array Flow, rate :: Maybe Ratio, cause :: Cause, delivered :: Boolean }
  | Enacted { round :: RoundIndex, id :: RuleId, rule :: Rule }
  | Repealed { round :: RoundIndex, id :: RuleId }
  | Estimated { round :: RoundIndex, aggregate :: ActorId, domain :: BehaviorDomain, factor :: Ratio }
  | ProcedureOutcome { round :: RoundIndex, candidateTransaction :: CandidateTransactionId, support :: Ratio, threshold :: Ratio, executed :: Boolean }

derive instance Generic Event _
derive instance Eq Event
instance Show Event where
  show = genericShow

type RoundRecord =
  { round :: RoundIndex
  , inputs :: Array SignedCandidateTransaction
  , events :: Array Event
  }

type RoundResult = { world :: WorldState, record :: RoundRecord }

-- | The immutable record of a game's resolved configuration and initial World State.
type Manifest =
  { possibleWorld :: String
  , engine :: String
  , draw :: String
  , initial :: WorldState
  }

-- | The engine's version, recorded in every Manifest beside `drawVersion`.
engineVersion :: String
engineVersion = "sp-engine/v1"

-- | The Manifest of a game whose configuration is named `gameboard` and which starts at `initial`.
manifestOf :: String -> WorldState -> Manifest
manifestOf possibleWorld initial = { possibleWorld, engine: engineVersion, draw: drawVersion, initial }

-- Shapes. The event and View shapes carry the closure the model and the Run Store encode.

rejectReasonShape :: Shape RejectReason
rejectReasonShape = genericSum "RejectReason"
  { "PoolExceeded": atTagged "a pool could not fill the CandidateTransaction" unitShape
  , "LimitPriceNotMet": atTagged "the limit price was not met" unitShape
  , "QuoteReservesShort": atTagged "the Exchange cannot pay its sellers" unitShape
  , "InputsNotHeld": atTagged "the Transformation's inputs are not held" idShape
  , "PartyCannotFund": atTagged "a party cannot fund the joint CandidateTransaction" unitShape
  , "JointBundlesOnlyTransfers": atTagged "a joint CandidateTransaction bundles something else" unitShape
  , "DuplicateCandidateTransactionId": atTagged "the CandidateTransaction's id is already used" unitShape
  , "OutsideProcedureAuthority": atTagged "outside the procedure's authority" unitShape
  , "UnknownProcedure": atTagged "no such procedure" unitShape
  , "UnknownActor": atTagged "no such Actor" idShape
  , "MustSign": atTagged "the Actor's signature is missing" idShape
  , "NotAuthorized": atTagged "the Actor is not authorized" idShape
  , "UnknownExchange": atTagged "no such Exchange" idShape
  , "UnknownResource": atTagged "no such Resource" idShape
  , "UnknownTransformation": atTagged "no such Transformation" idShape
  , "UnknownRule": atTagged "no such Rule" idShape
  , "RouteNeedsLimit": atTagged "a routed order has no limit price" unitShape
  , "NoExchangeAccess": atTagged "the Actor has no access to the Exchange" (pairOf idShape idShape)
  , "NoRouteAccess": atTagged "the Actor has no accessible Exchange for the pair" (pairOf idShape (pairOf idShape idShape))
  , "NotTransferable": atTagged "the Resource is not transferable" idShape
  , "OutOfScope": atTagged "the Rule is outside its governor's scope or declared form" ruleShape
  , "OnlyGoverningRepeal": atTagged "only the governing authority may repeal" unitShape
  , "BelowMinimum": atTagged "the order is below the Transformation's minimum" idShape
  , "AboveMaximum": atTagged "the order is above the Transformation's maximum" idShape
  , "OwnerMustSign": atTagged "the holder of what is drawn has not signed" idShape
  , "MissingBundledOrder": atTagged "the Transfer has no matching bundled order" unitShape
  , "Blocked": atTagged "blocked by a Rule of this kind" idShape
  , "UnknownAgreement": atTagged "no such agreement template" stringShape
  , "MissingArgument": atTagged "an agreement entered without an argument" (recordShape "Missing" { template: stringShape, name: stringShape })
  , "MissingParty": atTagged "an agreement entered without a party" (recordShape "Missing" { template: stringShape, name: stringShape })
  }

mechanismShape :: Shape Mechanism
mechanismShape = genericSum "Mechanism"
  { "Curve": atTagged "along an Exchange's curve" idShape
  , "Transformation": atTagged "a run of a Transformation" (recordShape "Transformation" { by: idShape, transformation: idShape, units: qShape })
  , "Direct": atTagged "moved directly" unitShape
  }

causeShape :: Shape Cause
causeShape = genericSum "Cause"
  { "OfIntent": atTagged "an Intent of a CandidateTransaction" (recordShape "OfIntent" { candidateTransaction: idShape, intent: intShape, mechanism: mechanismShape })
  , "OfRule": atTagged "a declared Rule kind" (recordShape "OfRule" { kind: idShape })
  , "OfRecurring": atTagged "a recurring-flow rule" (recordShape "OfRecurring" { rule: idShape, kind: idShape })
  , "OfAgreement": atTagged "an agreement entered during play" (recordShape "OfAgreement" { template: stringShape })
  }

-- | Everything the engine records, as the snapshot writes it: a tag and the record of its payload.
eventShape :: Shape Event
eventShape = genericSum "Event"
  { "Lapsed": atTagged "a CandidateTransaction that lapsed" (recordShape "Lapsed" { round: intShape, candidateTransaction: idShape })
  , "Rejected": atTagged "a CandidateTransaction the engine turned down" (recordShape "Rejected" { round: intShape, candidateTransaction: idShape, reason: rejectReasonShape })
  , "OrderRemoved": atTagged "one order of a CandidateTransaction removed at settlement" (recordShape "OrderRemoved" { round: intShape, candidateTransaction: idShape, reason: rejectReasonShape })
  , "Settled": atTagged "one settlement: its legs, its rate, and what caused it" (recordShape "Settled" { round: intShape, legs: arrayOf flowShape, rate: optionalOf qShape, cause: causeShape, delivered: boolShape })
  , "Enacted": atTagged "a Rule enacted" (recordShape "Enacted" { round: intShape, id: idShape, rule: ruleShape })
  , "Repealed": atTagged "a Rule repealed" (recordShape "Repealed" { round: intShape, id: idShape })
  , "Estimated": atTagged "a representation's factor estimated" (recordShape "Estimated" { round: intShape, aggregate: idShape, domain: idShape, factor: qShape })
  , "ProcedureOutcome": atTagged "a collective procedure's outcome" (recordShape "ProcedureOutcome" { round: intShape, candidateTransaction: idShape, support: qShape, threshold: qShape, executed: boolShape })
  }

-- | A View: what a Strategy or a seat sees.
viewShape :: Shape View
viewShape = recordOf "View"
  ( field @"round" "the round" intShape
      &. field @"self" "the Actor seeing it" actorShape
      &. field @"prices" "the price of each Exchange" (mapOf idShape qShape)
      &. field @"rules" "the Rules in force, by id" (mapOf idShape ruleShape)
      &. field @"transferability" "how each Resource may move" (mapOf idShape transferabilityShape)
      &. field @"events" "the public history" (arrayOf eventShape)
      &. field @"factors" "the factors of each behaviour domain" (mapOf idShape qShape)
      &. field @"toSign" "the CandidateTransactions the Actor is asked to sign" (arrayOf candidateTransactionShape)
      &. field @"pending" "the spawned transactions the Actor owes or is owed" (arrayOf pendingTransactionShape)
      &. field @"exchanges" "the Exchanges the Actor has access to" (arrayOf venueShape)
      &. field @"actors" "the known Actors" (arrayOf idShape)
      &. field @"transformations" "the known Transformations and their order bounds" (mapOf idShape boundsShape)
      &. field @"soleHoldings" "the Resources whose holder the Actor is" (arrayOf idShape)
      &. field @"ruleKinds" "the kinds of Rule declared" (mapOf idShape ruleKindDeclShape)
      &. field @"templates" "the agreements that can be entered, and what each needs" (mapOf stringShape (recordShape "TemplateTerms" { params: arrayOf stringShape, parties: arrayOf stringShape }))
  )

boundsShape :: Shape Bounds
boundsShape = recordOf "Bounds"
  ( field @"minimum" "the smallest order accepted" qShape
      &. field @"maximum" "the largest order accepted, if any" (optionalOf qShape)
  )

venueShape :: Shape Venue
venueShape = recordOf "Venue"
  ( field @"id" "the Exchange" idShape
      &. field @"base" "the Resource traded" idShape
      &. field @"quote" "the Resource it is priced in" idShape
  )

-- | A round's record as the Run Store holds it.
roundRecordShape :: Shape RoundRecord
roundRecordShape = recordOf "RoundRecord"
  ( field @"round" "the round" intShape
      &. field @"inputs" "every CandidateTransaction submitted in the round, with its signatures" (arrayOf signedCandidateTransactionShape)
      &. field @"events" "the round's events" (arrayOf eventShape)
  )

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

exchangeShape :: Shape Exchange
exchangeShape = recordShape "Exchange" { id: idShape, base: idShape, quote: idShape, cost: costFunctionShape, position: qShape, reserves: holdingsShape, access: (arrayOf idShape) }

drawShape :: Shape Draw
drawShape = recordShape "Draw" { resource: idShape, from: drawSourceShape, perUnit: qShape }

delayedFlowShape :: Shape DelayedFlow
delayedFlowShape = recordShape "DelayedFlow" { resource: idShape, holder: (optionalOf idShape), perUnit: qShape, rounds: (optionalOf intShape) }

paymentShape :: Shape Payment
paymentShape = recordShape "Payment" { curve: costFunctionShape, resource: idShape, to: idShape }

transformationShape :: Shape Transformation
transformationShape = recordShape "Transformation" { id: idShape, inputs: holdingsShape, outputs: holdingsShape, draws: (arrayOf drawShape), payment: (optionalOf paymentShape), delay: intShape, minimum: qShape, maximum: (optionalOf qShape), ownerSigns: boolShape, effects: (arrayOf delayedFlowShape) }

resourceDefinitionShape :: Shape ResourceDefinition
resourceDefinitionShape = recordShape "ResourceDefinition" { id: idShape, unit: stringShape, transferability: transferabilityShape, category: optionalOf stringShape }

estimatorShape :: Shape Estimator
estimatorShape = recordShape "Estimator" { reference: qShape, shrinkage: qShape, deviationBound: (optionalOf intervalShape), noiseSpread: qShape }

representationShape :: Shape Representation
representationShape = recordShape "Representation" { id: idShape, explicitActor: idShape, aggregate: idShape, domain: idShape, observes: idShape, weight: qShape, estimator: estimatorShape }

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
  { "FromActor": atTagged "one Actor" idShape
  , "FromEachHolder": atTagged "each Actor that holds the Resource" unitShape
  , "Created": atTagged "created in the destination" unitShape
  }

recurringDestinationShape :: Shape RecurringDestination
recurringDestinationShape = genericSum "RecurringDestination"
  { "ToActor": atTagged "one Actor" idShape
  , "ToSink": atTagged "nothing receives it" unitShape
  }

recurringAmountShape :: Shape RecurringAmount
recurringAmountShape = genericSum "RecurringAmount"
  { "FixedAmount": atTagged "a fixed quantity" qShape
  , "HeldAmount": atTagged "what the destination holds of this Resource" idShape
  , "FractionHeld": atTagged "a fraction of what the source holds" qShape
  , "Measured": atTagged "a fraction of what a query of the record reads" (recordShape "Measured" { ratio: qShape, query: legQueryShape })
  }

windowShape :: Shape Window
windowShape = genericEnum "Window"

sideShape :: Shape Side
sideShape = genericEnum "Side"

causePatternShape :: Shape CausePattern
causePatternShape = genericSum "CausePattern"
  { "CurveBase": atTagged "the base leg of an Exchange fill" unitShape
  , "TransformationPayment": atTagged "a transformation's payment leg" unitShape
  , "TransformationOf": atTagged "transformationFills of this Transformation" idShape
  }

legAggregateShape :: Shape LegAggregate
legAggregateShape = genericEnum "LegAggregate"

legQueryShape :: Shape LegQuery
legQueryShape = recordShape "LegQuery" { window: windowShape, party: idShape, side: sideShape, resource: optionalOf idShape, cause: causePatternShape, aggregate: legAggregateShape }

shortfallEffectShape :: Shape ShortfallEffect
shortfallEffectShape = genericSum "ShortfallEffect"
  { "NoEffect": atTagged "nothing" unitShape
  , "OffsetIncoming": atTagged "accrue against the flows the Actor receives, per unit short" qShape
  }

flowGateShape :: Shape FlowGate
flowGateShape = recordShape "FlowGate" { resource: idShape, exemptSharedExchange: boolShape, bundled: optionalOf idShape }

worldStateShape :: Shape WorldState
worldStateShape = recordShape "WorldState" { round: intShape, seed: (iso Seed unwrap intShape), actors: (mapOf idShape actorShape), resources: (mapOf idShape resourceDefinitionShape), exchanges: (mapOf idShape exchangeShape), transformations: (mapOf idShape transformationShape), representations: (arrayOf representationShape), numeraire: idShape, focus: optionalOf focusShape, flowGates: (arrayOf flowGateShape), groups: (mapOf idShape idShape), ruleKinds: (mapOf idShape ruleKindDeclShape), rules: (mapOf idShape ruleShape), pending: (arrayOf pendingTransactionShape), laws: (arrayOf authoredLawShape), templates: (mapOf stringShape worldTemplateShape), factors: (mapOf idShape holdingsShape), recurring: (arrayOf recurringFlowShape), events: (arrayOf eventShape) }

manifestShape :: Shape Manifest
manifestShape = recordShape "Manifest" { possibleWorld: stringShape, engine: stringShape, draw: stringShape, initial: worldStateShape }

-- | What to do at each kind of reference a premise's World State holds. `groupKey` is the Actor a group
-- | entry is about, separate from the group it counts as. An Aggregate's members and a procedure's weights are
-- | whole positions, since what becomes of them depends on the list they are in.
type WorldPositions f =
  { actor :: ActorId -> f ActorId
  , groupKey :: ActorId -> f ActorId
  , resource :: ResourceId -> f ResourceId
  , exchange :: ExchangeId -> f ExchangeId
  , transformation :: TransformationId -> f TransformationId
  , ruleKind :: RuleKindId -> f RuleKindId
  , members :: ActorId -> Array ActorId -> f (Array ActorId) -- the Aggregate, then its members
  , weights :: Map ActorId Ratio -> f (Map ActorId Ratio)
  }

-- | Every position kept as it is. A walk overrides the positions it changes.
sameWorld :: forall f. Applicative f => WorldPositions f
sameWorld = { actor: pure, groupKey: pure, resource: pure, exchange: pure, transformation: pure, ruleKind: pure, members: \_ -> pure, weights: pure }

-- | Every position read as nothing. A fold overrides the positions it reads.
ignoreWorld :: forall r. Monoid r => WorldPositions (Const r)
ignoreWorld =
  { actor: none, groupKey: none, resource: none, exchange: none, transformation: none, ruleKind: none, members: \_ -> none, weights: none }
  where
  none :: forall a b. a -> Const r b
  none _ = Const mempty

-- | What a world's references read as, in the order `traverseWorld` visits them.
foldWorld :: forall m r. Monoid m => WorldPositions (Const m) -> Record (WorldFields r) -> m
foldWorld p = un Const <<< traverseWorld p

-- | Rebuild the references of a premise's World State, position by position. The walk covers what a premise
-- | authors. What only a run produces (pending transactions, factors, events, the Representations instantiation adds) is
-- | not walked.
traverseWorld :: forall f r. Applicative f => WorldPositions f -> Record (WorldFields r) -> f (Record (WorldFields r))
traverseWorld t w =
  ( \exchanges actors transformations recurring groups rules numeraire focus flowGates ->
      w { exchanges = exchanges, actors = actors, transformations = transformations, recurring = recurring, groups = groups, rules = rules, numeraire = numeraire, focus = focus, flowGates = flowGates }
  )
    <$> traverse exchangeAt w.exchanges
    <*> traverse actorAt w.actors
    <*> traverse transformationAt w.transformations
    <*> traverse recurringAt w.recurring
    <*> keyedBy t.groupKey t.actor w.groups
    <*> traverse ruleAt w.rules
    <*> t.resource w.numeraire
    <*> traverse (\f -> f { resource = _ } <$> t.resource f.resource) w.focus
    <*> traverse flowGateAt w.flowGates
  where
  keyedBy :: forall k v u. Ord k => (k -> f k) -> (v -> f u) -> Map k v -> f (Map k u)
  keyedBy key value = map Map.fromFoldable <<< traverse (\(Tuple k v) -> Tuple <$> key k <*> value v) <<< (Map.toUnfoldable :: Map k v -> Array (Tuple k v))

  holdings :: Holdings -> f Holdings
  holdings = keyedBy t.resource pure

  exchangeAt m = (\access base quote reserves -> m { access = access, base = base, quote = quote, reserves = reserves })
    <$> traverse t.actor m.access
    <*> t.resource m.base
    <*> t.resource m.quote
    <*> holdings m.reserves

  actorAt e = (\accounts capabilities kind -> e { accounts = accounts, capabilities = capabilities, kind = kind })
    <$> holdings e.accounts
    <*> traverse capabilityAt e.capabilities
    <*> actorKindAt e.id e.kind
  actorKindAt self = case _ of
    Aggregate a -> (\members procedures -> Aggregate a { members = members, procedures = procedures })
      <$> t.members self a.members
      <*> traverse procedureAt a.procedures
    other -> pure other
  procedureAt p = (\weights authorizes -> p { weights = weights, authorizes = authorizes })
    <$> t.weights p.weights
    <*> traverse capabilityAt p.authorizes
  capabilityAt = case _ of
    MayTrade m -> MayTrade <$> t.exchange m
    MayTransfer r -> MayTransfer <$> t.resource r
    MayTransform x -> MayTransform <$> t.transformation x
    MayEnact k -> MayEnact <$> t.ruleKind k

  transformationAt x = (\inputs outputs draws effects payment -> x { inputs = inputs, outputs = outputs, draws = draws, effects = effects, payment = payment })
    <$> holdings x.inputs
    <*> holdings x.outputs
    <*> traverse (\d -> d { resource = _ } <$> t.resource d.resource) x.draws
    <*> traverse (\ef -> (\resource holder -> ef { resource = resource, holder = holder }) <$> t.resource ef.resource <*> traverse t.actor ef.holder) x.effects
    <*> traverse (\p -> (\to resource -> p { to = to, resource = resource }) <$> t.actor p.to <*> t.resource p.resource) x.payment

  recurringAt r = (\resource source destination amount -> r { resource = resource, source = source, destination = destination, amount = amount })
    <$> t.resource r.resource
    <*> sourceAt r.source
    <*> destinationAt r.destination
    <*> amountAt r.amount
  sourceAt = case _ of
    FromActor e -> FromActor <$> t.actor e
    other -> pure other
  destinationAt = case _ of
    ToActor e -> ToActor <$> t.actor e
    ToSink -> pure ToSink
  amountAt = case _ of
    HeldAmount c -> HeldAmount <$> t.resource c
    Measured m -> (\party resource cause -> Measured m { query = m.query { party = party, resource = resource, cause = cause } })
      <$> t.actor m.query.party
      <*> traverse t.resource m.query.resource
      <*> causePatternAt m.query.cause
    other -> pure other
  causePatternAt = case _ of
    TransformationOf x -> TransformationOf <$> t.transformation x
    other -> pure other

  ruleAt = case _ of
    FlowRule x -> (\kind governor counterparty over effect -> FlowRule x { kind = kind, scope = x.scope { governor = governor, counterparty = counterparty, over = over }, effect = effect })
      <$> t.ruleKind x.kind
      <*> t.actor x.scope.governor
      <*> t.actor x.scope.counterparty
      <*> traverse t.resource x.scope.over
      <*> flowEffectAt x.effect
    HoldingRule x -> (\kind resource -> HoldingRule x { kind = kind, resource = resource }) <$> t.ruleKind x.kind <*> t.resource x.resource
  flowEffectAt = case _ of
    Levy l -> (\in' basis -> Levy l { in = in', basis = basis }) <$> t.resource l.in <*> levyBasisAt l.basis
    Block -> pure Block
  levyBasisAt = case _ of
    PerUnit -> pure PerUnit
    AdValorem m -> AdValorem <$> t.exchange m

  flowGateAt g = (\resource bundled -> g { resource = resource, bundled = bundled }) <$> t.resource g.resource <*> traverse t.transformation g.bundled

-- | The Terms a template shows a would-be party.
templateTerms :: WorldTemplate -> TemplateTerms
templateTerms (WorldTemplate t) = { params: t.params, parties: t.parties }

-- | What a template refers to: Actors, Resources and Exchanges.
templateRefs :: WorldTemplate -> Array (Tuple RefKind String)
templateRefs (WorldTemplate t) = Array.concatMap obligation t.transaction.obligations
  where
  obligation o = partyRefs o.obligor <> effect o.effect <> Array.concatMap (condition <<< _.condition) o.when <> Array.concatMap spawn (o.next.performed <> o.next.short <> o.next.lapsed)
  effect = case _ of
    K.Move l -> flowRefs l
    K.Change _ -> []
  condition = case _ of
    K.StockAtLeast x -> stockRefs x.stock <> amountRefs x.amount
    K.Consent as -> map actorRef as
    K.QuantityAtLeast a -> amountRefs a
    K.Holds h -> partyRefs h.party
    _ -> []
  spawn s = Array.concatMap partyRefs (values s.parties) <> Array.concatMap amountRefs (values s.args)

-- | The templates a template's successors name.
spawnTargets :: WorldTemplate -> Array TemplateId
spawnTargets (WorldTemplate t) = Array.concatMap (\o -> map _.template (o.next.performed <> o.next.short <> o.next.lapsed)) t.transaction.obligations

-- | What a law refers to.
lawRefs :: AuthoredLaw -> Array (Tuple RefKind String)
lawRefs (AuthoredLaw l) = subject <> case l.operation of
  K.Emit f -> flowRefs f
  K.Withhold a -> amountRefs a
  K.Refuse -> []
  where
  subject = case l.subject of
    K.OfStock s -> [ resourceRef s.resource ]
    _ -> []

-- | The action a law is charged on: an Exchange or a Transformation, by id.
lawAction :: AuthoredLaw -> Maybe String
lawAction (AuthoredLaw l) = case l.trigger of
  K.OnAction a -> a.cause
  K.EachRound -> Nothing

actorRef :: ActorId -> Tuple RefKind String
actorRef = Tuple RefActor <<< idText

resourceRef :: ResourceId -> Tuple RefKind String
resourceRef = Tuple RefResource <<< idText

partyRefs :: K.Party -> Array (Tuple RefKind String)
partyRefs = case _ of
  K.Named e -> [ actorRef e ]
  K.Bound _ -> []

accountRefs :: K.Account -> Array (Tuple RefKind String)
accountRefs = case _ of
  K.HeldBy p -> partyRefs p
  K.Reserve m -> [ Tuple RefExchange (idText m) ]
  _ -> []

stockRefs :: K.StockRef -> Array (Tuple RefKind String)
stockRefs s = accountRefs s.at <> [ resourceRef s.resource ]

flowRefs :: forall q. K.FlowOwed q -> Array (Tuple RefKind String)
flowRefs l = accountRefs l.from <> accountRefs l.to <> [ resourceRef l.resource ] <> amountRefs l.owed

amountRefs :: forall q. K.Amount q -> Array (Tuple RefKind String)
amountRefs = case _ of
  K.Stock s -> stockRefs s
  K.Times t -> amountRefs t.amount
  K.Plus p -> amountRefs p.left <> amountRefs p.right
  K.Minus p -> amountRefs p.left <> amountRefs p.right
  K.Least p -> amountRefs p.left <> amountRefs p.right
  K.ShareOf s -> amountRefs s.limit <> Array.concatMap amountRefs s.among
  _ -> []
