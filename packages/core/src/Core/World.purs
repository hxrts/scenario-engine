-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed(..), drawVersion)
import Core.Num (Q, qShape)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Shape (atTagged, genericSum, recordShape, Format(..), Shape, arrayOf, boolShape, class Described, decodeJsonWith, encodeJsonWith, fieldOf, intShape, iso, mapOf, object, optionalOf, prim, stringShape, unitShape)
import Core.Types (Transferability, CapacityEffect, CostFunction, Estimator, Id, Interval, BehaviorDomain, Entity, EntityId, Market, MarketId, Price, Proposal, ProposalId, Quantity, Ratio, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, SignedProposal, Transformation, TransformationId, CapacityRef, entityShape, idShape, transferabilityShape, proposalShape, ruleShape, signedProposalShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)

-- | The World State's fields, with the seed as the row `r`. A World State has a seed only once it
-- | is instantiated (`seeded`); a decoded Gameboard's world is `UnseededWorld`.
type WorldFields r =
  ( round :: RoundIndex
  , entities :: Map EntityId Entity
  , resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , representations :: Array Representation
  , numeraire :: ResourceId -- the Resource facility costs are paid in
  , transport :: Map ResourceId TransformationId -- facility that carries a Resource between regions
  , jurisdiction :: Map EntityId EntityId -- governing Entity, when not the Entity itself
  , rules :: Map RuleId Rule -- Rules in force
  , scheduled :: Map RuleId (Maybe Rule) -- take effect at the next round boundary; Nothing repeals
  , deliveries :: Array Delivery -- due at Prepare
  , factors :: Map EntityId (Map BehaviorDomain Ratio) -- stored Representation adjustment factors
  , circulation :: Circulation
  , events :: Array Event -- public history
  | r
  )

type WorldState = Record (WorldFields (seed :: Seed)) -- seed: the game seed

-- | A World State before instantiation: every field but the seed.
type UnseededWorld = Record (WorldFields ())

-- | The World State with its game seed. Instantiation and fixtures are the only callers.
seeded :: Seed -> UnseededWorld -> WorldState
seeded seed w = Record.insert (Proxy :: Proxy "seed") seed w

-- | The recurring economic cycle (docs/02_type_system.md §6), as data.
type Circulation =
  { production :: Array { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef } -- delivered next round
  , flows :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Quantity } -- fixed income and expenditure
  , shares :: Array { from :: EntityId, to :: EntityId, share :: Ratio, source :: ShareSource } -- variable flows
  , requirements :: Array Requirement -- consumed at commit from the net position
  }

-- | What a variable flow is a share of, from the paying Entity's settlement this round.
data ShareSource
  = SalesOf ResourceId -- net Money revenue from selling this Resource on Markets
  | PayeeReceipts -- Money received as a facility payee

type Requirement =
  { entity :: EntityId
  , resource :: ResourceId
  , amount :: Maybe Quantity -- Nothing: equal to this round's purchases of the Resource
  , penalty :: Quantity -- reduction of fixed incoming flows per unit unmet
  }

-- | A scheduled change to a holding or a capacity, applied at Prepare.
type Delivery =
  { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity }

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
  }

-- | A Transformation's order bounds, as a View shows them.
type Bounds = { minimum :: Quantity, maximum :: Maybe Quantity }

-- | A Market as a Policy sees it: where a Resource (the base) is traded for the quote.
type Venue = { id :: MarketId, base :: ResourceId, quote :: ResourceId }

-- | Why the engine rejected a Proposal. Each constructor names one check, so a test or a view
-- | matches on the specific failure. `renderReason` gives the text the record stores and the
-- | dashboard shows; it is the only place that text is written.
data RejectReason
  = FacilityCapacityExceeded
  | LimitPriceNotMet
  | MarketCannotPaySellers
  | InputsNotHeld TransformationId
  | PartyCannotFund
  | DealBundlesOnlyTransfers
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
  | NoTransportFacility
  | CrossRegionNeedsTransport
  | ShipmentBanned
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
  FacilityCapacityExceeded -> "facility capacity exceeded"
  LimitPriceNotMet -> "limit price not met"
  MarketCannotPaySellers -> "the market cannot pay its sellers"
  InputsNotHeld t -> show t <> ": inputs are not held"
  PartyCannotFund -> "a party cannot fund the deal"
  DealBundlesOnlyTransfers -> "deals bundle only Transfers and Transforms"
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
  OutOfScope rule -> case rule of
    Tariff _ -> "a Tariff applies only to the enacting jurisdiction"
    AccessBan _ -> "an AccessBan applies only to the enacting jurisdiction"
    Withholding _ -> "withholding needs the capacity's owner"
  OnlyGoverningRepeal -> "only the governing authority may repeal a Rule"
  BelowMinimum t -> show t <> " order below its minimum"
  AboveMaximum t -> show t <> " order above its maximum"
  OwnerMustSign t -> "the owner of " <> show t <> "'s capacity must sign"
  NoTransportFacility -> "no transport facility"
  CrossRegionNeedsTransport -> "a cross-region Transfer needs a matching transport order by the sender"
  ShipmentBanned -> "shipment banned"
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
  | Traded { round :: RoundIndex, market :: MarketId, entity :: EntityId, quantity :: Quantity, price :: Price }
  | Transferred { round :: RoundIndex, from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | Tariffed { round :: RoundIndex, from :: EntityId, to :: EntityId, amount :: Quantity }
  | Transformed { round :: RoundIndex, by :: EntityId, transformation :: TransformationId, quantity :: Quantity, paid :: Quantity }
  | Delivered { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity }
  | Enacted { round :: RoundIndex, id :: RuleId, rule :: Rule }
  | Repealed { round :: RoundIndex, id :: RuleId }
  | Estimated { round :: RoundIndex, aggregate :: EntityId, domain :: BehaviorDomain, factor :: Ratio }
  | Consumed { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  -- an amount owed but not delivered: an unpaid payment (Transfer, tariff, facility payment),
  -- or an unmet operating requirement; the resource says which
  | Shortfall { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | Decayed { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | ProcedureOutcome { round :: RoundIndex, proposal :: ProposalId, support :: Ratio, threshold :: Ratio, executed :: Boolean }

derive instance Generic Target _
derive instance Eq Target
instance Show Target where
  show = genericShow

derive instance Generic ShareSource _
derive instance Eq ShareSource
instance Show ShareSource where
  show = genericShow

instance EncodeJson ShareSource where
  encodeJson = encodeJsonWith Snapshot shareSourceShape

instance DecodeJson ShareSource where
  decodeJson = decodeJsonWith Snapshot shareSourceShape

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

-- Shapes (design 4, §5.3). The rejection reason is stored as its text; the event, target and View
-- shapes carry the closure the model and the Run Store encode.

rejectReasonShape :: Shape RejectReason
rejectReasonShape = prim "RejectReason" (fromString <<< renderReason) (\j -> map Recorded (note "expected a rejection reason text" (toString j)))

targetShape :: Shape Target
targetShape = genericSum "Target"
  { "Holding": atTagged "a Resource held" idShape
  , "CapacityOf": atTagged "a capacity of a facility" idShape
  }

-- | Everything the engine records, as the snapshot writes it: a tag and the record of its payload.
eventShape :: Shape Event
eventShape = genericSum "Event"
  { "Lapsed": atTagged "a Proposal that lapsed" (recordShape "Lapsed" { round: intShape, proposal: idShape })
  , "Rejected": atTagged "a Proposal the engine turned down" (recordShape "Rejected" { round: intShape, proposal: idShape, reason: rejectReasonShape })
  , "OrderRemoved": atTagged "one order of a Proposal removed at settlement" (recordShape "OrderRemoved" { round: intShape, proposal: idShape, reason: rejectReasonShape })
  , "Traded": atTagged "a Market order filled" (recordShape "Traded" { round: intShape, market: idShape, entity: idShape, quantity: qShape, price: qShape })
  , "Transferred": atTagged "a Resource moved between Entities" (recordShape "Transferred" { round: intShape, from: idShape, to: idShape, resource: idShape, quantity: qShape })
  , "Tariffed": atTagged "a tariff paid on a shipment" (recordShape "Tariffed" { round: intShape, from: idShape, to: idShape, amount: qShape })
  , "Transformed": atTagged "a run of a transformation" (recordShape "Transformed" { round: intShape, by: idShape, transformation: idShape, quantity: qShape, paid: qShape })
  , "Delivered": atTagged "a delivery arrived" (recordShape "Delivered" { round: intShape, entity: idShape, target: targetShape, quantity: qShape })
  , "Enacted": atTagged "a Rule enacted" (recordShape "Enacted" { round: intShape, id: idShape, rule: ruleShape })
  , "Repealed": atTagged "a Rule repealed" (recordShape "Repealed" { round: intShape, id: idShape })
  , "Estimated": atTagged "a representation's factor estimated" (recordShape "Estimated" { round: intShape, aggregate: idShape, domain: idShape, factor: qShape })
  , "Consumed": atTagged "a Resource consumed" (recordShape "Consumed" { round: intShape, entity: idShape, resource: idShape, quantity: qShape })
  , "Shortfall": atTagged "an amount owed but not delivered" (recordShape "Shortfall" { round: intShape, entity: idShape, resource: idShape, quantity: qShape })
  , "Decayed": atTagged "a Resource lost to decay" (recordShape "Decayed" { round: intShape, entity: idShape, resource: idShape, quantity: qShape })
  , "ProcedureOutcome": atTagged "a collective procedure's outcome" (recordShape "ProcedureOutcome" { round: intShape, proposal: idShape, support: qShape, threshold: qShape, executed: boolShape })
  }

-- | A View: what a Policy or a seat sees (INV-16).
viewShape :: Shape View
viewShape = object "View"
  ( (\r s p rs tr e f t m es ts oc -> { round: r, self: s, prices: p, rules: rs, transferability: tr, events: e, factors: f, toSign: t, markets: m, entities: es, transformations: ts, ownedCapacities: oc })
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
costFunctionShape = recordShape "CostFunction" { base: qShape, slope: qShape, domain: intervalShape }

holdingsShape :: forall t. Shape (Map (Id t) Q)
holdingsShape = mapOf idShape qShape

marketShape :: Shape Market
marketShape = recordShape "Market" { id: idShape, base: idShape, quote: idShape, cost: costFunctionShape, position: qShape, reserves: holdingsShape, access: (arrayOf idShape) }

capacityEffectShape :: Shape CapacityEffect
capacityEffectShape = recordShape "CapacityEffect" { capacity: idShape, holder: (optionalOf idShape), perUnit: qShape, rounds: (optionalOf intShape) }

transformationShape :: Shape Transformation
transformationShape = recordShape "Transformation" { id: idShape, inputs: holdingsShape, outputs: holdingsShape, reserves: holdingsShape, cost: (optionalOf costFunctionShape), capacity: (optionalOf idShape), payee: (optionalOf idShape), delay: intShape, minimum: qShape, maximum: (optionalOf qShape), capacityUse: qShape, ownerSigns: boolShape, effects: (arrayOf capacityEffectShape) }

resourceDefinitionShape :: Shape ResourceDefinition
resourceDefinitionShape = recordShape "ResourceDefinition" { id: idShape, unit: stringShape, transferability: transferabilityShape, decay: (optionalOf (recordShape "DecayRule" { rate: qShape })), reservable: boolShape }

estimatorShape :: Shape Estimator
estimatorShape = recordShape "Estimator" { reference: qShape, shrinkage: qShape, deviationBound: (optionalOf intervalShape), noiseSpread: qShape }

representationShape :: Shape Representation
representationShape = recordShape "Representation" { id: idShape, actor: idShape, aggregate: idShape, domain: idShape, weight: qShape, estimator: estimatorShape }

deliveryShape :: Shape Delivery
deliveryShape = recordShape "Delivery" { round: intShape, entity: idShape, target: targetShape, quantity: qShape }

shareSourceShape :: Shape ShareSource
shareSourceShape = genericSum "ShareSource"
  { "SalesOf": atTagged "sales revenue of a Resource" idShape
  , "PayeeReceipts": atTagged "what the Entity received as a payee" unitShape
  }

circulationShape :: Shape Circulation
circulationShape = recordShape "Circulation" { production: (arrayOf productionShape), flows: (arrayOf flowShape), shares: (arrayOf shareShape), requirements: (arrayOf requirementShape) }
  where
  productionShape = recordShape "Production" { entity: idShape, resource: idShape, capacity: idShape }
  flowShape = recordShape "Flow" { from: idShape, to: idShape, resource: idShape, amount: qShape }
  shareShape = recordShape "Share" { from: idShape, to: idShape, share: qShape, source: shareSourceShape }
  requirementShape = recordShape "Requirement" { entity: idShape, resource: idShape, amount: (optionalOf qShape), penalty: qShape }

worldStateShape :: Shape WorldState
worldStateShape = recordShape "WorldState" { round: intShape, seed: (iso Seed unwrap intShape), entities: (mapOf idShape entityShape), resources: (mapOf idShape resourceDefinitionShape), markets: (mapOf idShape marketShape), transformations: (mapOf idShape transformationShape), representations: (arrayOf representationShape), numeraire: idShape, transport: (mapOf idShape idShape), jurisdiction: (mapOf idShape idShape), rules: (mapOf idShape ruleShape), scheduled: (mapOf idShape (optionalOf ruleShape)), deliveries: (arrayOf deliveryShape), factors: (mapOf idShape holdingsShape), circulation: circulationShape, events: (arrayOf eventShape) }

manifestShape :: Shape Manifest
manifestShape = recordShape "Manifest" { gameboard: stringShape, engine: stringShape, draw: stringShape, initial: worldStateShape }
