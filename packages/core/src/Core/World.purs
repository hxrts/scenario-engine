-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Shape (Format(..), Shape, arrayOf, boolShape, class Described, ctorOf, decodeJsonWith, encodeJsonWith, fieldOf, intShape, mapOf, object, prim, qShape, sumOf)
import Core.Types (BehaviorDomain, Entity, EntityId, Market, MarketId, Price, Proposal, ProposalId, Quantity, Ratio, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, SignedProposal, Transformation, TransformationId, CapacityRef, entityShape, idShape, proposalShape, ruleShape, signedProposalShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Either (note)
import Data.Maybe (Maybe(..))
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
  , rules :: Array Rule
  , events :: Array Event
  , factors :: Map BehaviorDomain Ratio
  , toSign :: Array Proposal
  , markets :: Array Venue -- the Markets the Entity has access to, in id order
  }

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
  encodeJson = genericEncodeJson

instance DecodeJson ShareSource where
  decodeJson = genericDecodeJson

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

-- Shapes (design 4, §5.3). The rejection reason is stored as its text; the event, target and View
-- shapes carry the closure the model and the Run Store encode.

rejectReasonShape :: Shape RejectReason
rejectReasonShape = prim "RejectReason" (fromString <<< renderReason) (\j -> map Recorded (note "expected a rejection reason text" (toString j)))

targetShape :: Shape Target
targetShape = sumOf "Target"
  [ ctorOf { tag: "Holding", desc: "a Resource held" } idShape
      ( \t -> case t of
          Holding r -> Just r
          _ -> Nothing
      )
      Holding
  , ctorOf { tag: "CapacityOf", desc: "a capacity of a facility" } idShape
      ( \t -> case t of
          CapacityOf c -> Just c
          _ -> Nothing
      )
      CapacityOf
  ]

-- | Everything the engine records, as the snapshot writes it: a tag and the record of its payload.
eventShape :: Shape Event
eventShape = sumOf "Event"
  [ ctorOf { tag: "Lapsed", desc: "a Proposal that lapsed" } lapsedShape
      ( \e -> case e of
          Lapsed r -> Just r
          _ -> Nothing
      )
      Lapsed
  , ctorOf { tag: "Rejected", desc: "a Proposal the engine turned down" } rejectedShape
      ( \e -> case e of
          Rejected r -> Just r
          _ -> Nothing
      )
      Rejected
  , ctorOf { tag: "OrderRemoved", desc: "one order of a Proposal removed at settlement" } orderRemovedShape
      ( \e -> case e of
          OrderRemoved r -> Just r
          _ -> Nothing
      )
      OrderRemoved
  , ctorOf { tag: "Tariffed", desc: "a tariff paid on a shipment" } tariffedShape
      ( \e -> case e of
          Tariffed r -> Just r
          _ -> Nothing
      )
      Tariffed
  , ctorOf { tag: "Traded", desc: "a Market order filled" } tradedShape
      ( \e -> case e of
          Traded r -> Just r
          _ -> Nothing
      )
      Traded
  , ctorOf { tag: "Transferred", desc: "a Resource moved between Entities" } transferredShape
      ( \e -> case e of
          Transferred r -> Just r
          _ -> Nothing
      )
      Transferred
  , ctorOf { tag: "Transformed", desc: "a run of a transformation" } transformedShape
      ( \e -> case e of
          Transformed r -> Just r
          _ -> Nothing
      )
      Transformed
  , ctorOf { tag: "Delivered", desc: "a delivery arrived" } deliveredShape
      ( \e -> case e of
          Delivered r -> Just r
          _ -> Nothing
      )
      Delivered
  , ctorOf { tag: "Enacted", desc: "a Rule enacted" } enactedShape
      ( \e -> case e of
          Enacted r -> Just r
          _ -> Nothing
      )
      Enacted
  , ctorOf { tag: "Repealed", desc: "a Rule repealed" } repealedShape
      ( \e -> case e of
          Repealed r -> Just r
          _ -> Nothing
      )
      Repealed
  , ctorOf { tag: "Estimated", desc: "a representation's factor estimated" } estimatedShape
      ( \e -> case e of
          Estimated r -> Just r
          _ -> Nothing
      )
      Estimated
  , ctorOf { tag: "Consumed", desc: "a Resource consumed" } consumedShape
      ( \e -> case e of
          Consumed r -> Just r
          _ -> Nothing
      )
      Consumed
  , ctorOf { tag: "Shortfall", desc: "an amount owed but not delivered" } shortfallShape
      ( \e -> case e of
          Shortfall r -> Just r
          _ -> Nothing
      )
      Shortfall
  , ctorOf { tag: "Decayed", desc: "a Resource lost to decay" } decayedShape
      ( \e -> case e of
          Decayed r -> Just r
          _ -> Nothing
      )
      Decayed
  , ctorOf { tag: "ProcedureOutcome", desc: "a collective procedure's outcome" } procedureOutcomeShape
      ( \e -> case e of
          ProcedureOutcome r -> Just r
          _ -> Nothing
      )
      ProcedureOutcome
  ]
  where
  lapsedShape = object "Lapsed"
    ( (\r p -> { round: r, proposal: p })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "proposal" "" idShape _.proposal
    )
  rejectedShape = object "Rejected"
    ( (\r p x -> { round: r, proposal: p, reason: x })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "proposal" "" idShape _.proposal
        <*> fieldOf "reason" "" rejectReasonShape _.reason
    )
  orderRemovedShape = object "OrderRemoved"
    ( (\r p x -> { round: r, proposal: p, reason: x })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "proposal" "" idShape _.proposal
        <*> fieldOf "reason" "" rejectReasonShape _.reason
    )
  tariffedShape = object "Tariffed"
    ( (\r f t a -> { round: r, from: f, to: t, amount: a })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "from" "" idShape _.from
        <*> fieldOf "to" "" idShape _.to
        <*> fieldOf "amount" "" qShape _.amount
    )
  tradedShape = object "Traded"
    ( (\r m e q p -> { round: r, market: m, entity: e, quantity: q, price: p })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "market" "" idShape _.market
        <*> fieldOf "entity" "" idShape _.entity
        <*> fieldOf "quantity" "" qShape _.quantity
        <*> fieldOf "price" "" qShape _.price
    )
  transferredShape = object "Transferred"
    ( (\r f t res q -> { round: r, from: f, to: t, resource: res, quantity: q })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "from" "" idShape _.from
        <*> fieldOf "to" "" idShape _.to
        <*> fieldOf "resource" "" idShape _.resource
        <*> fieldOf "quantity" "" qShape _.quantity
    )
  transformedShape = object "Transformed"
    ( (\r b t q p -> { round: r, by: b, transformation: t, quantity: q, paid: p })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "by" "" idShape _.by
        <*> fieldOf "transformation" "" idShape _.transformation
        <*> fieldOf "quantity" "" qShape _.quantity
        <*> fieldOf "paid" "" qShape _.paid
    )
  deliveredShape = object "Delivered"
    ( (\r e t q -> { round: r, entity: e, target: t, quantity: q })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "entity" "" idShape _.entity
        <*> fieldOf "target" "" targetShape _.target
        <*> fieldOf "quantity" "" qShape _.quantity
    )
  enactedShape = object "Enacted"
    ( (\r i x -> { round: r, id: i, rule: x })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "id" "" idShape _.id
        <*> fieldOf "rule" "" ruleShape _.rule
    )
  repealedShape = object "Repealed"
    ( (\r i -> { round: r, id: i })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "id" "" idShape _.id
    )
  estimatedShape = object "Estimated"
    ( (\r a d f -> { round: r, aggregate: a, domain: d, factor: f })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "aggregate" "" idShape _.aggregate
        <*> fieldOf "domain" "" idShape _.domain
        <*> fieldOf "factor" "" qShape _.factor
    )
  consumedShape = object "Consumed"
    ( (\r e res q -> { round: r, entity: e, resource: res, quantity: q })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "entity" "" idShape _.entity
        <*> fieldOf "resource" "" idShape _.resource
        <*> fieldOf "quantity" "" qShape _.quantity
    )
  shortfallShape = object "Shortfall"
    ( (\r e res q -> { round: r, entity: e, resource: res, quantity: q })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "entity" "" idShape _.entity
        <*> fieldOf "resource" "" idShape _.resource
        <*> fieldOf "quantity" "" qShape _.quantity
    )
  decayedShape = object "Decayed"
    ( (\r e res q -> { round: r, entity: e, resource: res, quantity: q })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "entity" "" idShape _.entity
        <*> fieldOf "resource" "" idShape _.resource
        <*> fieldOf "quantity" "" qShape _.quantity
    )
  procedureOutcomeShape = object "ProcedureOutcome"
    ( (\r p s t x -> { round: r, proposal: p, support: s, threshold: t, executed: x })
        <$> fieldOf "round" "" intShape _.round
        <*> fieldOf "proposal" "" idShape _.proposal
        <*> fieldOf "support" "" qShape _.support
        <*> fieldOf "threshold" "" qShape _.threshold
        <*> fieldOf "executed" "" boolShape _.executed
    )

-- | A View: what a Policy or a seat sees (INV-16).
viewShape :: Shape View
viewShape = object "View"
  ( (\r s p rs e f t m -> { round: r, self: s, prices: p, rules: rs, events: e, factors: f, toSign: t, markets: m })
      <$> fieldOf "round" "the round" intShape _.round
      <*> fieldOf "self" "the Entity seeing it" entityShape _.self
      <*> fieldOf "prices" "the price of each Market" (mapOf idShape qShape) _.prices
      <*> fieldOf "rules" "the Rules in force" (arrayOf ruleShape) _.rules
      <*> fieldOf "events" "the public history" (arrayOf eventShape) _.events
      <*> fieldOf "factors" "the factors of each behaviour domain" (mapOf idShape qShape) _.factors
      <*> fieldOf "toSign" "the Proposals the Entity is asked to sign" (arrayOf proposalShape) _.toSign
      <*> fieldOf "markets" "the Markets the Entity has access to" (arrayOf venueShape) _.markets
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
