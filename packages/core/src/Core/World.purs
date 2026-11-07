-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed)
import Record as Record
import Type.Proxy (Proxy(..))
import Core.Types (BehaviorDomain, Entity, EntityId, Market, MarketId, Price, Proposal, ProposalId, Quantity, Ratio, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleF(..), RuleId, SignedProposal, Transformation, TransformationId, CapacityRef)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
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
  }

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
  encodeJson = fromString <<< renderReason

instance DecodeJson RejectReason where
  decodeJson j = map Recorded (decodeJson j)

-- | A Rejected event for a Proposal the engine turned down in `round`.
rejection :: RoundIndex -> ProposalId -> RejectReason -> Event
rejection round proposal reason = Rejected { round, proposal, reason }

-- | Everything the engine records. The round record is the list of a round's events.
data Event
  = Lapsed { round :: RoundIndex, proposal :: ProposalId }
  | Rejected { round :: RoundIndex, proposal :: ProposalId, reason :: RejectReason }
  | Traded { round :: RoundIndex, market :: MarketId, entity :: EntityId, quantity :: Quantity, price :: Price }
  | Transferred { round :: RoundIndex, from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | Transformed { round :: RoundIndex, by :: EntityId, transformation :: TransformationId, quantity :: Quantity, paid :: Quantity }
  | Delivered { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity }
  | Enacted { round :: RoundIndex, id :: RuleId, rule :: Rule }
  | Repealed { round :: RoundIndex, id :: RuleId }
  | CapacityChanged { round :: RoundIndex, entity :: EntityId, capacity :: CapacityRef, delta :: Quantity }
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

instance EncodeJson Target where
  encodeJson = genericEncodeJson

instance DecodeJson Target where
  decodeJson = genericDecodeJson

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

instance EncodeJson Event where
  encodeJson = genericEncodeJson

instance DecodeJson Event where
  decodeJson = genericDecodeJson

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
