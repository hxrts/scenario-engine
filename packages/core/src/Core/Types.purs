-- | Core types (docs/02_type_system.md §2–§8). JSON codecs are the class instances
-- | declared beside each type.
module Core.Types where

import Prelude

import Core.Num (Q)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Array (last)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)

-- Identifiers ----------------------------------------------------------------

-- | An identifier, tagged by what it names.
newtype Id :: Type -> Type
newtype Id tag = Id String

derive instance Newtype (Id tag) _
derive instance Eq (Id tag)
derive instance Ord (Id tag)
derive newtype instance EncodeJson (Id tag)
derive newtype instance DecodeJson (Id tag)
instance Show (Id tag) where
  show (Id s) = s

data EntityTag
data ResourceTag
data MarketTag
data TransformationTag
data CapacityTag
data ProcedureTag
data RuleTag
data ProposalTag
data PolicyTag
data SeatTag
data RepresentationTag
data DomainTag

type EntityId = Id EntityTag
type ResourceId = Id ResourceTag
type MarketId = Id MarketTag
type TransformationId = Id TransformationTag
type CapacityRef = Id CapacityTag
type CollectiveProcedureId = Id ProcedureTag
type RuleId = Id RuleTag
type ProposalId = Id ProposalTag
type PolicyId = Id PolicyTag
type SeatId = Id SeatTag
type RepresentationId = Id RepresentationTag
type BehaviorDomain = Id DomainTag

type Quantity = Q
type Price = Q
type Ratio = Q
type RoundIndex = Int
type RoundOffset = Int
type Interval = { lo :: Q, hi :: Q }

type Holdings = Map ResourceId Quantity
type Capacities = Map CapacityRef Quantity

-- Entities (§2.1, §2.2) ------------------------------------------------------

type Entity =
  { id :: EntityId
  , accounts :: Holdings
  , capacities :: Capacities
  , capabilities :: Array Capability
  , kind :: EntityKind
  }

data EntityKind
  = Actor Controller
  | Aggregate Aggregate

-- | An Aggregate's policy, members, and collective procedures (§2.2).
type Aggregate =
  { policy :: PolicyId
  , members :: Array EntityId
  , procedures :: Map CollectiveProcedureId CollectiveProcedure
  }

data Controller
  = Seat SeatId
  | NPC PolicyId

type CollectiveProcedure =
  { weights :: Map EntityId Ratio
  , residualWeight :: Ratio
  , threshold :: Ratio
  , authorizes :: Array Capability
  }

-- Resources and capabilities (§2.3, §2.4) ------------------------------------

type ResourceDefinition =
  { id :: ResourceId
  , unit :: String
  , transferability :: Transferability
  , decay :: Maybe DecayRule
  , reservable :: Boolean
  }

data Transferability = Free | Restricted | NonTransferable

-- | Fraction of a held Resource lost per round. Instances scale annual rates to their round
-- | length (INV-28), so a yearly rate is converted before it is declared.
type DecayRule = { rate :: Ratio }

data Capability
  = MayTrade MarketId
  | MayTransfer ResourceId
  | MayTransform TransformationId
  | MayEnact RuleKind

derive instance Ord Capability

data RuleKind = TariffRule | AccessBanRule | WithholdingRule

derive instance Ord RuleKind

-- Intents and Proposals (§3) -------------------------------------------------

type Order =
  { market :: MarketId
  , quantity :: Quantity -- + buys, − sells the Market's base Resource
  , limitPrice :: Maybe Price
  }

-- | A base and quote Resource pair, which a routed order names in place of a Market.
type Pair = { base :: ResourceId, quote :: ResourceId }

-- | The Entity-facing best-execution order (docs/02 §5.2): the Trade with its market omitted. The
-- | engine routes it across the Markets for `pair` that the Entity has access to. It requires a limit.
type RouteOrder =
  { pair :: Pair
  , quantity :: Quantity
  , limitPrice :: Maybe Price
  }

type TransferSpec =
  { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }

type TransformSpec =
  { by :: EntityId, transformation :: TransformationId, quantity :: Quantity }

data Intent
  = Trade Order
  | Route RouteOrder
  | Transfer TransferSpec
  | Transform TransformSpec
  | ChangeRule RuleChange

data Rule
  = Tariff { jurisdiction :: EntityId, origin :: EntityId, rate :: Price }
  | AccessBan { jurisdiction :: EntityId, target :: EntityId }
  | Withholding { capacity :: CapacityRef, units :: Quantity }

data RuleChange
  = Enact Rule
  | Repeal RuleId

type Proposal =
  { id :: ProposalId
  , author :: EntityId
  , intents :: Array Intent
  , signers :: Array EntityId
  , procedure :: Maybe CollectiveProcedureId
  }

type SignedProposal = { proposal :: Proposal, signatures :: Array EntityId }

-- Markets and facilities (§5) ------------------------------------------------

type CostFunction =
  { base :: Price
  , slope :: Price -- p(q) = base + slope·q
  , domain :: Interval
  }

type Market =
  { id :: MarketId
  , base :: ResourceId
  , quote :: ResourceId
  , cost :: CostFunction
  , position :: Quantity
  , reserves :: Holdings
  , access :: Array EntityId
  }

type Transformation =
  { id :: TransformationId
  , inputs :: Holdings
  , outputs :: Holdings
  , reserves :: Holdings -- reserved per unit ordered: escrowed and released within the round, never consumed
  , cost :: Maybe CostFunction
  , capacity :: Maybe CapacityRef -- Nothing: uncapped
  , payee :: Maybe EntityId
  , delay :: RoundOffset
  , minimum :: Quantity -- smallest order accepted, e.g. a major military action
  , maximum :: Maybe Quantity -- largest order accepted per Entity per round
  , capacityUse :: Quantity -- capacity drawn per unit ordered
  , ownerSigns :: Boolean -- the capacity's owner must sign every order
  , effects :: Array CapacityEffect -- capacity changes after the delay
  }

-- | A per-unit change to a capacity, applied after the facility's delay. `holder` defaults
-- | to the ordering Entity; `rounds` limits how long the change lasts.
type CapacityEffect =
  { capacity :: CapacityRef, holder :: Maybe EntityId, perUnit :: Quantity, rounds :: Maybe Int }

-- Representation (§7) --------------------------------------------------------

type Representation =
  { id :: RepresentationId
  , actor :: EntityId
  , aggregate :: EntityId
  , domain :: BehaviorDomain
  , weight :: Ratio
  , estimator :: Estimator
  }

type Estimator =
  { reference :: Quantity
  , shrinkage :: Ratio
  , deviationBound :: Maybe Interval
  , noiseSpread :: Ratio
  }

-- Instances ------------------------------------------------------------------

derive instance Generic EntityKind _
derive instance Generic Controller _
derive instance Generic Transferability _
derive instance Generic Capability _
derive instance Generic RuleKind _
derive instance Generic Intent _
derive instance Generic Rule _
derive instance Generic RuleChange _

derive instance Eq EntityKind
derive instance Eq Controller
derive instance Eq Transferability
derive instance Eq Capability
derive instance Eq RuleKind
derive instance Eq Intent
derive instance Eq Rule
derive instance Eq RuleChange

instance Show EntityKind where
  show = genericShow

instance Show Controller where
  show = genericShow

instance Show Transferability where
  show = genericShow

instance Show Capability where
  show = genericShow

instance Show RuleKind where
  show = genericShow

instance Show Intent where
  show = genericShow

instance Show Rule where
  show = genericShow

instance Show RuleChange where
  show = genericShow

instance EncodeJson EntityKind where
  encodeJson = genericEncodeJson

instance DecodeJson EntityKind where
  decodeJson = genericDecodeJson

instance EncodeJson Controller where
  encodeJson = genericEncodeJson

instance DecodeJson Controller where
  decodeJson = genericDecodeJson

instance EncodeJson Transferability where
  encodeJson = genericEncodeJson

instance DecodeJson Transferability where
  decodeJson = genericDecodeJson

instance EncodeJson Capability where
  encodeJson = genericEncodeJson

instance DecodeJson Capability where
  decodeJson = genericDecodeJson

instance EncodeJson RuleKind where
  encodeJson = genericEncodeJson

instance DecodeJson RuleKind where
  decodeJson = genericDecodeJson

instance EncodeJson Intent where
  encodeJson = genericEncodeJson

instance DecodeJson Intent where
  decodeJson = genericDecodeJson

instance EncodeJson Rule where
  encodeJson = genericEncodeJson

instance DecodeJson Rule where
  decodeJson = genericDecodeJson

instance EncodeJson RuleChange where
  encodeJson = genericEncodeJson

instance DecodeJson RuleChange where
  decodeJson = genericDecodeJson

-- | Deterministic Proposal id: (round, author, index in the author's submission) (§3.3). This is
-- | the only constructor of a ProposalId; `submissionIndex` reads the index back out of it.
proposalId :: RoundIndex -> EntityId -> Int -> ProposalId
proposalId r (Id author) i = Id ("r" <> show r <> "/" <> author <> "/" <> show i)

-- | The index a Proposal id was built with: the segment after its last "/". Nothing for an id
-- | that `proposalId` could not have made.
submissionIndex :: ProposalId -> Maybe Int
submissionIndex (Id s) = Int.fromString =<< last (split (Pattern "/") s)

-- | The Resource a Behavior Domain names: a domain is the Resource whose purchases are observed.
domainResource :: BehaviorDomain -> ResourceId
domainResource (Id s) = Id s
