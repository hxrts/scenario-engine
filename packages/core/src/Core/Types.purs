-- | Core types (docs/02_type_system.md §2–§8). JSON codecs are the class instances
-- | declared beside each type.
module Core.Types where

import Prelude

import Core.Num (Q)
import Core.Shape (class Described, Format(..), Shape, TagForm(..), ctorOf, decodeJsonWith, encodeJsonWith, field, optionalOf, prim, qShape, rec2, rec3, rec4, sumOf)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (note)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Array (last)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
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

-- | An Intent with its quantities and limits in the filling `v`: `Q` for an Intent the engine
-- | settles (`Intent`), `Expr` for a Strategy's template (`Core.Strategy`). Every kind of Core
-- | Intent is one constructor here, so the filling is the only thing the two languages differ in.
data IntentF v
  = Trade { market :: MarketId, quantity :: v, limitPrice :: Maybe v }
  | Route { pair :: Pair, quantity :: v, limitPrice :: Maybe v }
  | Transfer { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: v }
  | Transform { by :: EntityId, transformation :: TransformationId, quantity :: v }
  | ChangeRule (RuleChangeF v)

-- | A Rule with its amounts in the filling `v`.
data RuleF v
  = Tariff { jurisdiction :: EntityId, origin :: EntityId, rate :: v }
  | AccessBan { jurisdiction :: EntityId, target :: EntityId }
  | Withholding { capacity :: CapacityRef, units :: v }

-- | Enact a Rule, or repeal one by the id the engine assigned it.
data RuleChangeF v
  = Enact (RuleF v)
  | Repeal RuleId

type Intent = IntentF Q
type Rule = RuleF Q
type RuleChange = RuleChangeF Q

-- | What a Proposal says before its id and author are assigned: its Intents, the signers it asks
-- | for, and the collective procedure it runs under (if any). Procedure is a Proposal concern, not an
-- | Intent kind (docs/02 §3). A Strategy's Deal and Procedure templates are `ProposalF Expr`.
type ProposalF v =
  { intents :: Array (IntentF v)
  , signers :: Array EntityId
  , procedure :: Maybe CollectiveProcedureId
  }

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
derive instance Generic (IntentF v) _
derive instance Generic (RuleF v) _
derive instance Generic (RuleChangeF v) _

derive instance Functor IntentF
derive instance Foldable IntentF
derive instance Traversable IntentF
derive instance Functor RuleF
derive instance Foldable RuleF
derive instance Traversable RuleF
derive instance Functor RuleChangeF
derive instance Foldable RuleChangeF
derive instance Traversable RuleChangeF

derive instance Eq EntityKind
derive instance Eq Controller
derive instance Eq Transferability
derive instance Eq Capability
derive instance Eq RuleKind
derive instance Eq v => Eq (IntentF v)
derive instance Eq v => Eq (RuleF v)
derive instance Eq v => Eq (RuleChangeF v)

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

instance Show v => Show (IntentF v) where
  show = genericShow

instance Show v => Show (RuleF v) where
  show = genericShow

instance Show v => Show (RuleChangeF v) where
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

-- Described shapes (design 4, §5.3). The engine's Intents, Rules and rule changes are described once
-- here. Their JSON codecs are the snapshot codec of the shape, so no second codec exists for them.
-- Each sum is written in the snapshot form (`{"tag":..,"values":[..]}`); the premise-format tags of
-- these types are added with the `schema` package (design 4, Phase 3).

idShape :: forall t. Shape (Id t)
idShape = prim "Id" (\(Id s) -> fromString s) (\j -> note "expected an id string" (map Id (toString j)))

instance Described (RuleF Q) where
  shape = ruleShape

instance Described (RuleChangeF Q) where
  shape = ruleChangeShape

instance Described (IntentF Q) where
  shape = intentShape

ruleShape :: Shape (RuleF Q)
ruleShape = sumOf "Rule" OneKey [] [ tariff, accessBan, withholding ]
  where
  tariff = ctorOf { tag: "Tariff", desc: "a tariff on the origin's goods, set by the jurisdiction" } tariffShape projTariff Tariff
  accessBan = ctorOf { tag: "AccessBan", desc: "a ban on shipments from the jurisdiction to the target" } accessBanShape projAccessBan AccessBan
  withholding = ctorOf { tag: "Withholding", desc: "units of a capacity withheld" } withholdingShape projWithholding Withholding
  projTariff = case _ of
    Tariff x -> Just x
    _ -> Nothing
  projAccessBan = case _ of
    AccessBan x -> Just x
    _ -> Nothing
  projWithholding = case _ of
    Withholding x -> Just x
    _ -> Nothing
  tariffShape = rec3 "Tariff"
    (field "jurisdiction" "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction)
    (field "origin" "origin" "the Entity whose goods it taxes" idShape _.origin)
    (field "rate" "rate" "the rate" qShape _.rate)
    (\j o r -> { jurisdiction: j, origin: o, rate: r })
  accessBanShape = rec2 "AccessBan"
    (field "jurisdiction" "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction)
    (field "target" "target" "the Entity whose shipments are banned" idShape _.target)
    (\j t -> { jurisdiction: j, target: t })
  withholdingShape = rec2 "Withholding"
    (field "capacity" "capacity" "the capacity withheld" idShape _.capacity)
    (field "units" "units" "units withheld" qShape _.units)
    (\c u -> { capacity: c, units: u })

ruleChangeShape :: Shape (RuleChangeF Q)
ruleChangeShape = sumOf "RuleChange" OneKey [] [ enact, repeal ]
  where
  enact = ctorOf { tag: "Enact", desc: "enact a Rule" } ruleShape projEnact Enact
  repeal = ctorOf { tag: "Repeal", desc: "repeal a Rule by its id" } idShape projRepeal Repeal
  projEnact = case _ of
    Enact r -> Just r
    _ -> Nothing
  projRepeal = case _ of
    Repeal r -> Just r
    _ -> Nothing

intentShape :: Shape (IntentF Q)
intentShape = sumOf "Intent" OneKey [] [ trade, route, transfer, transform, changeRule ]
  where
  trade = ctorOf { tag: "Trade", desc: "an order in a Market" } tradeShape projTrade Trade
  route = ctorOf { tag: "Route", desc: "a best-execution order on a pair of Resources" } routeShape projRoute Route
  transfer = ctorOf { tag: "Transfer", desc: "a Resource given from one Entity to another" } transferShape projTransfer Transfer
  transform = ctorOf { tag: "Transform", desc: "a run of a transformation" } transformShape projTransform Transform
  changeRule = ctorOf { tag: "ChangeRule", desc: "enact or repeal a Rule" } ruleChangeShape projChangeRule ChangeRule
  projTrade = case _ of
    Trade x -> Just x
    _ -> Nothing
  projRoute = case _ of
    Route x -> Just x
    _ -> Nothing
  projTransfer = case _ of
    Transfer x -> Just x
    _ -> Nothing
  projTransform = case _ of
    Transform x -> Just x
    _ -> Nothing
  projChangeRule = case _ of
    ChangeRule x -> Just x
    _ -> Nothing
  tradeShape = rec3 "Trade"
    (field "market" "market" "the Market" idShape _.market)
    (field "quantity" "quantity" "+ buys, - sells the Market's base Resource" qShape _.quantity)
    (field "limitPrice" "limitPrice" "the limit price, if any" (optionalOf qShape) _.limitPrice)
    (\m q l -> { market: m, quantity: q, limitPrice: l })
  routeShape = rec3 "Route"
    (field "pair" "pair" "the base and quote Resources" pairShape _.pair)
    (field "quantity" "quantity" "+ buys, - sells the base Resource" qShape _.quantity)
    (field "limitPrice" "limitPrice" "the limit price, if any" (optionalOf qShape) _.limitPrice)
    (\p q l -> { pair: p, quantity: q, limitPrice: l })
  pairShape = rec2 "Pair"
    (field "base" "base" "the base Resource" idShape _.base)
    (field "quote" "quote" "the quote Resource" idShape _.quote)
    (\b q -> { base: b, quote: q })
  transferShape = rec4 "Transfer"
    (field "from" "from" "the giving Entity" idShape _.from)
    (field "to" "to" "the receiving Entity" idShape _.to)
    (field "resource" "resource" "the Resource" idShape _.resource)
    (field "quantity" "quantity" "the quantity" qShape _.quantity)
    (\f t r q -> { from: f, to: t, resource: r, quantity: q })
  transformShape = rec3 "Transform"
    (field "by" "by" "the Entity that runs it" idShape _.by)
    (field "transformation" "transformation" "the transformation" idShape _.transformation)
    (field "quantity" "quantity" "the quantity run" qShape _.quantity)
    (\b t q -> { by: b, transformation: t, quantity: q })

instance EncodeJson (RuleF Q) where
  encodeJson = encodeJsonWith Snapshot ruleShape

instance DecodeJson (RuleF Q) where
  decodeJson = decodeJsonWith Snapshot ruleShape

instance EncodeJson (RuleChangeF Q) where
  encodeJson = encodeJsonWith Snapshot ruleChangeShape

instance DecodeJson (RuleChangeF Q) where
  decodeJson = decodeJsonWith Snapshot ruleChangeShape

instance EncodeJson (IntentF Q) where
  encodeJson = encodeJsonWith Snapshot intentShape

instance DecodeJson (IntentF Q) where
  decodeJson = decodeJsonWith Snapshot intentShape

proposalId :: RoundIndex -> EntityId -> Int -> ProposalId
proposalId r (Id author) i = Id ("r" <> show r <> "/" <> author <> "/" <> show i)

-- | The index a Proposal id was built with: the segment after its last "/". Nothing for an id
-- | that `proposalId` could not have made.
submissionIndex :: ProposalId -> Maybe Int
submissionIndex (Id s) = Int.fromString =<< last (split (Pattern "/") s)

-- | The Resource a Behavior Domain names: a domain is the Resource whose purchases are observed.
domainResource :: BehaviorDomain -> ResourceId
domainResource (Id s) = Id s
