-- | Core types (docs/02_type_system.md §2–§8). JSON codecs are the class instances
-- | declared beside each type.
module Core.Types where

import Prelude

import Core.Num (Q)
import Core.Shape (Format(..), Shape, arrayOf, class Described, ctorOf, decodeJsonWith, encodeJsonWith, fieldOf, mapOf, object, optionalOf, prim, qShape, sumOf, unitShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
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

-- | A base and quote Resource pair, which a routed order names in place of a Market.
type Pair = { base :: ResourceId, quote :: ResourceId }

-- | An Intent with its Market position in `m` and its quantities and limits in the filling `v`:
-- | `MarketId` and `Q` for an Intent the engine settles (`Intent`), `MarketRef` and `Expr` for a
-- | Strategy's template (`Core.Strategy`). Every kind of Core Intent is one constructor here, so the
-- | two fillings are the only things the two languages differ in.
data IntentF m v
  = Trade { market :: m, quantity :: v, limitPrice :: Maybe v }
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

type Intent = IntentF MarketId Q

-- | An Intent with its Market position resolved: `Nothing` when the position does not resolve. Only a
-- | Trade names a Market; every other kind passes through unchanged.
withMarket :: forall m n v. (m -> Maybe n) -> IntentF m v -> Maybe (IntentF n v)
withMarket f = case _ of
  Trade x -> (\m -> Trade { market: m, quantity: x.quantity, limitPrice: x.limitPrice }) <$> f x.market
  Route x -> Just (Route x)
  Transfer x -> Just (Transfer x)
  Transform x -> Just (Transform x)
  ChangeRule x -> Just (ChangeRule x)

type Rule = RuleF Q
type RuleChange = RuleChangeF Q

-- | What a Proposal says before its id and author are assigned: its Intents, the signers it asks
-- | for, and the collective procedure it runs under (if any). Procedure is a Proposal concern, not an
-- | Intent kind (docs/02 §3). A Strategy's Deal and Procedure templates are `ProposalF MarketRef Expr`.
type ProposalF m v =
  { intents :: Array (IntentF m v)
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
derive instance Generic (IntentF m v) _
derive instance Generic (RuleF v) _
derive instance Generic (RuleChangeF v) _

derive instance Functor (IntentF m)
derive instance Foldable (IntentF m)
derive instance Traversable (IntentF m)
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
derive instance (Eq m, Eq v) => Eq (IntentF m v)
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

instance (Show m, Show v) => Show (IntentF m v) where
  show = genericShow

instance Show v => Show (RuleF v) where
  show = genericShow

instance Show v => Show (RuleChangeF v) where
  show = genericShow

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

instance Described (IntentF MarketId Q) where
  shape = intentShape

ruleShape :: Shape (RuleF Q)
ruleShape = sumOf "Rule" [ tariff, accessBan, withholding ]
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
  tariffShape = object "Tariff"
    ( (\j o r -> { jurisdiction: j, origin: o, rate: r })
        <$> fieldOf "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction
        <*> fieldOf "origin" "the Entity whose goods it taxes" idShape _.origin
        <*> fieldOf "rate" "the rate" qShape _.rate
    )
  accessBanShape = object "AccessBan"
    ( (\j t -> { jurisdiction: j, target: t })
        <$> fieldOf "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction
        <*> fieldOf "target" "the Entity whose shipments are banned" idShape _.target
    )
  withholdingShape = object "Withholding"
    ( (\c u -> { capacity: c, units: u })
        <$> fieldOf "capacity" "the capacity withheld" idShape _.capacity
        <*> fieldOf "units" "units withheld" qShape _.units
    )

ruleChangeShape :: Shape (RuleChangeF Q)
ruleChangeShape = sumOf "RuleChange" [ enact, repeal ]
  where
  enact = ctorOf { tag: "Enact", desc: "enact a Rule" } ruleShape projEnact Enact
  repeal = ctorOf { tag: "Repeal", desc: "repeal a Rule by its id" } idShape projRepeal Repeal
  projEnact = case _ of
    Enact r -> Just r
    _ -> Nothing
  projRepeal = case _ of
    Repeal r -> Just r
    _ -> Nothing

intentShape :: Shape Intent
intentShape = sumOf "Intent" [ trade, route, transfer, transform, changeRule ]
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
  tradeShape = object "Trade"
    ( (\m q l -> { market: m, quantity: q, limitPrice: l })
        <$> fieldOf "market" "the Market" idShape _.market
        <*> fieldOf "quantity" "+ buys, - sells the Market's base Resource" qShape _.quantity
        <*> fieldOf "limitPrice" "the limit price, if any" (optionalOf qShape) _.limitPrice
    )
  routeShape = object "Route"
    ( (\p q l -> { pair: p, quantity: q, limitPrice: l })
        <$> fieldOf "pair" "the base and quote Resources" pairShape _.pair
        <*> fieldOf "quantity" "+ buys, - sells the base Resource" qShape _.quantity
        <*> fieldOf "limitPrice" "the limit price, if any" (optionalOf qShape) _.limitPrice
    )
  pairShape = object "Pair"
    ( (\b q -> { base: b, quote: q })
        <$> fieldOf "base" "the base Resource" idShape _.base
        <*> fieldOf "quote" "the quote Resource" idShape _.quote
    )
  transferShape = object "Transfer"
    ( (\f t r q -> { from: f, to: t, resource: r, quantity: q })
        <$> fieldOf "from" "the giving Entity" idShape _.from
        <*> fieldOf "to" "the receiving Entity" idShape _.to
        <*> fieldOf "resource" "the Resource" idShape _.resource
        <*> fieldOf "quantity" "the quantity" qShape _.quantity
    )
  transformShape = object "Transform"
    ( (\b t q -> { by: b, transformation: t, quantity: q })
        <$> fieldOf "by" "the Entity that runs it" idShape _.by
        <*> fieldOf "transformation" "the transformation" idShape _.transformation
        <*> fieldOf "quantity" "the quantity run" qShape _.quantity
    )

-- The entity, capability, and proposal shapes (design 4, §5.3). The closure of a View is described
-- here, so the model's requests and the Run Store's records encode through these shapes.

-- | A sum of constructors with no payload, written as its tag alone.
enumShape :: forall a. Eq a => String -> Array { tag :: String, value :: a } -> Shape a
enumShape name ctors = sumOf name (map nullary ctors)
  where
  nullary c = ctorOf { tag: c.tag, desc: "" } unitShape (\x -> if x == c.value then Just unit else Nothing) (const c.value)

transferabilityShape :: Shape Transferability
transferabilityShape = enumShape "Transferability"
  [ { tag: "Free", value: Free }, { tag: "Restricted", value: Restricted }, { tag: "NonTransferable", value: NonTransferable } ]

ruleKindShape :: Shape RuleKind
ruleKindShape = enumShape "RuleKind"
  [ { tag: "TariffRule", value: TariffRule }, { tag: "AccessBanRule", value: AccessBanRule }, { tag: "WithholdingRule", value: WithholdingRule } ]

capabilityShape :: Shape Capability
capabilityShape = sumOf "Capability"
  [ ctorOf { tag: "MayTrade", desc: "a Market the Entity may trade in" } idShape
      ( \c -> case c of
          MayTrade m -> Just m
          _ -> Nothing
      )
      MayTrade
  , ctorOf { tag: "MayTransfer", desc: "a Resource the Entity may transfer" } idShape
      ( \c -> case c of
          MayTransfer r -> Just r
          _ -> Nothing
      )
      MayTransfer
  , ctorOf { tag: "MayTransform", desc: "a Transformation the Entity may run" } idShape
      ( \c -> case c of
          MayTransform t -> Just t
          _ -> Nothing
      )
      MayTransform
  , ctorOf { tag: "MayEnact", desc: "a kind of Rule the Entity may enact" } ruleKindShape
      ( \c -> case c of
          MayEnact k -> Just k
          _ -> Nothing
      )
      MayEnact
  ]

controllerShape :: Shape Controller
controllerShape = sumOf "Controller"
  [ ctorOf { tag: "Seat", desc: "a seat of the game" } idShape
      ( \c -> case c of
          Seat s -> Just s
          _ -> Nothing
      )
      Seat
  , ctorOf { tag: "NPC", desc: "a Policy" } idShape
      ( \c -> case c of
          NPC p -> Just p
          _ -> Nothing
      )
      NPC
  ]

collectiveProcedureShape :: Shape CollectiveProcedure
collectiveProcedureShape = object "CollectiveProcedure"
  ( (\w r t a -> { weights: w, residualWeight: r, threshold: t, authorizes: a })
      <$> fieldOf "weights" "the weight of each member" (mapOf idShape qShape) _.weights
      <*> fieldOf "residualWeight" "the weight of the residual" qShape _.residualWeight
      <*> fieldOf "threshold" "the support needed" qShape _.threshold
      <*> fieldOf "authorizes" "the Capabilities the procedure authorizes" (arrayOf capabilityShape) _.authorizes
  )

aggregateShape :: Shape Aggregate
aggregateShape = object "Aggregate"
  ( (\p m pr -> { policy: p, members: m, procedures: pr })
      <$> fieldOf "policy" "the Policy that decides for it" idShape _.policy
      <*> fieldOf "members" "the Entities it is made of" (arrayOf idShape) _.members
      <*> fieldOf "procedures" "its collective procedures" (mapOf idShape collectiveProcedureShape) _.procedures
  )

entityKindShape :: Shape EntityKind
entityKindShape = sumOf "EntityKind"
  [ ctorOf { tag: "Actor", desc: "an Entity a Controller acts for" } controllerShape
      ( \k -> case k of
          Actor c -> Just c
          _ -> Nothing
      )
      Actor
  , ctorOf { tag: "Aggregate", desc: "an Entity made of members" } aggregateShape
      ( \k -> case k of
          Aggregate a -> Just a
          _ -> Nothing
      )
      Aggregate
  ]

entityShape :: Shape Entity
entityShape = object "Entity"
  ( (\i a c cs k -> { id: i, accounts: a, capacities: c, capabilities: cs, kind: k })
      <$> fieldOf "id" "the Entity's id" idShape _.id
      <*> fieldOf "accounts" "its holdings of each Resource" (mapOf idShape qShape) _.accounts
      <*> fieldOf "capacities" "its capacities" (mapOf idShape qShape) _.capacities
      <*> fieldOf "capabilities" "what it may do" (arrayOf capabilityShape) _.capabilities
      <*> fieldOf "kind" "an Actor or an Aggregate" entityKindShape _.kind
  )

proposalShape :: Shape Proposal
proposalShape = object "Proposal"
  ( (\i a is ss p -> { id: i, author: a, intents: is, signers: ss, procedure: p })
      <$> fieldOf "id" "the Proposal's id" idShape _.id
      <*> fieldOf "author" "the Entity that submitted it" idShape _.author
      <*> fieldOf "intents" "the Intents" (arrayOf intentShape) _.intents
      <*> fieldOf "signers" "the Entities asked to sign" (arrayOf idShape) _.signers
      <*> fieldOf "procedure" "the collective procedure, if any" (optionalOf idShape) _.procedure
  )

signedProposalShape :: Shape SignedProposal
signedProposalShape = object "SignedProposal"
  ( (\p s -> { proposal: p, signatures: s })
      <$> fieldOf "proposal" "the Proposal" proposalShape _.proposal
      <*> fieldOf "signatures" "the Entities that signed it" (arrayOf idShape) _.signatures
  )

instance Described Transferability where
  shape = transferabilityShape

instance Described RuleKind where
  shape = ruleKindShape

instance Described Capability where
  shape = capabilityShape

instance Described Controller where
  shape = controllerShape

instance Described EntityKind where
  shape = entityKindShape

instance EncodeJson Transferability where
  encodeJson = encodeJsonWith Snapshot transferabilityShape

instance DecodeJson Transferability where
  decodeJson = decodeJsonWith Snapshot transferabilityShape

instance EncodeJson RuleKind where
  encodeJson = encodeJsonWith Snapshot ruleKindShape

instance DecodeJson RuleKind where
  decodeJson = decodeJsonWith Snapshot ruleKindShape

instance EncodeJson Capability where
  encodeJson = encodeJsonWith Snapshot capabilityShape

instance DecodeJson Capability where
  decodeJson = decodeJsonWith Snapshot capabilityShape

instance EncodeJson Controller where
  encodeJson = encodeJsonWith Snapshot controllerShape

instance DecodeJson Controller where
  decodeJson = decodeJsonWith Snapshot controllerShape

instance EncodeJson EntityKind where
  encodeJson = encodeJsonWith Snapshot entityKindShape

instance DecodeJson EntityKind where
  decodeJson = decodeJsonWith Snapshot entityKindShape

instance EncodeJson (RuleF Q) where
  encodeJson = encodeJsonWith Snapshot ruleShape

instance DecodeJson (RuleF Q) where
  decodeJson = decodeJsonWith Snapshot ruleShape

instance EncodeJson (RuleChangeF Q) where
  encodeJson = encodeJsonWith Snapshot ruleChangeShape

instance DecodeJson (RuleChangeF Q) where
  decodeJson = decodeJsonWith Snapshot ruleChangeShape

instance EncodeJson (IntentF MarketId Q) where
  encodeJson = encodeJsonWith Snapshot intentShape

instance DecodeJson (IntentF MarketId Q) where
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
