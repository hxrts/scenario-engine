-- | Core types (docs/02_type_system.md §2–§8). JSON codecs are the class instances
-- | declared beside each type.
module Core.Types where

import Prelude

import Core.Num (Q, qShape)
import Core.Shape (Format(..), RecF, Shape, arrayOf, atTagged, class Described, decodeJsonWith, encodeJsonWith, fieldOf, genericEnum, genericSum, mapOf, object, optionalOf, prim, recordShape, stringShape, unitShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (note)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Array (last)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Const (Const(..))
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)

-- Identifiers ----------------------------------------------------------------

-- | An identifier's text.
idText :: forall t. Id t -> String
idText (Id s) = s

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

-- | The kind of thing a reference names, for a load-time check: the runtime twin of the tags below.
data RefKind
  = ResourceRef
  | Market
  | TransformationRef
  | EntityRef
  | CapacityName
  | SchemeRef
  | PolicyRef
  | ParameterRef
  | ProcedureRef
  | RuleKindRef

derive instance Eq RefKind

instance Show RefKind where
  show = case _ of
    ResourceRef -> "resource"
    Market -> "market"
    TransformationRef -> "transformation"
    EntityRef -> "entity"
    CapacityName -> "capacity"
    SchemeRef -> "scheme"
    PolicyRef -> "policy"
    ParameterRef -> "parameter"
    ProcedureRef -> "procedure"
    RuleKindRef -> "rule kind"

data EntityTag
data ResourceTag
data MarketTag
data TransformationTag
data CapacityTag
data ProcedureTag
data RuleTag
data RuleKindTag
data ProposalTag
data PolicyTag
data SeatTag
data RepresentationTag
data DomainTag
data RecurringFlowTag
data RecurringKindTag

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
type RecurringFlowId = Id RecurringFlowTag
type RecurringKindId = Id RecurringKindTag

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
  , reservable :: Boolean
  }

data Transferability = Free | Restricted | NonTransferable

data Capability
  = MayTrade MarketId
  | MayTransfer ResourceId
  | MayTransform TransformationId
  | MayEnact RuleKindId

derive instance Ord Capability

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

-- | The two ways a flows Rule applies: into its governor from the counterparty, or either way between them.
data Direction = Inbound | EitherWay

-- | What a Rule covers: conditioned Transfers between two groups, or one capacity.
data Scope
  = Flows { governor :: EntityId, counterparty :: EntityId, direction :: Direction }
  | OnCapacity CapacityRef

-- | What a Rule does: a levy at a rate of the flow, a block on it, or a reduction of a capacity.
data EffectF v = Levy v | Block | Reduce v

-- | A Rule with its amounts in the filling `v`. Its `kind` names a declared kind; the mechanism reads the
-- | `scope` and the `effect`, never the kind.
type RuleF v = { kind :: RuleKindId, scope :: Scope, effect :: EffectF v }

-- | The form of a Rule's effect, as a declaration states it.
data EffectKind = LevyEffect | BlockEffect | ReduceEffect

-- | The form of a Rule's scope, as a declaration states it.
data ScopeKind = InboundFlows | EitherWayFlows | CapacityScope

-- | A declared kind of Rule: the effect and scope a Rule of it must have, and its display text, with
-- | `{governor}`, `{counterparty}`, `{capacity}`, and `{amount}` as placeholders.
type RuleKindDecl = { effect :: EffectKind, scope :: ScopeKind, text :: String }

-- | Whether a Rule has the form its kind declares.
conforms :: forall v. RuleKindDecl -> RuleF v -> Boolean
conforms d r = effectOK && scopeOK
  where
  effectOK = case d.effect, r.effect of
    LevyEffect, Levy _ -> true
    BlockEffect, Block -> true
    ReduceEffect, Reduce _ -> true
    _, _ -> false
  scopeOK = case d.scope, r.scope of
    InboundFlows, Flows f -> f.direction == Inbound
    EitherWayFlows, Flows f -> f.direction == EitherWay
    CapacityScope, OnCapacity _ -> true
    _, _ -> false

-- | The Entity that governs a flows Rule; a capacity Rule has none.
governorOf :: forall v. RuleF v -> Maybe EntityId
governorOf r = case r.scope of
  Flows f -> Just f.governor
  OnCapacity _ -> Nothing

-- | Enact a Rule, or repeal one by the id the engine assigned it.
data RuleChangeF v
  = Enact (RuleF v)
  | Repeal RuleId

type Intent = IntentF MarketId Q

-- | An Intent with its Market position resolved: `Nothing` when the position does not resolve. Only a
-- | Trade names a Market; every other kind passes through unchanged.
withMarket :: forall m n v. (m -> Maybe n) -> IntentF m v -> Maybe (IntentF n v)
withMarket f = traverseIntent (samePositions { market = f })

-- | What to do at each kind of position an Intent holds. The Market position and the fillings may change
-- | type; a reference stays the reference it is.
type IntentPositions f m n v w =
  { market :: m -> f n
  , entity :: EntityId -> f EntityId
  , value :: v -> f w
  , resource :: ResourceId -> f ResourceId
  , transformation :: TransformationId -> f TransformationId
  , capacity :: CapacityRef -> f CapacityRef
  , rule :: RuleId -> f RuleId
  }

-- | Every position kept as it is. A walk overrides the positions it changes.
samePositions :: forall f m v. Applicative f => IntentPositions f m m v v
samePositions = { market: pure, entity: pure, value: pure, resource: pure, transformation: pure, capacity: pure, rule: pure }

-- | Every position read as nothing. A fold overrides the positions it reads.
ignorePositions :: forall r m n v w. Monoid r => IntentPositions (Const r) m n v w
ignorePositions =
  { market: \_ -> Const mempty
  , entity: \_ -> Const mempty
  , value: \_ -> Const mempty
  , resource: \_ -> Const mempty
  , transformation: \_ -> Const mempty
  , capacity: \_ -> Const mempty
  , rule: \_ -> Const mempty
  }

-- | What an Intent's positions read as, in the order `traverseIntent` visits them.
foldIntent :: forall r m v. Monoid r => IntentPositions (Const r) m Unit v Unit -> IntentF m v -> r
foldIntent p = un Const <<< traverseIntent p

-- | Rebuild an Intent position by position, in one fixed order: the Market or the pair's Resources, then a
-- | Transfer's Resource and parties or a Transform's Entity and Transformation, then the Rule's positions,
-- | then the quantity and the limit. Selector products and Proposal ids depend on this order, which is why
-- | walks go through here and not through the derived `Foldable` (it visits the limit before the quantity).
traverseIntent
  :: forall f m n v w
   . Applicative f
  => IntentPositions f m n v w
  -> IntentF m v
  -> f (IntentF n w)
traverseIntent t = case _ of
  Trade x -> (\market quantity limitPrice -> Trade { market, quantity, limitPrice }) <$> t.market x.market <*> t.value x.quantity <*> traverse t.value x.limitPrice
  Route x -> (\base quote quantity limitPrice -> Route { pair: { base, quote }, quantity, limitPrice }) <$> t.resource x.pair.base <*> t.resource x.pair.quote <*> t.value x.quantity <*> traverse t.value x.limitPrice
  Transfer x -> (\resource from to quantity -> Transfer { from, to, resource, quantity }) <$> t.resource x.resource <*> t.entity x.from <*> t.entity x.to <*> t.value x.quantity
  Transform x -> (\by transformation quantity -> Transform { by, transformation, quantity }) <$> t.entity x.by <*> t.transformation x.transformation <*> t.value x.quantity
  ChangeRule (Enact r) -> (\scope effect -> ChangeRule (Enact { kind: r.kind, scope, effect })) <$> scopeAt r.scope <*> effectAt r.effect
  ChangeRule (Repeal r) -> (ChangeRule <<< Repeal) <$> t.rule r
  where
  scopeAt = case _ of
    Flows f -> (\governor counterparty -> Flows { governor, counterparty, direction: f.direction }) <$> t.entity f.governor <*> t.entity f.counterparty
    OnCapacity c -> OnCapacity <$> t.capacity c
  effectAt = case _ of
    Levy v -> Levy <$> t.value v
    Block -> pure Block
    Reduce v -> Reduce <$> t.value v

type Rule = RuleF Q
type RuleKindId = Id RuleKindTag
type RuleChange = RuleChangeF Q

-- | Whether a Deal may bundle this Intent: a Transfer or a Transform. A Trade, a routed order, and a
-- | change of Rule are one party's own action (docs/02 §3.3). The one statement of the rule: screening,
-- | the premise shape of a Deal, and the prompt's wording all follow it.
dealIntent :: forall m v. IntentF m v -> Boolean
dealIntent = case _ of
  Transfer _ -> true
  Transform _ -> true
  Trade _ -> false
  Route _ -> false
  ChangeRule _ -> false

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

-- Markets and Transformations (§5) ------------------------------------------------

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

-- | A per-unit change to a capacity, applied after the Transformation's delay. `holder` defaults
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
derive instance Generic Direction _
derive instance Generic Scope _
derive instance Generic (EffectF v) _
derive instance Generic EffectKind _
derive instance Generic ScopeKind _
derive instance Generic (IntentF m v) _
derive instance Generic (RuleChangeF v) _

derive instance Functor (IntentF m)
derive instance Foldable (IntentF m)
derive instance Traversable (IntentF m)
derive instance Functor EffectF
derive instance Foldable EffectF
derive instance Traversable EffectF
derive instance Functor RuleChangeF
derive instance Foldable RuleChangeF
derive instance Traversable RuleChangeF

derive instance Eq EntityKind
derive instance Eq Controller
derive instance Eq Transferability
derive instance Eq Capability
derive instance Eq Direction
derive instance Eq Scope
derive instance Eq EffectKind
derive instance Eq ScopeKind
derive instance (Eq m, Eq v) => Eq (IntentF m v)
derive instance Eq v => Eq (EffectF v)
derive instance Eq v => Eq (RuleChangeF v)

instance Show EntityKind where
  show = genericShow

instance Show Controller where
  show = genericShow

instance Show Transferability where
  show = genericShow

instance Show Capability where
  show = genericShow

instance Show EffectKind where
  show = genericShow

instance Show ScopeKind where
  show = genericShow

instance Show Direction where
  show = genericShow

instance Show Scope where
  show = genericShow

instance (Show m, Show v) => Show (IntentF m v) where
  show = genericShow

instance Show v => Show (EffectF v) where
  show = genericShow

instance Show v => Show (RuleChangeF v) where
  show = genericShow

-- Described shapes (design 4, §5.3). The engine's Intents, Rules and rule changes are described once
-- here. Their JSON codecs are the snapshot codec of the shape, so no second codec exists for them.
-- Each sum is written in the snapshot form (`{"tag":..,"values":[..]}`); the premise-format tags of
-- these types are added with the `schema` package (design 4, Phase 3).

idShape :: forall t. Shape (Id t)
idShape = prim "Id" (\(Id s) -> fromString s) (\j -> note "expected an id string" (map Id (toString j)))

instance Described (RuleChangeF Q) where
  shape = ruleChangeShape

instance Described (IntentF MarketId Q) where
  shape = intentShape

directionShape :: Shape Direction
directionShape = genericEnum "Direction"

-- | A flows scope's body.
flowsBody :: Shape { governor :: EntityId, counterparty :: EntityId, direction :: Direction }
flowsBody = object "Flows"
  ( (\governor counterparty direction -> { governor, counterparty, direction })
      <$> fieldOf "governor" "the Entity that governs the Rule" idShape _.governor
      <*> fieldOf "counterparty" "the other party" idShape _.counterparty
      <*> fieldOf "direction" "inbound to the governor, or either way" directionShape _.direction
  )

scopeShape :: Shape Scope
scopeShape = genericSum "Scope"
  { "Flows": atTagged "conditioned Transfers between two groups" flowsBody
  , "OnCapacity": atTagged "one capacity" idShape
  }

effectShape :: forall v. Shape v -> Shape (EffectF v)
effectShape v = genericSum "Effect"
  { "Levy": atTagged "a levy at this rate of the flow" v
  , "Block": atTagged "the flow is refused" unitShape
  , "Reduce": atTagged "the capacity is reduced by these units" v
  }

ruleShape :: Shape (RuleF Q)
ruleShape = recordShape "Rule" { kind: idShape, scope: scopeShape, effect: effectShape qShape }

ruleKindDeclShape :: Shape RuleKindDecl
ruleKindDeclShape = recordShape "RuleKind" { effect: effectKindShape, scope: scopeKindShape, text: stringShape }

effectKindShape :: Shape EffectKind
effectKindShape = genericEnum "EffectKind"

scopeKindShape :: Shape ScopeKind
scopeKindShape = genericEnum "ScopeKind"

-- The bodies the snapshot, premise and template forms of an Intent share, over its filling `v`. The
-- tag forms differ by format; these do not.

-- | A Transfer.
transferBody :: forall v. Shape v -> Shape { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: v }
transferBody v = object "Transfer"
  ( (\f t r q -> { from: f, to: t, resource: r, quantity: q })
      <$> fieldOf "from" "the giving Entity" idShape _.from
      <*> fieldOf "to" "the receiving Entity" idShape _.to
      <*> fieldOf "resource" "the Resource" idShape _.resource
      <*> fieldOf "quantity" "the quantity" v _.quantity
  )

ruleChangeShape :: Shape (RuleChangeF Q)
ruleChangeShape = genericSum "RuleChange"
  { "Enact": atTagged "enact a Rule" ruleShape
  , "Repeal": atTagged "repeal a Rule by its id" idShape
  }

intentShape :: Shape Intent
intentShape = genericSum "Intent"
  { "Trade": atTagged "an order in a Market" tradeShape
  , "Route": atTagged "a best-execution order on a pair of Resources" routeShape
  , "Transfer": atTagged "a Resource given from one Entity to another" transferShape
  , "Transform": atTagged "a run of a transformation" transformShape
  , "ChangeRule": atTagged "enact or repeal a Rule" ruleChangeShape
  }
  where
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
        -- required by the rule, not the decoder: the engine records the rejection of a stored or model-written order without one
        <*> fieldOf "limitPrice" "the limit price, which a routed order must state; an order without one is rejected" ((optionalOf qShape) { desc = qShape.desc }) _.limitPrice
    )
  pairShape = object "Pair"
    ( (\b q -> { base: b, quote: q })
        <$> fieldOf "base" "the base Resource" idShape _.base
        <*> fieldOf "quote" "the quote Resource" idShape _.quote
    )
  transferShape = transferBody qShape
  transformShape = object "Transform"
    ( (\b t q -> { by: b, transformation: t, quantity: q })
        <$> fieldOf "by" "the Entity that runs it" idShape _.by
        <*> fieldOf "transformation" "the transformation" idShape _.transformation
        <*> fieldOf "quantity" "the quantity run" qShape _.quantity
    )

-- The entity, capability, and proposal shapes (design 4, §5.3). The closure of a View is described
-- here, so the model's requests and the Run Store's records encode through these shapes.

transferabilityShape :: Shape Transferability
transferabilityShape = genericEnum "Transferability"

capabilityShape :: Shape Capability
capabilityShape = genericSum "Capability"
  { "MayTrade": atTagged "a Market the Entity may trade in" idShape
  , "MayTransfer": atTagged "a Resource the Entity may transfer" idShape
  , "MayTransform": atTagged "a Transformation the Entity may run" idShape
  , "MayEnact": atTagged "a kind of Rule the Entity may enact" idShape
  }

controllerShape :: Shape Controller
controllerShape = genericSum "Controller"
  { "Seat": atTagged "a seat of the game" idShape
  , "NPC": atTagged "a Policy" idShape
  }

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
entityKindShape = genericSum "EntityKind"
  { "Actor": atTagged "an Entity a Controller acts for" controllerShape
  , "Aggregate": atTagged "an Entity made of members" aggregateShape
  }

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

instance EncodeJson Direction where
  encodeJson = encodeJsonWith Snapshot directionShape

instance DecodeJson Direction where
  decodeJson = decodeJsonWith Snapshot directionShape

instance EncodeJson Scope where
  encodeJson = encodeJsonWith Snapshot scopeShape

instance DecodeJson Scope where
  decodeJson = decodeJsonWith Snapshot scopeShape

instance EncodeJson (EffectF Q) where
  encodeJson = encodeJsonWith Snapshot (effectShape qShape)

instance DecodeJson (EffectF Q) where
  decodeJson = decodeJsonWith Snapshot (effectShape qShape)

instance EncodeJson EffectKind where
  encodeJson = encodeJsonWith Snapshot effectKindShape

instance DecodeJson EffectKind where
  decodeJson = decodeJsonWith Snapshot effectKindShape

instance EncodeJson ScopeKind where
  encodeJson = encodeJsonWith Snapshot scopeKindShape

instance DecodeJson ScopeKind where
  decodeJson = decodeJsonWith Snapshot scopeKindShape

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

-- | The author a Proposal id was built with: the segment before its index. Nothing for an id that
-- | `proposalId` could not have made.
proposalAuthor :: ProposalId -> Maybe EntityId
proposalAuthor (Id s) = case split (Pattern "/") s of
  [ _, author, _ ] -> Just (Id author)
  _ -> Nothing

-- | The id the engine gives the `i`th Rule a Proposal enacts.
ruleIdOf :: ProposalId -> Int -> RuleId
ruleIdOf p i = Id (show p <> "/rule-" <> show i)

-- | The index a Proposal id was built with: the segment after its last "/". Nothing for an id
-- | that `proposalId` could not have made.
submissionIndex :: ProposalId -> Maybe Int
submissionIndex (Id s) = Int.fromString =<< last (split (Pattern "/") s)

-- | The Resource a Behavior Domain names: a domain is the Resource whose purchases are observed.
domainResource :: BehaviorDomain -> ResourceId
domainResource (Id s) = Id s
