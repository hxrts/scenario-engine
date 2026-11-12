-- | The authored vocabulary: Actors, Capabilities, Intents, candidate Transactions, Rules and Transformations
-- | (docs/03_premise.md). Core knows none of it.
module Laws.Types (module Laws.Types, module Core.Types) where

import Core.Shape (Desc(..), defaultsTo, refersTo, Format(..), Shape, arrayOf, atTagged, decodeJsonWith, encodeWith, enumName, field, fieldOf, genericEnum, genericSum, keyedBy, mapOf, object, optionalOf, recordOf, recordShape, stringShape, unitShape, (&.))
import Core.Types (Id(..), idText, idShape, ActorTag, ResourceTag, ExchangeTag, ActorId, ResourceId, ExchangeId, Quantity, Price, Ratio, RoundIndex, RoundOffset, Interval, Holdings, CostFunction, Exchange)
import Prelude
import Core.Num (Q, qShape)
import Core.Num as Q
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Array (last)
import Data.Array as Array
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Const (Const(..))
import Data.Newtype (un)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.String as String
import Type.Proxy (Proxy(..))

-- Identifiers ----------------------------------------------------------------

-- | The kind of thing a reference names, for a load-time check: the runtime twin of the tags below.
data RefKind
  = RefResource
  | RefExchange
  | RefTransformation
  | RefActor
  | RefStrategy
  | RefParameter
  | RefProcedure
  | RefRuleKind
  | RefTemplate

derive instance Eq RefKind

derive instance Generic RefKind _

-- | The kind in words: the constructor without `Ref`, in lower case, a capital K as a word break (`RefRuleKind` is "rule kind").
instance Show RefKind where
  show k = String.toLower (replaceAll (Pattern "K") (Replacement " K") (String.drop 3 (enumName k)))

-- | The kind of reference an id of this tag is, so a reference cannot pair a kind with the wrong id.
class RefKindOf :: Type -> Constraint
class RefKindOf tag where
  refKindOf :: Proxy tag -> RefKind

instance RefKindOf ActorTag where
  refKindOf _ = RefActor

instance RefKindOf ResourceTag where
  refKindOf _ = RefResource

instance RefKindOf ExchangeTag where
  refKindOf _ = RefExchange

instance RefKindOf TransformationTag where
  refKindOf _ = RefTransformation

instance RefKindOf StrategyTag where
  refKindOf _ = RefStrategy

instance RefKindOf ProcedureTag where
  refKindOf _ = RefProcedure

instance RefKindOf RuleKindTag where
  refKindOf _ = RefRuleKind

-- | An id described as naming its kind of thing in the World, so a vocabulary can be derived from the description.
refIdShape :: forall t. RefKindOf t => Shape (Id t)
refIdShape = refersTo (show (refKindOf (Proxy :: Proxy t))) idShape

-- | The name of an agreement template.
templateNameShape :: Shape String
templateNameShape = refersTo (show RefTemplate) stringShape

-- | An id the engine assigns during a game, which no World at load can list.
assignedIdShape :: forall t. Shape (Id t)
assignedIdShape = idShape { desc = DPrim "engine-assigned id" }

data TransformationTag
data ProcedureTag
data RuleTag
data RuleKindTag
data CandidateTransactionTag
data StrategyTag
data SeatTag
data RepresentationTag
data DomainTag
data RecurringFlowTag
data RecurringKindTag

type TransformationId = Id TransformationTag
type CollectiveProcedureId = Id ProcedureTag
type RuleId = Id RuleTag
type CandidateTransactionId = Id CandidateTransactionTag
type StrategyId = Id StrategyTag
type SeatId = Id SeatTag
type RepresentationId = Id RepresentationTag
type BehaviorDomain = Id DomainTag
type RecurringFlowId = Id RecurringFlowTag
type RecurringKindId = Id RecurringKindTag

-- Actors (§2.1, §2.2) ------------------------------------------------------

type Actor =
  { id :: ActorId
  , accounts :: Holdings
  , capabilities :: Array Capability
  , kind :: ActorKind
  }

data ActorKind
  = PlayableActor SeatId
  | NonPlayableActor StrategyId
  | Aggregate Aggregate

-- | An Aggregate's strategy, members, and collective procedures (§2.2).
type Aggregate =
  { strategy :: StrategyId
  , members :: Array ActorId
  , procedures :: Map CollectiveProcedureId CollectiveProcedure
  }

type CollectiveProcedure =
  { weights :: Map ActorId Ratio
  , residualWeight :: Ratio
  , threshold :: Ratio
  , authorizes :: Array Capability
  }

-- Resources and capabilities (§2.3, §2.4) ------------------------------------

type ResourceDefinition =
  { id :: ResourceId
  , unit :: String
  , transferability :: Transferability
  , category :: Maybe String -- a premise's own grouping of the Resource; only the views read it
  }

data Transferability = Free | Restricted | NonTransferable

data Capability
  = MayTrade ExchangeId
  | MayTransfer ResourceId
  | MayTransform TransformationId
  | MayEnact RuleKindId

derive instance Ord Capability

-- Intents and candidate Transactions (§3) -------------------------------------------------

-- | A base and quote Resource pair, which a routed order names in place of an Exchange.
type Pair = { base :: ResourceId, quote :: ResourceId }

-- | An Intent with its Exchange position in `m` and its quantities and limits in the filling `v`:
-- | `ExchangeId` and `Q` for an Intent the engine settles (`Intent`), `ExchangeRef` and `Expr` for a
-- | StrategyRules's template (`Laws.StrategyRules`). Every kind of Core Intent is one constructor here, so the
-- | two fillings are the only things the two languages differ in.
data IntentF m v
  = Trade { exchange :: m, quantity :: v, limitPrice :: Maybe v }
  | Route { pair :: Pair, quantity :: v, limitPrice :: Maybe v }
  | Transfer { from :: ActorId, to :: ActorId, resource :: ResourceId, quantity :: v }
  | Transform { by :: ActorId, transformation :: TransformationId, quantity :: v }
  | ChangeRule (RuleChangeF v)
  | Enter (EnterF v)

-- | Entering an agreement: the template, its arguments, and the Actor bound to each of its parties.
type EnterF v = { template :: String, args :: Array { name :: String, value :: v }, parties :: Array { role :: String, actor :: ActorId } }

-- | Which way a flows Rule applies, relative to its governor: flows into it, out of it, or either way.
data Direction = Inbound | Outbound | EitherWay

-- | What a flows Rule covers: Transfers between two groups.
type FlowScope = { governor :: ActorId, counterparty :: ActorId, direction :: Direction, over :: Array ResourceId }

-- | What a levy is charged on: the units moved, or their value at an Exchange's beginning-of-round price.
data LevyBasis = PerUnit | AdValorem ExchangeId

-- | Who receives a levy: the group the flow is going to, the Rule's governor, or nobody.
data LevyRecipient = LevyToDestination | LevyToGovernor | LevyToSink

-- | Who pays a levy: the side the flow leaves (the seller of a trade), or the side it reaches (the buyer).
data LevyPayer = PaidBySender | PaidByReceiver

-- | A levy: its rate on its basis, the Resource it is paid in, who receives it, and who pays it.
type LevyF v = { rate :: v, basis :: LevyBasis, in :: ResourceId, to :: LevyRecipient, payer :: LevyPayer }

-- | What a flows Rule does: a levy on the flow, or a block on it.
data FlowEffect v = Levy (LevyF v) | Block

-- | A Rule, with its amounts in the filling `v`: one on flows, or one reducing what the holder of a Resource can
-- | have drawn. Its `kind` names a declared kind; the mechanism reads the rest, never the kind. A combination
-- | that would do nothing, such as a levy on what is drawn, has no form.
data RuleF v
  = FlowRule { kind :: RuleKindId, scope :: FlowScope, effect :: FlowEffect v }
  | HoldingRule { kind :: RuleKindId, resource :: ResourceId, reduce :: v }

-- | Who governs a flows Rule, which fixes its direction: a flows Rule's direction is relative to its governor.
data FlowGovernance = GovernedByDestination | GovernedBySource | GovernedByEither

-- | The form a kind of Rule takes, and so who governs it. A holding Rule is governed by the Resource's holder.
data RuleForm = LevyFlows FlowGovernance | BlockFlows FlowGovernance | ReduceHolding

-- | A declared kind of Rule: its form, and its display text, with `{governor}`, `{counterparty}`,
-- | `{resource}`, and `{amount}` as placeholders.
type RuleKindDecl = { form :: RuleForm, text :: String }

-- | The direction a governance fixes.
governedDirection :: FlowGovernance -> Direction
governedDirection = case _ of
  GovernedByDestination -> Inbound
  GovernedBySource -> Outbound
  GovernedByEither -> EitherWay

-- | The kind a Rule declares.
ruleKind :: forall v. RuleF v -> RuleKindId
ruleKind = case _ of
  FlowRule r -> r.kind
  HoldingRule r -> r.kind

-- | Whether a Rule has the form its kind declares.
conforms :: forall v. RuleKindDecl -> RuleF v -> Boolean
conforms d = case d.form, _ of
  LevyFlows g, FlowRule { scope, effect: Levy _ } -> covers g scope
  BlockFlows g, FlowRule { scope, effect: Block } -> covers g scope
  ReduceHolding, HoldingRule _ -> true
  _, _ -> false
  where
  -- a flows Rule covering no Resource would do nothing
  covers g scope = scope.direction == governedDirection g && not (Array.null scope.over)

-- | The Actor that governs a flows Rule; a holding Rule has none.
governorOf :: forall v. RuleF v -> Maybe ActorId
governorOf = case _ of
  FlowRule r -> Just r.scope.governor
  HoldingRule _ -> Nothing

-- | Enact a Rule, or repeal one by the id the engine assigned it.
data RuleChangeF v
  = Enact (RuleF v)
  | Repeal RuleId

type Intent = IntentF ExchangeId Q

-- | An Intent with its Exchange position resolved: `Nothing` when the position does not resolve. Only a
-- | Trade names an Exchange; every other kind passes through unchanged.
withExchange :: forall m n v. (m -> Maybe n) -> IntentF m v -> Maybe (IntentF n v)
withExchange f = traverseIntent (samePositions { exchange = f })

-- | What to do at each kind of position an Intent holds. The Exchange position and the fillings may change
-- | type; a reference stays the reference it is.
type IntentPositions f m n v w =
  { exchange :: m -> f n
  , actor :: ActorId -> f ActorId
  , value :: v -> f w
  , resource :: ResourceId -> f ResourceId
  , transformation :: TransformationId -> f TransformationId
  , rule :: RuleId -> f RuleId
  }

-- | Every position kept as it is. A walk overrides the positions it changes.
samePositions :: forall f m v. Applicative f => IntentPositions f m m v v
samePositions = { exchange: pure, actor: pure, value: pure, resource: pure, transformation: pure, rule: pure }

-- | Every position read as nothing. A fold overrides the positions it reads.
ignorePositions :: forall r m n v w. Monoid r => IntentPositions (Const r) m n v w
ignorePositions =
  { exchange: \_ -> Const mempty
  , actor: \_ -> Const mempty
  , value: \_ -> Const mempty
  , resource: \_ -> Const mempty
  , transformation: \_ -> Const mempty
  , rule: \_ -> Const mempty
  }

-- | What an Intent's positions read as, in the order `traverseIntent` visits them.
foldIntent :: forall r m v. Monoid r => IntentPositions (Const r) m Unit v Unit -> IntentF m v -> r
foldIntent p = un Const <<< traverseIntent p

-- | Rebuild an Intent position by position, in one fixed order: the Exchange or the pair's Resources, then a
-- | Transfer's Resource and parties or a Transform's Actor and Transformation, then the Rule's positions,
-- | then the quantity and the limit. Selector products and candidate Transaction ids depend on this order, which is why
-- | walks go through here and not through the derived `Foldable` (it visits the limit before the quantity).
traverseIntent
  :: forall f m n v w
   . Applicative f
  => IntentPositions f m n v w
  -> IntentF m v
  -> f (IntentF n w)
traverseIntent t = case _ of
  Trade x -> (\exchange quantity limitPrice -> Trade { exchange, quantity, limitPrice }) <$> t.exchange x.exchange <*> t.value x.quantity <*> traverse t.value x.limitPrice
  Route x -> (\base quote quantity limitPrice -> Route { pair: { base, quote }, quantity, limitPrice }) <$> t.resource x.pair.base <*> t.resource x.pair.quote <*> t.value x.quantity <*> traverse t.value x.limitPrice
  Transfer x -> (\resource from to quantity -> Transfer { from, to, resource, quantity }) <$> t.resource x.resource <*> t.actor x.from <*> t.actor x.to <*> t.value x.quantity
  Transform x -> (\by transformation quantity -> Transform { by, transformation, quantity }) <$> t.actor x.by <*> t.transformation x.transformation <*> t.value x.quantity
  ChangeRule (Enact (FlowRule r)) -> (\governor counterparty over effect -> ChangeRule (Enact (FlowRule { kind: r.kind, scope: { governor, counterparty, direction: r.scope.direction, over }, effect }))) <$> t.actor r.scope.governor <*> t.actor r.scope.counterparty <*> traverse t.resource r.scope.over <*> effectAt r.effect
  ChangeRule (Enact (HoldingRule r)) -> (\resource reduce -> ChangeRule (Enact (HoldingRule { kind: r.kind, resource, reduce }))) <$> t.resource r.resource <*> t.value r.reduce
  ChangeRule (Repeal r) -> (ChangeRule <<< Repeal) <$> t.rule r
  Enter e -> (\parties args -> Enter { template: e.template, args, parties }) <$> traverse (\p -> p { actor = _ } <$> t.actor p.actor) e.parties <*> traverse (\a -> a { value = _ } <$> t.value a.value) e.args
  where
  effectAt = case _ of
    Levy l -> (\rate in' -> Levy l { rate = rate, in = in' }) <$> t.value l.rate <*> t.resource l.in
    Block -> pure Block

type Rule = RuleF Q
type RuleKindId = Id RuleKindTag
type RuleChange = RuleChangeF Q

-- | Whether a joint candidate Transaction may bundle this Intent: a Transfer or a Transform. A Trade, a routed order, and a
-- | change of Rule are one party's own action (docs/02 §3.3). The one statement of the rule: screening,
-- | the premise shape of a joint candidate Transaction, and the prompt's wording all follow it.
jointIntent :: forall m v. IntentF m v -> Boolean
jointIntent = case _ of
  Transfer _ -> true
  Transform _ -> true
  Trade _ -> false
  Route _ -> false
  ChangeRule _ -> false
  Enter _ -> true

-- | What a candidate Transaction says before its id and author are assigned: its Intents, the signers it asks
-- | for, and the collective procedure it runs under (if any). Procedure is a candidate Transaction concern, not an
-- | Intent kind (docs/02 §3). A StrategyRules's Joint and Procedure templates are `ProposalF ExchangeRef Expr`.
type CandidateTransactionF m v = Record (CandidateTransactionRow m v ())

-- | The Intents a candidate Transaction holds and whom it asks to sign: what every form of a candidate Transaction shares.
type CandidateTransactionCore m v r =
  ( intents :: Array (IntentF m v)
  , signers :: Array ActorId
  | r
  )

type CandidateTransactionRow m v r = CandidateTransactionCore m v (procedure :: Maybe CollectiveProcedureId | r)

type CandidateTransaction = Record (CandidateTransactionRow ExchangeId Q (id :: CandidateTransactionId, author :: ActorId))

type SignedCandidateTransaction = { candidateTransaction :: CandidateTransaction, signatures :: Array ActorId }

-- Exchanges and Transformations (§5) ------------------------------------------------

-- | What a Transformation's orders pay: priced on the curve, in the Resource, to the Actor.
type Payment = { curve :: CostFunction, resource :: ResourceId, to :: ActorId }

type Transformation =
  { id :: TransformationId
  , inputs :: Holdings -- consumed per unit ordered
  , outputs :: Holdings
  , draws :: Array Draw -- held for the round per unit ordered and released at commit, never consumed
  , payment :: Maybe Payment -- what an order pays, if anything
  , delay :: RoundOffset
  , minimum :: Quantity -- smallest order accepted
  , maximum :: Maybe Quantity -- largest total accepted per Actor per round, across its candidate Transactions
  , ownerSigns :: Boolean -- the holder of the Resource drawn must sign every order
  , effects :: Array DelayedFlow -- what settles after the delay
  }

-- | Whose holding a draw is taken from: the Actor running the order, or the unique positive holder of the
-- | Resource.
data DrawSource = Executor | HolderOf

-- | A quantity of a Resource held for the round per unit ordered, and released at commit.
type Draw = { resource :: ResourceId, from :: DrawSource, perUnit :: Quantity }

-- | A per-unit quantity of a Resource created for a holder or sunk from it, settling after the Transformation's
-- | delay. `holder` defaults to the ordering Actor; `rounds` limits how long it lasts.
type DelayedFlow =
  { resource :: ResourceId, holder :: Maybe ActorId, perUnit :: Quantity, rounds :: Maybe Int }

-- | The draw a Transformation takes from a Resource's holder, if it has one.
holderDraw :: Transformation -> Maybe Draw
holderDraw = Array.find (\d -> d.from == HolderOf) <<< _.draws

-- | The holder draw that forms a pool: one of a positive quantity.
poolDraw :: Transformation -> Maybe Draw
poolDraw tr = case holderDraw tr of
  Just d | d.perUnit > Q.zero -> Just d
  _ -> Nothing

-- | The draws an order takes from its own executor.
executorDraws :: Transformation -> Array Draw
executorDraws = Array.filter (\d -> d.from == Executor) <<< _.draws

-- Representation (§7) --------------------------------------------------------

type Representation =
  { id :: RepresentationId
  , explicitActor :: ActorId
  , aggregate :: ActorId
  , domain :: BehaviorDomain
  , observes :: ResourceId -- the Resource whose purchases are observed
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

derive instance Generic ActorKind _
derive instance Generic Transferability _
derive instance Generic DrawSource _
derive instance Generic Capability _
derive instance Generic Direction _
derive instance Generic (FlowEffect v) _
derive instance Generic LevyBasis _
derive instance Generic LevyRecipient _
derive instance Generic LevyPayer _
derive instance Generic (RuleF v) _
derive instance Generic FlowGovernance _
derive instance Generic RuleForm _
derive instance Generic (IntentF m v) _
derive instance Generic (RuleChangeF v) _

derive instance Functor (IntentF m)
derive instance Foldable (IntentF m)
derive instance Traversable (IntentF m)
derive instance Functor FlowEffect
derive instance Foldable FlowEffect
derive instance Traversable FlowEffect
derive instance Functor RuleF
derive instance Foldable RuleF
derive instance Traversable RuleF
derive instance Functor RuleChangeF
derive instance Foldable RuleChangeF
derive instance Traversable RuleChangeF

derive instance Eq ActorKind
derive instance Eq Transferability
derive instance Eq DrawSource
derive instance Eq Capability
derive instance Eq Direction
derive instance Eq FlowGovernance
derive instance Eq LevyBasis
derive instance Eq LevyRecipient
derive instance Eq LevyPayer
derive instance Eq RuleForm
derive instance (Eq m, Eq v) => Eq (IntentF m v)
derive instance Eq v => Eq (FlowEffect v)
derive instance Eq v => Eq (RuleF v)
derive instance Eq v => Eq (RuleChangeF v)

instance Show ActorKind where
  show = genericShow

instance Show Transferability where
  show = genericShow

instance Show DrawSource where
  show = genericShow

instance Show Capability where
  show = genericShow

instance Show FlowGovernance where
  show = genericShow

instance Show LevyBasis where
  show = genericShow

instance Show LevyPayer where
  show = genericShow

instance Show LevyRecipient where
  show = genericShow

instance Show RuleForm where
  show = genericShow

instance Show Direction where
  show = genericShow

instance (Show m, Show v) => Show (IntentF m v) where
  show = genericShow

instance Show v => Show (FlowEffect v) where
  show = genericShow

instance Show v => Show (RuleF v) where
  show = genericShow

instance Show v => Show (RuleChangeF v) where
  show = genericShow

-- Described shapes. The engine's Intents, Rules and rule changes are described once
-- here. Their JSON codecs are the snapshot codec of the shape, so no second codec exists for them.
-- Each sum is written in the snapshot form (`{"tag":..,"values":[..]}`); the premise-format tags of
-- these types are in the `schema` package.

-- | An object keyed by id, read into a Map.
idMap :: forall t a. Shape a -> Shape (Map (Id t) a)
idMap s = idMapWith (const s)

-- | An object keyed by id, each entry read knowing its key (a record whose id is its key).
idMapWith :: forall t a. (Id t -> Shape a) -> Shape (Map (Id t) a)
idMapWith = keyedBy Id idText

directionShape :: Shape Direction
directionShape = genericEnum "Direction"

flowScopeShape :: Shape FlowScope
flowScopeShape = recordOf "FlowScope"
  ( field @"governor" "the Actor that governs the Rule" refIdShape
      &. field @"counterparty" "the other party" refIdShape
      &. field @"direction" "inbound to the governor, outbound from it, or either way" directionShape
      &. field @"over" "the Resources the Rule covers" (arrayOf refIdShape)
  )

flowEffectShape :: forall v. Shape v -> Shape (FlowEffect v)
flowEffectShape v = genericSum "FlowEffect"
  { "Levy": atTagged "a levy on the flow" (levyShape v)
  , "Block": atTagged "the flow is refused" unitShape
  }

levyBasisShape :: Shape LevyBasis
levyBasisShape = genericSum "LevyBasis"
  { "PerUnit": atTagged "per unit moved" unitShape
  , "AdValorem": atTagged "on the value moved, at this Exchange's price" refIdShape
  }

levyRecipientShape :: Shape LevyRecipient
levyRecipientShape = genericEnum "LevyRecipient"

levyPayerShape :: Shape LevyPayer
levyPayerShape = genericEnum "LevyPayer"

levyShape :: forall v. Shape v -> Shape (LevyF v)
levyShape v = recordOf "Levy"
  ( field @"rate" "the rate on the basis" v
      &. field @"basis" "what the levy is charged on" levyBasisShape
      &. field @"in" "the Resource it is paid in" refIdShape
      &. field @"to" "who receives it" levyRecipientShape
      &. field @"payer" "who pays it (default the sender)" (defaultsTo PaidBySender levyPayerShape)
  )

ruleShape :: Shape (RuleF Q)
ruleShape = genericSum "Rule"
  { "FlowRule": atTagged "a Rule on Transfers between two groups" (recordShape "FlowRule" { kind: refIdShape, scope: flowScopeShape, effect: flowEffectShape qShape })
  , "HoldingRule": atTagged "a Rule reducing what the holder of a Resource can have drawn" (recordShape "HoldingRule" { kind: refIdShape, resource: refIdShape, reduce: qShape })
  }

ruleKindDeclShape :: Shape RuleKindDecl
ruleKindDeclShape = recordShape "RuleKind" { form: ruleFormShape, text: stringShape }

flowGovernanceShape :: Shape FlowGovernance
flowGovernanceShape = genericEnum "FlowGovernance"

ruleFormShape :: Shape RuleForm
ruleFormShape = genericSum "RuleForm"
  { "LevyFlows": atTagged "a levy on flows" flowGovernanceShape
  , "BlockFlows": atTagged "a block on flows" flowGovernanceShape
  , "ReduceHolding": atTagged "a reduction of what can be drawn" unitShape
  }

-- The bodies the snapshot, premise and template forms of an Intent share, over its filling `v`. The
-- tag forms differ by format; these do not.

-- | Entering an agreement.
enterBody :: forall v. Shape v -> Shape (EnterF v)
enterBody v = recordOf "Enter"
  ( field @"template" "the template of the agreement" templateNameShape
      &. field @"args" "the value of each of its arguments" (arrayOf (recordShape "Arg" { name: stringShape, value: v }))
      &. field @"parties" "the Actor bound to each of its parties; every one must sign" (arrayOf (recordShape "Party" { role: stringShape, actor: refIdShape }))
  )

-- | A Transfer.
transferBody :: forall v. Shape v -> Shape { from :: ActorId, to :: ActorId, resource :: ResourceId, quantity :: v }
transferBody v = recordOf "Transfer"
  ( field @"from" "the giving Actor" refIdShape
      &. field @"to" "the receiving Actor" refIdShape
      &. field @"resource" "the Resource" refIdShape
      &. field @"quantity" "the quantity" v
  )

ruleChangeShape :: Shape (RuleChangeF Q)
ruleChangeShape = genericSum "RuleChange"
  { "Enact": atTagged "enact a Rule" ruleShape
  , "Repeal": atTagged "repeal a Rule by its id" assignedIdShape
  }

intentShape :: Shape Intent
intentShape = genericSum "Intent"
  { "Trade": atTagged "an order in an Exchange" tradeShape
  , "Route": atTagged "a best-execution order on a pair of Resources" routeShape
  , "Transfer": atTagged "a Resource given from one Actor to another" transferShape
  , "Transform": atTagged "a run of a transformation" transformShape
  , "ChangeRule": atTagged "enact or repeal a Rule" ruleChangeShape
  , "Enter": atTagged "enter an agreement: every party named must sign" (enterBody qShape)
  }
  where
  tradeShape = recordOf "Trade"
    ( field @"exchange" "the Exchange" refIdShape
        &. field @"quantity" "+ buys, - sells the Exchange's base Resource" qShape
        &. field @"limitPrice" "the limit price, if any" (optionalOf qShape)
    )
  routeShape = object "Route"
    ( (\p q l -> { pair: p, quantity: q, limitPrice: l })
        <$> fieldOf "pair" "the base and quote Resources" pairShape _.pair
        <*> fieldOf "quantity" "+ buys, - sells the base Resource" qShape _.quantity
        -- required by the rule, not the decoder: the engine records the rejection of a stored or model-written order without one
        <*> fieldOf "limitPrice" "the limit price, which a routed order must state; an order without one is rejected" ((optionalOf qShape) { desc = qShape.desc }) _.limitPrice
    )
  pairShape = recordOf "Pair"
    ( field @"base" "the base Resource" refIdShape
        &. field @"quote" "the quote Resource" refIdShape
    )
  transferShape = transferBody qShape
  transformShape = recordOf "Transform"
    ( field @"by" "the Actor that runs it" refIdShape
        &. field @"transformation" "the transformation" refIdShape
        &. field @"quantity" "the quantity run" qShape
    )

-- The Actor, capability, and proposal shapes. The closure of a View is described
-- here, so the model's requests and the Run Store's records encode through these shapes.

transferabilityShape :: Shape Transferability
transferabilityShape = genericEnum "Transferability"

drawSourceShape :: Shape DrawSource
drawSourceShape = genericEnum "DrawSource"

capabilityShape :: Shape Capability
capabilityShape = genericSum "Capability"
  { "MayTrade": atTagged "an Exchange the Actor may trade in" idShape
  , "MayTransfer": atTagged "a Resource the Actor may transfer" idShape
  , "MayTransform": atTagged "a Transformation the Actor may run" idShape
  , "MayEnact": atTagged "a kind of Rule the Actor may enact" idShape
  }

collectiveProcedureShape :: Shape CollectiveProcedure
collectiveProcedureShape = recordOf "CollectiveProcedure"
  ( field @"weights" "the weight of each member" (mapOf idShape qShape)
      &. field @"residualWeight" "the weight of the residual" qShape
      &. field @"threshold" "the support needed" qShape
      &. field @"authorizes" "the Capabilities the procedure authorizes" (arrayOf capabilityShape)
  )

aggregateShape :: Shape Aggregate
aggregateShape = recordOf "Aggregate"
  ( field @"strategy" "the Strategy that decides for it" idShape
      &. field @"members" "the Actors it is made of" (arrayOf idShape)
      &. field @"procedures" "its collective procedures" (mapOf idShape collectiveProcedureShape)
  )

actorKindShape :: Shape ActorKind
actorKindShape = genericSum "ActorKind"
  { "PlayableActor": atTagged "an Actor played from outside the Premise, by a human or an LLM" idShape
  , "NonPlayableActor": atTagged "an Actor run by a Strategy the Premise defines" idShape
  , "Aggregate": atTagged "an Actor made of members" aggregateShape
  }

actorShape :: Shape Actor
actorShape = recordOf "Actor"
  ( field @"id" "the Actor's id" idShape
      &. field @"accounts" "its holdings of each Resource" (mapOf idShape qShape)
      &. field @"capabilities" "what it may do" (arrayOf capabilityShape)
      &. field @"kind" "an Actor or an Aggregate" actorKindShape
  )

candidateTransactionShape :: Shape CandidateTransaction
candidateTransactionShape = recordOf "CandidateTransaction"
  ( field @"id" "the CandidateTransaction's id" idShape
      &. field @"author" "the Actor that submitted it" idShape
      &. field @"intents" "the Intents" (arrayOf intentShape)
      &. field @"signers" "the Actors asked to sign" (arrayOf idShape)
      &. field @"procedure" "the collective procedure, if any" (optionalOf idShape)
  )

signedCandidateTransactionShape :: Shape SignedCandidateTransaction
signedCandidateTransactionShape = recordOf "SignedCandidateTransaction"
  ( field @"candidateTransaction" "the CandidateTransaction" candidateTransactionShape
      &. field @"signatures" "the Actors that signed it" (arrayOf idShape)
  )

instance EncodeJson DrawSource where
  encodeJson = encodeWith Snapshot drawSourceShape

instance DecodeJson DrawSource where
  decodeJson = decodeJsonWith Snapshot drawSourceShape

instance EncodeJson Transferability where
  encodeJson = encodeWith Snapshot transferabilityShape

instance DecodeJson Transferability where
  decodeJson = decodeJsonWith Snapshot transferabilityShape

instance EncodeJson Capability where
  encodeJson = encodeWith Snapshot capabilityShape

instance DecodeJson Capability where
  decodeJson = decodeJsonWith Snapshot capabilityShape

instance EncodeJson ActorKind where
  encodeJson = encodeWith Snapshot actorKindShape

instance DecodeJson ActorKind where
  decodeJson = decodeJsonWith Snapshot actorKindShape

instance EncodeJson Direction where
  encodeJson = encodeWith Snapshot directionShape

instance DecodeJson Direction where
  decodeJson = decodeJsonWith Snapshot directionShape

instance EncodeJson (RuleChangeF Q) where
  encodeJson = encodeWith Snapshot ruleChangeShape

instance DecodeJson (RuleChangeF Q) where
  decodeJson = decodeJsonWith Snapshot ruleChangeShape

instance EncodeJson (IntentF ExchangeId Q) where
  encodeJson = encodeWith Snapshot intentShape

instance DecodeJson (IntentF ExchangeId Q) where
  decodeJson = decodeJsonWith Snapshot intentShape

candidateTransactionId :: RoundIndex -> ActorId -> Int -> CandidateTransactionId
candidateTransactionId r (Id author) i = Id ("r" <> show r <> "/" <> author <> "/" <> show i)

-- | A Transform's body, and one whose `by` is implicit, and a repeal's: the forms every Intent format writes.
transformBody :: forall v. Shape v -> Shape { by :: ActorId, transformation :: TransformationId, quantity :: v }
transformBody v = recordOf "Transform"
  ( field @"by" "the Actor that runs it" idShape
      &. field @"transformation" "the transformation" idShape
      &. field @"quantity" "the quantity run" v
  )

ownTransformBody :: forall v. Shape v -> Shape { transformation :: TransformationId, quantity :: v }
ownTransformBody v = recordOf "Transform"
  ( field @"transformation" "the transformation" idShape
      &. field @"quantity" "the quantity run" v
  )

repealBody :: Shape { rule :: RuleId }
repealBody = recordOf "Repeal" (field @"rule" "the Rule, by the id the engine gave it" idShape)

ruleIdOf :: CandidateTransactionId -> Int -> RuleId
ruleIdOf p i = Id (show p <> "/rule-" <> show i)

-- | The index a candidate Transaction id was built with: the segment after its last "/". Nothing for an id
-- | that `proposalId` could not have made.
submissionIndex :: CandidateTransactionId -> Maybe Int
submissionIndex (Id s) = Int.fromString =<< last (split (Pattern "/") s)

instance EncodeJson (RuleF Q) where
  encodeJson = encodeWith Snapshot ruleShape

instance DecodeJson (RuleF Q) where
  decodeJson = decodeJsonWith Snapshot ruleShape

instance EncodeJson RuleForm where
  encodeJson = encodeWith Snapshot ruleFormShape

instance DecodeJson RuleForm where
  decodeJson = decodeJsonWith Snapshot ruleFormShape

instance EncodeJson FlowGovernance where
  encodeJson = encodeWith Snapshot flowGovernanceShape

instance DecodeJson FlowGovernance where
  decodeJson = decodeJsonWith Snapshot flowGovernanceShape

