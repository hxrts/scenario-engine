-- | The premise format of the World (docs/03_gameboard.md §3–§5): Resources, Markets, facilities,
-- | Entities with their Aggregates and procedures, Representation schemes and assignments, and
-- | circulation. Each section is a field set (`RecF`) over the World, so a section file and the
-- | whole Gameboard use one description. A key the decoder defaults is `Maybe` here: an author
-- | who writes `"capacityUse": "1"` gets it written back, and one who omits it gets nothing back.
-- | `worldToCore` fills the defaults and produces the core World State (Core.World).
module Schema.Premise.World
  ( Keyed
  , keyedToMap
  , ResourceDoc
  , CostDoc
  , MarketDoc
  , FacilityDoc
  , EntityDoc
  , AggregateDoc
  , ProcedureDoc
  , RepresentationDoc
  , SchemeDoc
  , AssignmentDoc
  , CirculationDoc
  , ProductionDoc
  , FlowDoc
  , ShareDoc
  , RequirementDoc
  , WorldDoc
  , worldRec
  , schemeShape
  , assignmentShape
  , schemeToCore
  , worldToCore
  , EffectDoc
  , EstimatorDoc
  ) where

import Prelude

import Core.Num (Q, qShape)
import Core.Num as Q
import Core.Shape (RecF, Shape, nameShape, recordShape, atBare, atKindArg, boolShape, contramapRec, fieldOf, genericPremiseSum, intShape, keyedAlways, listOf, object, optionalOf, refined, stringShape)
import Core.Types (Aggregate, BehaviorDomain, Capability, CapacityRef, EntityId, PolicyId, RepresentationId, ResourceId, CapacityEffect, CollectiveProcedure, Controller(..), Entity, EntityKind(..), Estimator, Id(..), Interval, Market, Representation, ResourceDefinition, Rule, Transferability, Transformation, TransformationId, idShape)
import Core.Types (FlowGovernance(..), RuleForm, RuleKindDecl)
import Core.Types as Types
import Core.World (FlowGate, RecurringFlow, UnseededWorld)
import Data.Array (mapMaybe, mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Schema.Gameboard (Scheme)
import Schema.Recurring (ShareSource)
import Schema.Recurring as Recurring
import Schema.Premise.Actor (DefinitionDoc, definitionShape)
import Schema.Premise.Intent (capabilityShape, ruleShape)
import Schema.Premise.Tags (premiseEnum, transferabilityTags)

-- | A keyed list as written: the object's pairs, in the order given.
type Keyed a = Array (Tuple String a)

-- Resources and Markets -----------------------------------------------------------------------

type ResourceDoc = { transferability :: Transferability, reservable :: Maybe Boolean, decay :: Maybe Q }

type CostDoc = { intercept :: Q, slope :: Q, lo :: Q, hi :: Q }

type MarketDoc = { base :: ResourceId, quote :: ResourceId, cost :: CostDoc, reserves :: Keyed Q, access :: Array EntityId, position :: Maybe Q }

-- | A facility (a transformation): its inputs, outputs and escrow, its payment and capacity,
-- | and its timing, limits and effects. Every key is optional.
type FacilityDoc =
  { inputs :: Maybe (Keyed Q)
  , outputs :: Maybe (Keyed Q)
  , escrowPerUnit :: Maybe (Keyed Q)
  , payment :: Maybe { curve :: CostDoc, resource :: ResourceId, to :: EntityId }
  , capacity :: Maybe CapacityRef
  , delay :: Maybe Int
  , minimum :: Maybe Q
  , maximum :: Maybe Q
  , capacityUse :: Maybe Q
  , ownerSigns :: Maybe Boolean
  , effects :: Maybe (Array EffectDoc)
  }

type EffectDoc = { capacity :: CapacityRef, holder :: Maybe EntityId, perUnit :: Q, rounds :: Maybe Int }

-- Entities ------------------------------------------------------------------------------------

-- | An Entity: its accounts, capabilities and capacities, and either its Actor definition or its
-- | Aggregate (procedures, members and policy). The definition is read by the Gameboard.
type EntityDoc =
  { accounts :: Keyed Q
  , capabilities :: Array Capability
  , capacities :: Maybe (Keyed Q)
  , definition :: Maybe DefinitionDoc
  , aggregate :: Maybe AggregateDoc
  }

type AggregateDoc = { policy :: PolicyId, members :: Maybe (Array EntityId), procedures :: Maybe (Keyed ProcedureDoc) }

-- | A collective procedure on an Aggregate's own Proposals.
type ProcedureDoc = { weights :: Keyed Q, residualWeight :: Q, threshold :: Q, authorizes :: Array Capability }

-- Representations -----------------------------------------------------------------------------

type EstimatorDoc = { reference :: Q, shrinkage :: Q, bound :: Maybe Q, noiseSpread :: Maybe Q }

-- | A Representation of an Actor in a world (a fixture's, flattened with its estimator).
type RepresentationDoc =
  { id :: RepresentationId
  , actor :: EntityId
  , aggregate :: EntityId
  , domain :: BehaviorDomain
  , weight :: Q
  , estimator :: EstimatorDoc
  }

-- | A Gameboard's scheme: the Representation an Actor's behaviour is estimated under.
type SchemeDoc = { id :: String, eligibleActors :: Array EntityId, target :: EntityId, domain :: BehaviorDomain, estimator :: EstimatorDoc }

-- | An instantiation's assignment of an Actor to a scheme; the weight defaults to 1.
type AssignmentDoc = { scheme :: String, actor :: EntityId, weight :: Maybe Q }

-- Circulation ---------------------------------------------------------------------------------

type CirculationDoc =
  { production :: Maybe (Array ProductionDoc)
  , flows :: Maybe (Array FlowDoc)
  , shares :: Maybe (Array ShareDoc)
  , requirements :: Maybe (Array RequirementDoc)
  }

type ProductionDoc = { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef }

type FlowDoc = { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Q }

type ShareDoc = { from :: EntityId, to :: EntityId, share :: Q, source :: ShareSource }

type RequirementDoc = { entity :: EntityId, resource :: ResourceId, amount :: Maybe Q, penalty :: Maybe Q }

-- | The World as written, with its name and starting round. Every section is a key of the
-- | Gameboard's merged object; `numeraire`, the Resources, Markets, facilities and Entities are required.
type WorldDoc =
  { name :: Maybe String
  , round :: Maybe Int
  , numeraire :: ResourceId
  , resources :: Keyed ResourceDoc
  , markets :: Keyed MarketDoc
  , transformations :: Keyed FacilityDoc
  , flowGates :: Maybe (Array FlowGate)
  , groups :: Maybe (Keyed EntityId)
  , ruleKinds :: Maybe (Keyed RuleKindDecl)
  , entities :: Keyed EntityDoc
  , rules :: Maybe (Array (Rule))
  , representations :: Maybe (Array RepresentationDoc)
  , circulation :: Maybe CirculationDoc
  }

-- Shapes --------------------------------------------------------------------------------------

transferabilityShape :: Shape Transferability
transferabilityShape = premiseEnum "Transferability" Types.transferabilityShape transferabilityTags

resourceShape :: Shape ResourceDoc
resourceShape = object "Resource"
  ( mk <$> fieldOf "transferability" "how it may move" transferabilityShape _.transferability
      <*> fieldOf "reservable" "may be reserved for a deployment (default false)" (optionalOf boolShape) _.reservable
      <*> fieldOf "decay" "the share lost per round" (optionalOf qShape) _.decay
  )
  where
  mk t r d = { transferability: t, reservable: r, decay: d }

costShape :: Shape CostDoc
costShape = object "Cost"
  ( mk <$> fieldOf "intercept" "the price at zero" qShape _.intercept
      <*> fieldOf "slope" "the price per unit" qShape _.slope
      <*> fieldOf "lo" "the lower bound of the domain" qShape _.lo
      <*> fieldOf "hi" "the upper bound of the domain" qShape _.hi
  )
  where
  mk b s l h = { intercept: b, slope: s, lo: l, hi: h }

marketShape :: Shape MarketDoc
marketShape = object "Market"
  ( mk <$> fieldOf "base" "the Resource traded" idShape _.base
      <*> fieldOf "quote" "the Resource it is priced in" idShape _.quote
      <*> fieldOf "cost" "the cost curve" costShape _.cost
      <*> fieldOf "reserves" "the reserves, by Resource" (keyedAlways qShape) _.reserves
      <*> fieldOf "access" "the Entities that may trade" (listOf idShape) _.access
      <*> fieldOf "position" "the starting net position (default 0)" (optionalOf qShape) _.position
  )
  where
  mk b q' c r a p = { base: b, quote: q', cost: c, reserves: r, access: a, position: p }

effectShape :: Shape EffectDoc
effectShape = object "Effect"
  ( mk <$> fieldOf "capacity" "the capacity changed" idShape _.capacity
      <*> fieldOf "holder" "the Entity whose capacity it is (default the ordering Entity)" (optionalOf idShape) _.holder
      <*> fieldOf "perUnit" "the change per unit ordered" qShape _.perUnit
      <*> fieldOf "rounds" "how many rounds it lasts" (optionalOf intShape) _.rounds
  )
  where
  mk c h p r = { capacity: c, holder: h, perUnit: p, rounds: r }

facilityShape :: Shape FacilityDoc
facilityShape = object "Facility"
  ( mk <$> fieldOf "inputs" "consumed per unit" (optionalOf (keyedAlways qShape)) _.inputs
      <*> fieldOf "outputs" "produced per unit" (optionalOf (keyedAlways qShape)) _.outputs
      <*> fieldOf "escrowPerUnit" "reserved per unit, released within the round" (optionalOf (keyedAlways qShape)) _.escrowPerUnit
      <*> fieldOf "payment" "what each order pays: its curve, its Resource, and who is paid" (optionalOf (recordShape "Payment" { curve: costShape, resource: idShape, to: idShape })) _.payment
      <*> fieldOf "capacity" "the capacity it draws on" (optionalOf idShape) _.capacity
      <*> fieldOf "delay" "rounds before its effects apply" (optionalOf intShape) _.delay
      <*> fieldOf "minimum" "the smallest order accepted" (optionalOf qShape) _.minimum
      <*> fieldOf "maximum" "the largest order accepted per Entity per round" (optionalOf qShape) _.maximum
      <*> fieldOf "capacityUse" "capacity drawn per unit ordered (default 1)" (optionalOf qShape) _.capacityUse
      <*> fieldOf "ownerSigns" "the capacity's owner must sign each order (default false)" (optionalOf boolShape) _.ownerSigns
      <*> fieldOf "effects" "changes to capacities" (optionalOf (listOf effectShape)) _.effects
  )
  where
  mk i o r c cap d mn mx cu os ef = { inputs: i, outputs: o, escrowPerUnit: r, payment: c, capacity: cap, delay: d, minimum: mn, maximum: mx, capacityUse: cu, ownerSigns: os, effects: ef }

aggregateShape :: Shape AggregateDoc
aggregateShape = object "Aggregate"
  ( mk <$> fieldOf "policy" "the Policy that drives it" idShape _.policy
      <*> fieldOf "members" "the Entities that are its members" (optionalOf (listOf idShape)) _.members
      <*> fieldOf "procedures" "its collective procedures, by name" (optionalOf (keyedAlways procedureShape)) _.procedures
  )
  where
  mk p m ps = { policy: p, members: m, procedures: ps }

procedureShape :: Shape ProcedureDoc
procedureShape = object "Procedure"
  ( mk <$> fieldOf "weights" "the weight of each member" (keyedAlways qShape) _.weights
      <*> fieldOf "residualWeight" "the weight of the rest" qShape _.residualWeight
      <*> fieldOf "threshold" "the weight that carries it" qShape _.threshold
      <*> fieldOf "authorizes" "the capabilities it grants" (listOf capabilityShape) _.authorizes
  )
  where
  mk w r t a = { weights: w, residualWeight: r, threshold: t, authorizes: a }

entityShape :: Shape EntityDoc
entityShape = object "Entity"
  ( mk <$> fieldOf "accounts" "the holdings" (keyedAlways qShape) _.accounts
      <*> fieldOf "capabilities" "the capabilities it holds" (listOf capabilityShape) _.capabilities
      <*> fieldOf "capacities" "the capacities it holds" (optionalOf (keyedAlways qShape)) _.capacities
      <*> fieldOf "definition" "its Actor definition" (optionalOf definitionShape) _.definition
      <*> fieldOf "aggregate" "its Aggregate" (optionalOf aggregateShape) _.aggregate
  )
  where
  mk a c cap d ag = { accounts: a, capabilities: c, capacities: cap, definition: d, aggregate: ag }

estimatorRec :: RecF EstimatorDoc EstimatorDoc
estimatorRec = mk <$> fieldOf "reference" "the reference quantity" qShape _.reference
  <*> fieldOf "shrinkage" "the shrinkage toward the reference" qShape _.shrinkage
  <*> fieldOf "bound" "a symmetric bound on the deviation" (optionalOf qShape) _.bound
  <*> fieldOf "noiseSpread" "the noise spread (default 0)" (optionalOf qShape) _.noiseSpread
  where
  mk r s b n = { reference: r, shrinkage: s, bound: b, noiseSpread: n }

estimatorShape :: Shape EstimatorDoc
estimatorShape = object "Estimator" estimatorRec

representationRec :: RecF RepresentationDoc RepresentationDoc
representationRec = mk <$> fieldOf "id" "the Representation" idShape _.id
  <*> fieldOf "actor" "the Actor represented" idShape _.actor
  <*> fieldOf "aggregate" "the Aggregate that holds the estimate" idShape _.aggregate
  <*> fieldOf "domain" "the behaviour domain" idShape _.domain
  <*> fieldOf "weight" "the weight in the Aggregate's estimate" qShape _.weight
  <*> contramapRec _.estimator estimatorRec
  where
  mk i a g d w e = { id: i, actor: a, aggregate: g, domain: d, weight: w, estimator: e }

representationShape :: Shape RepresentationDoc
representationShape = object "Representation" representationRec

schemeShape :: Shape SchemeDoc
schemeShape = object "Scheme"
  ( mk <$> fieldOf "id" "the scheme's name" stringShape _.id
      <*> fieldOf "eligibleActors" "the Actors it may estimate" (listOf idShape) _.eligibleActors
      <*> fieldOf "target" "the Aggregate it estimates" idShape _.target
      <*> fieldOf "domain" "the behaviour domain" idShape _.domain
      <*> fieldOf "estimator" "the estimator" estimatorShape _.estimator
  )
  where
  mk i e t d est = { id: i, eligibleActors: e, target: t, domain: d, estimator: est }

assignmentShape :: Shape AssignmentDoc
assignmentShape = object "Assignment"
  ( mk <$> fieldOf "scheme" "the scheme assigned" stringShape _.scheme
      <*> fieldOf "actor" "the Actor assigned" idShape _.actor
      <*> fieldOf "weight" "the weight (default 1)" (optionalOf qShape) _.weight
  )
  where
  mk s a w = { scheme: s, actor: a, weight: w }

circulationShape :: Shape CirculationDoc
circulationShape = object "Circulation"
  ( mk <$> fieldOf "production" "capacity that produces a Resource" (optionalOf (listOf productionShape)) _.production
      <*> fieldOf "flows" "fixed payments per round" (optionalOf (listOf flowShape)) _.flows
      <*> fieldOf "shares" "variable payments, as shares" (optionalOf (listOf shareShape)) _.shares
      <*> fieldOf "requirements" "what an Entity must consume" (optionalOf (listOf requirementShape)) _.requirements
  )
  where
  mk p f s r = { production: p, flows: f, shares: s, requirements: r }

productionShape :: Shape ProductionDoc
productionShape = object "Production"
  ( mk <$> fieldOf "entity" "the Entity" idShape _.entity
      <*> fieldOf "resource" "the Resource produced" idShape _.resource
      <*> fieldOf "capacity" "the capacity that produces it" idShape _.capacity
  )
  where
  mk e r c = { entity: e, resource: r, capacity: c }

-- | A fixed payment per round, which is never negative (docs/03 §5.4).
flowShape :: Shape FlowDoc
flowShape = object "Flow"
  ( mk <$> fieldOf "from" "the paying Entity" idShape _.from
      <*> fieldOf "to" "the receiving Entity" idShape _.to
      <*> fieldOf "resource" "the Resource paid" idShape _.resource
      <*> fieldOf "amount" "the amount per round" (refined "a flow amount must not be negative" (_ >= Q.zero) qShape) _.amount
  )
  where
  mk f t r a = { from: f, to: t, resource: r, amount: a }

shareShape :: Shape ShareDoc
shareShape = object "Share"
  ( mk <$> fieldOf "from" "the paying Entity" idShape _.from
      <*> fieldOf "to" "the receiving Entity" idShape _.to
      <*> fieldOf "share" "the share of the source" qShape _.share
      <*> fieldOf "source" "what the share is of" shareSourceShape _.source
  )
  where
  mk f t s src = { from: f, to: t, share: s, source: src }

shareSourceShape :: Shape ShareSource
shareSourceShape = genericPremiseSum "ShareSource"
  { "SalesOf": atKindArg { tag: "sales", desc: "net Money revenue from selling a Resource on Markets" } idShape
  , "PayeeReceipts": atBare { tag: "payee", desc: "Money received as a facility payee" }
  }

requirementShape :: Shape RequirementDoc
requirementShape = object "Requirement"
  ( mk <$> fieldOf "entity" "the Entity that needs it" idShape _.entity
      <*> fieldOf "resource" "the Resource needed" idShape _.resource
      <*> fieldOf "amount" "the amount per round (default: this round's purchases)" (optionalOf qShape) _.amount
      <*> fieldOf "penalty" "the reduction of fixed incomes per unit unmet (default 0)" (optionalOf qShape) _.penalty
  )
  where
  mk e r a p = { entity: e, resource: r, amount: a, penalty: p }

-- | The World's sections as field sets over the World.
nameF :: RecF WorldDoc (Maybe String)
nameF = fieldOf "name" "the premise's name" (optionalOf stringShape) _.name

roundF :: RecF WorldDoc (Maybe Int)
roundF = fieldOf "round" "the starting round (default 1)" (optionalOf intShape) _.round

numeraireF :: RecF WorldDoc ResourceId
numeraireF = fieldOf "numeraire" "the unit of account, which the views use for net worth" idShape _.numeraire

resourcesF :: RecF WorldDoc (Keyed ResourceDoc)
resourcesF = fieldOf "resources" "the Resources" (keyedAlways resourceShape) _.resources

marketsF :: RecF WorldDoc (Keyed MarketDoc)
marketsF = fieldOf "markets" "the Markets" (keyedAlways marketShape) _.markets

transformationsF :: RecF WorldDoc (Keyed FacilityDoc)
transformationsF = fieldOf "transformations" "the facilities" (keyedAlways facilityShape) _.transformations

gatesF :: RecF WorldDoc (Maybe (Array FlowGate))
gatesF = fieldOf "flowGates" "the conditions on a Transfer of a Resource" (optionalOf (listOf (recordShape "FlowGate" { resource: idShape, exemptSharedMarket: boolShape, bundled: optionalOf idShape }))) _.flowGates

jurisdictionF :: RecF WorldDoc (Maybe (Keyed EntityId))
jurisdictionF = fieldOf "groups" "the group each Entity counts as, where it is not its own" (optionalOf (keyedAlways idShape)) _.groups

ruleKindsF :: RecF WorldDoc (Maybe (Keyed RuleKindDecl))
ruleKindsF = fieldOf "ruleKinds" "the kinds of Rule the premise declares" (optionalOf (keyedAlways ruleKindDecl)) _.ruleKinds

ruleKindDecl :: Shape RuleKindDecl
ruleKindDecl = recordShape "RuleKindDecl" { form: ruleForm, text: stringShape }

-- | A kind's form, written `levy:destination`, `block:either`, or `reduce`.
ruleForm :: Shape RuleForm
ruleForm = genericPremiseSum "RuleForm"
  { "LevyFlows": atKindArg { tag: "levy", desc: "a levy on flows" } governance
  , "BlockFlows": atKindArg { tag: "block", desc: "a block on flows" } governance
  , "ReduceCapacity": atBare { tag: "reduce", desc: "a reduction of a capacity, governed by its owner" }
  }
  where
  governance = nameShape "FlowGovernance" "who governs: destination, origin or either" governed [ GovernedByDestination, GovernedBySource, GovernedByEither ]
  governed = case _ of
    GovernedByDestination -> "destination"
    GovernedBySource -> "origin"
    GovernedByEither -> "either"

entitiesF :: RecF WorldDoc (Keyed EntityDoc)
entitiesF = fieldOf "entities" "the Entities" (keyedAlways entityShape) _.entities

rulesF :: RecF WorldDoc (Maybe (Array Rule))
rulesF = fieldOf "rules" "the Rules in force from the start" (optionalOf (listOf ruleShape)) _.rules

representationsF :: RecF WorldDoc (Maybe (Array RepresentationDoc))
representationsF = fieldOf "representations" "the Representations bound in the world" (optionalOf (listOf representationShape)) _.representations

circulationF :: RecF WorldDoc (Maybe CirculationDoc)
circulationF = fieldOf "circulation" "the recurring economic cycle" (optionalOf circulationShape) _.circulation

worldRec :: RecF WorldDoc WorldDoc
worldRec = mk <$> nameF <*> roundF <*> numeraireF <*> resourcesF <*> marketsF <*> transformationsF <*> gatesF <*> jurisdictionF <*> ruleKindsF <*> entitiesF <*> rulesF <*> representationsF <*> circulationF
  where
  mk n r nu res mks trs tp j rk ent ru rep c =
    { name: n, round: r, numeraire: nu, resources: res, markets: mks, transformations: trs, flowGates: tp, groups: j, ruleKinds: rk, entities: ent, rules: ru, representations: rep, circulation: c }

-- Conversions ---------------------------------------------------------------------------------

-- | A keyed list as holdings: the keys become typed ids.
keyedToMap :: forall t a. Keyed a -> Map.Map (Id t) a
keyedToMap = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v)

estimatorToCore :: EstimatorDoc -> Estimator
estimatorToCore e =
  { reference: e.reference
  , shrinkage: e.shrinkage
  , deviationBound: map (\b -> { lo: Q.neg b, hi: b }) e.bound
  , noiseSpread: fromMaybe Q.zero e.noiseSpread
  }

schemeToCore :: SchemeDoc -> Scheme
schemeToCore s =
  { id: s.id
  , eligibleActors: s.eligibleActors
  , target: s.target
  , domain: s.domain
  , estimator: estimatorToCore s.estimator
  }

-- | The core World State of a World as written (unseeded), with every default filled in.
worldToCore :: WorldDoc -> UnseededWorld
worldToCore w =
  { round: fromMaybe 1 w.round
  , entities: Map.fromFoldable (map (\(Tuple k e) -> Tuple (Id k) (entityToCore k e)) w.entities)
  , resources: Map.fromFoldable (map (\(Tuple k r) -> Tuple (Id k) (resourceToCore k r)) w.resources)
  , markets: Map.fromFoldable (map (\(Tuple k m) -> Tuple (Id k) (marketToCore k m)) w.markets)
  , transformations: Map.fromFoldable (map (\(Tuple k f) -> Tuple (Id k) (facilityToCore k f)) w.transformations)
  , representations: map representationToCore (fromMaybe [] w.representations)
  , numeraire: w.numeraire
  , flowGates: fromMaybe [] w.flowGates
  , groups: keyedToMap (fromMaybe [] w.groups)
  , ruleKinds: keyedToMap (fromMaybe [] w.ruleKinds)
  , rules: Map.fromFoldable (mapWithIndex (\i r -> Tuple (Id ("rule-" <> show i)) r) (fromMaybe [] w.rules))
  , scheduled: Map.empty
  , deliveries: []
  , factors: Map.empty
  , recurring: recurringOf w.numeraire (fromMaybe emptyCirculation w.circulation) w.resources
  , events: []
  }
  where
  emptyCirculation :: CirculationDoc
  emptyCirculation = { production: Nothing, flows: Nothing, shares: Nothing, requirements: Nothing }

resourceToCore :: String -> ResourceDoc -> ResourceDefinition
resourceToCore k r =
  { id: Id k
  , unit: k
  , transferability: r.transferability
  , reservable: fromMaybe false r.reservable
  }

costToCore :: CostDoc -> { intercept :: Q, slope :: Q, domain :: Interval }
costToCore c = { intercept: c.intercept, slope: c.slope, domain: { lo: c.lo, hi: c.hi } }

marketToCore :: String -> MarketDoc -> Market
marketToCore k m =
  { id: Id k
  , base: m.base
  , quote: m.quote
  , cost: costToCore m.cost
  , position: fromMaybe Q.zero m.position
  , reserves: keyedToMap m.reserves
  , access: m.access
  }

facilityToCore :: String -> FacilityDoc -> Transformation
facilityToCore k f =
  { id: Id k
  , inputs: keyedToMap (fromMaybe [] f.inputs)
  , outputs: keyedToMap (fromMaybe [] f.outputs)
  , escrowPerUnit: keyedToMap (fromMaybe [] f.escrowPerUnit)
  , payment: map (\p -> p { curve = costToCore p.curve }) f.payment
  , capacity: f.capacity
  , delay: fromMaybe 0 f.delay
  , minimum: fromMaybe Q.zero f.minimum
  , maximum: f.maximum
  , capacityUse: fromMaybe Q.one f.capacityUse
  , ownerSigns: fromMaybe false f.ownerSigns
  , effects: map effectToCore (fromMaybe [] f.effects)
  }

effectToCore :: EffectDoc -> CapacityEffect
effectToCore e = { capacity: e.capacity, holder: e.holder, perUnit: e.perUnit, rounds: e.rounds }

entityToCore :: String -> EntityDoc -> Entity
entityToCore k e =
  { id: Id k
  , accounts: keyedToMap e.accounts
  , capacities: keyedToMap (fromMaybe [] e.capacities)
  , capabilities: e.capabilities
  , kind: maybe (Actor (Seat (Id k))) (\a -> Aggregate (aggregateToCore a)) e.aggregate
  }

aggregateToCore :: AggregateDoc -> Aggregate
aggregateToCore a =
  { policy: a.policy
  , members: fromMaybe [] a.members
  , procedures: Map.fromFoldable (map (\(Tuple p pr) -> Tuple (Id p) (procedureToCore pr)) (fromMaybe [] a.procedures))
  }

procedureToCore :: ProcedureDoc -> CollectiveProcedure
procedureToCore p =
  { weights: keyedToMap p.weights
  , residualWeight: p.residualWeight
  , threshold: p.threshold
  , authorizes: p.authorizes
  }

representationToCore :: RepresentationDoc -> Representation
representationToCore r =
  { id: r.id
  , actor: r.actor
  , aggregate: r.aggregate
  , domain: r.domain
  , weight: r.weight
  , estimator: estimatorToCore r.estimator
  }

-- | A premise's recurring cycle and its Resources' decay, as the rules the engine evaluates.
recurringOf :: ResourceId -> CirculationDoc -> Keyed ResourceDoc -> Array RecurringFlow
recurringOf numeraire c rs = Recurring.compile
  { production: fromMaybe [] c.production
  , flows: fromMaybe [] c.flows
  , shares: map shareToCore (fromMaybe [] c.shares)
  , requirements: map (\r -> r { penalty = fromMaybe Q.zero r.penalty }) (fromMaybe [] c.requirements)
  , decays: mapMaybe (\(Tuple k r) -> (\rate -> { resource: Id k, rate }) <$> r.decay) rs
  }
  where
  -- a share is paid in the premise's numeraire
  shareToCore s = { from: s.from, to: s.to, resource: numeraire, share: s.share, basis: Recurring.shareBasis s.source }
