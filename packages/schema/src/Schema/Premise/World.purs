-- | The premise format of the World (docs/03_gameboard.md §3–§5): Resources, Markets, facilities,
-- | Entities with their Aggregates and procedures, Representation schemes and assignments, and
-- | circulation. Each shape reads the core type directly (Core.Types, Core.World): a key with a default
-- | is omitted when it holds that default, and a record keyed by its id reads the id from the key.
-- | `worldFields` is every section but the Entities, so a Gameboard can read the Entities with their
-- | Actor definitions and a fixture without.
module Schema.Premise.World
  ( PremiseWorld
  , worldFields
  , worldRec
  , entityRec
  , rulesFromList
  , schemeShape
  , assignmentShape
  , checkReservations
  ) where

import Prelude

import Core.Num (Q, qShape)
import Core.Num as Q
import Core.Shape (RecF, Shape, arrayOf, defaulted, atBare, atKindArg, boolShape, contramapRec, defaultsTo, emptyByDefault, fieldOf, genericPremiseSum, intShape, iso, listOf, nameShape, object, optionalOf, recordShape, refined, stringShape)
import Core.Types (Aggregate, CapacityEffect, CollectiveProcedure, Controller(..), CostFunction, Entity, EntityId, EntityKind(..), Estimator, FlowGovernance(..), Id(..), Interval, Market, MarketId, Payment, Representation, ResourceDefinition, ResourceId, Rule, RuleForm, RuleId, RuleKindDecl, Transferability, Transformation, TransformationId, idMap, idMapWith, idShape, idText)
import Core.Types as Types
import Core.World (FlowGate, UnseededWorld)
import Data.Array as Array
import Data.Array (concatMap, elem, filter, find, mapMaybe, mapWithIndex, sortWith)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import Schema.Gameboard (Scheme)
import Schema.Premise.Intent (capabilityShape, ruleShape)
import Schema.Premise.Query (measureShape)
import Schema.Premise.Tags (premiseEnum, transferabilityTags)
import Schema.Recurring (Authored, Circulation, Flow, Production, Requirement, Share, compileIn)

-- Resources and Markets -----------------------------------------------------------------------

transferabilityShape :: Shape Transferability
transferabilityShape = premiseEnum "Transferability" Types.transferabilityShape transferabilityTags

-- | A Resource as a premise writes it: what the World holds, and what only the document holds.
type ResourceEntry = { transferability :: Transferability, reservable :: Boolean, decay :: Maybe Q }

resourceEntry :: Shape ResourceEntry
resourceEntry = object "Resource"
  ( mk <$> fieldOf "transferability" "how it may move" transferabilityShape _.transferability
      <*> fieldOf "reservable" "may be escrowed by a facility (default false)" (defaultsTo false boolShape) _.reservable
      <*> fieldOf "decay" "the share lost per round" (optionalOf qShape) _.decay
  )
  where
  mk t r d = { transferability: t, reservable: r, decay: d }

-- | A curve, its domain written as its `lo` and `hi`.
costShape :: Shape CostFunction
costShape = object "Cost"
  ( mk <$> fieldOf "intercept" "the price at zero" qShape _.intercept
      <*> fieldOf "slope" "the price per unit" qShape _.slope
      <*> fieldOf "lo" "the lower bound of the domain" qShape _.domain.lo
      <*> fieldOf "hi" "the upper bound of the domain" qShape _.domain.hi
  )
  where
  mk b s l h = { intercept: b, slope: s, domain: { lo: l, hi: h } }

marketShape :: MarketId -> Shape Market
marketShape k = object "Market"
  ( mk <$> fieldOf "base" "the Resource traded" idShape _.base
      <*> fieldOf "quote" "the Resource it is priced in" idShape _.quote
      <*> fieldOf "cost" "the cost curve" costShape _.cost
      <*> fieldOf "reserves" "the reserves, by Resource" (idMap qShape) _.reserves
      <*> fieldOf "access" "the Entities that may trade" (listOf idShape) _.access
      <*> fieldOf "position" "the starting net position (default 0)" (defaultsTo Q.zero qShape) _.position
  )
  where
  mk b q' c r a p = { id: k, base: b, quote: q', cost: c, position: p, reserves: r, access: a }

effectShape :: Shape CapacityEffect
effectShape = object "Effect"
  ( mk <$> fieldOf "capacity" "the capacity changed" idShape _.capacity
      <*> fieldOf "holder" "the Entity whose capacity it is (default the ordering Entity)" (optionalOf idShape) _.holder
      <*> fieldOf "perUnit" "the change per unit ordered" qShape _.perUnit
      <*> fieldOf "rounds" "how many rounds it lasts" (optionalOf intShape) _.rounds
  )
  where
  mk c h p r = { capacity: c, holder: h, perUnit: p, rounds: r }

paymentShape :: Shape Payment
paymentShape = recordShape "Payment" { curve: costShape, resource: idShape, to: idShape }

-- | A facility (a transformation). Every key is optional.
facilityShape :: TransformationId -> Shape Transformation
facilityShape k = object "Facility"
  ( mk <$> fieldOf "inputs" "consumed per unit" (emptyByDefault (idMap qShape)) _.inputs
      <*> fieldOf "outputs" "produced per unit" (emptyByDefault (idMap qShape)) _.outputs
      <*> fieldOf "escrowPerUnit" "reserved per unit, released within the round" (emptyByDefault (idMap qShape)) _.escrowPerUnit
      <*> fieldOf "payment" "what each order pays: its curve, its Resource, and who is paid" (optionalOf paymentShape) _.payment
      <*> fieldOf "capacity" "the capacity it draws on" (optionalOf idShape) _.capacity
      <*> fieldOf "delay" "rounds before its effects apply" (defaultsTo 0 intShape) _.delay
      <*> fieldOf "minimum" "the smallest order accepted" (defaultsTo Q.zero qShape) _.minimum
      <*> fieldOf "maximum" "the largest total accepted per Entity per round" (optionalOf qShape) _.maximum
      <*> fieldOf "capacityUse" "capacity drawn per unit ordered (default 1)" (defaultsTo Q.one qShape) _.capacityUse
      <*> fieldOf "ownerSigns" "the capacity's owner must sign each order (default false)" (defaultsTo false boolShape) _.ownerSigns
      <*> fieldOf "effects" "changes to capacities" (arrayOf effectShape) _.effects
  )
  where
  mk i o r pay cap d mn mx cu os ef = { id: k, inputs: i, outputs: o, escrowPerUnit: r, payment: pay, capacity: cap, delay: d, minimum: mn, maximum: mx, capacityUse: cu, ownerSigns: os, effects: ef }

-- Entities ------------------------------------------------------------------------------------

aggregateShape :: Shape Aggregate
aggregateShape = object "Aggregate"
  ( mk <$> fieldOf "policy" "the Policy that drives it" idShape _.policy
      <*> fieldOf "members" "the Entities that are its members" (arrayOf idShape) _.members
      <*> fieldOf "procedures" "its collective procedures, by name" (emptyByDefault (idMap procedureShape)) _.procedures
  )
  where
  mk p m ps = { policy: p, members: m, procedures: ps }

procedureShape :: Shape CollectiveProcedure
procedureShape = object "Procedure"
  ( mk <$> fieldOf "weights" "the weight of each member" (idMap qShape) _.weights
      <*> fieldOf "residualWeight" "the weight of the rest" qShape _.residualWeight
      <*> fieldOf "threshold" "the weight that carries it" qShape _.threshold
      <*> fieldOf "authorizes" "the capabilities it grants" (listOf capabilityShape) _.authorizes
  )
  where
  mk w r t a = { weights: w, residualWeight: r, threshold: t, authorizes: a }

-- | An Entity: its accounts, capabilities and capacities, and its Aggregate when it is one. An Entity
-- | with no Aggregate is an Actor at the seat of its own id.
entityRec :: EntityId -> RecF Entity Entity
entityRec k = mk <$> fieldOf "accounts" "the holdings" (idMap qShape) _.accounts
  <*> fieldOf "capabilities" "the capabilities it holds" (listOf capabilityShape) _.capabilities
  <*> fieldOf "capacities" "the capacities it holds" (emptyByDefault (idMap qShape)) _.capacities
  <*> fieldOf "aggregate" "its Aggregate" (optionalOf aggregateShape) (aggregateOf <<< _.kind)
  where
  mk a c cap ag = { id: k, accounts: a, capacities: cap, capabilities: c, kind: maybe (Actor (Seat (Id (idText k)))) Aggregate ag }
  aggregateOf = case _ of
    Aggregate a -> Just a
    Actor _ -> Nothing

-- Representations -----------------------------------------------------------------------------

-- | The symmetric bound on a deviation, written as its half-width.
boundShape :: Shape (Maybe Interval)
boundShape = iso (map (\b -> { lo: Q.neg b, hi: b })) (map _.hi) (optionalOf qShape)

estimatorRec :: RecF Estimator Estimator
estimatorRec = mk <$> fieldOf "reference" "the reference quantity" qShape _.reference
  <*> fieldOf "shrinkage" "the shrinkage toward the reference" qShape _.shrinkage
  <*> fieldOf "bound" "a symmetric bound on the deviation" boundShape _.deviationBound
  <*> fieldOf "noiseSpread" "the noise spread (default 0)" (defaultsTo Q.zero qShape) _.noiseSpread
  where
  mk r s b n = { reference: r, shrinkage: s, deviationBound: b, noiseSpread: n }

estimatorShape :: Shape Estimator
estimatorShape = object "Estimator" estimatorRec

observesNote :: String
observesNote = "the Resource whose purchases are observed (default: the Resource the domain names)"

-- | What a premise writes for `observes`: nothing when it is the Resource the domain names.
writtenObserves :: forall r. { domain :: Types.BehaviorDomain, observes :: ResourceId | r } -> Maybe ResourceId
writtenObserves r = if idText r.observes == idText r.domain then Nothing else Just r.observes

-- | A Representation of an Actor in a world (a fixture's, flattened with its estimator).
representationShape :: Shape Representation
representationShape = object "Representation"
  ( mk <$> fieldOf "id" "the Representation" idShape _.id
      <*> fieldOf "actor" "the Actor represented" idShape _.actor
      <*> fieldOf "aggregate" "the Aggregate that holds the estimate" idShape _.aggregate
      <*> fieldOf "domain" "the behaviour domain" idShape _.domain
      <*> fieldOf "observes" observesNote (optionalOf idShape) writtenObserves
      <*> fieldOf "weight" "the weight in the Aggregate's estimate" qShape _.weight
      <*> contramapRec _.estimator estimatorRec
  )
  where
  mk i a g d o w e = { id: i, actor: a, aggregate: g, domain: d, observes: maybe (Id (idText d)) identity o, weight: w, estimator: e }

-- | A Gameboard's scheme: the Representation an Actor's behaviour is estimated under.
schemeShape :: Shape Scheme
schemeShape = object "Scheme"
  ( mk <$> fieldOf "id" "the scheme's name" stringShape _.id
      <*> fieldOf "eligibleActors" "the Actors it may estimate" (listOf idShape) _.eligibleActors
      <*> fieldOf "target" "the Aggregate it estimates" idShape _.target
      <*> fieldOf "domain" "the behaviour domain" idShape _.domain
      <*> fieldOf "observes" observesNote (optionalOf idShape) writtenObserves
      <*> fieldOf "estimator" "the estimator" estimatorShape _.estimator
  )
  where
  mk i e t d o est = { id: i, eligibleActors: e, target: t, domain: d, observes: maybe (Id (idText d)) identity o, estimator: est }

-- | An instantiation's assignment of an Actor to a scheme; the weight defaults to 1.
assignmentShape :: Shape { scheme :: String, actor :: EntityId, weight :: Q.Q }
assignmentShape = object "Assignment"
  ( mk <$> fieldOf "scheme" "the scheme assigned" stringShape _.scheme
      <*> fieldOf "actor" "the Actor assigned" idShape _.actor
      <*> fieldOf "weight" "the weight (default 1)" (defaultsTo Q.one qShape) _.weight
  )
  where
  mk s a w = { scheme: s, actor: a, weight: w }

-- Circulation ---------------------------------------------------------------------------------

circulationShape :: Shape Circulation
circulationShape = defaulted emptyCirculation none $ object "Circulation"
  ( mk <$> fieldOf "production" "capacity that produces a Resource" (arrayOf productionShape) _.production
      <*> fieldOf "flows" "fixed payments per round" (arrayOf flowShape) _.flows
      <*> fieldOf "shares" "variable payments, as shares" (arrayOf shareShape) _.shares
      <*> fieldOf "requirements" "what an Entity must consume" (arrayOf requirementShape) _.requirements
  )
  where
  mk p f s r = { production: p, flows: f, shares: s, requirements: r }
  emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }
  none c = Array.null c.production && Array.null c.flows && Array.null c.shares && Array.null c.requirements

productionShape :: Shape Production
productionShape = recordShape "Production" { entity: idShape, resource: idShape, capacity: idShape }

-- | A fixed payment per round, which is never negative (docs/03 §5.4).
flowShape :: Shape Flow
flowShape = recordShape "Flow" { from: idShape, to: idShape, resource: idShape, amount: refined "a flow amount must not be negative" (_ >= Q.zero) qShape }

shareShape :: Shape Share
shareShape = object "Share"
  ( (\f t s src -> { from: f, to: t, share: s, source: src })
      <$> fieldOf "from" "the paying Entity" idShape _.from
      <*> fieldOf "to" "the receiving Entity" idShape _.to
      <*> fieldOf "share" "the share of the source" qShape _.share
      <*> fieldOf "source" "what the share is of: a query with no party, the payer being it" measureShape _.source
  )

requirementShape :: Shape Requirement
requirementShape = object "Requirement"
  ( (\e r a p -> { entity: e, resource: r, amount: a, penalty: p })
      <$> fieldOf "entity" "the Entity that needs it" idShape _.entity
      <*> fieldOf "resource" "the Resource needed" idShape _.resource
      <*> fieldOf "amount" "the amount per round (default: this round's purchases)" (optionalOf qShape) _.amount
      <*> fieldOf "penalty" "the reduction of fixed incomes per unit unmet (default 0)" (defaultsTo Q.zero qShape) _.penalty
  )

-- The World -----------------------------------------------------------------------------------

-- | Rules as written: a list. Each takes the id of its position, `rule-0`, `rule-1`, and so on.
rulesFromList :: Array Rule -> Map RuleId Rule
rulesFromList = Map.fromFoldable <<< mapWithIndex (\i r -> Tuple (Id ("rule-" <> show i)) r)

-- | The list a Map of Rules is written as: in the order of its positions.
rulesToList :: Map RuleId Rule -> Array Rule
rulesToList m = map snd (sortWith position (Map.toUnfoldable m))
  where
  position (Tuple (Id k) _) = Tuple (String.length k) k

rulesShape :: Shape (Map RuleId Rule)
rulesShape = iso rulesFromList rulesToList (arrayOf ruleShape)

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

-- | A World as a premise writes it: the World, and what only the document holds.
type PremiseWorld = { world :: UnseededWorld, authored :: Authored }

-- | The premise document's reservation check: a facility may escrow only a Resource the document marks
-- | `reservable`. An unknown Resource is reported by validation's reference check.
checkReservations :: Authored -> UnseededWorld -> Either String Unit
checkReservations a w = case concatMap offending (Map.toUnfoldable w.transformations) of
  [] -> Right unit
  bad -> Left (joinWith "; " bad)
  where
  notReservable k = Map.member k w.resources && not (k `elem` a.reservable)
  offending (Tuple t f) = map (\r -> "facility " <> show t <> " reserves " <> show r <> ", which is not reservable") (filter notReservable (Array.fromFoldable (Map.keys f.escrowPerUnit)))

entriesOf :: PremiseWorld -> Map ResourceId ResourceEntry
entriesOf p = Map.mapMaybeWithKey (\k d -> Just { transferability: d.transferability, reservable: k `elem` p.authored.reservable, decay: _.rate <$> find ((_ == k) <<< _.resource) p.authored.decays }) p.world.resources

-- | The World's sections but its Entities, as a function of them: a Gameboard's Entity entries also
-- | carry Actor definitions. The World's key is the starting round (default 1).
worldFields :: RecF PremiseWorld (Map EntityId Entity -> PremiseWorld)
worldFields = mk
  <$> fieldOf "round" "the starting round (default 1)" (defaultsTo 1 intShape) _.world.round
  <*> fieldOf "numeraire" "the unit of account, which the views use for net worth" idShape _.world.numeraire
  <*> fieldOf "resources" "the Resources" (idMap resourceEntry) entriesOf
  <*> fieldOf "markets" "the Markets" (idMapWith marketShape) _.world.markets
  <*> fieldOf "transformations" "the facilities" (idMapWith facilityShape) _.world.transformations
  <*> fieldOf "flowGates" "the conditions on a Transfer of a Resource" (arrayOf gateShape) _.world.flowGates
  <*> fieldOf "groups" "the group each Entity counts as, where it is not its own" (emptyByDefault (idMap idShape)) _.world.groups
  <*> fieldOf "ruleKinds" "the kinds of Rule the premise declares" (emptyByDefault (idMap ruleKindDecl)) _.world.ruleKinds
  <*> fieldOf "rules" "the Rules in force from the start" rulesShape _.world.rules
  <*> fieldOf "representations" "the Representations bound in the world" (arrayOf representationShape) _.world.representations
  <*> fieldOf "circulation" "the recurring economic cycle" circulationShape _.authored.circulation
  where
  gateShape = recordShape "FlowGate" { resource: idShape, exemptSharedMarket: boolShape, bundled: optionalOf idShape } :: Shape FlowGate
  mk round numeraire entries markets transformations flowGates groups ruleKinds rules representations circulation entities =
    let
      decays = mapMaybe (\(Tuple k e) -> (\rate -> { resource: k, rate }) <$> e.decay) (Map.toUnfoldable entries)
      reservable = map (\(Tuple k _) -> k) (filter (\(Tuple _ e) -> e.reservable) (Map.toUnfoldable entries))
      resources = Map.mapMaybeWithKey (\k e -> Just ({ id: k, unit: idText k, transferability: e.transferability } :: ResourceDefinition)) entries
    in
      { authored: { circulation, reservable, decays }
      , world: { round, numeraire, resources, markets, transformations, flowGates, groups, ruleKinds, rules, representations, entities, scheduled: Map.empty, deliveries: [], factors: Map.empty, recurring: compileIn numeraire circulation decays, events: [] }
      }

-- | The whole World, with its Entities by key: a fixture's.
worldRec :: RecF PremiseWorld PremiseWorld
worldRec = (#) <$> fieldOf "entities" "the Entities" (idMapWith (\k -> object "Entity" (entityRec k))) _.world.entities <*> worldFields
