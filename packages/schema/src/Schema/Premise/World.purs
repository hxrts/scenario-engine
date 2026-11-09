-- | The premise format of the World (docs/03_premise.md §3–§5): Resources, Exchanges, transformations,
-- | Actors with their Aggregates and procedures, Representation schemes and assignments, and
-- | circulation. Each shape reads the core type directly (Laws.Types, Laws.World): a key with a default
-- | is omitted when it holds that default, and a record keyed by its id reads the id from the key.
-- | `worldFields` is every section but the Actors, so a possible World can read the Actors with their
-- | Actor definitions and a fixture without.
module Schema.Premise.World
  ( PremiseWorld
  , worldFields
  , worldRec
  , actorRec
  , rulesFromList
  , schemeShape
  , assignmentShape
  , checkReservations
  ) where

import Prelude
import Laws.World (focusShape, FlowGate, UnseededWorld)

import Core.Num (Q, qShape)
import Core.Num as Q
import Core.Shape (RecF, Shape, arrayOf, atBare, atKindArg, boolShape, contramapRec, defaulted, defaultsTo, emptyByDefault, field, fieldOf, genericPremiseSum, intShape, iso, listOf, nameShape, object, optionalOf, recordOf, recordShape, refined, stringShape, (&.))
import Laws.Types (Aggregate, DelayedFlow, Draw, DrawSource, CollectiveProcedure, CostFunction, Actor, ActorId, ActorKind(..), Estimator, FlowGovernance(..), Id(..), Interval, Exchange, ExchangeId, Payment, Representation, ResourceDefinition, ResourceId, Rule, RuleForm, RuleId, RuleKindDecl, Transferability, Transformation, TransformationId, executorDraws, idMap, idMapWith, idShape, idText)
import Laws.Types as Types
import Data.Array as Array
import Data.Array (concatMap, elem, filter, find, mapMaybe, mapWithIndex, sortWith)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import Schema.PossibleWorld (Scheme)
import Schema.Premise.Intent (capabilityShape, ruleShape)
import Schema.Premise.Query (measureShape)
import Schema.Recurring (Authored, Circulation, Flow, Production, Requirement, Share, compileIn)

-- Resources and Exchanges -----------------------------------------------------------------------

transferabilityShape :: Shape Transferability
transferabilityShape = genericPremiseSum "Transferability"
  { "Free": atBare { tag: "Free", desc: "tradable and transferable" }
  , "Restricted": atBare { tag: "Restricted", desc: "transferred only by an Actor with the matching transfer capability" }
  , "NonTransferable": atBare { tag: "NonTransferable", desc: "never moves between Actors" }
  }

-- | A Resource as a premise writes it: what the World holds, and what only the document holds.
type ResourceEntry = { transferability :: Transferability, reservable :: Boolean, decay :: Maybe Q, category :: Maybe String }

resourceEntry :: Shape ResourceEntry
resourceEntry = recordOf "Resource"
  ( field @"transferability" "how it may move" transferabilityShape
      &. field @"reservable" "may be escrowed by a transformation (default false)" (defaultsTo false boolShape)
      &. field @"decay" "the share lost per round" (optionalOf qShape)
      &. field @"category" "the premise's own grouping of it, which the views read" (optionalOf stringShape)
  )

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

exchangeShape :: ExchangeId -> Shape Exchange
exchangeShape k = object "Exchange"
  ( mk <$> fieldOf "base" "the Resource traded" idShape _.base
      <*> fieldOf "quote" "the Resource it is priced in" idShape _.quote
      <*> fieldOf "cost" "the cost curve" costShape _.cost
      <*> fieldOf "reserves" "the reserves, by Resource" (idMap qShape) _.reserves
      <*> fieldOf "access" "the Actors that may trade" (listOf idShape) _.access
      <*> fieldOf "position" "the starting net position (default 0)" (defaultsTo Q.zero qShape) _.position
  )
  where
  mk b q' c r a p = { id: k, base: b, quote: q', cost: c, position: p, reserves: r, access: a }

drawSourceShape :: Shape DrawSource
drawSourceShape = genericPremiseSum "DrawSource"
  { "Executor": atBare { tag: "Executor", desc: "the Actor running the order" }
  , "HolderOf": atBare { tag: "HolderOf", desc: "the unique positive holder of the Resource" }
  }

drawShape :: Shape Draw
drawShape = recordShape "Draw" { resource: idShape, from: drawSourceShape, perUnit: qShape }

effectShape :: Shape DelayedFlow
effectShape = recordOf "Effect"
  ( field @"resource" "the Resource created or sunk" idShape
      &. field @"holder" "the Actor it is for (default the ordering Actor)" (optionalOf idShape)
      &. field @"perUnit" "the change per unit ordered" qShape
      &. field @"rounds" "how many rounds it lasts" (optionalOf intShape)
  )

paymentShape :: Shape Payment
paymentShape = recordShape "Payment" { curve: costShape, resource: idShape, to: idShape }

-- | A transformation (a transformation). Every key is optional.
transformationShape :: TransformationId -> Shape Transformation
transformationShape k = object "Transformation"
  ( mk <$> fieldOf "inputs" "consumed per unit" (emptyByDefault (idMap qShape)) _.inputs
      <*> fieldOf "outputs" "produced per unit" (emptyByDefault (idMap qShape)) _.outputs
      <*> fieldOf "draws" "held per unit for the round, never consumed" (arrayOf drawShape) _.draws
      <*> fieldOf "payment" "what each order pays: its curve, its Resource, and who is paid" (optionalOf paymentShape) _.payment
      <*> fieldOf "delay" "rounds before its effects apply" (defaultsTo 0 intShape) _.delay
      <*> fieldOf "minimum" "the smallest order accepted" (defaultsTo Q.zero qShape) _.minimum
      <*> fieldOf "maximum" "the largest total accepted per Actor per round" (optionalOf qShape) _.maximum
      <*> fieldOf "ownerSigns" "the holder of the Resource drawn must sign each order (default false)" (defaultsTo false boolShape) _.ownerSigns
      <*> fieldOf "effects" "what is created or sunk for a holder after the delay" (arrayOf effectShape) _.effects
  )
  where
  mk i o r pay d mn mx os ef = { id: k, inputs: i, outputs: o, draws: r, payment: pay, delay: d, minimum: mn, maximum: mx, ownerSigns: os, effects: ef }

-- Actors ------------------------------------------------------------------------------------

aggregateShape :: Shape Aggregate
aggregateShape = recordOf "Aggregate"
  ( field @"strategy" "the Strategy that drives it" idShape
      &. field @"members" "the Actors that are its members" (arrayOf idShape)
      &. field @"procedures" "its collective procedures, by name" (emptyByDefault (idMap procedureShape))
  )

procedureShape :: Shape CollectiveProcedure
procedureShape = recordOf "Procedure"
  ( field @"weights" "the weight of each member" (idMap qShape)
      &. field @"residualWeight" "the weight of the rest" qShape
      &. field @"threshold" "the weight that carries it" qShape
      &. field @"authorizes" "the capabilities it grants" (listOf capabilityShape)
  )

-- | An Actor: its accounts and capabilities, and its Aggregate when it is one. An Actor
-- | with no Aggregate is an Actor at the seat of its own id.
actorRec :: ActorId -> RecF Actor Actor
actorRec k = mk <$> fieldOf "accounts" "the holdings" (idMap qShape) _.accounts
  <*> fieldOf "capabilities" "the capabilities it holds" (listOf capabilityShape) _.capabilities
  <*> fieldOf "aggregate" "its Aggregate" (optionalOf aggregateShape) (aggregateOf <<< _.kind)
  where
  mk a c ag = { id: k, accounts: a, capabilities: c, kind: maybe (PlayableActor (Id (idText k))) Aggregate ag }
  aggregateOf = case _ of
    Aggregate a -> Just a
    PlayableActor _ -> Nothing
    NonPlayableActor _ -> Nothing

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
      <*> fieldOf "explicitActor" "the PlayableActor represented" idShape _.explicitActor
      <*> fieldOf "aggregate" "the Aggregate that holds the estimate" idShape _.aggregate
      <*> fieldOf "domain" "the behaviour domain" idShape _.domain
      <*> fieldOf "observes" observesNote (optionalOf idShape) writtenObserves
      <*> fieldOf "weight" "the weight in the Aggregate's estimate" qShape _.weight
      <*> contramapRec _.estimator estimatorRec
  )
  where
  mk i a g d o w e = { id: i, explicitActor: a, aggregate: g, domain: d, observes: maybe (Id (idText d)) identity o, weight: w, estimator: e }

-- | A possible World's scheme: the Representation an Actor's behaviour is estimated under.
schemeShape :: Shape Scheme
schemeShape = object "Scheme"
  ( mk <$> fieldOf "id" "the scheme's name" stringShape _.id
      <*> fieldOf "eligibleActors" "the PlayableActors it may estimate" (listOf idShape) _.eligibleActors
      <*> fieldOf "target" "the Aggregate it estimates" idShape _.target
      <*> fieldOf "domain" "the behaviour domain" idShape _.domain
      <*> fieldOf "observes" observesNote (optionalOf idShape) writtenObserves
      <*> fieldOf "estimator" "the estimator" estimatorShape _.estimator
  )
  where
  mk i e t d o est = { id: i, eligibleActors: e, target: t, domain: d, observes: maybe (Id (idText d)) identity o, estimator: est }

-- | An instantiation's assignment of an Actor to a scheme; the weight defaults to 1.
assignmentShape :: Shape { scheme :: String, explicitActor :: ActorId, weight :: Q.Q }
assignmentShape = recordOf "Assignment"
  ( field @"scheme" "the scheme assigned" stringShape
      &. field @"explicitActor" "the PlayableActor assigned" idShape
      &. field @"weight" "the weight (default 1)" (defaultsTo Q.one qShape)
  )

-- Circulation ---------------------------------------------------------------------------------

circulationShape :: Shape Circulation
circulationShape = defaulted emptyCirculation none $ recordOf "Circulation"
  ( field @"production" "what a holding of a capacity produces" (arrayOf productionShape)
      &. field @"flows" "fixed payments per round" (arrayOf flowShape)
      &. field @"shares" "variable payments, as shares" (arrayOf shareShape)
      &. field @"requirements" "what an Actor must consume" (arrayOf requirementShape)
  )
  where
  emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }
  none c = Array.null c.production && Array.null c.flows && Array.null c.shares && Array.null c.requirements

productionShape :: Shape Production
productionShape = recordShape "Production" { actor: idShape, resource: idShape, capacity: idShape }

-- | A fixed payment per round, which is never negative (docs/03 §5.4).
flowShape :: Shape Flow
flowShape = recordShape "Flow" { from: idShape, to: idShape, resource: idShape, amount: refined "a flow amount must not be negative" (_ >= Q.zero) qShape }

shareShape :: Shape Share
shareShape = recordOf "Share"
  ( field @"from" "the paying Actor" idShape
      &. field @"to" "the receiving Actor" idShape
      &. field @"share" "the share of the source" qShape
      &. field @"source" "what the share is of: a query with no party, the payer being it" measureShape
  )

requirementShape :: Shape Requirement
requirementShape = recordOf "Requirement"
  ( field @"actor" "the Actor that needs it" idShape
      &. field @"resource" "the Resource needed" idShape
      &. field @"amount" "the amount per round (default: this round's purchases)" (optionalOf qShape)
      &. field @"penalty" "the reduction of fixed incomes per unit unmet (default 0)" (defaultsTo Q.zero qShape)
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
  , "ReduceHolding": atBare { tag: "reduce", desc: "a reduction of what can be drawn, governed by the holder" }
  }
  where
  governance = nameShape "FlowGovernance" "who governs: destination, origin or either" governed [ GovernedByDestination, GovernedBySource, GovernedByEither ]
  governed = case _ of
    GovernedByDestination -> "destination"
    GovernedBySource -> "origin"
    GovernedByEither -> "either"

-- | A World as a premise writes it: the World, and what only the document holds.
type PremiseWorld = { world :: UnseededWorld, authored :: Authored }

-- | The premise document's reservation check: a transformation may escrow only a Resource the document marks
-- | `reservable`: every draw `from: Executor` names one. An unknown Resource is reported by validation's reference check.
checkReservations :: Authored -> UnseededWorld -> Either String Unit
checkReservations a w = case concatMap offending (Map.toUnfoldable w.transformations) of
  [] -> Right unit
  bad -> Left (joinWith "; " bad)
  where
  notReservable k = Map.member k w.resources && not (k `elem` a.reservable)
  offending (Tuple t f) = map (\r -> "transformation " <> show t <> " reserves " <> show r <> ", which is not reservable") (filter notReservable (map _.resource (executorDraws f)))

entriesOf :: PremiseWorld -> Map ResourceId ResourceEntry
entriesOf p = Map.mapMaybeWithKey (\k d -> Just { transferability: d.transferability, category: d.category, reservable: k `elem` p.authored.reservable, decay: _.rate <$> find ((_ == k) <<< _.resource) p.authored.decays }) p.world.resources

-- | The World's sections but its Actors, as a function of them: a possible World's Actor entries also
-- | carry Actor definitions. The World's key is the starting round (default 1).
worldFields :: RecF PremiseWorld (Map ActorId Actor -> PremiseWorld)
worldFields = mk
  <$> fieldOf "round" "the starting round (default 1)" (defaultsTo 1 intShape) _.world.round
  <*> fieldOf "numeraire" "the unit of account, which the views use for net worth" idShape _.world.numeraire
  <*> fieldOf "focus" "the Resource the exchange views centre on" (optionalOf focusShape) _.world.focus
  <*> fieldOf "resources" "the Resources" (idMap resourceEntry) entriesOf
  <*> fieldOf "exchanges" "the Exchanges" (idMapWith exchangeShape) _.world.exchanges
  <*> fieldOf "transformations" "the transformations" (idMapWith transformationShape) _.world.transformations
  <*> fieldOf "flowGates" "the conditions on a Transfer of a Resource" (arrayOf gateShape) _.world.flowGates
  <*> fieldOf "groups" "the group each Actor counts as, where it is not its own" (emptyByDefault (idMap idShape)) _.world.groups
  <*> fieldOf "ruleKinds" "the kinds of Rule the premise declares" (emptyByDefault (idMap ruleKindDecl)) _.world.ruleKinds
  <*> fieldOf "rules" "the Rules in force from the start" rulesShape _.world.rules
  <*> fieldOf "representations" "the Representations bound in the world" (arrayOf representationShape) _.world.representations
  <*> fieldOf "circulation" "the recurring economic cycle" circulationShape _.authored.circulation
  where
  gateShape = recordShape "FlowGate" { resource: idShape, exemptSharedExchange: boolShape, bundled: optionalOf idShape } :: Shape FlowGate
  mk round numeraire focus entries exchanges transformations flowGates groups ruleKinds rules representations circulation actors =
    let
      decays = mapMaybe (\(Tuple k e) -> (\rate -> { resource: k, rate }) <$> e.decay) (Map.toUnfoldable entries)
      reservable = map (\(Tuple k _) -> k) (filter (\(Tuple _ e) -> e.reservable) (Map.toUnfoldable entries))
      resources = Map.mapMaybeWithKey (\k e -> Just ({ id: k, unit: idText k, transferability: e.transferability, category: e.category } :: ResourceDefinition)) entries
    in
      { authored: { circulation, reservable, decays }
      , world: { round, numeraire, focus, resources, exchanges, transformations, flowGates, groups, ruleKinds, rules, representations, actors, pending: [], laws: [], templates: Map.empty, factors: Map.empty, recurring: compileIn numeraire circulation decays, events: [] }
      }

-- | The whole World, with its Actors by key: a fixture's.
worldRec :: RecF PremiseWorld PremiseWorld
worldRec = (#) <$> fieldOf "actors" "the Actors" (idMapWith (\k -> object "Actor" (actorRec k))) _.world.actors <*> worldFields
