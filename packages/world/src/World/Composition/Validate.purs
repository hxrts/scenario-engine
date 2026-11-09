-- | Validation (docs/03_premise.md §12): structural references, economic and Representation
-- | checks, resolution consistency, and feasibility. Imports Instantiate and Reconcile, so
-- | the instantiation and reconciliation checks run as part of validation.
module World.Composition.Validate
  ( validateComposition
  , unknownNames
  , Names
  , worldNames
  , compositionNames
  ) where

import Prelude

import Core.Map (entries, values)

import World.Composition.Instantiate (instantiate, withoutExplicitActor)
import World.Composition.Reconcile (reconcile)
import Schema.Composition (Bound(..), Fallback(..), PremiseComposition, InstantiationConfig, Measure(..), MetricRef, capacityCategory)
import Schema.Composition as Schema
import World.Error (ValidationError(..))
import Core.Num as Q
import Laws.Query (aggregateOf, isAggregate)
import Laws.Types (DrawSource(..), Actor, ActorId, Id(..), Exchange, ExchangeId, StrategyId, RefKind(..), ResourceDefinition, ResourceId, RuleKindDecl, RuleKindId, Transformation, TransformationId, idText)
import Core.Kernel.Consume (validateTemplates)
import Data.Newtype (unwrap)
import Laws.World (TemplateTerms, WorldTemplate, templateTerms, RecurringAmount(..), RecurringDestination(..), WorldFields, WorldPositions, foldWorld, ignoreWorld, lawAction, lawRefs, spawnTargets, templateRefs)
import Data.Const (Const(..))
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, length, mapMaybe, nub, null)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (Tuple(..), snd)
import Foreign.Object as Object

-- | Structural, economic, Representation, resolution-consistency, and feasibility checks.
validateComposition :: PremiseComposition -> InstantiationConfig -> Either (Array ValidationError) PremiseComposition
validateComposition gb config =
  let
    structural = references <> delays <> holderChecks <> fallbacks <> objectives <> procedures <> roundCount <> definitions <> estimators <> duplicateAdjustments <> openingState <> agreements
    feasibility = either identity (const []) (instantiate gb config)
    consistency = if null (structural <> feasibility) then concatMap consistent (fromFoldable (Map.keys gb.explicitActors)) else []
    errors = structural <> feasibility <> consistency
  in
    if null errors then Right gb else Left errors
  where
  w = gb.world
  actors = values w.actors
  transformations = values w.transformations
  aggregates = mapMaybe (\e -> map { actor: e, aggregate: _ } (aggregateOf e)) actors
  -- whether the Actor with this id is an Aggregate (an unknown id is not)
  isAggregateId e = maybe false isAggregate (Map.lookup e w.actors)
  -- what is held of a Resource the premise calls a capacity, by name
  held = filter isCapacity (nub (concatMap (\e -> fromFoldable (Map.keys e.accounts)) actors))
  isCapacity r = (Map.lookup r w.resources >>= _.category) == Just capacityCategory
  holderDraws = concatMap (\t -> map (Tuple t.id) (filter ((_ == HolderOf) <<< _.from) t.draws)) transformations
  -- what a holder draw, an effect, or the production list refers to
  referenced = nub
    ( map (_.resource <<< snd) holderDraws
        <> concatMap (map _.resource <<< _.effects) transformations
        <> mapMaybe producedFrom w.recurring
    )

  references = unknownNames (compositionNames gb) (worldRefs w <> compositionRefs gb config)
    <> map (UnknownReference RefResource <<< idText) (filter (\c -> not (c `elem` referenced)) held)
    <> map (UnknownReference RefActor <<< idText) (filter (\e -> not (Map.member e gb.explicitActors)) (fromFoldable (Map.keys config.fallbacks)))
  templates = entries w.templates
  agreements = unknownNames (compositionNames gb) (concatMap (templateRefs <<< snd) templates <> concatMap lawRefs w.laws)
    <> map InvalidTemplate (nub (filter (\t -> not (Map.member t w.templates)) (concatMap (spawnTargets <<< snd) templates)))
    <> either (pure <<< InvalidTemplate <<< show) (const []) (validateTemplates (map unwrap w.templates))
    <> map (UnknownReference RefTransformation) (filter (\a -> not (Map.member (Id a) w.exchanges || Map.member (Id a) w.transformations)) (mapMaybe lawAction w.laws))
  delays = map (NegativeDelay <<< _.id) (filter (\t -> t.delay < 0) transformations)
  holderChecks = concatMap productionHeld w.recurring
    <> concatMap coverageAggregate (entries gb.explicitActors)
    <> concatMap schemeTarget gb.schemes
    <> map (DuplicateHolder <<< show) (filter holdersDuplicated drawnSolely)
    <> map (\(Tuple t d) -> DrawWithoutHolder (show t) (show d.resource)) (filter noHolder holderDraws)
    <> map (TooManyHolderDraws <<< show <<< _.id) (filter (\t -> length (filter ((_ == HolderOf) <<< _.from) t.draws) > 1) transformations)
  -- a Resource read as an amount held is held by each producer under one name; one drawn from its holder has one holder
  sharedProduction c = any (\p -> producedFrom p == Just c) w.recurring
  drawnSolely = filter (not <<< sharedProduction) (nub (map (_.resource <<< snd) holderDraws))
  holdersDuplicated r = length (filter (Map.member r <<< _.accounts) actors) > 1
  noHolder (Tuple _ d) = not (any (Map.member d.resource <<< _.accounts) actors)
  productionHeld p = case p.destination, p.amount of
    ToActor e, HeldAmount c | Just x <- Map.lookup e w.actors, not (Map.member c x.accounts) -> [ ProductionWithoutHolding e (show c) ]
    _, _ -> []
  -- the holding a rule produces from
  producedFrom r = case r.amount of
    HeldAmount c -> Just c
    _ -> Nothing

  coverageAggregate (Tuple e d) = case d.fallback of
    Just (ImplicitCoverage agg) | Map.member agg w.actors && not (isAggregateId agg) -> [ CoverageNotAggregate e agg ]
    _ -> []
  schemeTarget s = if Map.member s.target w.actors && not (isAggregateId s.target) then [ SchemeTargetNotAggregate s.id s.target ] else []
  fallbacks = concatMap (\(Tuple e d) -> if not d.required && isNothing d.fallback then [ MissingFallback e ] else []) (entries gb.explicitActors)
  objectives = concatMap (\(Tuple e d) -> if isNothing d.objective then [ MissingObjective e ] else []) (entries gb.explicitActors)
  procedures = concatMap
    (\x -> if any (\p -> not (all (_ `elem` x.actor.capabilities) p.authorizes)) (values x.aggregate.procedures) then [ ProcedureExceedsAuthority x.actor.id ] else [])
    aggregates
  -- each Actor is a defined Actor or an Aggregate, never both and never neither
  definitions = concatMap definition actors
  definition e = case Map.lookup e.id gb.explicitActors, isAggregate e of
    Just _, true -> [ DefinitionAndAggregate e.id ]
    -- an engine-only world, such as a fixture's, defines no Actors at all
    Nothing, false -> if Map.isEmpty gb.explicitActors then [] else [ NoDefinition e.id ]
    Just d, false -> (if null e.capabilities then [ NoCapabilities e.id ] else []) <> (if d.dependency == "" then [ NoDependency e.id ] else [])
    Nothing, true -> []
  estimators = map (BadEstimator <<< _.id) (filter (not <<< estimatorInRange <<< _.estimator) gb.schemes)
  estimatorInRange e = e.reference >= Q.zero && e.shrinkage >= Q.zero && e.shrinkage <= Q.one && e.noiseSpread >= Q.zero && maybe true (\b -> b.lo <= b.hi) e.deviationBound
  duplicateAdjustments = concatMap (\s -> map (DuplicateAdjustment s.id) (filter (adjustedElsewhere s) s.eligibleActors)) gb.schemes
  adjustedElsewhere s e = any (\o -> o.id /= s.id && o.target == s.target && o.domain == s.domain && o.observes == s.observes && e `elem` o.eligibleActors) gb.schemes
  openingState = concatMap openingExchange (values w.exchanges) <> map (NegativeHolding <<< _.id) (filter (any (_ < Q.zero) <<< values <<< _.accounts) actors)
  openingExchange m
    | m.position < m.cost.domain.lo || m.position > m.cost.domain.hi || any (_ < Q.zero) (values m.reserves) = [ InfeasibleExchange (show m.id) ]
    | otherwise = []
  roundCount =
    if config.rounds < gb.rounds.min || config.rounds > gb.rounds.max then [ RoundsOutOfRange { rounds: config.rounds, min: gb.rounds.min, max: gb.rounds.max } ]
    else []
  consistent e = case Map.lookup e gb.explicitActors of
    Just { fallback: Just (ImplicitCoverage _) } ->
      case instantiate gb config, instantiate gb (withoutExplicitActor e config) of
        Right a, Right b -> either (\ds -> [ ResolutionInconsistent e ds ]) (const []) (reconcile a b)
        _, _ -> []
    _ -> []

-- | Every name the world refers to, each with what it must name.
worldRefs :: forall r. Record (WorldFields r) -> Array (Tuple RefKind String)
worldRefs = foldWorld
  (ignoreWorld :: WorldPositions (Const (Array (Tuple RefKind String))))
    { actor = ref RefActor
    , groupKey = ref RefActor
    , resource = ref RefResource
    , exchange = ref RefExchange
    , transformation = ref RefTransformation
    , ruleKind = ref RefRuleKind
    , members = \_ -> Const <<< map (name RefActor)
    , weights = Const <<< map (name RefActor) <<< fromFoldable <<< Map.keys
    }
  where
  name :: forall t. RefKind -> Id t -> Tuple RefKind String
  name kind x = Tuple kind (idText x)

  ref :: forall t b. RefKind -> Id t -> Const (Array (Tuple RefKind String)) b
  ref kind x = Const [ name kind x ]

-- | The names the composition's objectives, schemes, and fallbacks refer to, and the configuration's.
compositionRefs :: PremiseComposition -> InstantiationConfig -> Array (Tuple RefKind String)
compositionRefs gb config =
  concatMap metricRefs (concatMap _.criteria (mapMaybe _.objective explicitActors) >>= \c -> [ c.metric ] <> boundMetric c.atLeast)
    <> map (name RefActor) (concatMap coverageOf fallbacks)
    <> concatMap (\s -> [ name RefActor s.target ] <> map (name RefActor) s.eligibleActors <> [ name RefResource s.observes ]) gb.schemes
    <> map (name RefStrategy) (map _.strategy (mapMaybe aggregateOf (values gb.world.actors)) <> concatMap _.nonPlayableActor explicitActors <> concatMap institutional fallbacks)
  where
  explicitActors = values gb.explicitActors
  -- an institutional fallback binds a catalog Strategy, declared or overridden in the configuration
  fallbacks = mapMaybe _.fallback explicitActors <> values config.fallbacks

  name :: forall t. RefKind -> Id t -> Tuple RefKind String
  name kind x = Tuple kind (idText x)
  -- a criterion's metric, and the metric its bound reads when it is one
  boundMetric = case _ of
    Fixed _ -> []
    Of m -> [ m ]

  metricRefs :: MetricRef -> Array (Tuple RefKind String)
  metricRefs m = map (name RefActor) m.actors <> case m.measure of
    HoldingOf r -> [ name RefResource r ]
    ShortfallOf r -> [ name RefResource r ]
    ShortfallRounds r -> [ name RefResource r ]
    Schema.PayeeReceipts -> []
    VolumeOf t -> [ name RefTransformation t ]
  -- the Aggregate a coverage fallback names
  coverageOf = case _ of
    ImplicitCoverage agg -> [ agg ]
    _ -> []
  institutional = case _ of
    InstitutionalStrategy p -> [ p ]
    _ -> []

-- | What a reference can name.
type Names m r t k =
  { actors :: Map ActorId Actor
  , exchanges :: Map ExchangeId m
  , resources :: Map ResourceId r
  , transformations :: Map TransformationId t
  , ruleKinds :: Map RuleKindId k
  , templates :: Map String TemplateTerms
  , parameters :: Set String
  , strategies :: Set StrategyId
  }

-- | The names of a world on its own: a literal premise has no parameters, schemes, or Strategies.
worldNames :: forall m r t k rest. { actors :: Map ActorId Actor, exchanges :: Map ExchangeId m, resources :: Map ResourceId r, transformations :: Map TransformationId t, ruleKinds :: Map RuleKindId k, templates :: Map String WorldTemplate | rest } -> Names m r t k
worldNames w =
  { actors: w.actors
  , exchanges: w.exchanges
  , resources: w.resources
  , transformations: w.transformations
  , ruleKinds: w.ruleKinds
  , templates: map templateTerms w.templates
  , parameters: Set.empty
  , strategies: Set.empty
  }

-- | The names a composition offers a reference.
compositionNames :: PremiseComposition -> Names Exchange ResourceDefinition Transformation RuleKindDecl
compositionNames gb = (worldNames gb.world)
  { parameters = Set.fromFoldable (Object.keys gb.parameters)
  , strategies = Map.keys gb.strategies
  }

-- | The names the scope lacks: one error per unknown name, for every kind of reference.
unknownNames :: forall m r t k. Names m r t k -> Array (Tuple RefKind String) -> Array ValidationError
unknownNames w = map (\(Tuple kind name) -> UnknownReference kind name) <<< filter (\(Tuple kind name) -> not (known kind name))
  where
  known kind name = case kind of
    RefActor -> Map.member (Id name) w.actors
    RefExchange -> Map.member (Id name) w.exchanges
    RefResource -> Map.member (Id name) w.resources
    RefTransformation -> Map.member (Id name) w.transformations
    RefParameter -> Set.member name w.parameters
    RefProcedure -> any (maybe false (Map.member (Id name) <<< _.procedures) <<< aggregateOf) (values w.actors)
    RefStrategy -> Set.member (Id name) w.strategies
    RefRuleKind -> Map.member (Id name) w.ruleKinds
    RefTemplate -> Map.member name w.templates
