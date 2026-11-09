-- | Filtering a premise: the one operation that takes a subset of a premise, for a
-- | cut-down game or a fixture. What belongs to something dropped goes with it: an account, a capability, a
-- | Exchange's access entry, a recurring flow, a Rule, an actor definition, a seat. What a kept thing needs
-- | is not supplied: a kept thing that refers to a dropped one fails `checkComposition`, the one reference
-- | check, so a subset cannot silently depend on what it left out.
module Assembly.Restrict
  ( Selection
  , everything
  , restrict
  ) where

import Prelude

import World.Composition.Validate (compositionNames, unknownNames, worldNames)
import Core.Map (values)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (Id(..), holderDraw, Capability(..), ActorId, ExchangeId, RefKind(..), ResourceId, RuleF(..), TransformationId, idText)
import Laws.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), lawAction, lawRefs, templateRefs)
import Data.Array (null, all, concat, concatMap, elem, filter, fromFoldable, mapMaybe, nub)
import Data.Either (Either(..), isRight)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Assembly.Composition (CompositionParts, checkComposition, checkRefs)
import Schema.Composition (PremiseComposition)

-- | What to keep. `accounts` narrows an Actor's accounts further, to those Resources; an Actor it does
-- | not name keeps every account of a kept Resource.
type Selection =
  { actors :: Array ActorId
  , resources :: Array ResourceId
  , exchanges :: Array ExchangeId
  , transformations :: Array TransformationId
  , accounts :: Map ActorId (Array ResourceId)
  }

-- | The Selection that keeps a composition whole.
everything :: PremiseComposition -> Selection
everything gb =
  { actors: fromFoldable (Map.keys gb.world.actors)
  , resources: fromFoldable (Map.keys gb.world.resources)
  , exchanges: fromFoldable (Map.keys gb.world.exchanges)
  , transformations: fromFoldable (Map.keys gb.world.transformations)
  , accounts: Map.empty
  }

-- | The premise cut down to the Selection, checked. A name the premise does not have is an error, and so
-- | is a kept thing that refers to a dropped one.
restrict :: Selection -> CompositionParts -> Either AssemblyError CompositionParts
restrict s p = case unknownNames (compositionNames p.composition) named of
  [] -> checkComposition (filtered s p)
  es -> Left (Invalid es)
  where
  named = concat
    [ map (Tuple RefActor <<< idText) (s.actors <> fromFoldable (Map.keys s.accounts))
    , map (Tuple RefResource <<< idText) s.resources
    , map (Tuple RefExchange <<< idText) s.exchanges
    , map (Tuple RefTransformation <<< idText) s.transformations
    ]

filtered :: Selection -> CompositionParts -> CompositionParts
filtered s p = p
  { composition = gb { strategies = Map.filter resolves gb.strategies }
  , config = p.config
      { seats = Map.filterKeys keptActor p.config.seats
      , nonPlayableActors = Map.filterKeys keptActor p.config.nonPlayableActors
      , fallbacks = Map.filterKeys keptActor p.config.fallbacks
      , representations = filter (\r -> keptActor r.explicitActor && r.scheme `elem` map _.id schemes) p.config.representations
      }
  , presets = Object.filter resolves p.presets
  }
  where
  keptActor e = e `elem` s.actors
  selected r = r `elem` s.resources
  old = p.composition
  authored = old.authored
  circulation = authored.circulation
  transformations = Map.filterKeys (_ `elem` s.transformations) old.world.transformations
  recurring = filter keptFlow old.world.recurring
  keptFlow r = selected r.resource && all keptActor (flowParties r) && all selected (flowResources r)
  -- what a kept Transformation or a production still draws on or changes is kept, whatever the Selection says
  keptResource r = selected r || r `elem` drawn
  -- a gate goes with its Resource, and with the Transformation that carries it
  keptGate g = keptResource g.resource && maybe true (_ `elem` s.transformations) g.bundled
  drawn = nub (concatMap (\t -> map _.resource (holderDrawOf t) <> map _.resource t.effects) (values transformations) <> mapMaybe producedFrom recurring)
  holderDrawOf = fromFoldable <<< holderDraw
  keptAccount e r = r `elem` drawn || selected r && maybe true (elem r) (Map.lookup e s.accounts)
  keptCapability = case _ of
    MayTrade m -> m `elem` s.exchanges
    MayTransfer r -> keptResource r
    MayTransform t -> t `elem` s.transformations
    MayEnact _ -> true
  actorOf e = e
    { accounts = Map.filterKeys (keptAccount e.id) e.accounts
    , capabilities = filter keptCapability e.capabilities
    }
  keptRule = case _ of
    FlowRule x -> keptActor x.scope.governor && keptActor x.scope.counterparty && all keptResource x.scope.over
    HoldingRule x -> x.resource `elem` drawn
  keptNames = worldNames
    { actors: Map.filterKeys keptActor old.world.actors
    , exchanges: Map.filterKeys (_ `elem` s.exchanges) old.world.exchanges
    , resources: Map.filterKeys keptResource old.world.resources
    , transformations
    , ruleKinds: old.world.ruleKinds
    , templates: old.world.templates
    }
  -- a law goes with what it names, and with the action it is charged on
  keptLaw l = null (unknownNames keptNames (lawRefs l)) && maybe true (\a -> Map.member (Id a) keptNames.exchanges || Map.member (Id a) transformations) (lawAction l)
  schemes = filter (\x -> keptActor x.target && keptResource x.observes) old.schemes
  gb = old
    { world = old.world
        { actors = map actorOf (Map.filterKeys keptActor old.world.actors)
        , resources = Map.filterKeys keptResource old.world.resources
        , exchanges = map (\m -> m { access = filter keptActor m.access }) (Map.filterKeys (_ `elem` s.exchanges) old.world.exchanges)
        , transformations = transformations
        , representations = filter (\r -> keptActor r.explicitActor && keptActor r.aggregate) old.world.representations
        , flowGates = filter keptGate old.world.flowGates
        , groups = Map.filterWithKey (\k v -> keptActor k && keptActor v) old.world.groups
        , rules = Map.filter keptRule old.world.rules
        , recurring = recurring
        , templates = Map.filter (null <<< unknownNames keptNames <<< templateRefs) old.world.templates
        , laws = filter keptLaw old.world.laws
        }
    , authored = authored
        { circulation =
            { production: filter (\x -> keptActor x.actor && keptResource x.resource) circulation.production
            , flows: filter (\x -> keptActor x.from && keptActor x.to && keptResource x.resource) circulation.flows
            , shares: filter (\x -> keptActor x.from && keptActor x.to && maybe true keptResource x.source.resource) circulation.shares
            , requirements: filter (\x -> keptActor x.actor && keptResource x.resource) circulation.requirements
            }
        , reservable = filter keptResource authored.reservable
        , decays = filter (keptResource <<< _.resource) authored.decays
        }
    , explicitActors = Map.filterKeys keptActor old.explicitActors
    , schemes = map (\x -> x { eligibleActors = filter keptActor x.eligibleActors }) schemes
    }

  -- a StrategyRules naming something dropped cannot run here; a kept Actor that needs it fails the check
  resolves :: StrategyRules -> Boolean
  resolves = isRight <<< checkRefs "" (compositionNames gb)

-- | The Actors a recurring flow names.
flowParties :: RecurringFlow -> Array ActorId
flowParties r = source <> destination <> measured
  where
  source = case r.source of
    FromActor e -> [ e ]
    _ -> []
  destination = case r.destination of
    ToActor e -> [ e ]
    ToSink -> []
  measured = case r.amount of
    Measured { query } -> [ query.party ]
    _ -> []

-- | The Resources a recurring flow's amount reads.
flowResources :: RecurringFlow -> Array ResourceId
flowResources r = case r.amount of
  Measured { query } -> fromFoldable query.resource
  _ -> []

-- | The holding a recurring flow produces from.
producedFrom :: RecurringFlow -> Maybe ResourceId
producedFrom r = case r.amount of
  HeldAmount c -> Just c
  _ -> Nothing
