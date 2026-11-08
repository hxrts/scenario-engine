-- | Filtering a premise: the one operation that takes a subset of a premise, for a
-- | cut-down game or a fixture. What belongs to something dropped goes with it: an account, a capability, a
-- | Market's access entry, a recurring flow, a Rule, an actor definition, a seat. What a kept thing needs
-- | is not supplied: a kept thing that refers to a dropped one fails `checkPremise`, the one reference
-- | check, so a subset cannot silently depend on what it left out.
module Game.Restrict
  ( Selection
  , everything
  , restrict
  ) where

import Prelude

import Board.Gameboard.Validate (gameboardNames, unknownNames)
import Core.Map (values)
import Core.Strategy (Strategy)
import Core.Types (Capability(..), EntityId, MarketId, RefKind(..), ResourceId, RuleF(..), TransformationId, idText)
import Core.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..))
import Data.Array (all, concat, elem, filter, fromFoldable, mapMaybe, nub)
import Data.Either (Either(..), isRight)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Game.Error (GameError(..), Stage(..))
import Game.Premise (GameboardParts, checkPremise, checkRefs)
import Schema.Gameboard (Gameboard)

-- | What to keep. `accounts` narrows an Entity's accounts further, to those Resources; an Entity it does
-- | not name keeps every account of a kept Resource.
type Selection =
  { entities :: Array EntityId
  , resources :: Array ResourceId
  , markets :: Array MarketId
  , transformations :: Array TransformationId
  , accounts :: Map EntityId (Array ResourceId)
  }

-- | The Selection that keeps a Gameboard whole.
everything :: Gameboard -> Selection
everything gb =
  { entities: fromFoldable (Map.keys gb.world.entities)
  , resources: fromFoldable (Map.keys gb.world.resources)
  , markets: fromFoldable (Map.keys gb.world.markets)
  , transformations: fromFoldable (Map.keys gb.world.transformations)
  , accounts: Map.empty
  }

-- | The premise cut down to the Selection, checked. A name the premise does not have is an error, and so
-- | is a kept thing that refers to a dropped one.
restrict :: Selection -> GameboardParts -> Either GameError GameboardParts
restrict s p = case unknownNames (gameboardNames p.gameboard) named of
  [] -> checkPremise (filtered s p)
  es -> Left (Invalid AtLoad es)
  where
  named = concat
    [ map (Tuple EntityRef <<< idText) (s.entities <> fromFoldable (Map.keys s.accounts))
    , map (Tuple ResourceRef <<< idText) s.resources
    , map (Tuple Market <<< idText) s.markets
    , map (Tuple TransformationRef <<< idText) s.transformations
    ]

filtered :: Selection -> GameboardParts -> GameboardParts
filtered s p = p
  { gameboard = gb { policies = Map.filter resolves gb.policies }
  , config = p.config
      { seats = Map.filterKeys keptEntity p.config.seats
      , npcs = Map.filterKeys keptEntity p.config.npcs
      , fallbacks = Map.filterKeys keptEntity p.config.fallbacks
      , representations = filter (\r -> keptEntity r.actor && r.scheme `elem` map _.id schemes) p.config.representations
      }
  , presets = Object.filter resolves p.presets
  }
  where
  keptEntity e = e `elem` s.entities
  keptResource r = r `elem` s.resources
  old = p.gameboard
  authored = old.authored
  circulation = authored.circulation
  transformations = Map.filterKeys (_ `elem` s.transformations) old.world.transformations
  recurring = filter keptFlow old.world.recurring
  keptFlow r = keptResource r.resource && all keptEntity (flowParties r) && all keptResource (flowResources r)
  -- a capacity is kept while a kept Transformation or production still draws on it
  drawn = nub (mapMaybe _.capacity (values transformations) <> foldMap (map _.capacity <<< _.effects) (values transformations) <> mapMaybe producedFrom recurring)
  keptAccount e r = keptResource r && maybe true (elem r) (Map.lookup e s.accounts)
  keptCapability = case _ of
    MayTrade m -> m `elem` s.markets
    MayTransfer r -> keptResource r
    MayTransform t -> t `elem` s.transformations
    MayEnact _ -> true
  entityOf e = e
    { accounts = Map.filterKeys (keptAccount e.id) e.accounts
    , capacities = Map.filterKeys (_ `elem` drawn) e.capacities
    , capabilities = filter keptCapability e.capabilities
    }
  keptRule = case _ of
    FlowRule x -> keptEntity x.scope.governor && keptEntity x.scope.counterparty && all keptResource x.scope.over
    CapacityRule x -> x.capacity `elem` drawn
  schemes = filter (\x -> keptEntity x.target && keptResource x.observes) old.schemes
  gb = old
    { world = old.world
        { entities = map entityOf (Map.filterKeys keptEntity old.world.entities)
        , resources = Map.filterKeys keptResource old.world.resources
        , markets = map (\m -> m { access = filter keptEntity m.access }) (Map.filterKeys (_ `elem` s.markets) old.world.markets)
        , transformations = transformations
        , representations = filter (\r -> keptEntity r.actor && keptEntity r.aggregate) old.world.representations
        , flowGates = filter (keptResource <<< _.resource) old.world.flowGates
        , groups = Map.filterWithKey (\k v -> keptEntity k && keptEntity v) old.world.groups
        , rules = Map.filter keptRule old.world.rules
        , recurring = recurring
        }
    , authored = authored
        { circulation =
            { production: filter (\x -> keptEntity x.entity && keptResource x.resource) circulation.production
            , flows: filter (\x -> keptEntity x.from && keptEntity x.to && keptResource x.resource) circulation.flows
            , shares: filter (\x -> keptEntity x.from && keptEntity x.to && maybe true keptResource x.source.resource) circulation.shares
            , requirements: filter (\x -> keptEntity x.entity && keptResource x.resource) circulation.requirements
            }
        , reservable = filter keptResource authored.reservable
        , decays = filter (keptResource <<< _.resource) authored.decays
        }
    , actors = Map.filterKeys keptEntity old.actors
    , schemes = map (\x -> x { eligibleActors = filter keptEntity x.eligibleActors }) schemes
    }

  -- a Strategy naming something dropped cannot run here; a kept Entity that needs it fails the check
  resolves :: Strategy -> Boolean
  resolves = isRight <<< checkRefs "" (gameboardNames gb)

-- | The Entities a recurring flow names.
flowParties :: RecurringFlow -> Array EntityId
flowParties r = source <> destination <> measured
  where
  source = case r.source of
    FromEntity e -> [ e ]
    _ -> []
  destination = case r.destination of
    ToEntity e -> [ e ]
    ToSink -> []
  measured = case r.amount of
    Measured { query } -> [ query.party ]
    _ -> []

-- | The Resources a recurring flow's amount reads.
flowResources :: RecurringFlow -> Array ResourceId
flowResources r = case r.amount of
  Measured { query } -> fromFoldable query.resource
  _ -> []

-- | The capacity a recurring flow produces from.
producedFrom :: RecurringFlow -> Maybe _
producedFrom r = case r.amount of
  CapacityHeld c -> Just c
  _ -> Nothing
