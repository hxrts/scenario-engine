-- | Filtering a setting: the one operation that takes a subset of a setting, for a
-- | cut-down game or a fixture. What belongs to something dropped goes with it: an account, a capability, a
-- | Market's access entry, a recurring flow, a Rule, an actor definition, a seat. What a kept thing needs
-- | is not supplied: a kept thing that refers to a dropped one fails `checkSetting`, the one reference
-- | check, so a subset cannot silently depend on what it left out.
module Game.Restrict
  ( Selection
  , everything
  , restrict
  ) where

import Prelude

import Board.Gameboard.Validate (gameboardNames, unknownNames)
import Core.Map (values)
import Laws.Strategy (Strategy)
import Laws.Types (holderDraw, Capability(..), EntityId, MarketId, RefKind(..), ResourceId, RuleF(..), TransformationId, idText)
import Laws.World (RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..))
import Data.Array (all, concat, concatMap, elem, filter, fromFoldable, mapMaybe, nub)
import Data.Either (Either(..), isRight)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Game.Error (GameError(..))
import Game.Setting (GameboardParts, checkSetting, checkRefs)
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

-- | The setting cut down to the Selection, checked. A name the setting does not have is an error, and so
-- | is a kept thing that refers to a dropped one.
restrict :: Selection -> GameboardParts -> Either GameError GameboardParts
restrict s p = case unknownNames (gameboardNames p.gameboard) named of
  [] -> checkSetting (filtered s p)
  es -> Left (Invalid es)
  where
  named = concat
    [ map (Tuple RefEntity <<< idText) (s.entities <> fromFoldable (Map.keys s.accounts))
    , map (Tuple RefResource <<< idText) s.resources
    , map (Tuple RefMarket <<< idText) s.markets
    , map (Tuple RefTransformation <<< idText) s.transformations
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
  selected r = r `elem` s.resources
  old = p.gameboard
  authored = old.authored
  circulation = authored.circulation
  transformations = Map.filterKeys (_ `elem` s.transformations) old.world.transformations
  recurring = filter keptFlow old.world.recurring
  keptFlow r = selected r.resource && all keptEntity (flowParties r) && all selected (flowResources r)
  -- what a kept Transformation or a production still draws on or changes is kept, whatever the Selection says
  keptResource r = selected r || r `elem` drawn
  -- a gate goes with its Resource, and with the Transformation that carries it
  keptGate g = keptResource g.resource && maybe true (_ `elem` s.transformations) g.bundled
  drawn = nub (concatMap (\t -> map _.resource (holderDrawOf t) <> map _.resource t.effects) (values transformations) <> mapMaybe producedFrom recurring)
  holderDrawOf = fromFoldable <<< holderDraw
  keptAccount e r = r `elem` drawn || selected r && maybe true (elem r) (Map.lookup e s.accounts)
  keptCapability = case _ of
    MayTrade m -> m `elem` s.markets
    MayTransfer r -> keptResource r
    MayTransform t -> t `elem` s.transformations
    MayEnact _ -> true
  entityOf e = e
    { accounts = Map.filterKeys (keptAccount e.id) e.accounts
    , capabilities = filter keptCapability e.capabilities
    }
  keptRule = case _ of
    FlowRule x -> keptEntity x.scope.governor && keptEntity x.scope.counterparty && all keptResource x.scope.over
    HoldingRule x -> x.resource `elem` drawn
  schemes = filter (\x -> keptEntity x.target && keptResource x.observes) old.schemes
  gb = old
    { world = old.world
        { entities = map entityOf (Map.filterKeys keptEntity old.world.entities)
        , resources = Map.filterKeys keptResource old.world.resources
        , markets = map (\m -> m { access = filter keptEntity m.access }) (Map.filterKeys (_ `elem` s.markets) old.world.markets)
        , transformations = transformations
        , representations = filter (\r -> keptEntity r.actor && keptEntity r.aggregate) old.world.representations
        , flowGates = filter keptGate old.world.flowGates
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

-- | The holding a recurring flow produces from.
producedFrom :: RecurringFlow -> Maybe ResourceId
producedFrom r = case r.amount of
  HeldAmount c -> Just c
  _ -> Nothing
