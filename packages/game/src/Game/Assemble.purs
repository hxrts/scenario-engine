-- | Assembly: a premise, the seat bindings, and the overrides become one `Game` (work/4.md §8.1). This is
-- | the only place a Gameboard is instantiated and its seats are bound; every runner (simulator, live
-- | play, the server's derive and launch) assembles through `assemble`.
module Game.Assemble
  ( assemble
  , checkHomes
  , configuredSeats
  , presetBindings
  , liveBindings
  , assemblePreset
  , withRounds
  ) where

import Prelude

import Board.Gameboard.Instantiate (instantiate)
import Core.Draw (drawVersion)
import Core.Query (homeMarket)
import Core.Strategy (InWorld, Strategy)
import Core.Types (Entity, EntityId)
import Data.Array (elem, find, fromFoldable, null)
import Core.World (WorldState)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Error (GameError(..))
import Game.Override (Override, resolve)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Bindings, Game, SeatBinding(..))
import Policy.InWorld (acts, inWorldOf)
import Schema.Gameboard (Gameboard, InitialWorld, InstantiationConfig)

-- | A game from a premise, its seat bindings, and the overrides applied to it. Fails when an override
-- | names something the baseline lacks, when the world does not instantiate, or when a seat that acts
-- | has no home market (checked before any round).
assemble :: Premise -> Bindings -> Array Override -> Either GameError Game
assemble premise bindings overrides = case premise of
  GameboardPremise p -> do
    resolved <- resolve { gameboard: p.gameboard, config: p.config } overrides
    { initial, inWorld } <- startOf resolved.gameboard resolved.config
    pure
      { manifest: initial.manifest
      , world: initial.world
      , inWorld
      , rounds: resolved.config.rounds
      , parameters: resolved.gameboard.parameters
      , seats: bindings
      }
  LiteralPremise p
    | not (null overrides) -> Left LiteralOverrides
    | otherwise ->
        pure
          { manifest: { gameboard: "literal", engine: "sp-engine/v1", draw: drawVersion, initial: p.world }
          , world: p.world
          , inWorld: { strategies: Map.empty, params: Object.empty } :: InWorld
          , rounds: p.rounds
          , parameters: Object.empty
          , seats: bindings
          }

-- | A Gameboard under its configuration: the initial world and the in-world Policies. Fails when the
-- | world does not instantiate, or when a seat that acts has no home market.
startOf :: Gameboard -> InstantiationConfig -> Either GameError { initial :: InitialWorld, inWorld :: InWorld }
startOf gameboard config = case instantiate gameboard config of
  Left es -> Left (Instantiation es)
  Right initial -> do
    let inWorld = inWorldOf gameboard
    lmap NoHomeMarket (checkHomes (acts inWorld) initial.world)
    pure { initial, inWorld }

-- | Every Entity that acts (`acts`: a seat, or an Entity whose Policy runs) needs a home market, because
-- | `$home` names it. An acting Entity with none is a startup error, named here, before any round.
checkHomes :: (Entity -> Boolean) -> WorldState -> Either EntityId Unit
checkHomes actsFn w = case find (\e -> actsFn e && isNothing (homeMarket w e.id)) (fromFoldable (Map.values w.entities)) of
  Just e -> Left e.id
  Nothing -> Right unit

-- | The seats the configuration names, in order.
configuredSeats :: InstantiationConfig -> Array EntityId
configuredSeats config = fromFoldable (Map.keys config.seats)

-- | Every listed seat bound to the preset its name picks. A preset that does not exist is an error, not
-- | a seat that silently submits nothing.
presetBindings :: Object Strategy -> (EntityId -> String) -> Array EntityId -> Either GameError Bindings
presetBindings presets preset ids = Map.fromFoldable <$> traverse bind ids
  where
  bind e = (\s -> Tuple e (Strategy s)) <$> lookup (preset e)
  lookup name = maybe (Left (UnknownPreset name)) Right (Object.lookup name presets)

-- | The game a Gameboard premise plays with every configured seat bound to one preset, under overrides,
-- | with the round count given (or the configuration's when Nothing). An unknown preset is an error.
assemblePreset :: GameboardParts -> String -> Array Override -> Maybe Int -> Either GameError Game
assemblePreset p preset overrides rounds = do
  bindings <- presetBindings p.presets (const preset) (configuredSeats p.config)
  game <- assemble (GameboardPremise p) bindings overrides
  pure (withRounds rounds game)

-- | The game with its round count replaced, when one is given (a launch's or a variant's `rounds`).
withRounds :: Maybe Int -> Game -> Game
withRounds = maybe identity (\n g -> g { rounds = n })

-- | The seat bindings of a live session: each listed human is a Human seat, and every other configured
-- | seat is an Agent.
liveBindings :: Array EntityId -> InstantiationConfig -> Bindings
liveBindings humans config = Map.fromFoldable (map (\e -> Tuple e (if elem e humans then Human else Agent)) (configuredSeats config))
