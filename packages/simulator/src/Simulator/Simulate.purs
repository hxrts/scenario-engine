-- | Simulation Specifications and the one function that runs them (docs/06_simulator.md
-- | §3–§6). Every experiment, counterfactual, and agency analysis composes `simulate`.
module Simulator.Simulate
  ( Context
  , SimulationSpec
  , SeatPolicy(..)
  , Override(..)
  , Run
  , resolve
  , simulate
  , resume
  , simulateWith
  , resumeWith
  , startOf
  , presetSeats
  , presetSpec
  , seats
  ) where

import Prelude

import Board.Gameboard.Schema (Gameboard, InitialWorld, InstantiationConfig)
import Board.Gameboard.Instantiate (checkHomes, instantiate)
import Core.Draw (Seed(..))
import Core.Engine.Commit (EngineError)
import Core.Num (Q)
import Core.Query (homeMarket)
import Core.Types (CapacityRef, EntityId)
import Core.World (Manifest, RoundResult, WorldState)
import Data.Array (any, filter, fromFoldable)
import Data.Foldable (foldM)
import Data.Traversable (traverse)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Effect (Effect)
import Policy.InWorld (InWorld, acts, inWorldOf)
import Effects.Round (runGame)
import Effects.Signatures (INTERPRET, NARRATE, RECORD, SEAT_INPUT)
import Handlers.Seats (SeatSource, collectRecords, handleSeats, strategies)
import Handlers.Sim (runSim)
import Run as R
import Type.Row (type (+))
import Core.Strategy (Strategy)

-- | What a run is resolved against: the Gameboard, its default configuration, the presets.
type Context = { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Strategy }

data SeatPolicy = Preset String | Custom Strategy

-- | Declarative overrides of a baseline (§3.1). Values change within the Gameboard's
-- | structure; none touches engine code.
data Override
  = GameSeed Int
  | Parameter String Q
  | Capacity EntityId CapacityRef Q
  | Shrinkage String Q
  | Implicit EntityId -- the Actor is not explicit; its fallback applies

type SimulationSpec =
  { seats :: Map EntityId SeatPolicy -- unlisted seats submit nothing
  , playerSeed :: Seed
  , overrides :: Array Override
  , rounds :: Maybe Int
  }

type Run = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }

-- | Apply a spec's overrides to the baseline Gameboard and configuration.
resolve :: Context -> SimulationSpec -> Either String { gameboard :: Gameboard, config :: InstantiationConfig }
resolve ctx spec = foldM apply { gameboard: ctx.gameboard, config: ctx.config } spec.overrides
  where
  -- an override naming something the baseline does not have is an error, not a silent no-op
  apply acc = case _ of
    GameSeed s -> Right acc { config = acc.config { seed = Seed s } }
    Parameter name q
      | Object.member name acc.gameboard.parameters -> Right acc { gameboard = acc.gameboard { parameters = Object.insert name q acc.gameboard.parameters } }
      | otherwise -> Left ("override: unknown parameter " <> name)
    Capacity e ref q
      | Map.member e acc.gameboard.world.entities -> Right acc { gameboard = acc.gameboard { world = acc.gameboard.world { entities = Map.update (\x -> Just x { capacities = Map.insert ref q x.capacities }) e acc.gameboard.world.entities } } }
      | otherwise -> Left ("override: unknown entity " <> show e)
    Shrinkage scheme q
      | any (\s -> s.id == scheme) acc.gameboard.schemes -> Right acc { gameboard = acc.gameboard { schemes = map (\s -> if s.id == scheme then s { estimator = s.estimator { shrinkage = q } } else s) acc.gameboard.schemes } }
      | otherwise -> Left ("override: unknown scheme " <> show scheme)
    Implicit e -> Right acc { config = acc.config { seats = Map.delete e acc.config.seats, npcs = Map.delete e acc.config.npcs, representations = filter (\r -> r.actor /= e) acc.config.representations } }

type Prepared =
  { manifest :: Manifest
  , world :: WorldState
  , game :: R.Run (SEAT_INPUT + INTERPRET + NARRATE + RECORD + ()) (Either EngineError WorldState)
  , seats :: SeatSource
  }

-- | A Gameboard under its configuration: the initial world and the in-world Policies. Fails when the
-- | world does not instantiate, or when a seat that acts has no home market (checked before any round).
startOf :: Gameboard -> InstantiationConfig -> Either String { initial :: InitialWorld, inWorld :: InWorld }
startOf gameboard config = case instantiate gameboard config of
  Left es -> Left (show es)
  Right initial -> do
    let inWorld = inWorldOf gameboard
    checkHomes (acts inWorld) initial.world
    pure { initial, inWorld }

-- | Instantiate a spec: the initial world, its game, and its seat source.
prepare :: Context -> SimulationSpec -> Maybe { world :: WorldState, done :: Int } -> Either String Prepared
prepare ctx spec from = do
  { gameboard, config } <- resolve ctx spec
  { initial, inWorld } <- startOf gameboard config
  -- a preset named by a seat must exist; a typo is an error, not a seat that silently does nothing
  bound <- traverse
    ( case _ of
        Preset name -> maybe (Left ("unknown preset " <> name)) Right (Object.lookup name ctx.presets)
        Custom p -> Right p
    )
    spec.seats
  let
    w0 = maybe initial.world _.world from
    remaining = fromMaybe config.rounds spec.rounds - maybe 0 _.done from
  pure
    { manifest: initial.manifest
    , world: w0
    , game: runGame inWorld remaining w0
    , seats: strategies { bound, params: gameboard.parameters, home: homeMarket initial.world, playerSeed: spec.playerSeed }
    }

-- | Play a spec to completion with records kept in memory. Pure.
simulate :: Context -> SimulationSpec -> Either String Run
simulate ctx spec = resume ctx spec Nothing

-- | Continue a run from a saved snapshot after `done` rounds (§8). Seat randomness is keyed by
-- | round, so a resumed run matches the uninterrupted one.
resume :: Context -> SimulationSpec -> Maybe { world :: WorldState, done :: Int } -> Either String Run
resume ctx spec from = prepare ctx spec from <#> \p ->
  let
    out = collectRecords (handleSeats p.seats p.game)
  in
    { manifest: p.manifest, playerSeed: spec.playerSeed, records: out.records, final: out.value }

-- | Play a spec under the Sim handler, persisting each round through `store` as it completes.
simulateWith :: Context -> SimulationSpec -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either String (Either EngineError WorldState))
simulateWith ctx spec store = resumeWith ctx spec Nothing store

-- | Continue a stored run from its snapshot after `done` rounds, under the Sim handler, persisting
-- | the new rounds through `store`. `store` runs only once the spec has instantiated, so a spec
-- | that is rejected leaves the caller's store untouched.
resumeWith :: Context -> SimulationSpec -> Maybe { world :: WorldState, done :: Int } -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either String (Either EngineError WorldState))
resumeWith ctx spec from store = case prepare ctx spec from of
  Left e -> pure (Left e)
  Right p -> do
    persist <- store p.manifest
    Right <$> runSim p.seats persist p.game

-- | Bind each seat to a preset by name.
presetSeats :: (EntityId -> String) -> Array EntityId -> Map EntityId SeatPolicy
presetSeats preset ids = Map.fromFoldable (map (\s -> Tuple s (Preset (preset s))) ids)

-- | The seats the context's configuration names.
seats :: Context -> Array EntityId
seats ctx = fromFoldable (Map.keys ctx.config.seats)

-- | A spec that binds every seat of the context to one preset.
presetSpec :: Context -> String -> Seed -> Array Override -> Maybe Int -> SimulationSpec
presetSpec ctx preset playerSeed overrides rounds = { seats: presetSeats (const preset) (seats ctx), playerSeed, overrides, rounds }
