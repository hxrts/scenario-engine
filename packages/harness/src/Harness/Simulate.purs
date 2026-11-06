-- | Simulation Specifications and the one function that runs them (docs/06_simulator.md
-- | §3–§6). Every experiment, counterfactual, and agency analysis composes `simulate`.
module Harness.Simulate
  ( Context
  , SimulationSpec
  , SeatPolicy(..)
  , Override(..)
  , Run
  , resolve
  , simulate
  , resume
  , simulateWith
  , seatsOf
  ) where

import Prelude

import Board.Gameboard (Gameboard, InstantiationConfig, instantiate)
import Core.Draw (Seed(..))
import Core.Engine (EngineError)
import Core.Num (Q)
import Core.Types (CapacityRef, Controller(..), EntityId, EntityKind(..))
import Core.World (Manifest, RoundResult, WorldState)
import Data.Array (concatMap, filter, foldl, fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Effect (Effect)
import Effects (RECORD, SEAT_INPUT, homeMarket, inWorldOf, runGame)
import Handlers.Seats (SeatSource, collectRecords, handleSeats, programs)
import Handlers.Sim (runSim)
import Run as R
import Type.Row (type (+))
import Policy.Program (Program)

-- | What a run is resolved against: the Gameboard, its default configuration, the presets.
type Context = { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Program }

data SeatPolicy = Preset String | Custom Program

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
resolve :: Context -> SimulationSpec -> { gameboard :: Gameboard, config :: InstantiationConfig }
resolve ctx spec = foldl apply { gameboard: ctx.gameboard, config: ctx.config } spec.overrides
  where
  apply acc = case _ of
    GameSeed s -> acc { config = acc.config { seed = Seed s } }
    Parameter name q -> acc { gameboard = acc.gameboard { parameters = Object.insert name q acc.gameboard.parameters } }
    Capacity e ref q -> acc { gameboard = acc.gameboard { world = acc.gameboard.world { entities = Map.update (\x -> Just x { capacities = Map.insert ref q x.capacities }) e acc.gameboard.world.entities } } }
    Shrinkage scheme q -> acc { gameboard = acc.gameboard { schemes = map (\s -> if s.id == scheme then s { estimator = s.estimator { shrinkage = q } } else s) acc.gameboard.schemes } }
    Implicit e -> acc { config = acc.config { seats = Map.delete e acc.config.seats, npcs = Map.delete e acc.config.npcs, representations = filter (\r -> r.actor /= e) acc.config.representations } }

type Prepared =
  { manifest :: Manifest
  , world :: WorldState
  , game :: R.Run (SEAT_INPUT + RECORD + ()) (Either EngineError WorldState)
  , seats :: SeatSource
  }

-- | Instantiate a spec: the initial world, its game, and its seat source.
prepare :: Context -> SimulationSpec -> Maybe { world :: WorldState, done :: Int } -> Either String Prepared
prepare ctx spec from =
  let
    { gameboard, config } = resolve ctx spec
  in
    case instantiate gameboard config, inWorldOf gameboard of
      Left es, _ -> Left (show es)
      _, Left e -> Left (show e)
      Right initial, Right inWorld ->
        let
          w0 = maybe initial.world _.world from
          remaining = fromMaybe config.rounds spec.rounds - maybe 0 _.done from
          bound = Map.mapMaybe
            ( case _ of
                Preset name -> Object.lookup name ctx.presets
                Custom p -> Just p
            )
            spec.seats
        in
          Right
            { manifest: initial.manifest
            , world: w0
            , game: runGame inWorld remaining w0
            , seats: programs { bound, params: gameboard.parameters, home: homeMarket initial.world, playerSeed: spec.playerSeed }
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
simulateWith ctx spec store = case prepare ctx spec Nothing of
  Left e -> pure (Left e)
  Right p -> do
    persist <- store p.manifest
    Right <$> runSim p.seats persist p.game

-- | The seats of an instantiated world.
seatsOf :: WorldState -> Array EntityId
seatsOf w = concatMap
  ( \e -> case e.kind of
      Actor (Seat _) -> [ e.id ]
      _ -> []
  )
  (fromFoldable (Map.values w.entities))
