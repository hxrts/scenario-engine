-- | The Run Store adapter (docs/06_simulator.md §8): the manifest written before Round 1,
-- | then each round's record, snapshot, assertions, and versioned metrics saved atomically.
-- | Runs are append-only; reads go through `RunStoreRead`.
module Adapters.Store
  ( Db
  , RunInfo
  , StoredRound
  , RunStoreRead
  , open
  , createRun
  , nowIso
  , persister
  , setStatus
  , finish
  , reader
  , metricsVersion
  ) where

import Prelude

import Core.Engine (prepareRound)
import Core.Invariants (check)
import Core.Market (marginal)
import Core.World (Manifest, RoundResult, WorldState)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn6, Fn7, runFn1, runFn2, runFn3, runFn6, runFn7)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Ref as Ref

foreign import data Db :: Type

type RunInfo = { id :: String, name :: String, status :: String, created :: String }

type StoredRound = { round :: Int, record :: String, snapshot :: String, assertions :: String }

type Metric = { metric :: String, version :: String, value :: String }

foreign import openImpl :: String -> Effect Db
foreign import createRunImpl :: Fn6 Db String String String String String (Effect Unit)
foreign import appendRoundImpl :: Fn7 Db String Int String String String (Array Metric) (Effect Unit)
foreign import setStatusImpl :: Fn3 Db String String (Effect Unit)
foreign import listRunsImpl :: Fn1 Db (Effect (Array RunInfo))
foreign import manifestImpl :: Fn2 Db String (Effect (Nullable { spec :: String, manifest :: String }))
foreign import roundsImpl :: Fn2 Db String (Effect (Array StoredRound))
foreign import metricImpl :: Fn3 Db String String (Effect (Array { round :: Int, value :: String }))

open :: String -> Effect Db
open = openImpl

-- | The version of the metric definitions; recomputing a projection never edits records.
metricsVersion :: String
metricsVersion = "v1"

-- | Register a run and its manifest before Round 1.
foreign import nowIsoImpl :: Effect String

-- | The current time, for a run's `created` field.
nowIso :: Effect String
nowIso = nowIsoImpl

createRun :: Db -> { id :: String, name :: String, created :: String, spec :: Json, manifest :: Manifest } -> Effect Unit
createRun db r = runFn6 createRunImpl db r.id r.name (stringify r.spec) (stringify (encodeJson r.manifest)) r.created

-- | A RECORD target for the Sim handler: saves each round as it completes.
persister :: Db -> String -> WorldState -> Effect (RoundResult -> Effect Unit)
persister db id initial = do
  previous <- Ref.new initial
  pure \r -> do
    before <- Ref.read previous
    runFn7 appendRoundImpl db id r.record.round
      (stringify (encodeJson r.record))
      (stringify (encodeJson r.world))
      (stringify (encodeJson (check (prepareRound before) r)))
      (metrics r)
    Ref.write r.world previous

-- | Versioned projections: each Market's price after the round.
metrics :: RoundResult -> Array Metric
metrics r = map (\(Tuple id m) -> { metric: "price:" <> show id, version: metricsVersion, value: show (marginal m.cost m.position) })
  (Map.toUnfoldable r.world.markets)

-- | Mark a finished run completed, or failed if it ended in an error.
finish :: forall e a. Db -> String -> Either e a -> Effect Unit
finish db id out = setStatus db id (either (const "failed") (const "completed") out)

setStatus :: Db -> String -> String -> Effect Unit
setStatus db id status = runFn3 setStatusImpl db id status

type RunStoreRead =
  { runs :: Effect (Array RunInfo)
  , manifest :: String -> Effect (Maybe { spec :: String, manifest :: String })
  , rounds :: String -> Effect (Array StoredRound)
  , metric :: String -> String -> Effect (Array { round :: Int, value :: String })
  }

reader :: Db -> RunStoreRead
reader db =
  { runs: runFn1 listRunsImpl db
  , manifest: \id -> toMaybe <$> runFn2 manifestImpl db id
  , rounds: runFn2 roundsImpl db
  , metric: runFn3 metricImpl db
  }
