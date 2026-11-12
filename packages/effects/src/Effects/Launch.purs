-- | Starting a run and playing it to completion, as a program over the Run Store and system rows. The server and the
-- | CLI both launch through it, each under its own role.
module Effects.Launch
  ( LAUNCHES
  , Launched
  , Start
  , start
  , freshRunId
  , nextRunId
  ) where

import Prelude

import Core.Draw (Seed(..))
import Data.Array (foldl, mapMaybe)
import Data.Either (Either(..))

import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Effects.Observe (listSummaries)
import Effects.Play (playRun)
import Effects.Signatures (CLOCK, ENTROPY, OBSERVE, PLAY, PROCESS, PlayRequest(..))
import Effects.Store (checkRunId)
import Effects.System (now, ownerOf, resolveSeed)
import Assembly.Assemble (assembleCast)
import Assembly.Override (Override)
import Assembly.Recipe (encodeSpec)
import Schema.Cast (Cast)
import Schema.Run as RunClass
import Assembly.Composition (CompositionParts)
import Run (Run)
import Schema.Composition (compositionRef, gameIdOf, premisesText)
import Type.Row (type (+))

type LAUNCHES r = OBSERVE + PLAY + CLOCK + ENTROPY + PROCESS + r

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

-- | A simulated run to start: its name, its cast (and the preset it names every seat on, when it is one preset's run),
-- | its player seed (Nothing draws one), round count (Nothing is the configuration's), overrides, and the suite's
-- | cache key it is stored under.
type Start =
  { name :: String
  , cast :: Cast
  , preset :: Maybe String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , overrides :: Array Override
  , cacheKey :: Maybe String
  }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the game assembles. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: forall r. String -> CompositionParts -> Start -> Run (LAUNCHES r) (Either String Launched)
start role p l = freshRunId >>= case _ of
  Left e -> pure (Left e)
  Right id -> startAs role id p l

-- | A freshly minted run id, or why it may not name a run.
freshRunId :: forall r. Run (LAUNCHES r) (Either String String)
freshRunId = checkRunId <$> nextRunId

startAs :: forall r. String -> String -> CompositionParts -> Start -> Run (LAUNCHES r) (Either String Launched)
startAs role id p l = do
  seed <- resolveSeed (Seed <$> l.playerSeed)
  case assembleCast p l.cast l.overrides l.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      let
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf p.composition), composition: compositionRef p.composition, premises: premisesText p.composition, playerSeed: seed, preset: l.preset, cast: l.cast, rounds: Just game.rounds, overrides: l.overrides, model: Nothing, humans: Nothing, runClass: RunClass.Simulated, cacheKey: l.cacheKey, tag: Nothing, metadata: Nothing }
      created <- now
      owner <- ownerOf role
      attempt <- playRun (Simulated { seed, game, run: { id, name: l.name, created, spec, manifest: game.manifest, owner } })
      pure (attempt $> { id, playerSeed: unwrap seed })

-- | The next run id: one past the highest stored.
nextRunId :: forall r. Run (LAUNCHES r) String
nextRunId = do
  rows <- listSummaries
  let
    cited = map _.id rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))
