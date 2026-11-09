-- | Starting a run and playing it to completion, as a program over the Run Store and system rows. The server and the
-- | CLI both launch through it, each under its own role.
module Effects.Launch
  ( LAUNCHES
  , Launched
  , Start
  , start
  , freshRunId
  , nextRunId
  , runExperiment
  ) where

import Prelude

import Control.Alt ((<|>))
import Core.Draw (Seed(..))
import Data.Array (any, concatMap, foldl, mapMaybe, null, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldM, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Effects.Observe (listSets, listSummaries)
import Effects.Play (playRun)
import Effects.Signatures (CLOCK, ENTROPY, OBSERVE, PLAY, PROCESS, PlayRequest(..), STORE)
import Effects.Store (checkRunId, createSet)
import Effects.System (now, ownerOf, resolveSeed)
import Assembly.Assemble (assemblePreset, checkRounds)
import Assembly.Experiment (ExperimentRequest, variants)
import Assembly.Override (Override)
import Assembly.Recipe (Derivation, encodeSpec)
import Assembly.Premise (PossibleWorldParts)
import Run (Run)
import Schema.PossibleWorld (gameIdOf)
import Type.Row (type (+))

type LAUNCHES r = OBSERVE + PLAY + CLOCK + ENTROPY + PROCESS + r

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

-- | A run to start: its name, what it is played from, and, when it is derived, its source and kind.
type Start =
  { name :: String
  , preset :: String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , overrides :: Array Override
  , derivation :: Maybe Derivation
  }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the game assembles. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: forall r. String -> PossibleWorldParts -> Start -> Run (LAUNCHES r) (Either String Launched)
start role p l = freshRunId >>= case _ of
  Left e -> pure (Left e)
  Right id -> startAs role id p l

-- | A freshly minted run id, or why it may not name a run.
freshRunId :: forall r. Run (LAUNCHES r) (Either String String)
freshRunId = checkRunId <$> nextRunId

startAs :: forall r. String -> String -> PossibleWorldParts -> Start -> Run (LAUNCHES r) (Either String Launched)
startAs role id p l = do
  seed <- resolveSeed (Seed <$> l.playerSeed)
  case assemblePreset p l.preset l.overrides l.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      let
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf p.possibleWorld), playerSeed: seed, preset: Just l.preset, rounds: Just game.rounds, overrides: l.overrides, derivation: l.derivation, model: Nothing, humans: Nothing }
      created <- now
      owner <- ownerOf role
      attempt <- playRun (Simulated { seed, game, run: { id, name: l.name, created, spec, manifest: game.manifest, owner } })
      pure (attempt $> { id, playerSeed: unwrap seed })

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: forall r. Run (LAUNCHES r) String
nextRunId = do
  rows <- listSummaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure r.source) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | Run every game an experiment generates and save the runs as one named Run set, the first as its baseline. Every
-- | run uses the one player seed, drawn once if none was given. The first game that does not start stops the
-- | experiment; the runs before it stay, as any launched run does.
runExperiment :: forall r. String -> PossibleWorldParts -> ExperimentRequest -> Run (LAUNCHES + STORE + r) (Either String { set :: String, runs :: Array String })
runExperiment role p req = do
  existing <- listSets
  case unit of
    _ | any (\s -> s.name == req.name) existing -> pure (Left ("a scenario set named " <> req.name <> " already exists"))
    _ | null generated -> pure (Left "the experiment generates no runs")
    _ | Left e <- traverse_ (checkRounds p.possibleWorld) req.rounds -> pure (Left (show e))
    _ -> do
      seed <- resolveSeed (Seed <$> req.playerSeed)
      foldM (step seed) (Right []) generated >>= case _ of
        Left e -> pure (Left e)
        Right runs -> case uncons runs of
          Nothing -> pure (Left "the experiment generates no runs")
          Just { head } -> do
            createSet { name: req.name, baseline: head, runs }
            pure (Right { set: req.name, runs })
  where
  generated = variants req.experiment
  step _ (Left e) _ = pure (Left e)
  step seed (Right done) v =
    start role p
      { name: req.name <> " (" <> v.label <> ")"
      , preset: req.preset
      , playerSeed: v.playerSeed <|> Just (unwrap seed)
      , rounds: req.rounds
      , overrides: v.overrides
      , derivation: Nothing
      } <#> map (\l -> done <> [ l.id ])
