-- | The run operations on a stored run (docs/06_simulator.md §9.1, §9.2): derive (duplicate, rerun,
-- | variant, counterfactual), resume, archive and unarchive, and delete. Each validates its request
-- | and persists the result; the dashboard only sends the request and shows the reply.
module Server.Derive
  ( deriveRun
  , resumeRun
  , setArchive
  , deleteOne
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.World (WorldState)
import Data.Array (last, length, null)
import Data.Either (Either(..), either, note)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effects.Api (DeriveRequest, Reply)
import Effects.Signatures (RunError(..), Stored, StoredRound)
import Effects.Observe (readManifest, readRounds)
import Effects.Store (deleteRun, setArchived, setSpec, setStatus)
import Views.Metrics (roundMetrics)
import Handlers.Store (StoreBackend, recorder, runObserve, runStore)
import Server.Errors (describe, failWith, ok)
import Server.Launch (play, start)
import Server.Cache (Cache)
import Server.Read (loadRun, runInfo)
import Game.Assemble (assemblePreset, checkRounds)
import Game.Premise (GameboardParts)
import Schema.Run (Spec, DeriveKind, deriveKindShape, DeriveKind(..), RunStatus(..), deriveKindName, runStatusName)
import Core.Shape (Format(Premise), Shape, boolShape, intShape, optionalOf, recordShape, stringShape)
import Handlers.Api.Route as Route
import Game.Recipe (Recipe, decodeSpec, deriveRecipe)
import Simulator.Records (recordsEqual)
import Views.Common (records)
import Simulator.Simulate (resumeWith)

-- | `POST /api/runs/:id/derive`. The body's kind decides what may change:
--   duplicate, rerun: the source's spec unchanged, under a new name (rerun is the claim that the
--     records reproduce, so it takes no change at all);
--   variant: the source's preset, player seed, or rounds replaced (a null player seed is drawn);
--   counterfactual: the source's overrides plus the given ones.
-- Absent fields keep the source's values. Every derived run records its source and kind.
-- A rerun also reports `recordsMatch`: whether its round records equal the source's. The source's
-- records must be stored and readable, and the source completed, before the rerun starts; otherwise
-- the reply is 409 and nothing is created.
deriveRun :: Cache -> GameboardParts -> StoreBackend -> String -> DeriveRequest -> Effect Reply
deriveRun cache premise store sourceId body = do
  let kind = body.kind
  runInfo store sourceId >>= case _ of
    Nothing -> failWith 404 ("no run " <> sourceId)
    Just info -> do
      -- a rerun is judged against the source's stored records, so they are read before it starts
      source <- if kind == Rerun then Just <$> loadRun cache store sourceId else pure Nothing
      case source of
        Just (Left Missing) -> failWith 404 ("no run " <> sourceId)
        Just (Left (Unreadable why)) -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": " <> why)
        Just (Right src)
          | info.status /= Completed -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it is " <> runStatusName info.status <> ", not completed")
          | null (records src) -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it has no stored round records")
        _ -> do
          stored <- runObserve store (readManifest sourceId)
          case stored <#> \m -> m.spec >>= decodeSpec of
            Nothing -> failWith 500 ("run " <> sourceId <> " has no stored spec")
            Just (Left e) -> failWith 500 ("run " <> sourceId <> " is unreadable: " <> e)
            Just (Right base) -> case deriveRecipe kind body base of
              Left e -> failWith 400 e
              Right _ | kind == Variant, Just bad <- outOfRange -> failWith 400 bad
              Right recipe -> do
                let name = fromMaybe (info.name <> " (" <> deriveKindName kind <> ")") body.name
                started <- start premise store
                  { name
                  , preset: recipe.preset
                  , playerSeed: recipe.playerSeed
                  , rounds: recipe.rounds
                  , overrides: recipe.overrides
                  , derivation: Just { source: sourceId, kind }
                  }
                case started, source of
                  Left e, _ -> failWith 400 e
                  Right l, Just (Right src) -> do
                    -- the new run's records are read back from the store, as the source's are
                    new <- loadRun cache store l.id
                    case new of
                      Right n -> replied l.id (Just (recordsEqual (records src) (records n)))
                      Left e -> failWith 500 ("the rerun " <> l.id <> " is unreadable: " <> describe e)
                  Right l, _ -> replied l.id Nothing
  where
  -- a variant is a new game, so a round count it asks for obeys the Gameboard's range; a duplicate or a
  -- rerun replays what was recorded
  outOfRange = either (Just <<< show) (const Nothing) (traverse_ (checkRounds premise.gameboard) body.rounds)
  -- the reply to a derive: the new run, its source and kind, and anything the kind adds
  replied id recordsMatch = pure (Route.replyIn Premise derivedShape 201 { id, source: sourceId, kind: body.kind, recordsMatch })

-- | The reply to a derive; `recordsMatch` only for a rerun. The premise format leaves it out otherwise.
derivedShape :: Shape { id :: String, source :: String, kind :: DeriveKind, recordsMatch :: Maybe Boolean }
derivedShape = recordShape "Derived" { id: stringShape, source: stringShape, kind: deriveKindShape, recordsMatch: optionalOf boolShape }

-- | `POST /api/runs/:id/resume` with `{ "rounds": n }`: a completed run is extended to n rounds,
-- | starting from the snapshot after its last completed round. The run keeps its id.
resumeRun :: GameboardParts -> StoreBackend -> String -> Int -> Effect Reply
resumeRun premise store id rounds =
  runInfo store id >>= case _ of
    Nothing -> failWith 404 ("no run " <> id)
    Just info
      | info.status /= Completed -> failWith 400 ("only a completed run can be resumed; " <> id <> " is " <> runStatusName info.status)
      | otherwise -> do
          stored <- runObserve store (readManifest id)
          rows <- runObserve store (readRounds id)
          case stored, last rows of
            Just m, Just latest -> case prepare m.spec latest (length rows) of
              Left e -> failWith 500 ("run " <> id <> " is unreadable: " <> e)
              Right p
                | rounds <= p.done -> failWith 400 ("rounds must exceed the " <> show p.done <> " completed rounds of " <> id)
                | otherwise -> do
                    case assemblePreset premise p.base.preset p.base.overrides (Just rounds) of
                      Left e -> failWith 400 (show e)
                      Right game -> do
                        attempt <- play store id $ resumeWith p.seed game { world: p.snapshot, done: p.done } \_ -> do
                          runStore store (setStatus id Running Nothing)
                          runStore store (setSpec id p.spec { rounds = Just rounds })
                          recorder store roundMetrics id p.snapshot
                        case attempt of
                          Left e -> failWith 400 e
                          Right r -> either (\e -> failWith 500 ("run " <> id <> " failed: " <> e)) (const (ok resumedShape { id, rounds })) r
            _, _ -> failWith 500 ("run " <> id <> " is unreadable: it has no manifest or no rounds")
  where
  -- what resuming needs: the spec, its recipe and seed, the last snapshot, and the completed round count
  prepare :: Stored Spec -> StoredRound -> Int -> Either String { spec :: Spec, base :: Recipe, seed :: Seed, snapshot :: WorldState, done :: Int }
  prepare text latest done = do
    spec <- text
    base <- decodeSpec spec
    seed <- note "the stored spec has no player seed" base.playerSeed
    snapshot <- latest.snapshot
    pure { spec, base, seed: Seed seed, snapshot, done }

-- | `POST /api/runs/:id/archive` and `/unarchive`. Idempotent.
setArchive :: StoreBackend -> String -> Boolean -> Effect Reply
setArchive store id archived = do
  present <- hasRun store id
  if not present then failWith 404 ("no run " <> id)
  else do
    runStore store (setArchived id archived)
    ok archivedShape { id, archived }

-- | `DELETE /api/runs/:id?confirm=:id`: the run with its rounds and metrics. Runs that cite it keep
-- | their source field, which then resolves to deleted.
deleteOne :: StoreBackend -> String -> String -> Effect Reply
deleteOne store id confirm
  | confirm /= id = failWith 400 ("deleting " <> id <> " needs ?confirm=" <> id)
  | otherwise = do
      n <- runStore store (deleteRun id)
      if n == 0 then failWith 404 ("no run " <> id) else ok deletedShape { deleted: id }

resumedShape :: Shape { id :: String, rounds :: Int }
resumedShape = recordShape "Resumed" { id: stringShape, rounds: intShape }

archivedShape :: Shape { id :: String, archived :: Boolean }
archivedShape = recordShape "Archived" { id: stringShape, archived: boolShape }

deletedShape :: Shape { deleted :: String }
deletedShape = recordShape "Deleted" { deleted: stringShape }

hasRun :: StoreBackend -> String -> Effect Boolean
hasRun store id = isJust <$> runInfo store id
