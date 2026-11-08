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
import Data.Array (last, length, null, filter)
import Data.Either (Either(..), either, note)
import Data.Foldable (traverse_)
import Core.Types (Id(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effects.Api (DeriveRequest)
import Effects.Signatures (RunError(..), Stored, StoredRound)
import Effects.Observe (readManifest, readRounds)
import Effects.Store (deleteRun, setArchived, setSpec, setStatus, createRun)
import Views.Keys as Keys
import Views.Metrics (roundMetrics)
import Handlers.Store (StoreBackend, recorder, runObserve, runStore)
import Server.Errors (Answer, badRequest, conflict, describe, internal, notFound, ok)
import Server.Launch (freshRunId, play, serverRole, start)
import Game.Setting (Setting(..))
import Adapters.Clock (nowIso)
import Adapters.SQLite (processOwner)
import Server.Cache (Cache)
import Server.Read (loadRun, runInfo)
import Game.Assemble (assemble, assemblePreset, checkRounds, liveBindings, withRounds)
import Server.Settings (Settings, ofSpec)
import Schema.Run (Spec, DeriveKind(..), RunStatus(..))
import Core.Shape (Format(Authored))
import Handlers.Api.Route as Route
import Game.Recipe (Recipe, decodeSpec, deriveRecipe)
import Views.Common (records)
import Simulator.Simulate (replayGame, resumeWith)

-- | `POST /api/runs/:id/derive`. The body's kind decides what may change:
--   duplicate, rerun: the source's spec unchanged, under a new name (rerun is the claim that the
--     records reproduce, so it takes no change at all);
--   variant: the source's preset, player seed, or rounds replaced (a null player seed is drawn);
--   counterfactual: the source's overrides plus the given ones.
-- Absent fields keep the source's values. Every derived run records its source and kind.
-- A rerun also reports `recordsMatch`: whether its round records equal the source's. The source's
-- records must be stored and readable, and the source completed, before the rerun starts; otherwise
-- the reply is 409 and nothing is created.
deriveRun :: Cache -> Settings -> StoreBackend -> String -> DeriveRequest -> Effect Answer
deriveRun cache settings store sourceId body = do
  runInfo store sourceId >>= case _ of
    Nothing -> notFound ("no run " <> sourceId)
    Just info -> runObserve store (readManifest sourceId) >>= case _ of
      Just { spec: Right spec } | isNothing spec.preset -> deriveSession cache settings store sourceId body info spec
      _ -> deriveFromRecipe cache settings store sourceId body info

deriveFromRecipe :: forall r. Cache -> Settings -> StoreBackend -> String -> DeriveRequest -> { name :: String, status :: RunStatus | r } -> Effect Answer
deriveFromRecipe cache settings store sourceId body info = do
  let kind = body.kind
  do
    -- a rerun is judged against the source's stored records, so they are read before it starts
    source <- if kind == Rerun then Just <$> loadRun cache store sourceId else pure Nothing
    case source of
      Just (Left Missing) -> notFound ("no run " <> sourceId)
      Just (Left (Unreadable why)) -> conflict ("cannot compare the rerun with " <> sourceId <> ": " <> why)
      Just (Right src)
        | info.status /= Completed -> conflict ("cannot compare the rerun with " <> sourceId <> ": it is " <> show info.status <> ", not completed")
        | null (records src) -> conflict ("cannot compare the rerun with " <> sourceId <> ": it has no stored round records")
      _ -> do
        stored <- runObserve store (readManifest sourceId)
        case stored <#> \m -> m.spec >>= \spec -> { base: _, setting: _ } <$> decodeSpec spec <*> ofSpec settings spec of
          Nothing -> internal ("run " <> sourceId <> " has no stored spec")
          Just (Left e) -> internal ("run " <> sourceId <> " is unreadable: " <> e)
          Just (Right { base, setting }) -> case deriveRecipe kind body base of
            Left e -> badRequest e
            Right _ | kind == Variant, Just bad <- outOfRange setting -> badRequest bad
            Right recipe -> do
              let name = fromMaybe (info.name <> " (" <> show kind <> ")") body.name
              started <- start setting store
                { name
                , preset: recipe.preset
                , playerSeed: recipe.playerSeed
                , rounds: recipe.rounds
                , overrides: recipe.overrides
                , derivation: Just { source: sourceId, kind }
                }
              case started, source of
                Left e, _ -> badRequest e
                Right l, Just (Right src) -> do
                  -- the new run's records are read back from the store, as the source's are
                  new <- loadRun cache store l.id
                  case new of
                    Right n -> replied l.id (Just (records src == records n))
                    Left e -> internal ("the rerun " <> l.id <> " is unreadable: " <> describe e)
                Right l, _ -> replied l.id Nothing
  where
  -- a variant is a new game, so a round count it asks for obeys the Gameboard's range; a duplicate or a
  -- rerun replays what was recorded
  outOfRange setting = either (Just <<< show) (const Nothing) (traverse_ (checkRounds setting.gameboard) body.rounds)
  -- the reply to a derive: the new run, its source and kind, and anything the kind adds
  replied id recordsMatch = pure (Right (Route.replyIn Authored Keys.derived 201 { id, source: sourceId, kind: body.kind, recordsMatch }))

-- | A derive from a live session. It has no preset, so a variant or counterfactual cannot be played
-- | from it; a duplicate or rerun replays its recorded Proposals and signatures (docs/06 §9.1).
deriveSession :: forall r. Cache -> Settings -> StoreBackend -> String -> DeriveRequest -> { name :: String, status :: RunStatus | r } -> Spec -> Effect Answer
deriveSession cache settings store sourceId body info spec
  | body.kind == Variant || body.kind == Counterfactual = badRequest ("a live session has no preset, so a " <> show body.kind <> " cannot be derived from " <> sourceId)
  | info.status /= Completed = conflict ("cannot replay " <> sourceId <> ": it is " <> show info.status <> ", not completed")
  | otherwise =
      case deriveRecipe body.kind body { preset: "", playerSeed: spec.playerSeed, rounds: spec.rounds, overrides: [] }, ofSpec settings spec of
        Left e, _ -> badRequest e
        _, Left e -> internal ("run " <> sourceId <> " is unreadable: " <> e)
        Right _, Right setting -> loadRun cache store sourceId >>= case _ of
          Left e -> conflict ("cannot replay " <> sourceId <> ": " <> describe e)
          Right src -> replay setting src
      where
      humans = map Id (filter (_ /= "") (split (Pattern ",") (fromMaybe "" spec.humans)))
      seed = Seed (fromMaybe 0 spec.playerSeed)
      stored = spec { source = Just sourceId, kind = Just body.kind }
      replay setting src = case map (withRounds spec.rounds) (assemble (GameboardSetting setting) (liveBindings humans setting.config) []) of
        Left e -> badRequest (show e)
        Right g -> case replayGame seed g (records src) of
          Left e -> conflict ("cannot replay " <> sourceId <> ": " <> e)
          Right out -> freshRunId store >>= case _ of
            Left e -> badRequest e
            Right id -> do
              created <- nowIso
              owner <- processOwner serverRole
              attempt <- play store id do
                runStore store (createRun { id, name: fromMaybe (info.name <> " (" <> show body.kind <> ")") body.name, created, spec: stored, manifest: g.manifest, source: Just sourceId, kind: Just body.kind, owner })
                save <- recorder store roundMetrics id g.world
                traverse_ (\r -> save r []) out.records
                pure (Right out.value :: Either String _)
              case attempt of
                Left e -> badRequest e
                Right (Left e) -> internal ("run " <> id <> " failed: " <> e)
                Right (Right _) | body.kind == Rerun -> do
                  new <- loadRun cache store id
                  case new of
                    Right n -> replied id (Just (records src == records n))
                    Left e -> internal ("the rerun " <> id <> " is unreadable: " <> describe e)
                Right (Right _) -> replied id Nothing
      replied id recordsMatch = pure (Right (Route.replyIn Authored Keys.derived 201 { id, source: sourceId, kind: body.kind, recordsMatch }))

-- | `POST /api/runs/:id/resume` with `{ "rounds": n }`: a completed run is extended to n rounds,
-- | starting from the snapshot after its last completed round. The run keeps its id.
resumeRun :: Settings -> StoreBackend -> String -> Int -> Effect Answer
resumeRun settings store id rounds =
  runInfo store id >>= case _ of
    Nothing -> notFound ("no run " <> id)
    Just info
      | info.status /= Completed -> badRequest ("only a completed run can be resumed; " <> id <> " is " <> show info.status)
      | otherwise -> do
          stored <- runObserve store (readManifest id)
          rows <- runObserve store (readRounds id)
          case stored, last rows of
            Just m, _ | Right spec <- m.spec, isNothing spec.preset -> badRequest ("a live session has no preset, so it cannot be resumed; " <> id)
            Just m, Just latest -> case prepare m.spec latest (length rows) of
              Left e -> internal ("run " <> id <> " is unreadable: " <> e)
              Right p
                | rounds <= p.done -> badRequest ("rounds must exceed the " <> show p.done <> " completed rounds of " <> id)
                | otherwise -> do
                    case assemblePreset p.setting p.base.preset p.base.overrides (Just rounds) of
                      Left e -> badRequest (show e)
                      Right game -> do
                        attempt <- play store id $ resumeWith p.seed game { world: p.snapshot, done: p.done } \_ -> do
                          runStore store (setStatus id Running Nothing)
                          runStore store (setSpec id p.spec { rounds = Just rounds })
                          recorder store roundMetrics id p.snapshot
                        case attempt of
                          Left e -> badRequest e
                          Right r -> either (\e -> internal ("run " <> id <> " failed: " <> e)) (const (ok Keys.resumed { id, rounds })) r
            _, _ -> internal ("run " <> id <> " is unreadable: it has no manifest or no rounds")
  where
  -- what resuming needs: the spec, its recipe and seed, the last snapshot, and the completed round count
  prepare :: Stored Spec -> StoredRound -> Int -> Either String { spec :: Spec, base :: Recipe, setting :: _, seed :: Seed, snapshot :: WorldState, done :: Int }
  prepare text latest done = do
    spec <- text
    base <- decodeSpec spec
    setting <- ofSpec settings spec
    seed <- note "the stored spec has no player seed" base.playerSeed
    snapshot <- latest.snapshot
    pure { spec, base, setting, seed: Seed seed, snapshot, done }

-- | `POST /api/runs/:id/archive` and `/unarchive`. Idempotent.
setArchive :: StoreBackend -> String -> Boolean -> Effect Answer
setArchive store id archived = do
  present <- hasRun store id
  if not present then notFound ("no run " <> id)
  else do
    runStore store (setArchived id archived)
    ok Keys.archived { id, archived }

-- | `DELETE /api/runs/:id?confirm=:id`: the run with its rounds and metrics. Runs that cite it keep
-- | their source field, which then resolves to deleted.
deleteOne :: StoreBackend -> String -> String -> Effect Answer
deleteOne store id confirm
  | confirm /= id = badRequest ("deleting " <> id <> " needs ?confirm=" <> id)
  | otherwise = do
      n <- runStore store (deleteRun id)
      if n == 0 then notFound ("no run " <> id) else ok Keys.deleted { deleted: id }

hasRun :: StoreBackend -> String -> Effect Boolean
hasRun store id = isJust <$> runInfo store id
