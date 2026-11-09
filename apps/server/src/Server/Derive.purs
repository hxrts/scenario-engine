-- | The run operations on a stored run (docs/06_simulator.md §9.1, §9.2): derive (duplicate, rerun,
-- | variant, counterfactual), resume, archive and unarchive, and delete. Each validates its request
-- | and persists the result; the dashboard only sends the request and shows the reply.
module Server.Derive
  ( deriveRun
  , resumeRun
  , stepRun
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
import Data.String (Pattern(..), joinWith, split)
import Effects.Play (playRun)
import Effects.System (now, ownerOf)
import Run (Run)
import Server.Rows (SERVER)
import Effects.Api (DeriveRequest)
import Effects.Signatures (PlayRequest(..), RunError(..), Stored, StoredRound)
import Effects.Observe (readManifest, readRounds)
import Effects.Store (deleteRun, setArchived)
import Views.Keys as Keys
import Server.Errors (Answer, badRequest, conflict, describe, internal, notFound, ok)
import Effects.Launch (freshRunId, start)
import Server.Launch (serverRole)
import Game.Setting (GameboardParts, Setting(..))
import Server.Read (loadRun, runInfo)
import Game.Assemble (assemble, assemblePreset, checkRounds, liveBindings, withRounds)
import Server.Sets (setsNaming)
import Server.Settings (Settings, ofSpec)
import Schema.Run (Spec, DeriveKind(..), RunStatus(..))
import Core.Shape (Format(Authored))
import Handlers.Api.Route as Route
import Game.Recipe (Recipe, decodeSpec, deriveRecipe)
import Views.Common (records)
import Handlers.Simulate (replayGame)

-- | `POST /api/runs/:id/derive`. The body's kind decides what may change:
--   duplicate, rerun: the source's spec unchanged, under a new name (rerun is the claim that the
--     records reproduce, so it takes no change at all);
--   variant: the source's preset, player seed, or rounds replaced (a null player seed is drawn);
--   counterfactual: the source's overrides plus the given ones.
-- Absent fields keep the source's values. Every derived run records its source and kind.
-- A rerun also reports `recordsMatch`: whether its round records equal the source's. The source's
-- records must be stored and readable, and the source completed, before the rerun starts; otherwise
-- the reply is 409 and nothing is created.
deriveRun :: forall o. Settings -> String -> DeriveRequest -> Run (SERVER o) Answer
deriveRun settings sourceId body = do
  runInfo sourceId >>= case _ of
    Nothing -> notFound ("no run " <> sourceId)
    Just info -> (readManifest sourceId) >>= case _ of
      Just { spec: Right spec } | isNothing spec.preset -> deriveSession settings sourceId body info spec
      _ -> deriveFromRecipe settings sourceId body info

deriveFromRecipe :: forall o r. Settings -> String -> DeriveRequest -> { name :: String, status :: RunStatus | r } -> Run (SERVER o) Answer
deriveFromRecipe settings sourceId body info = do
  let kind = body.kind
  do
    -- a rerun is judged against the source's stored records, so they are read before it starts
    source <- if kind == Rerun then Just <$> loadRun sourceId else pure Nothing
    case source of
      Just (Left Missing) -> notFound ("no run " <> sourceId)
      Just (Left (Unreadable why)) -> conflict ("cannot compare the rerun with " <> sourceId <> ": " <> why)
      Just (Right src)
        | info.status /= Completed -> conflict ("cannot compare the rerun with " <> sourceId <> ": it is " <> show info.status <> ", not completed")
        | null (records src) -> conflict ("cannot compare the rerun with " <> sourceId <> ": it has no stored round records")
      _ -> do
        stored <- (readManifest sourceId)
        case stored <#> \m -> m.spec >>= \spec -> { base: _, setting: _ } <$> decodeSpec spec <*> ofSpec settings spec of
          Nothing -> internal ("run " <> sourceId <> " has no stored spec")
          Just (Left e) -> internal ("run " <> sourceId <> " is unreadable: " <> e)
          Just (Right { base, setting }) -> case deriveRecipe kind body base of
            Left e -> badRequest e
            Right _ | kind == Variant, Just bad <- outOfRange setting -> badRequest bad
            Right recipe -> do
              let name = fromMaybe (info.name <> " (" <> show kind <> ")") body.name
              started <- start serverRole setting
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
                  new <- loadRun l.id
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
deriveSession :: forall o r. Settings -> String -> DeriveRequest -> { name :: String, status :: RunStatus | r } -> Spec -> Run (SERVER o) Answer
deriveSession settings sourceId body info spec
  | body.kind == Variant || body.kind == Counterfactual = badRequest ("a live session has no preset, so a " <> show body.kind <> " cannot be derived from " <> sourceId)
  | info.status /= Completed = conflict ("cannot replay " <> sourceId <> ": it is " <> show info.status <> ", not completed")
  | otherwise =
      case deriveRecipe body.kind body { preset: "", playerSeed: spec.playerSeed, rounds: spec.rounds, overrides: [] }, ofSpec settings spec of
        Left e, _ -> badRequest e
        _, Left e -> internal ("run " <> sourceId <> " is unreadable: " <> e)
        Right _, Right setting -> loadRun sourceId >>= case _ of
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
          Right out -> freshRunId >>= case _ of
            Left e -> badRequest e
            Right id -> do
              created <- now
              owner <- ownerOf serverRole
              attempt <- playRun (Replayed { game: g, run: { id, name: fromMaybe (info.name <> " (" <> show body.kind <> ")") body.name, created, spec: stored, manifest: g.manifest, owner }, records: out.records, value: out.value })
              case attempt of
                Left e -> badRequest e
                Right (Left e) -> internal ("run " <> id <> " failed: " <> e)
                Right (Right _) | body.kind == Rerun -> do
                  new <- loadRun id
                  case new of
                    Right n -> replied id (Just (records src == records n))
                    Left e -> internal ("the rerun " <> id <> " is unreadable: " <> describe e)
                Right (Right _) -> replied id Nothing
      replied id recordsMatch = pure (Right (Route.replyIn Authored Keys.derived 201 { id, source: sourceId, kind: body.kind, recordsMatch }))

-- | `POST /api/runs/:id/resume` with `{ "rounds": n }`: a completed run is extended to n rounds,
-- | starting from the snapshot after its last completed round. The run keeps its id.
resumeRun :: forall o. Settings -> String -> Int -> Run (SERVER o) Answer
resumeRun settings id rounds = continueRun settings id "resumed" \p ->
  if rounds <= p.done then Left ("rounds must exceed the " <> show p.done <> " completed rounds of " <> id)
  else Right { rounds, spec: p.spec { rounds = Just rounds } }

-- | `POST /api/runs/:id/step`: one round of a completed run that is short of its target, through the same
-- | snapshot path as resume. The target does not change.
stepRun :: forall o. Settings -> String -> Run (SERVER o) Answer
stepRun settings id = continueRun settings id "stepped" \p -> case p.spec.rounds of
  Just target | p.done < target -> Right { rounds: p.done + 1, spec: p.spec }
  _ -> Left (id <> " has played all " <> show p.done <> " of its rounds")

-- | Continue a completed run from its last snapshot, to the round count and with the spec `decide` gives.
continueRun :: forall o. Settings -> String -> String -> ({ done :: Int, spec :: Spec } -> Either String { rounds :: Int, spec :: Spec }) -> Run (SERVER o) Answer
continueRun settings id verb decide =
  runInfo id >>= case _ of
    Nothing -> notFound ("no run " <> id)
    Just info
      | info.status /= Completed -> badRequest ("only a completed run can be " <> verb <> "; " <> id <> " is " <> show info.status)
      | otherwise -> do
          stored <- (readManifest id)
          rows <- (readRounds id)
          case stored, last rows of
            Just m, _ | Right spec <- m.spec, isNothing spec.preset -> badRequest ("a live session has no preset, so it cannot be " <> verb <> "; " <> id)
            Just m, Just latest -> case prepare m.spec latest (length rows) of
              Left e -> internal ("run " <> id <> " is unreadable: " <> e)
              Right p -> case decide { done: p.done, spec: p.spec } of
                Left e -> badRequest e
                Right next -> case assemblePreset p.setting p.base.preset p.base.overrides (Just next.rounds) of
                  Left e -> badRequest (show e)
                  Right game -> do
                    attempt <- playRun (Continued { id, seed: p.seed, game, world: p.snapshot, done: p.done, spec: next.spec })
                    case attempt of
                      Left e -> badRequest e
                      Right r -> either (\e -> internal ("run " <> id <> " failed: " <> e)) (const (ok Keys.resumed { id, rounds: next.rounds })) r
            _, _ -> internal ("run " <> id <> " is unreadable: it has no manifest or no rounds")
  where
  -- what resuming needs: the spec, its recipe and seed, the last snapshot, and the completed round count
  prepare :: Stored Spec -> StoredRound -> Int -> Either String { spec :: Spec, base :: Recipe, setting :: GameboardParts, seed :: Seed, snapshot :: WorldState, done :: Int }
  prepare text latest done = do
    spec <- text
    base <- decodeSpec spec
    setting <- ofSpec settings spec
    seed <- note "the stored spec has no player seed" base.playerSeed
    snapshot <- latest.snapshot
    pure { spec, base, setting, seed: Seed seed, snapshot, done }

-- | `POST /api/runs/:id/archive` and `/unarchive`. Idempotent.
setArchive :: forall o. String -> Boolean -> Run (SERVER o) Answer
setArchive id archived = do
  present <- hasRun id
  if not present then notFound ("no run " <> id)
  else do
    (setArchived id archived)
    ok Keys.archived { id, archived }

-- | `DELETE /api/runs/:id?confirm=:id`: the run with its rounds and metrics. Runs that cite it keep
-- | their source field, which then resolves to deleted.
deleteOne :: forall o. String -> String -> Run (SERVER o) Answer
deleteOne id confirm
  | confirm /= id = badRequest ("deleting " <> id <> " needs ?confirm=" <> id)
  | otherwise = setsNaming id >>= \naming ->
      if not (null naming) then conflict (id <> " is in the scenario sets " <> joinWith ", " (map _.name naming) <> "; delete them first")
      else do
        n <- deleteRun id
        if n == 0 then notFound ("no run " <> id) else ok Keys.deleted { deleted: id }

hasRun :: forall o. String -> Run (SERVER o) Boolean
hasRun id = isJust <$> runInfo id
