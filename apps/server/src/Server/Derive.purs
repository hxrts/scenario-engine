-- | The run operations on a stored run (docs/06_simulator.md §9.1, §9.2): derive (duplicate, rerun,
-- | variant, counterfactual), resume, archive and unarchive, and delete. Each validates its request
-- | and persists the result; the dashboard only sends the request and shows the reply.
module Server.Derive
  ( Resume
  , deriveRun
  , resumeRun
  , setArchive
  , deleteOne
  ) where

import Prelude

import Effects.Signatures (Spec, Stored, StoredRound)
import Effects.Store (deleteRun, failRun, listRuns, readManifest, readRounds, setArchived, setSpec, setStatus)
import Handlers.Store (StoreBackend, finish, persister, runStore)
import Board.Decode.Common (readJson)
import Core.Draw (Seed(..))
import Core.World (WorldState)
import Data.Argonaut.Core (Json, isNull, toString)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (elem, filter, find, last, length, null)
import Data.Either (Either(..), either, hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (message, throwException, try)
import Foreign.Object (Object)
import Foreign.Object as Object
import Simulator.Overrides (decodeOverrides)
import Simulator.Records (recordsEqual)
import Simulator.Simulate (Context, Override, resumeWith)
import Http (Response)
import Server.Errors (LoadError(..), describe, failWith, json, ok)
import Server.Launch (presetSpec, start)
import Server.Read (loadRun)

-- | `POST /api/runs/:id/resume` body: the round count the run is extended to.
type Resume = { rounds :: Int }

-- | What a new run is played from: its preset, player seed (Nothing draws one), round count
-- | (Nothing is the configuration's), and overrides. It is what a run's stored spec records.
type Recipe = { preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override }

-- | `POST /api/runs/:id/derive`. The body's kind decides what may change:
--   duplicate, rerun: the source's spec unchanged, under a new name (rerun is the claim that the
--     records reproduce, so it takes no change at all);
--   variant: the source's preset, player seed, or rounds replaced (a null player seed is drawn);
--   counterfactual: the source's overrides plus the given ones.
-- Absent fields keep the source's values. Every derived run records its source and kind.
-- A rerun also reports `recordsMatch`: whether its round records equal the source's. The source's
-- records must be stored and readable, and the source completed, before the rerun starts; otherwise
-- the reply is 409 and nothing is created.
deriveRun :: Context -> StoreBackend -> String -> String -> Effect Response
deriveRun ctx store sourceId body = case readJson body :: Either String (Object Json) of
  Left e -> failWith 400 e
  Right o -> case Object.lookup "kind" o >>= toString of
    Just kind | kind `elem` kinds -> do
      runs <- runStore store listRuns
      case find (\r -> r.id == sourceId) runs of
        Nothing -> failWith 404 ("no run " <> sourceId)
        Just info -> do
          -- a rerun is judged against the source's stored records, so they are read before it starts
          source <- if kind == "rerun" then Just <$> loadRun store sourceId else pure Nothing
          case source of
            Just (Left Missing) -> failWith 404 ("no run " <> sourceId)
            Just (Left (Unreadable why)) -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": " <> why)
            Just (Right src)
              | info.status /= "completed" -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it is " <> info.status <> ", not completed")
              | null src.records -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it has no stored round records")
            _ -> do
              stored <- runStore store (readManifest sourceId)
              case stored <#> \m -> m.spec >>= storedRecipe of
                Nothing -> failWith 500 ("run " <> sourceId <> " has no stored spec")
                Just (Left e) -> failWith 500 ("run " <> sourceId <> " is unreadable: " <> e)
                Just (Right base) -> case deriveRecipe kind o base of
                  Left e -> failWith 400 e
                  Right recipe -> do
                    let name = fromMaybe (info.name <> " (" <> kind <> ")") (Object.lookup "name" o >>= toString)
                    started <- start ctx store
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
                        new <- loadRun store l.id
                        case new of
                          Right n -> json 201
                            ( Object.fromFoldable
                                [ Tuple "id" (encodeJson l.id)
                                , Tuple "source" (encodeJson sourceId)
                                , Tuple "kind" (encodeJson kind)
                                , Tuple "recordsMatch" (encodeJson (recordsEqual src.records n.records))
                                ]
                            )
                          Left e -> failWith 500 ("the rerun " <> l.id <> " is unreadable: " <> describe e)
                      Right l, _ -> json 201 { id: l.id, source: sourceId, kind }
    _ -> failWith 400 ("kind must be one of " <> joinWith ", " kinds)
  where
  kinds = [ "duplicate", "rerun", "variant", "counterfactual" ]

-- | The recipe a derived run is played from, given the kind and the body.
deriveRecipe :: String -> Object Json -> Recipe -> Either String Recipe
deriveRecipe kind body base = case kind of
  "duplicate" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "rerun" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "variant" -> do
    unchanged [ "overrides" ]
    preset <- maybe (pure base.preset) (\j -> note "preset must be a string" (toString j)) (Object.lookup "preset" body)
    playerSeed <- case Object.lookup "playerSeed" body of
      Nothing -> pure base.playerSeed
      Just j | isNull j -> pure Nothing
      Just j -> Just <$> intField "playerSeed" j
    rounds <- case Object.lookup "rounds" body of
      Nothing -> pure base.rounds
      Just j | isNull j -> pure base.rounds
      Just j -> Just <$> intField "rounds" j
    pure { preset, playerSeed, rounds, overrides: base.overrides }
  _ -> do
    unchanged [ "preset", "playerSeed", "rounds" ]
    given <- case Object.lookup "overrides" body of
      Nothing -> Left "a counterfactual needs overrides"
      Just j -> decodeOverrides j
    if null given then Left "a counterfactual needs at least one override" else pure unit
    pure base { overrides = base.overrides <> given }
  where
  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array String -> Either String Unit
  unchanged fields = case find (\f -> Object.member f body) fields of
    Just f -> Left (kind <> " does not take " <> f <> "; only a variant or counterfactual changes the spec")
    Nothing -> pure unit

  intField :: String -> Json -> Either String Int
  intField name j = note (name <> " must be an integer") (decodeIntJson j)

-- | An integer from a JSON number, or Nothing.
decodeIntJson :: Json -> Maybe Int
decodeIntJson j = hush (decodeJson j)

-- | The recipe a stored specification was played with.
storedRecipe :: Object String -> Either String Recipe
storedRecipe s = do
  preset <- note "the stored spec has no preset" (Object.lookup "preset" s)
  playerSeed <- note "the stored spec has no player seed" (Object.lookup "playerSeed" s >>= Int.fromString)
  overrides <- case Object.lookup "overrides" s of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: Object.lookup "rounds" s >>= Int.fromString, overrides }

-- | `POST /api/runs/:id/resume` with `{ "rounds": n }`: a completed run is extended to n rounds,
-- | starting from the snapshot after its last completed round. The run keeps its id.
resumeRun :: Context -> StoreBackend -> String -> String -> Effect Response
resumeRun ctx store id body = case readJson body :: Either String Resume of
  Left e -> failWith 400 e
  Right { rounds } -> do
    runs <- runStore store listRuns
    case find (\r -> r.id == id) runs of
      Nothing -> failWith 404 ("no run " <> id)
      Just info
        | info.status /= "completed" -> failWith 400 ("only a completed run can be resumed; " <> id <> " is " <> info.status)
        | otherwise -> do
            stored <- runStore store (readManifest id)
            rows <- runStore store (readRounds id)
            case stored, last rows of
              Just m, Just latest -> case prepare m.spec latest (length rows) of
                Left e -> failWith 500 ("run " <> id <> " is unreadable: " <> e)
                Right p
                  | rounds <= p.done -> failWith 400 ("rounds must exceed the " <> show p.done <> " completed rounds of " <> id)
                  | otherwise -> do
                      attempt <- try $ resumeWith ctx (presetSpec ctx p.base.preset p.seed p.base.overrides (Just rounds)) (Just { world: p.snapshot, done: p.done }) \_ -> do
                        runStore store (setStatus id "running")
                        runStore store (setSpec id (Object.insert "rounds" (show rounds) p.spec))
                        persister store id p.snapshot
                      case attempt of
                        Left ex -> do
                          runStore store (failRun id ("exception: " <> message ex))
                          throwException ex
                        Right (Left e) -> failWith 400 e
                        Right (Right r) -> do
                          runStore store (finish id r)
                          either (\e -> failWith 500 ("run " <> id <> " failed: " <> show e)) (const (ok { id, rounds })) r
              _, _ -> failWith 500 ("run " <> id <> " is unreadable: it has no manifest or no rounds")
  where
  -- what resuming needs: the spec, its recipe and seed, the last snapshot, and the completed round count
  prepare :: Stored Spec -> StoredRound -> Int -> Either String { spec :: Spec, base :: Recipe, seed :: Seed, snapshot :: WorldState, done :: Int }
  prepare text latest done = do
    spec <- text
    base <- storedRecipe spec
    seed <- note "the stored spec has no player seed" base.playerSeed
    snapshot <- latest.snapshot
    pure { spec, base, seed: Seed seed, snapshot, done }

-- | `POST /api/runs/:id/archive` and `/unarchive`. Idempotent.
setArchive :: StoreBackend -> String -> Boolean -> Effect Response
setArchive store id archived = do
  present <- hasRun store id
  if not present then failWith 404 ("no run " <> id)
  else do
    runStore store (setArchived id archived)
    ok { id, archived }

-- | `DELETE /api/runs/:id?confirm=:id`: the run with its rounds and metrics. Runs that cite it keep
-- | their source field, which then resolves to deleted.
deleteOne :: StoreBackend -> String -> String -> Effect Response
deleteOne store id confirm
  | confirm /= id = failWith 400 ("deleting " <> id <> " needs ?confirm=" <> id)
  | otherwise = do
      n <- runStore store (deleteRun id)
      if n == 0 then failWith 404 ("no run " <> id) else ok { deleted: id }

hasRun :: StoreBackend -> String -> Effect Boolean
hasRun store id = (\runs -> length (filter (\r -> r.id == id) runs) > 0) <$> runStore store listRuns
