-- | The capstone's live path and persistence (docs/08_coverage.md): a recorded transcript replayed
-- | through a stubbed model (no network, no `pi`), and the DSL-authored capstone run round-tripped
-- | through the Run Store.
module Test.Adapters.Capstone (spec) where

import Prelude

import Adapters.Llm (Backend, interpreter)
import Adapters.Store (createRun, open, persister, reader)
import Board.Decode (readJson)
import Board.Gameboard (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Engine.Prepare (observe)
import Core.Types (Entity, EntityId, Id(..))
import Core.World (RoundRecord, WorldState)
import Data.Argonaut.Core (Json, fromString, stringify, toArray, toObject, toString)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, length)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (for_, traverse)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effects.Environment (inWorldOf)
import Effects.Round (runGame)
import Effects.Signatures (Interpretation(..), InterpretF(..))
import Foreign.Object as Object
import Handlers.Replay (runReplay)
import Harness.Capstone (Capstone, decodeCapstone)
import Harness.Simulate (SeatPolicy(..), simulate, simulateWith)
import Partial.Unsafe (unsafeCrashWith)
import Adapters.Scenarios (readScenario, readText)
import Scenarios.Load (load, loadPresets, loadRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- Recorded transcript ----------------------------------------------------------------------

-- | One recorded exchange: the player's line, the model's reply, and what the interpreter must
-- | make of it.
type Entry = { text :: String, reply :: Json, expect :: Json }

orThrow :: forall a. Either String a -> Aff a
orThrow = either (throwError <<< error) pure

-- | `packages/scenarios/capstone/transcript.json`, decoded to its entries.
transcript :: Aff (Array Entry)
transcript = do
  file <- liftEffect (readText "capstone/transcript.json") >>= orThrow >>= (orThrow <<< loadRun)
  entries <- orThrow (note' "the transcript must be an array" (toArray file))
  traverse entry entries
  where
  entry j = do
    o <- orThrow (note' "an entry must be an object" (toObject j))
    text <- orThrow (note' "an entry needs its text" (Object.lookup "text" o >>= toString))
    reply <- orThrow (note' "an entry needs its reply" (Object.lookup "reply" o))
    expect <- orThrow (note' "an entry needs its expectation" (Object.lookup "expect" o))
    pure { text, reply, expect }

  note' :: forall a. String -> Maybe a -> Either String a
  note' what = maybe (Left what) Right

-- | A model stub that answers each call with the reply recorded for the line the call names.
replayed :: Array Entry -> Backend
replayed entries =
  { model: "recorded"
  , call: \_ input -> case find (\e -> contains (Pattern e.text) input) entries of
      Just e -> pure { text: stringify e.reply, raw: "" }
      Nothing -> throwError (error ("no recorded reply for: " <> input))
  }

type Game = { world :: WorldState, lookup :: EntityId -> Maybe Entity }

setup :: Aff Game
setup = do
  loaded <- liftEffect (readScenario "fractured-world/v0") >>= orThrow >>= (orThrow <<< load)
  case instantiate loaded.gameboard loaded.config of
    Right initial -> pure { world: initial.world, lookup: \e -> Map.lookup e initial.world.entities }
    Left _ -> throwError (error "instantiation")

-- | The US seat's view of the initial world, for the interpreter.
interpretAs :: Game -> String -> Backend -> Aff Interpretation
interpretAs g text backend =
  interpreter backend (const (pure unit)) g.lookup
    (Interpret (Id "US") (observe g.world (fromMaybe' (\_ -> unsafeCrashWith "US") (Map.lookup (Id "US") g.world.entities))) text identity)

-- | Every recorded line gives the interpretation its recording expects.
check :: Entry -> Interpretation -> Aff Unit
check e out = case stringOf "kind" e.expect of
  Just "understood" -> do
    -- the Intents in the engine's own JSON encoding, compared as JSON
    let intents = fromMaybe' (\_ -> []) (arrayOf "intents" e.expect)
    case out of
      Understood ds _ -> map (\d -> stringify (encodeJson d.intents)) ds `shouldEqual` [ stringify (encodeJson intents) ]
      other -> fail ("expected an understood action, got " <> show' other)
  Just "clarify" -> case out of
    Clarify q -> q `shouldEqual` fromMaybe' (\_ -> "") (stringOf "message" e.expect)
    other -> fail ("expected a clarification, got " <> show' other)
  Just "inadmissible" -> case out of
    Inadmissible r -> r `shouldEqual` fromMaybe' (\_ -> "") (stringOf "message" e.expect)
    other -> fail ("expected an inadmissible action, got " <> show' other)
  _ -> fail ("unknown recorded kind for " <> e.text)
  where
  show' = case _ of
    Understood _ _ -> "understood"
    Clarify q -> "clarify " <> q
    Inadmissible r -> "inadmissible " <> r

stringOf :: String -> Json -> Maybe String
stringOf k j = toObject j >>= Object.lookup k >>= toString

arrayOf :: String -> Json -> Maybe (Array Json)
arrayOf k j = toObject j >>= Object.lookup k >>= toArray

-- The capstone through the store ------------------------------------------------------------

capstone :: Aff Capstone
capstone = do
  file <- liftEffect (readText "capstone/run.json") >>= orThrow >>= (orThrow <<< loadRun)
  orThrow (decodeCapstone file)

spec :: Spec Unit
spec = describe "Capstone: recorded transcript and the Run Store" do
  it "each recorded line maps to the canonical Intents it records (stubbed model)" do
    g <- setup
    entries <- transcript
    for_ entries \e -> do
      out <- interpretAs g e.text (replayed entries)
      check e out

  it "an ambiguous recorded line is a clarification, never an action" do
    g <- setup
    entries <- transcript
    out <- interpretAs g "buy some energy" (replayed entries)
    case out of
      Clarify q -> q `shouldEqual` "How much energy, on which market?"
      _ -> fail "expected a clarification"

  it "a recorded backend failure is a run failure, naming the model and its cause" do
    g <- setup
    entries <- transcript
    let
      broken = { model: "recorded", call: \_ _ -> throwError (error "language model backend failed (recorded): spawn pi ENOENT") }
    out <- attempt (interpretAs g "buy ten energy" broken)
    case out of
      Left e -> (contains (Pattern "language model backend failed") (message e)) `shouldEqual` true
      Right _ -> fail "a failed backend must fail the call"
    -- the same line, replayed, still interprets: the failure was the backend's, not the transcript's
    ok <- interpretAs g "buy ten energy" (replayed entries)
    case ok of
      Understood _ _ -> pure unit
      _ -> fail "replayed line should interpret"

  it "the capstone run round-trips through the store: saved, reloaded byte-identically, and replayed" do
    loaded <- liftEffect (readScenario "fractured-world/v0") >>= orThrow >>= (orThrow <<< load)
    presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
    cap <- capstone
    let
      ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
      spec' = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }
    db <- liftEffect (open ":memory:")
    stored <- liftEffect $ simulateWith ctx spec' \manifest -> do
      createRun db { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: fromString "capstone", manifest, source: Nothing, kind: Nothing }
      persister db "capstone" manifest.initial
    _ <- orThrow stored
    let store = reader db
    rows <- liftEffect (store.rounds "capstone")
    length rows `shouldEqual` cap.rounds
    records <- orThrow (traverse (\r -> readJson r.record :: Either String RoundRecord) rows)
    snapshots <- orThrow (traverse (\r -> readJson r.snapshot :: Either String WorldState) rows)
    map (stringify <<< encodeJson) records `shouldEqual` map _.record rows
    map (stringify <<< encodeJson) snapshots `shouldEqual` map _.snapshot rows
    pure' <- orThrow (simulate ctx spec')
    snapshots `shouldEqual` map _.world pure'.records
    case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
      Right initial, Right inWorld -> case runReplay cap.rounds records (runGame inWorld cap.rounds initial.world) of
        Left e -> fail e
        Right out -> map _.world out.records `shouldEqual` snapshots
      _, _ -> fail "instantiation"
