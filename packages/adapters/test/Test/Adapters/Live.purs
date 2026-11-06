-- | Phase 8: the interpretation suite (docs/06_simulator.md §7.4) against a stubbed model,
-- | a scripted live session that replays exactly, and an all-AI-agent game.
module Test.Adapters.Live (spec) where

import Prelude

import Adapters.Llm (Backend, agents, interpreter, narrator, stub)
import Board.Gameboard (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Num as Q
import Core.Types (EntityId, Id(..), Intent(..))
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (elem, length)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effects (ClockF(..), Deadline(..), InWorld, InterpretF(..), Interpretation(..), NarrateF(..), inWorldOf, runGame)
import Core.Engine (observe)
import Data.Maybe (fromMaybe')
import Partial.Unsafe (unsafeCrashWith)
import Handlers.Live (Seat(..), runLive)
import Handlers.Replay (runReplay)
import Handlers.Seats (programs)
import Policy.Interpret (Draft)
import Core.Draw (Seed(..))
import Scenarios (load)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Game = { world :: WorldState, inWorld :: InWorld }

setup :: Aff Game
setup = liftEffect (load "fractured-world/v0") >>= case _ of
  Left e -> throwError (error e)
  Right { gameboard, config } -> case instantiate gameboard config, inWorldOf gameboard of
    Right initial, Right inWorld -> pure { world: initial.world, inWorld }
    _, _ -> throwError (error "instantiation")

buy :: String -> Int -> Draft
buy market n = { intents: [ Trade { market: Id market, quantity: Q.fromInt n, limitPrice: Nothing } ], signers: [], procedure: Nothing }

json :: forall a. EncodeJson a => a -> String
json = stringify <<< encodeJson

-- | The model stub: canned replies by what the text says.
model :: Backend
model = stub \system input ->
  if contains (Pattern "front page") system then json { text: "Markets moved." }
  else if contains (Pattern "step\":\"propose") input then json { kind: "understood", proposals: [ buy "energy-atlantic" 1 ] }
  else if contains (Pattern "step\":\"sign") input then json { kind: "understood", sign: ([] :: Array String) }
  else if contains (Pattern "buy ten energy") input then json { kind: "understood", proposals: [ buy "energy-atlantic" 10 ] }
  else if contains (Pattern "buy some energy") input then json { kind: "clarify", message: "How much energy, on which market?" }
  else if contains (Pattern "seize the Chinese fleet") input then json { kind: "inadmissible", message: "the US holds no authority over Chinese forces" }
  else if contains (Pattern "build Chinese tanks") input then json { kind: "understood", proposals: [ { intents: [ Transform { by: Id "US", transformation: Id "defense-procurement-cn", quantity: Q.fromInt 1 } ], signers: ([] :: Array EntityId), procedure: (Nothing :: Maybe String) } ] }
  else if contains (Pattern "nothing") input then json { kind: "understood", proposals: ([] :: Array Draft) }
  else "not json"

canonical :: Interpretation -> String
canonical = case _ of
  Understood ds ids -> "understood " <> json ds <> " " <> json ids
  Clarify q -> "clarify " <> q
  Inadmissible r -> "inadmissible " <> r

spec :: Spec Unit
spec = do
  describe "Interpretation suite (§7.4)" do
    let
      run' g text = canonical <$> interpreter model (const (pure unit)) (\e -> Map.lookup e g.world.entities) (Interpret (Id "US") (observe g.world (fromMaybe' (\_ -> unsafeCrashWith "US") (Map.lookup (Id "US") g.world.entities))) text identity)
    it "text → expected canonical Intents" do
      g <- setup
      run' g "buy ten energy" >>= shouldEqual ("understood " <> json [ buy "energy-atlantic" 10 ] <> " []")
    it "ambiguous → clarification" do
      g <- setup
      run' g "buy some energy" >>= shouldEqual "clarify How much energy, on which market?"
      run' g "mumble" >>= shouldEqual "clarify I could not read that as an action. Please restate it."
    it "inadmissible → flagged (by the model, or by the capability check)" do
      g <- setup
      run' g "seize the Chinese fleet" >>= shouldEqual "inadmissible the US holds no authority over Chinese forces"
      run' g "build Chinese tanks" >>= shouldEqual "inadmissible the action needs a capability this Actor does not hold"
  describe "Live play" do
    it "a scripted live session equals replaying its recorded Proposals and signatures" do
      g <- setup
      out <- live g 3 \e -> if e == Id "US" then Human else if e == Id "DE" then Agent else Program
      length out.records `shouldEqual` 3
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true
      out.narration `shouldEqual` [ "Markets moved.", "Markets moved.", "Markets moved." ]
      replayed g 3 out.records `shouldEqual` map _.world out.records
    it "an all-AI-agent game completes five rounds and replays from its records" do
      g <- setup
      out <- live g 5 (const Agent)
      length out.records `shouldEqual` 5
      replayed g 5 out.records `shouldEqual` map _.world out.records
  where
  replayed g n records = map _.world (runReplay (map _.record records) (runGame g.inWorld n g.world)).records

  live :: Game -> Int -> (EntityId -> Seat) -> Aff { records :: Array RoundResult, narration :: Array String }
  live g n seat = do
    recs <- liftEffect (Ref.new [])
    texts <- liftEffect (Ref.new [])
    let
      noLog = const (pure unit)
      -- the US player's script: buy in Round 1, then nothing; confirm every candidate
      ask p
        | p.question /= "" = pure "y"
        | p.window == ProposeDeadline && p.round == 1 = pure "buy ten energy"
        | otherwise = pure "nothing"
    _ <- runLive
      { seat
      , ask
      , interpret: interpreter model noLog (\e -> Map.lookup e g.world.entities)
      , clock: \(Await _ _ a) -> pure a
      , agents: agents model noLog (const "Secure energy at a fair price.")
      , programs: programs { bound: Map.empty, params: g.inWorld.params, home: const (Id "energy-atlantic"), playerSeed: Seed 1 }
      , record: \r -> do
          t <- narrator model noLog (Narrate r identity)
          liftEffect (Ref.modify_ (_ <> [ r ]) recs *> Ref.modify_ (_ <> [ t ]) texts)
      }
      (runGame g.inWorld n g.world)
    { records: _, narration: _ } <$> liftEffect (Ref.read recs) <*> liftEffect (Ref.read texts)
