-- | Phase 8: the interpretation suite (docs/06_simulator.md §7.4) against a stubbed model,
-- | a scripted live session that replays exactly, and an all-AI-agent game.
module Test.Adapters.Live (spec) where

import Prelude

import Adapters.Llm (Backend, agents, interpreter, narrator, stub)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Num as Q
import Core.Types (EntityId, Id(..), Intent(..))
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (elem, filter, length)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Environment (InWorld, inWorldOf)
import Effects.Round (runGame)
import Effects.Signatures (ClockF(..), Deadline(..), InterpretF(..), Interpretation(..), NarrateF(..))
import Core.Engine.Prepare (observe)
import Partial.Unsafe (unsafeCrashWith)
import Handlers.Live (Prompt, Seat(..), runLive)
import Handlers.Replay (runReplay)
import Handlers.Seats (programs)
import Policy.Interpret (Draft)
import Core.Draw (Seed(..))
import Adapters.Scenarios (readScenario)
import Scenarios.Load (load)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

type Game = { world :: WorldState, inWorld :: InWorld }

setup :: Aff Game
setup = liftEffect (readScenario "fractured-world/v0") >>= case _ of
  Left e -> throwError (error e)
  Right s -> case load s of
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
  else if contains (Pattern "sign everything") input then json { kind: "understood", proposals: ([] :: Array Draft), sign: [ "not-a-proposal" ] }
  else if contains (Pattern "nothing") input then json { kind: "understood", proposals: ([] :: Array Draft) }
  else "not json"

canonical :: Interpretation -> String
canonical = case _ of
  Understood ds ids -> "understood " <> json ds <> " " <> json ids
  Clarify q -> "clarify " <> q
  Inadmissible r -> "inadmissible " <> r

type Session = { records :: Array RoundResult, narration :: Array String, notices :: Array String }

-- | The US player's script: buy in Round 1, then nothing; confirm every candidate.
scripted :: Prompt -> Aff String
scripted p
  | p.question /= "" = pure "y"
  | p.window == ProposeDeadline && p.round == 1 = pure "buy ten energy"
  | otherwise = pure "nothing"

-- | The same script, declining every confirmation.
declining :: Prompt -> Aff String
declining p = if p.question /= "" then pure "n" else scripted p

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
    it "sign ids the model names but the Actor may not sign are dropped (view.toSign intersection)" do
      g <- setup
      run' g "sign everything" >>= shouldEqual "understood [] []"
  describe "Backend failures" do
    it "a backend failure is fatal to the call: the error propagates with its cause" do
      g <- setup
      let
        broken = { model: "broken", call: \_ _ -> throwError (error "language model backend failed (broken): spawn pi ENOENT") }
        view = observe g.world (fromMaybe' (\_ -> unsafeCrashWith "US") (Map.lookup (Id "US") g.world.entities))
      out <- attempt (interpreter broken (const (pure unit)) (\e -> Map.lookup e g.world.entities) (Interpret (Id "US") view "buy ten energy" identity))
      case out of
        Left e -> message e `shouldSatisfy` contains (Pattern "spawn pi ENOENT")
        Right _ -> fail "expected the backend failure to reach the caller"
  describe "Live play" do
    it "a scripted live session equals replaying its recorded Proposals and signatures" do
      g <- setup
      out <- live g 3 \e -> if e == Id "US" then Human else if e == Id "DE" then Agent else Program
      length out.records `shouldEqual` 3
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true
      out.narration `shouldEqual` [ "Markets moved.", "Markets moved.", "Markets moved." ]
      replayed g 3 out.records `shouldEqual` map _.world out.records
    it "a declined confirmation drops the candidate and tells the seat" do
      g <- setup
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Program) declining
      -- the NPC and Aggregate Policies still propose; the declined seat's candidate does not
      length (filter (\s -> s.proposal.author == Id "US") (bind out.records _.record.inputs)) `shouldEqual` 0
      out.notices `shouldEqual` [ "Round 1 propose window: 1 item(s) not confirmed, so nothing was submitted" ]
    it "an all-AI-agent game completes five rounds and replays from its records" do
      g <- setup
      out <- live g 5 (const Agent)
      length out.records `shouldEqual` 5
      replayed g 5 out.records `shouldEqual` map _.world out.records
  where
  replayed g n records = either unsafeCrashWith (map _.world <<< _.records) (runReplay n (map _.record records) (runGame g.inWorld n g.world))

  live :: Game -> Int -> (EntityId -> Seat) -> Aff Session
  live g n seat = liveWith g n seat scripted

  liveWith :: Game -> Int -> (EntityId -> Seat) -> (Prompt -> Aff String) -> Aff Session
  liveWith g n seat ask = do
    recs <- liftEffect (Ref.new [])
    texts <- liftEffect (Ref.new [])
    notes <- liftEffect (Ref.new [])
    let noLog = const (pure unit)
    _ <- runLive
      { seat
      , ask
      , notify: \_ text -> liftEffect (Ref.modify_ (_ <> [ text ]) notes)
      , interpret: interpreter model noLog (\e -> Map.lookup e g.world.entities)
      , clock: \(Await _ _ a) -> pure a
      , agents: agents model noLog (const "Secure energy at a fair price.")
      , programs: programs { bound: Map.empty, params: g.inWorld.params, home: const (Just (Id "energy-atlantic")), playerSeed: Seed 1 }
      , record: \r -> do
          t <- narrator model noLog (Narrate r identity)
          liftEffect (Ref.modify_ (_ <> [ r ]) recs *> Ref.modify_ (_ <> [ t ]) texts)
      }
      (runGame g.inWorld n g.world)
    { records: _, narration: _, notices: _ } <$> liftEffect (Ref.read recs) <*> liftEffect (Ref.read texts) <*> liftEffect (Ref.read notes)
