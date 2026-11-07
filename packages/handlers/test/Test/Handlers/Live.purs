-- | Live play (docs/06_simulator.md §7.4) against a stub model: the human seats type, the AI seats
-- | speak, the round's transcripts go through one joint pass, and the front page is narrated. The stub
-- | model answers each call with a typed reply encoded as its text, so the prompts, decoding, and
-- | fallbacks of `Handlers.Llm` run as they do in production. No test calls a real model; the billed
-- | check is `Test.Adapters.LiveInterpret`, which is not part of the suite.
module Test.Handlers.Live (spec) where

import Prelude

import Testkit.Meridian (unseated)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Core.Draw (Seed(..))
import Core.Types (EntityId, Id(..), IntentF(..), ProposalId)
import Core.World (Event(..), RoundResult, WorldState, renderReason)
import Data.Array (any, elem, filter, length, mapMaybe)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effects.Round (runGame)
import Effects.Signatures (NarrateF(..))
import Handlers.Llm (Backend, agents, interpreter, listing, narrator)
import Handlers.Llm.Wire (interpretPrompt, narratePrompt)
import Core.Proposal (ProposedDeal)
import Handlers.Live (runLive)
import Handlers.Seats (seatEnv)
import Game.Types (Game, SeatBinding(..))
import Handlers.Replay (runReplay)
import Partial.Unsafe (unsafeCrashWith)
import Test.Handlers.Stub (answering, dealOf, replyJoint, replyText, tradeTenEnergy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Core.Num as Q

setup :: Aff Game
setup = unseated

-- | The stub model for live play: the joint pass proposes a trade when a transcript names one, the
-- | narrator writes a fixed front page, and each AI seat says one line.
model :: String -> String -> String
model template input
  | template == interpretPrompt = replyJoint (if contains (Pattern "buy ten energy") input then [ tradeTenEnergy ] else [])
  | template == narratePrompt = replyText "Markets moved."
  | otherwise = replyText "nothing"

-- | A stub model answering `answer`, with the number of calls it has been asked.
withAnswers :: (String -> String -> String) -> Aff Backend
withAnswers answer = do
  counter <- liftEffect (Ref.new 0)
  pure (answering counter answer)

-- | The model's answers when the joint pass proposes `ps` and the rest answer as `model` does.
jointAnswers :: Array ProposedDeal -> String -> String -> String
jointAnswers ps template input
  | template == interpretPrompt = replyJoint ps
  | otherwise = model template input

type Session = { records :: Array RoundResult, narration :: Array String, notices :: Array String }

-- | The US player's script: buy in Round 1, then nothing.
scripted :: String -> Aff String
scripted prompt
  | contains (Pattern "[round 1, propose window]") prompt = pure "buy ten energy"
  | otherwise = pure "nothing"

-- | A live session of `n` rounds with the given seats and human answers.
liveWith :: Game -> Int -> (EntityId -> SeatBinding) -> (String -> Aff String) -> Backend -> Aff Session
liveWith g0 n seat input backend = do
  recs <- liftEffect (Ref.new [])
  texts <- liftEffect (Ref.new [])
  notes <- liftEffect (Ref.new [])
  let g = g0 { seats = Map.fromFoldable (map (\e -> Tuple e (seat e)) (Map.keys g0.world.entities # Array.fromFoldable)) }
  _ <- runLive
    { seed: Seed 1
    , input
    , window: pure unit
    , say: \_ -> pure unit
    , notify: \_ text -> liftEffect (Ref.modify_ (_ <> [ text ]) notes)
    , interpret: interpreter backend g.world
    , agents: agents backend (const "Secure energy at a fair price.")
    , narrate: \failed r -> do
        t <- narrator backend failed (Narrate r identity)
        liftEffect (Ref.modify_ (_ <> [ t ]) texts)
        pure t
    , record: \r _ -> liftEffect (Ref.modify_ (_ <> [ r ]) recs)
    }
    (g { rounds = n })
  { records: _, narration: _, notices: _ } <$> liftEffect (Ref.read recs) <*> liftEffect (Ref.read texts) <*> liftEffect (Ref.read notes)

-- | Replay the recorded rounds: the same worlds, with no model call.
replayed :: Game -> Int -> Array RoundResult -> Array WorldState
replayed g n records = case runReplay (seatEnv (Seed 1) g) n (map _.record records) (runGame g.inWorld n g.world) of
  Left e -> unsafeCrashWith e
  Right r -> map _.world r.records

lapsed :: ProposalId -> RoundResult -> Boolean
lapsed pid r = any
  ( \ev -> case ev of
      Lapsed l -> l.proposal == pid
      _ -> false
  )
  r.record.events

spec :: Spec Unit
spec = do
  describe "Live play" do
    it "a scripted live session equals replaying its recorded Proposals and signatures" do
      g <- setup
      backend <- withAnswers model
      out <- liveWith g 3 (\e -> if e == Id "US" then Human else if e == Id "DE" then Agent else Script []) scripted backend
      length out.records `shouldEqual` 3
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true
      out.narration `shouldEqual` [ "Markets moved.", "Markets moved.", "Markets moved." ]
      replayed g 3 out.records `shouldEqual` map _.world out.records

    it "an undecodable joint reply submits nothing, tells the speaker, and the run completes" do
      g <- setup
      backend <- withAnswers garbled
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` false
      map (contains (Pattern "so nothing was submitted")) out.notices `shouldEqual` [ true ]

    it "an undecodable joint reply: the front page is the plain listing, and only the joint pass calls the model" do
      g <- setup
      counter <- liftEffect (Ref.new 0)
      backend <- pure (answering counter garbled)
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      map listing out.records `shouldEqual` out.narration
      map (contains (Pattern "Round 1:")) out.narration `shouldEqual` [ true ]
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 1

    it "an unknown market in a joint reply is a rejected Proposal; the round's other Proposals still execute" do
      g <- setup
      let
        bad = tradeTenEnergy { intents = [ Trade { market: Id "no-such-market", quantity: Q.fromInt 10, limitPrice: Nothing } ] }
      backend <- withAnswers (jointAnswers [ tradeTenEnergy, bad ])
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      let
        events = bind out.records (_.record.events)
        rejections' = mapMaybe
          ( \ev -> case ev of
              Rejected r -> Just { proposal: r.proposal, reason: r.reason }
              _ -> Nothing
          )
          events
        executed = filter
          ( \ev -> case ev of
              Traded t -> t.entity == Id "US" && t.market == Id "energy-atlantic"
              _ -> false
          )
          events
      map _.proposal rejections' `shouldEqual` [ Id "r1/US/1" ]
      map (contains (Pattern "no-such-market") <<< renderReason <<< _.reason) rejections' `shouldEqual` [ true ]
      length executed `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true

    it "a one-sided deal lapses; a deal both sides describe is signed by both" do
      g <- setup
      let
        deal t i
          | t == interpretPrompt && contains (Pattern "I agree") i = replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ] ]
          | t == interpretPrompt && contains (Pattern "I sell") i = replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US" ] ]
          | t == interpretPrompt = replyJoint []
          | otherwise = model t i

        says :: String -> Aff String
        says prompt = pure (if contains (Pattern "US>") prompt then "I sell DE ten energy" else "nothing")
      backendOne <- withAnswers deal
      one <- liveWith g 1 (const Human) says backendOne
      map (lapsed (Id "r1/US/0")) one.records `shouldEqual` [ true ]
      let
        agreeing :: String -> Aff String
        agreeing prompt = pure (if contains (Pattern "US>") prompt then "I sell DE ten energy" else "I agree")
      backendBoth <- withAnswers deal
      both <- liveWith g 1 (const Human) agreeing backendBoth
      map (lapsed (Id "r1/US/0")) both.records `shouldEqual` [ false ]
      let
        signedBy = map _.signatures (filter (\s -> s.proposal.id == Id "r1/US/0") (bind both.records _.record.inputs))
      signedBy `shouldEqual` [ [ Id "US", Id "DE" ] ]

    it "an all-AI-agent game completes five rounds and replays from its records" do
      g <- setup
      backend <- withAnswers model
      out <- liveWith g 5 (const Agent) (const (pure "")) backend
      length out.records `shouldEqual` 5
      replayed g 5 out.records `shouldEqual` map _.world out.records
  where
  -- a joint pass that does not decode, and a narrator and seats that answer normally
  garbled :: String -> String -> String
  garbled template input
    | template == interpretPrompt = if contains (Pattern "buy ten energy") input then "not json" else replyJoint []
    | template == narratePrompt = "not json"
    | otherwise = replyText "nothing"
