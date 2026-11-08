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
import Core.Slice (event, matches, pickOf)
import Core.World (RoundRecord, RoundResult, WorldState, renderReason)
import Data.Array (any, elem, filter, length, mapMaybe)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effects.Signatures (NarrateF(..))
import Handlers.Llm (Backend, interpreter, listing, narrator, speak)
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
    , window: Nothing
    , say: \_ -> pure unit
    , notify: \_ text -> liftEffect (Ref.modify_ (_ <> [ text ]) notes)
    , interpret: interpreter backend g.world
    , speak: speak backend (const "Secure energy at a fair price.")
    , narrate: \failed r -> do
        t <- narrator backend failed (Narrate r identity)
        liftEffect (Ref.modify_ (_ <> [ t ]) texts)
        pure t
    , record: \r _ -> Ref.modify_ (_ <> [ r ]) recs
    }
    (g { rounds = n })
  { records: _, narration: _, notices: _ } <$> liftEffect (Ref.read recs) <*> liftEffect (Ref.read texts) <*> liftEffect (Ref.read notes)

-- | Replay the recorded rounds: the same worlds, with no model call.
replayedRecords :: Game -> Int -> Array RoundResult -> Array RoundRecord
replayedRecords g n records = case runReplay (seatEnv (Seed 1) g) n (map _.record records) g of
  Left e -> unsafeCrashWith e
  Right r -> map _.record r.records

replayed :: Game -> Int -> Array RoundResult -> Array WorldState
replayed g n records = case runReplay (seatEnv (Seed 1) g) n (map _.record records) g of
  Left e -> unsafeCrashWith e
  Right r -> map _.world r.records

lapsed :: ProposalId -> RoundResult -> Boolean
lapsed pid r = any
  (matches (event @"Lapsed") (\l -> l.proposal == pid))
  r.record.events

spec :: Spec Unit
spec = do
  describe "Live play" do
    it "a scripted live session equals replaying its recorded Proposals and signatures" do
      g <- unseated
      backend <- withAnswers model
      out <- liveWith g 3 (\e -> if e == Id "US" then Human else if e == Id "DE" then Agent else Script []) scripted backend
      length out.records `shouldEqual` 3
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true
      out.narration `shouldEqual` [ "Markets moved.", "Markets moved.", "Markets moved." ]
      replayed g 3 out.records `shouldEqual` map _.world out.records
      replayedRecords g 3 out.records `shouldEqual` map _.record out.records

    it "an undecodable joint reply submits nothing, tells the speaker, and the run completes" do
      g <- unseated
      backend <- withAnswers garbled
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` false
      map (contains (Pattern "so nothing was submitted")) out.notices `shouldEqual` [ true ]

    it "an undecodable joint reply: the front page is the plain listing, and only the joint pass calls the model (its first call and two corrections)" do
      g <- unseated
      counter <- liftEffect (Ref.new 0)
      backend <- pure (answering counter garbled)
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      map listing out.records `shouldEqual` out.narration
      map (contains (Pattern "Round 1:")) out.narration `shouldEqual` [ true ]
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 3

    it "an unknown market in a joint reply is a rejected Proposal; the round's other Proposals still execute" do
      g <- unseated
      let
        bad = tradeTenEnergy { intents = [ Trade { market: Id "no-such-market", quantity: Q.fromInt 10, limitPrice: Nothing } ] }
      backend <- withAnswers (jointAnswers [ tradeTenEnergy, bad ])
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Script []) scripted backend
      length out.records `shouldEqual` 1
      let
        events = bind out.records (_.record.events)
        rejections' = mapMaybe
          (pickOf (event @"Rejected") >=> \r -> Just { proposal: r.proposal, reason: r.reason })
          events
        executed = filter
          (matches (event @"Traded") (\t -> t.entity == Id "US" && t.market == Id "energy-atlantic"))
          events
      map _.proposal rejections' `shouldEqual` [ Id "r1/US/1" ]
      map (contains (Pattern "no-such-market") <<< renderReason <<< _.reason) rejections' `shouldEqual` [ true ]
      length executed `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true

    it "a one-sided deal lapses; a deal both sides describe is signed by both" do
      g <- unseated
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
      signedBy `shouldEqual` [ [ Id "DE", Id "US" ] ]
      replayedRecords g 1 both.records `shouldEqual` map _.record both.records

    it "two humans are both prompted in one window, and a human not reached before it closes is silent" do
      g <- unseated
      asked <- liftEffect (Ref.new [])
      closing <- AVar.empty
      backend <- withAnswers model
      let
        base = g { seats = Map.fromFoldable (map (\e -> Tuple e (if e == Id "US" || e == Id "DE" then Human else Script [])) (Array.fromFoldable (Map.keys g.world.entities))), rounds = 1 }
        -- DE is asked first: its answer closes the window, so US (asked later) is not reached
        input prompt = do
          liftEffect (Ref.modify_ (_ <> [ prompt ]) asked)
          _ <- AVar.tryPut unit closing
          delay (Milliseconds 20.0)
          pure "nothing"
      _ <- runLive
        { seed: Seed 1
        , input
        , window: Just (AVar.read closing)
        , say: \_ -> pure unit
        , notify: \_ _ -> pure unit
        , interpret: interpreter backend g.world
        , speak: speak backend (const "")
        , narrate: \failed r -> narrator backend failed (Narrate r identity)
        , record: \_ _ -> pure unit
        }
        base
      prompts <- liftEffect (Ref.read asked)
      map (contains (Pattern "DE>")) prompts `shouldEqual` [ true ]
      asked' <- liftEffect (Ref.new [])
      let answersAll prompt = liftEffect (Ref.modify_ (_ <> [ prompt ]) asked') $> "nothing"
      _ <- liveWith g 1 (\e -> if e == Id "US" || e == Id "DE" then Human else Script []) answersAll backend
      map (\p -> contains (Pattern "DE>") p || contains (Pattern "US>") p) <$> liftEffect (Ref.read asked') >>= (_ `shouldEqual` [ true, true ])

    it "an all-AI-agent game completes five rounds and replays from its records" do
      g <- unseated
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
