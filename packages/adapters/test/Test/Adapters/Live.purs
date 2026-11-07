-- | The joint pass and live play (docs/06_simulator.md §6.1, §7.4) against a stubbed model: a
-- | matching pair of transcripts reconciles into one Proposal, a one-sided deal keeps its author
-- | as the only signer, and an undecodable reply submits nothing. No test here calls a real model;
-- | the billed check is `Test.Adapters.LiveInterpret`, which is not part of the suite.
module Test.Adapters.Live (spec) where

import Prelude

import Adapters.Llm (Backend, agents, interpreter, listing, narrator, stub)
import Adapters.Scenarios (readScenario)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (EntityId, Id(..), Intent(..), ProposalId)
import Core.World (Event(..), RoundResult, WorldState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (any, elem, filter, length, mapMaybe)
import Data.Either (Either(..), either, isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Environment (InWorld, inWorldOf)
import Effects.Round (runGame)
import Effects.Signatures (ClockF(..), Deadline(..), InterpretF(..), JointProposal, NarrateF(..), Transcript)
import Handlers.Live (Prompt, Seat(..), runLive)
import Handlers.Replay (runReplay)
import Handlers.Seats (programs)
import Partial.Unsafe (unsafeCrashWith)
import Scenarios.Load (load)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type Game = { world :: WorldState, inWorld :: InWorld }

setup :: Aff Game
setup = liftEffect (readScenario "meridian/v0") >>= case _ of
  Left e -> throwError (error e)
  Right s -> case load s of
    Left e -> throwError (error e)
    Right { gameboard, config } -> case instantiate gameboard config, inWorldOf gameboard of
      Right initial, Right inWorld -> pure { world: initial.world, inWorld }
      _, _ -> throwError (error "instantiation")

json :: forall a. EncodeJson a => a -> String
json = stringify <<< encodeJson

tr :: String -> String -> Transcript
tr seat text = { seat: Id seat, text }

-- | The reply that reconciles a deal: `author` offers Transfers to `signers`, and `agreed` names
-- | the seats whose transcripts describe it.
dealReply :: EntityId -> Array EntityId -> Array EntityId -> String
dealReply author signers agreed = json
  { proposals: [ { author, intents: [ Transfer { from: author, to: Id "DE", resource: Id "E", quantity: Q.fromInt 10 } ], signers, agreed } ] }

tradeTenEnergy :: String
tradeTenEnergy = json
  { proposals:
      [ { author: Id "US"
        , intents: [ Trade { market: Id "energy-atlantic", quantity: Q.fromInt 10, limitPrice: Nothing } ]
        , signers: ([] :: Array EntityId)
        , agreed: [ Id "US" ]
        }
      ]
  }

-- | One joint pass over the given transcripts with a fixed reply.
jointWith :: Game -> Backend -> Array Transcript -> Aff (Either String (Array JointProposal))
jointWith g backend ts = interpreter backend (const (pure unit)) g.world (Interpret 1 ts identity)

fixed :: String -> Backend
fixed reply = stub \_ _ -> reply

-- | The one Proposal a joint pass must return.
single :: Either String (Array JointProposal) -> Aff JointProposal
single = case _ of
  Right [ j ] -> pure j
  Right js -> fail ("expected one Proposal, got " <> show (length js)) *> throwError (error "single")
  Left why -> fail ("the reply should decode: " <> why) *> throwError (error "single")

-- | The stubbed model for the live sessions: a trade by US when its transcript names it, no
-- | Proposals otherwise, and a one-line transcript from every AI seat.
model :: Backend
model = stub \system input ->
  if contains (Pattern "front page") system then json { text: "Markets moved." }
  else if contains (Pattern "one to three sentences") system then json { text: "nothing" }
  else if contains (Pattern "buy ten energy") input then tradeTenEnergy
  else "{\"proposals\":[]}"

type Session = { records :: Array RoundResult, narration :: Array String, notices :: Array String }

-- | The US player's script: buy in Round 1, then nothing.
scripted :: Prompt -> Aff String
scripted p
  | p.round == 1 && p.window == ProposeDeadline = pure "buy ten energy"
  | otherwise = pure "nothing"

-- | A live session of `n` rounds with the given seats, human answers, and model.
liveWith :: Game -> Int -> (EntityId -> Seat) -> (Prompt -> Aff String) -> Backend -> Aff Session
liveWith g n seat ask backend = do
  recs <- liftEffect (Ref.new [])
  texts <- liftEffect (Ref.new [])
  notes <- liftEffect (Ref.new [])
  let noLog = const (pure unit)
  _ <- runLive
    { seat
    , ask
    , notify: \_ text -> liftEffect (Ref.modify_ (_ <> [ text ]) notes)
    , interpret: interpreter backend noLog g.world
    , clock: \(Await _ _ a) -> pure a
    , agents: agents backend noLog (const "Secure energy at a fair price.")
    , programs: programs { bound: Map.empty, params: g.inWorld.params, home: const (Just (Id "energy-atlantic")), playerSeed: Seed 1 }
    , record: \r failed -> do
        t <- narrator backend noLog failed (Narrate r identity)
        liftEffect (Ref.modify_ (_ <> [ r ]) recs *> Ref.modify_ (_ <> [ t ]) texts)
    }
    (runGame g.inWorld n g.world)
  { records: _, narration: _, notices: _ } <$> liftEffect (Ref.read recs) <*> liftEffect (Ref.read texts) <*> liftEffect (Ref.read notes)

-- | Replay the recorded rounds: the same worlds, with no model call.
replayed :: Game -> Int -> Array RoundResult -> Array WorldState
replayed g n records = either unsafeCrashWith (map _.world <<< _.records) (runReplay n (map _.record records) (runGame g.inWorld n g.world))

lapsed :: ProposalId -> RoundResult -> Boolean
lapsed pid r = any
  ( \ev -> case ev of
      Lapsed l -> l.proposal == pid
      _ -> false
  )
  r.record.events

spec :: Spec Unit
spec = do
  describe "Joint pass (§7.4)" do
    it "two transcripts describing one deal reconcile into one Proposal with both as signers" do
      g <- setup
      let
        both = dealReply (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ]
      j <- single =<< jointWith g (fixed both) [ tr "US" "I sell DE ten energy", tr "DE" "I buy ten energy from the US" ]
      j.author `shouldEqual` Id "US"
      j.signed `shouldEqual` [ Id "US", Id "DE" ]

    it "a one-sided description keeps only its author as signer" do
      g <- setup
      let
        one = dealReply (Id "US") [ Id "DE" ] [ Id "US" ]
      j <- single =<< jointWith g (fixed one) [ tr "US" "I sell DE ten energy", tr "DE" "nothing" ]
      j.signed `shouldEqual` [ Id "US" ]

    it "a reply that does not decode is a failure of the pass, which the caller turns into an empty submission" do
      g <- setup
      out <- jointWith g (fixed "not json") [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true

    it "a clarification-shaped reply is not a Proposal list, so nothing is submitted" do
      g <- setup
      out <- jointWith g (fixed "{\"kind\":\"clarify\",\"message\":\"How much energy?\"}") [ tr "US" "buy some energy" ]
      isLeft out `shouldEqual` true

    it "an author who did not speak is dropped" do
      g <- setup
      out <- jointWith g (fixed (dealReply (Id "SA") [ Id "DE" ] [ Id "SA" ])) [ tr "US" "I sell DE ten energy" ]
      out `shouldEqual` Right []

  describe "Backend failures" do
    it "a backend failure is fatal to the call: the error propagates with its cause" do
      g <- setup
      let
        broken = { model: "broken", call: \_ _ -> throwError (error "language model backend failed (broken): spawn pi ENOENT") }
      out <- attempt (jointWith g broken [ tr "US" "buy ten energy" ])
      case out of
        Left e -> message e `shouldEqual` "language model backend failed (broken): spawn pi ENOENT"
        Right _ -> fail "expected the backend failure to reach the caller"

  describe "Live play" do
    it "a scripted live session equals replaying its recorded Proposals and signatures" do
      g <- setup
      out <- liveWith g 3 (\e -> if e == Id "US" then Human else if e == Id "DE" then Agent else Program) scripted model
      length out.records `shouldEqual` 3
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true
      out.narration `shouldEqual` [ "Markets moved.", "Markets moved.", "Markets moved." ]
      replayed g 3 out.records `shouldEqual` map _.world out.records

    it "an undecodable joint reply submits nothing, tells the speaker, and the run completes" do
      g <- setup
      let
        garbled = stub \_ input -> if contains (Pattern "buy ten energy") input then "not json" else "{\"proposals\":[]}"
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Program) scripted garbled
      length out.records `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` false
      map (contains (Pattern "so nothing was submitted")) out.notices `shouldEqual` [ true ]

    it "an undecodable joint reply: nothing is submitted, the front page is the plain listing, and the round's model call to narrate is not made" do
      g <- setup
      calls <- liftEffect (Ref.new 0)
      let
        garbled = stub \_ input -> if contains (Pattern "buy ten energy") input then "not json" else "{\"proposals\":[]}"
        counted = garbled
          { call = \system input -> do
              if contains (Pattern "front page") system then liftEffect (Ref.modify_ (_ + 1) calls) else pure unit
              garbled.call system input
          }
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Program) scripted counted
      length out.records `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` false
      map (contains (Pattern "so nothing was submitted")) out.notices `shouldEqual` [ true ]
      map listing out.records `shouldEqual` out.narration
      map (contains (Pattern "Round 1:")) out.narration `shouldEqual` [ true ]
      modelCalls <- liftEffect (Ref.read calls)
      modelCalls `shouldEqual` 0

    it "an unknown market in a joint reply is a rejected Proposal; the round's other Proposals still execute" do
      g <- setup
      let
        mixed = stub \system input ->
          if contains (Pattern "front page") system then json { text: "Markets moved." }
          else if contains (Pattern "one to three sentences") system then json { text: "nothing" }
          else if contains (Pattern "buy ten energy") input then json
            { proposals:
                [ { author: Id "US", intents: [ Trade { market: Id "energy-atlantic", quantity: Q.fromInt 10, limitPrice: Nothing } ], signers: ([] :: Array EntityId), agreed: [ Id "US" ] }
                , { author: Id "US", intents: [ Trade { market: Id "no-such-market", quantity: Q.fromInt 10, limitPrice: Nothing } ], signers: ([] :: Array EntityId), agreed: [ Id "US" ] }
                ]
            }
          else "{\"proposals\":[]}"
      out <- liveWith g 1 (\e -> if e == Id "US" then Human else Program) scripted mixed
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
      map (contains (Pattern "no-such-market") <<< _.reason) rejections' `shouldEqual` [ true ]
      length executed `shouldEqual` 1
      (Id "r1/US/0" `elem` map _.proposal.id (bind out.records _.record.inputs)) `shouldEqual` true

    it "a one-sided deal lapses; a deal both sides describe is signed by both" do
      g <- setup
      let
        deal = stub \system input ->
          if contains (Pattern "front page") system then json { text: "Markets moved." }
          else if contains (Pattern "I agree") input then dealReply (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ]
          else if contains (Pattern "I sell") input then dealReply (Id "US") [ Id "DE" ] [ Id "US" ]
          else "{\"proposals\":[]}"

        says :: Prompt -> Aff String
        says p = pure (if p.seat == Id "US" then "I sell DE ten energy" else "nothing")
      one <- liveWith g 1 (const Human) says deal
      map (lapsed (Id "r1/US/0")) one.records `shouldEqual` [ true ]
      let
        agreeing :: Prompt -> Aff String
        agreeing p = pure (if p.seat == Id "US" then "I sell DE ten energy" else "I agree")
      both <- liveWith g 1 (const Human) agreeing deal
      map (lapsed (Id "r1/US/0")) both.records `shouldEqual` [ false ]
      let
        signedBy = map _.signatures (filter (\s -> s.proposal.id == Id "r1/US/0") (bind both.records _.record.inputs))
      signedBy `shouldEqual` [ [ Id "US", Id "DE" ] ]

    it "an all-AI-agent game completes five rounds and replays from its records" do
      g <- setup
      out <- liveWith g 5 (const Agent) (const (pure "")) model
      length out.records `shouldEqual` 5
      replayed g 5 out.records `shouldEqual` map _.world out.records
