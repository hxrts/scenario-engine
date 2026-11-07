-- | The model handler (docs/06_simulator.md §14): the prompts each request sends, how a reply decodes,
-- | the joint pass's reconciliation and fallbacks, the evidence each call leaves, and the recording of
-- | exchanges that replays. The model is a stub backend (`Test.Handlers.Stub`); no `pi` process runs.
module Test.Handlers.Llm (spec) where

import Prelude

import Adapters.Filesystem (fileOps)
import Premises.Tree (readPremise)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Proposal (JointProposal, Transcript)
import Core.Engine.Prepare (observe)
import Core.Types (Id(..))
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..), either, isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits as SCU
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Round (runGame)
import Effects.Signatures (InterpretF(..), NarrateF(..))
import Foreign.Object (Object)
import Handlers.Llm (Backend, agents, interpreter, listing, narrator)
import Handlers.Llm.Wire (ProposedDeal, decodeJoint, decodeText, interpretPrompt, narratePrompt, speakPrompt)
import Handlers.Recording (encodeRecording)
import Policy.InWorld (inWorldOf)
import Core.Strategy (InWorld)
import Test.Handlers.Run (meridian, runTest)
import Game.Types (Game)
import Test.Handlers.Stub (answering, backendOf, dealOf, decodeRecording, replayCall, replyJoint, replyText, tradeTenEnergy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Core.Num (Q)

setup :: Aff Game
setup = meridian

tr :: String -> String -> Transcript
tr seat text = { seat: Id seat, text }

-- | A backend that answers every call with the same reply text.
answerWith :: String -> Aff Backend
answerWith text = do
  counter <- liftEffect (Ref.new 0)
  pure (answering counter (\_ _ -> text))

-- | One joint pass over the transcripts.
jointWith :: Game -> Backend -> Array Transcript -> Aff (Either String (Array JointProposal))
jointWith g backend ts = interpreter backend g.world (Interpret 1 ts identity)

-- | The one Proposal a joint pass must return.
single :: Either String (Array JointProposal) -> Aff JointProposal
single = case _ of
  Right [ j ] -> pure j
  Right js -> fail ("expected one Proposal, got " <> show (length js)) *> throwError (error "single")
  Left why -> fail ("the reply should decode: " <> why) *> throwError (error "single")

-- | One round with no seat strategies: its record is what the front page is written from.
oneRound :: Game -> Aff RoundResult
oneRound g = do
  let out = runTest (Seed 1) g (runGame g.inWorld 1 g.world)
  case out.records of
    [ r ] -> pure r
    _ -> fail "one round should be recorded" *> throwError (error "round")

sample :: ProposedDeal
sample = { author: Id "US", intents: [], signers: [ Id "DE" ], agreed: [ Id "US" ] }

spec :: Spec Unit
spec = do
  describe "Prompts" do
    it "each request has its own task: the joint pass reconciles, the narrator writes the front page, a seat speaks" do
      contains (Pattern "reconcile them into Proposals") interpretPrompt `shouldEqual` true
      contains (Pattern "front page") narratePrompt `shouldEqual` true
      contains (Pattern "one to three sentences") speakPrompt `shouldEqual` true
      contains (Pattern "Never invent quantities") narratePrompt `shouldEqual` true

    it "the joint pass's reply schema names every field of a Proposal" do
      contains (Pattern "{\"proposals\":[{\"author\":EntityId,\"intents\":[Intent],\"signers\":[EntityId],\"agreed\":[EntityId]}]}") interpretPrompt `shouldEqual` true

  describe "Reply decoding" do
    it "a joint reply decodes into its Proposals, as the model states them" do
      case decodeJoint ("{\"proposals\":[" <> stringify (encodeJson sample) <> "]}") of
        Right [ d ] -> do
          d.author `shouldEqual` Id "US"
          d.signers `shouldEqual` [ Id "DE" ]
          d.agreed `shouldEqual` [ Id "US" ]
        _ -> fail "one Proposal should decode"

    it "a reply wrapped in prose or a code fence still decodes" do
      decodeJoint "Here is the joint pass:\n```json\n{\"proposals\":[]}\n```" `shouldEqual` Right []

    it "a clarification is not a Proposal list, so it does not decode" do
      isLeft (decodeJoint "{\"kind\":\"clarify\",\"message\":\"How much energy?\"}") `shouldEqual` true

    it "text that is not JSON does not decode" do
      isLeft (decodeJoint "not json") `shouldEqual` true

    it "a narrator or seat reply decodes its text; a reply without text does not" do
      decodeText "{\"text\":\"Markets moved.\"}" `shouldEqual` Right "Markets moved."
      isLeft (decodeText "{\"proposals\":[]}") `shouldEqual` true

  describe "Joint pass (§7.4)" do
    it "two transcripts describing one deal reconcile into one Proposal with both as signers" do
      g <- setup
      backend <- answerWith (replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell DE ten energy", tr "DE" "I buy ten energy from the US" ]
      j.author `shouldEqual` Id "US"
      j.signed `shouldEqual` [ Id "US", Id "DE" ]

    it "a one-sided description keeps only its author as signer" do
      g <- setup
      backend <- answerWith (replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell DE ten energy", tr "DE" "nothing" ]
      j.signed `shouldEqual` [ Id "US" ]

    it "a reply that does not decode is a Left, which the caller turns into an empty submission" do
      g <- setup
      backend <- answerWith "not a Proposal list"
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true

    it "an author who did not speak is dropped" do
      g <- setup
      backend <- answerWith (replyJoint [ dealOf (Id "SA") [ Id "DE" ] [ Id "SA" ] ])
      out <- jointWith g backend [ tr "US" "I sell DE ten energy" ]
      out `shouldEqual` Right []

    it "an empty Intent list, and a signer that is not an Entity, are dropped from the Proposal" do
      g <- setup
      let bare = (dealOf (Id "US") [ Id "NOBODY", Id "DE" ] [ Id "US" ]) { intents = [] }
      empty <- answerWith (replyJoint [ bare ])
      out <- jointWith g empty [ tr "US" "nothing" ]
      out `shouldEqual` Right []
      named <- answerWith (replyJoint [ dealOf (Id "US") [ Id "NOBODY", Id "DE" ] [ Id "US" ] ])
      j <- single =<< jointWith g named [ tr "US" "I sell DE ten energy" ]
      j.draft.signers `shouldEqual` [ Id "DE" ]

    it "a backend failure is fatal to the call: the error propagates with its cause" do
      g <- setup
      let broken = backendOf (\_ _ -> throwError (error "language model backend failed (broken): spawn pi ENOENT"))
      out <- attempt (jointWith g broken [ tr "US" "buy ten energy" ])
      case out of
        Left e -> message e `shouldEqual` "language model backend failed (broken): spawn pi ENOENT"
        Right _ -> fail "expected the backend failure to reach the caller"

  describe "Evidence and recording" do
    it "each call leaves its evidence: the effect, the model, digests of the template and payload, the raw stream by path, and the decode" do
      g <- setup
      evidence <- liftEffect (Ref.new [])
      let
        backend =
          { model: "fake/model"
          , call: \_ _ -> pure { text: replyJoint [], raw: "raw/1.jsonl" }
          , evidence: \e -> Ref.modify_ (_ <> [ e ]) evidence
          , exchange: \_ -> pure unit
          }
      _ <- jointWith g backend [ tr "US" "buy ten energy" ]
      evs <- liftEffect (Ref.read evidence)
      map _.effect evs `shouldEqual` [ "interpret" ]
      map _.decoded evs `shouldEqual` [ "ok" ]
      map (\e -> { m: e.model, raw: e.raw, template: SCU.length e.template, input: SCU.length e.input }) evs
        `shouldEqual` [ { m: "fake/model", raw: "raw/1.jsonl", template: 16, input: 16 } ]

    it "a reply that does not decode is a Left, and its evidence says so" do
      g <- setup
      evidence <- liftEffect (Ref.new [])
      let backend = { model: "fake/model", call: \_ _ -> pure { text: "not json", raw: "" }, evidence: \e -> Ref.modify_ (_ <> [ e ]) evidence, exchange: \_ -> pure unit }
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true
      evs <- liftEffect (Ref.read evidence)
      map (\e -> contains (Pattern "error: ") e.decoded) evs `shouldEqual` [ true ]

    it "a recorded exchange replays: the recording file reads back, and the replayed Proposal equals the live one" do
      g <- setup
      exchanges <- liftEffect (Ref.new [])
      let
        live =
          { model: "live"
          , call: \_ _ -> pure { text: replyJoint [ tradeTenEnergy ], raw: "" }
          , evidence: \_ -> pure unit
          , exchange: \x -> Ref.modify_ (_ <> [ x ]) exchanges
          }
      j1 <- single =<< jointWith g live [ tr "US" "buy ten energy" ]
      rec <- liftEffect (Ref.read exchanges)
      length rec `shouldEqual` 1
      back <- either (\e -> throwError (error ("the recording should decode: " <> e))) pure (decodeRecording (encodeRecording rec))
      j2 <- single =<< jointWith g (backendOf (replayCall back)) [ tr "US" "buy ten energy" ]
      j2.signed `shouldEqual` j1.signed
      j2.author `shouldEqual` j1.author

    it "a failed call is not recorded" do
      g <- setup
      exchanges <- liftEffect (Ref.new [])
      let
        failing =
          { model: "x"
          , call: \_ _ -> throwError (error "down")
          , evidence: \_ -> pure unit
          , exchange: \x -> Ref.modify_ (_ <> [ x ]) exchanges
          }
      _ <- attempt (jointWith g failing [ tr "US" "buy ten energy" ])
      rec <- liftEffect (Ref.read exchanges)
      length rec `shouldEqual` 0

  describe "Front page and AI-agent seats" do
    it "a round whose joint pass did not decode gets the plain listing, and makes no model call" do
      g <- setup
      r <- oneRound g
      counter <- liftEffect (Ref.new 0)
      text <- narrator (answering counter (\_ _ -> replyText "Markets moved.")) true (Narrate r identity)
      text `shouldEqual` listing r
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 0

    it "the model's front page is its text; a reply that does not decode is the plain listing" do
      g <- setup
      r <- oneRound g
      good <- answerWith (replyText "Markets moved.")
      narrated <- narrator good false (Narrate r identity)
      narrated `shouldEqual` "Markets moved."
      bad <- answerWith "not json"
      fallback <- narrator bad false (Narrate r identity)
      fallback `shouldEqual` listing r

    it "a seat speaks its reply text; a reply that does not decode is an empty transcript" do
      g <- setup
      view <- case Map.lookup (Id "DE") g.world.entities of
        Just de -> pure (observe g.world de)
        Nothing -> fail "Germany should be an Entity" *> throwError (error "entity")
      spoken <- answerWith (replyText "I will buy ten energy")
      said <- (agents spoken (const "Secure energy.")).speak (Id "DE") view
      said `shouldEqual` "I will buy ten energy"
      silent <- answerWith "not json"
      none <- (agents silent (const "Secure energy.")).speak (Id "DE") view
      none `shouldEqual` ""
