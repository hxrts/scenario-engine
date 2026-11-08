-- | The model handler (docs/06_simulator.md §14): the prompts each request sends, how a reply decodes,
-- | the joint pass's reconciliation and fallbacks, the evidence each call leaves, and the recording of
-- | exchanges that replays. The model is a stub backend (`Test.Handlers.Stub`); no `pi` process runs.
module Test.Handlers.Llm (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Schema.Joint (JointProposal, ProposedDeal, Transcript, proposedDealShape)
import Core.Engine.Prepare (observe)
import Core.Num as Q
import Core.Types (Id(..), Intent, IntentF(..), intentShape)
import Core.World (RoundResult)
import Data.Argonaut.Core (stringify, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length)
import Data.Either (Either(..), either, hush, isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits as SCU
import Effect.Aff (Aff, attempt, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Signatures (InterpretF(..), NarrateF(..))
import Handlers.Llm (Backend, interpreter, listing, narrator, speak)
import Handlers.Llm.Wire (decodeJoint, decodeText, interpretPrompt, jointReplyShape, narratePrompt, speakPrompt)
import Handlers.Recording (encodeRecording)
import Handlers.Seats (playPure, seatEnv)
import Testkit.Meridian (unseated)
import Game.Types (Game)
import Data.Argonaut.Decode (fromJsonString)
import Schema.Decode.Common (decodeError)
import Test.Handlers.Stub (answering, backendOf, dealOf, replayCall, replyJoint, replyText, tradeTenEnergy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Core.Shape (Format(..), encodeWith, prose)
import Foreign.Object as Object

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

-- | A string field of a JSON object payload, read back from its text.
stringField :: String -> String -> Maybe String
stringField key text = do
  json <- hush (jsonParser text)
  o <- toObject json
  Object.lookup key o >>= toString

-- | One round with no seat strategies: its record is what the front page is written from.
oneRound :: Game -> Aff RoundResult
oneRound g = do
  let out = playPure (seatEnv (Seed 1) g) (g).seats g.inWorld 1 g.world
  case out.records of
    [ r ] -> pure r
    _ -> fail "one round should be recorded" *> throwError (error "round")

-- | The Intent the joint prompt renders as its example.
example :: Intent
example = Transfer { from: Id "US", to: Id "DE", resource: Id "M", quantity: Q.fromInt 10 }

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

    it "the joint pass's prompt renders its action list and its reply schema from the shapes" do
      contains (Pattern (prose intentShape.desc)) interpretPrompt `shouldEqual` true
      contains (Pattern "{\"tag\":\"Trade\",\"values\":[") interpretPrompt `shouldEqual` true
      contains (Pattern "{\"Trade\"") interpretPrompt `shouldEqual` false
      contains (Pattern (stringify (encodeWith Snapshot intentShape example))) interpretPrompt `shouldEqual` true
      contains (Pattern (prose jointReplyShape.desc)) interpretPrompt `shouldEqual` true
      contains (Pattern "agreed") interpretPrompt `shouldEqual` true

    it "the joint pass's prompt states the Deal rule screening applies" do
      contains (Pattern "bundles only Transfer and Transform Intents") interpretPrompt `shouldEqual` true

  describe "Reply decoding" do
    it "a joint reply decodes into its Proposals, as the model states them" do
      case decodeJoint ("{\"proposals\":[" <> stringify (encodeWith Snapshot proposedDealShape sample) <> "]}") of
        Right [ d ] -> do
          d.author `shouldEqual` Id "US"
          d.signers `shouldEqual` [ Id "DE" ]
          d.agreed `shouldEqual` [ Id "US" ]
        _ -> fail "one Proposal should decode"

    it "a reply wrapped in prose or a code fence still decodes" do
      decodeJoint "Here is the joint pass:\n```json\n{\"proposals\":[]}\n```" `shouldEqual` Right []

    it "a reply with trailing prose containing a brace decodes its first object" do
      decodeJoint "```json\n{\"proposals\":[]}\n```\nNote: `{}` means none" `shouldEqual` Right []

    it "a brace inside a JSON string does not close the object" do
      decodeText "{\"text\":\"a } and a \\\" quote\"} trailing }" `shouldEqual` Right "a } and a \" quote"

    it "a clarification is not a Proposal list, so it does not decode" do
      isLeft (decodeJoint "{\"kind\":\"clarify\",\"message\":\"How much energy?\"}") `shouldEqual` true

    it "text that is not JSON does not decode" do
      isLeft (decodeJoint "not json") `shouldEqual` true

    it "a narrator or seat reply decodes its text; a reply without text does not" do
      decodeText "{\"text\":\"Markets moved.\"}" `shouldEqual` Right "Markets moved."
      isLeft (decodeText "{\"proposals\":[]}") `shouldEqual` true

  describe "Joint pass (§7.4)" do
    it "two transcripts describing one deal reconcile into one Proposal with both as signers" do
      g <- unseated
      backend <- answerWith (replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell DE ten energy", tr "DE" "I buy ten energy from the US" ]
      j.author `shouldEqual` Id "US"
      j.signed `shouldEqual` [ Id "US", Id "DE" ]

    it "a one-sided description keeps only its author as signer" do
      g <- unseated
      backend <- answerWith (replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell DE ten energy", tr "DE" "nothing" ]
      j.signed `shouldEqual` [ Id "US" ]

    it "a reply that does not decode is a Left, which the caller turns into an empty submission" do
      g <- unseated
      backend <- answerWith "not a Proposal list"
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true

    it "a joint reply that does not decode is corrected: the second call carries the request, the previous reply and the decode error, and its valid reply is the result" do
      g <- unseated
      calls <- liftEffect (Ref.new [])
      let
        backend = backendOf \system input -> do
          liftEffect (Ref.modify_ (_ <> [ { system, input } ]) calls)
          n <- liftEffect (Ref.read calls)
          pure { text: if length n == 1 then "not json" else replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US", Id "DE" ] ], raw: "" }
      j <- single =<< jointWith g backend [ tr "US" "I sell DE ten energy", tr "DE" "I buy ten energy from the US" ]
      j.signed `shouldEqual` [ Id "US", Id "DE" ]
      seen <- liftEffect (Ref.read calls)
      map _.system seen `shouldEqual` [ interpretPrompt, interpretPrompt ]
      case decodeJoint "not json" of
        Left err -> case map _.input seen of
          [ _, second ] -> do
            stringField "error" second `shouldEqual` Just err
            stringField "previous_reply" second `shouldEqual` Just "not json"
            contains (Pattern "\"request\"") second `shouldEqual` true
          _ -> fail "the joint pass should make exactly two calls"
        Right _ -> fail "a reply of plain text should not decode"

    it "a joint reply that still does not decode after two corrections is three calls, then a Left" do
      g <- unseated
      counter <- liftEffect (Ref.new 0)
      out <- jointWith g (answering counter (\_ _ -> "not json")) [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 3

    it "a valid first reply makes exactly one call, with no correction" do
      g <- unseated
      counter <- liftEffect (Ref.new 0)
      _ <- single =<< jointWith g (answering counter (\_ _ -> replyJoint [ dealOf (Id "US") [ Id "DE" ] [ Id "US" ] ])) [ tr "US" "I sell DE ten energy" ]
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 1

    it "an author who did not speak is dropped" do
      g <- unseated
      backend <- answerWith (replyJoint [ dealOf (Id "SA") [ Id "DE" ] [ Id "SA" ] ])
      out <- jointWith g backend [ tr "US" "I sell DE ten energy" ]
      out `shouldEqual` Right []

    it "an empty Intent list, and a signer that is not an Entity, are dropped from the Proposal" do
      g <- unseated
      let bare = (dealOf (Id "US") [ Id "NOBODY", Id "DE" ] [ Id "US" ]) { intents = [] }
      empty <- answerWith (replyJoint [ bare ])
      out <- jointWith g empty [ tr "US" "nothing" ]
      out `shouldEqual` Right []
      named <- answerWith (replyJoint [ dealOf (Id "US") [ Id "NOBODY", Id "DE" ] [ Id "US" ] ])
      j <- single =<< jointWith g named [ tr "US" "I sell DE ten energy" ]
      j.draft.signers `shouldEqual` [ Id "DE" ]

    it "a backend failure is fatal to the call: the error propagates with its cause" do
      g <- unseated
      let broken = backendOf (\_ _ -> throwError (error "language model backend failed (broken): spawn pi ENOENT"))
      out <- attempt (jointWith g broken [ tr "US" "buy ten energy" ])
      case out of
        Left e -> message e `shouldEqual` "language model backend failed (broken): spawn pi ENOENT"
        Right _ -> fail "expected the backend failure to reach the caller"

  describe "Evidence and recording" do
    it "each call leaves its evidence: the effect, the model, digests of the template and payload, the raw stream by path, and the decode" do
      g <- unseated
      evidence <- liftEffect (Ref.new [])
      let
        backend = (backendOf \_ _ -> pure { text: replyJoint [], raw: "raw/1.jsonl" }) { model = "fake/model", evidence = \e -> Ref.modify_ (_ <> [ e ]) evidence }
      _ <- jointWith g backend [ tr "US" "buy ten energy" ]
      evs <- liftEffect (Ref.read evidence)
      map _.effect evs `shouldEqual` [ "interpret" ]
      map _.decoded evs `shouldEqual` [ "ok" ]
      map (\e -> { m: e.model, raw: e.raw, template: SCU.length e.template, input: SCU.length e.input }) evs
        `shouldEqual` [ { m: "fake/model", raw: "raw/1.jsonl", template: 16, input: 16 } ]

    it "a reply that does not decode is a Left, and each attempt's evidence says so" do
      g <- unseated
      evidence <- liftEffect (Ref.new [])
      let backend = (backendOf \_ _ -> pure { text: "not json", raw: "" }) { evidence = \e -> Ref.modify_ (_ <> [ e ]) evidence }
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true
      evs <- liftEffect (Ref.read evidence)
      map (\e -> contains (Pattern "error: ") e.decoded) evs `shouldEqual` [ true, true, true ]

    it "a recorded exchange replays: the recording file reads back, and the replayed Proposal equals the live one" do
      g <- unseated
      exchanges <- liftEffect (Ref.new [])
      let
        live = (backendOf \_ _ -> pure { text: replyJoint [ tradeTenEnergy ], raw: "" }) { exchange = \x -> Ref.modify_ (_ <> [ x ]) exchanges }
      j1 <- single =<< jointWith g live [ tr "US" "buy ten energy" ]
      rec <- liftEffect (Ref.read exchanges)
      length rec `shouldEqual` 1
      back <- either (\e -> throwError (error ("the recording should decode: " <> e))) pure (decodeError (fromJsonString (encodeRecording rec)))
      replay <- liftEffect (replayCall back)
      j2 <- single =<< jointWith g (backendOf replay) [ tr "US" "buy ten energy" ]
      j2.signed `shouldEqual` j1.signed
      j2.author `shouldEqual` j1.author

    it "a failed call leaves evidence naming why, and rethrows" do
      g <- unseated
      evidence <- liftEffect (Ref.new [])
      let failing = (backendOf \_ _ -> throwError (error "down")) { evidence = \e -> Ref.modify_ (_ <> [ e ]) evidence }
      out <- attempt (jointWith g failing [ tr "US" "buy ten energy" ])
      isLeft out `shouldEqual` true
      evs <- liftEffect (Ref.read evidence)
      map _.decoded evs `shouldEqual` [ "error: the call failed: down" ]

    it "a model call that never returns fails at the backend's timeout" do
      g <- unseated
      let hung = (backendOf \_ _ -> makeAff \_ -> pure nonCanceler) { timeoutMs = 5 }
      out <- attempt (jointWith g hung [ tr "US" "buy ten energy" ])
      either (contains (Pattern "timed out") <<< message) (const false) out `shouldEqual` true

    it "a failed call is not recorded" do
      g <- unseated
      exchanges <- liftEffect (Ref.new [])
      let
        failing = (backendOf \_ _ -> throwError (error "down")) { exchange = \x -> Ref.modify_ (_ <> [ x ]) exchanges }
      _ <- attempt (jointWith g failing [ tr "US" "buy ten energy" ])
      rec <- liftEffect (Ref.read exchanges)
      length rec `shouldEqual` 0

  describe "Front page and AI-agent seats" do
    it "a round whose joint pass did not decode gets the plain listing, and makes no model call" do
      g <- unseated
      r <- oneRound g
      counter <- liftEffect (Ref.new 0)
      text <- narrator (answering counter (\_ _ -> replyText "Markets moved.")) true (Narrate r identity)
      text `shouldEqual` listing r
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 0

    it "the model's front page is its text; a reply that does not decode is the plain listing" do
      g <- unseated
      r <- oneRound g
      good <- answerWith (replyText "Markets moved.")
      narrated <- narrator good false (Narrate r identity)
      narrated `shouldEqual` "Markets moved."
      bad <- answerWith "not json"
      fallback <- narrator bad false (Narrate r identity)
      fallback `shouldEqual` listing r

    it "a seat speaks its reply text; a reply that does not decode is an empty transcript" do
      g <- unseated
      view <- case Map.lookup (Id "DE") g.world.entities of
        Just de -> pure (observe g.world de)
        Nothing -> fail "Germany should be an Entity" *> throwError (error "entity")
      spoken <- answerWith (replyText "I will buy ten energy")
      said <- speak spoken (const "Secure energy.") (Id "DE") view
      said `shouldEqual` "I will buy ten energy"
      silent <- answerWith "not json"
      none <- speak silent (const "Secure energy.") (Id "DE") view
      none `shouldEqual` ""
