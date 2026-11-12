-- | The model handler (docs/07_simulator.md §14): the prompts each request sends, how a reply decodes,
-- | the joint pass's reconciliation and fallbacks, the evidence each call leaves, and the recording of
-- | turns that replays. The model is a stub backend (`Test.Handlers.Stub`); no `pi` process runs.
-- | Pins: the model wire and reply JSON of docs/07 §14 (the prompt text is read through the prompt names).
module Test.Handlers.Llm (spec) where

import Prelude

import Core.Shape (Desc(..), kindsNamed, Format(..), descKeys, encodeWith, prose)
import Data.Foldable (sum, elem, for_)

import Handlers.Llm.Joint (jointRequest, interpreter)
import Data.Array (sort, fromFoldable, length, mapMaybe)
import Data.Map as Map
import Foreign.Object as Object
import Laws.Types (idText, Id(..), Intent, IntentF(..), intentShape)
import Data.Maybe (Maybe(..), maybe)
import Effect.Ref as Ref
import Effect.Class (liftEffect)

import Control.Monad.Error.Class (throwError)
import Schema.Joint (JointCandidateTransaction, ProposedJoint, Transcript, jointReplyShape, proposedJointShape)
import Laws.Engine.Prepare (observe)
import Core.Num as Q
import Data.Argonaut.Core (stringify, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either, hush, isLeft)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Array.NonEmpty as NEA
import Data.String.CodeUnits as SCU
import Effect.Aff (Aff, attempt, makeAff, nonCanceler)
import Effect.Exception (error, message)
import Handlers.Llm.Backend (Backend)
import Handlers.Llm.Speak (speak)
import Handlers.Llm.Wire (decodeJoint, decodeText)
import Handlers.Llm.Call (interpretCall, speakCall)
import Handlers.Llm.Prompt (interpretPrompt, speakPrompt)
import Handlers.Recording (encodeRecording)
import Testkit.Run (unseated, pacificParts)
import Assembly.Types (PreparedRun)
import Test.Handlers.Stub (answering, backendOf, decodeRecording, jointOf, replayCall, replyJoint, replyText, tradeTenEnergy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

tr :: String -> String -> Transcript
tr seat text = { seat: Id seat, text, enteredBy: Nothing }

-- | A backend that answers every call with the same reply text.
answerWith :: String -> Aff Backend
answerWith text = do
  counter <- liftEffect (Ref.new 0)
  pure (answering counter (\_ _ -> text))

-- | One joint pass over the transcripts.
jointWith :: PreparedRun -> Backend -> Array Transcript -> Aff (Either String (Array JointCandidateTransaction))
jointWith g backend ts = interpreter backend g.world 1 ts

-- | The one candidate Transaction a joint pass must return.
single :: Either String (Array JointCandidateTransaction) -> Aff JointCandidateTransaction
single = case _ of
  Right [ j ] -> pure j
  Right js -> fail ("expected one CandidateTransaction, got " <> show (length js)) *> throwError (error "single")
  Left why -> fail ("the reply should decode: " <> why) *> throwError (error "single")

-- | A string field of a JSON object payload, read back from its text.
stringField :: String -> String -> Maybe String
stringField key text = do
  json <- hush (jsonParser text)
  o <- toObject json
  Object.lookup key o >>= toString

-- | The Intent the joint prompt renders as its example.
example :: Intent
example = Transfer { from: Id "US", to: Id "CN", resource: Id "M", quantity: Q.fromInt 10 }

sample :: ProposedJoint
sample = { author: Id "US", intents: [], signers: [ Id "CN" ], agreed: [ Id "US" ] }

-- | How many ids in a description do not say what they name.
plainIds :: Desc -> Int
plainIds = case _ of
  DPrim "Id" -> 1
  DArray x -> plainIds x
  DOpt x -> plainIds x
  DKeyed x -> plainIds x
  DTuple a b -> plainIds a + plainIds b
  DRec _ fs -> sum (map (plainIds <<< _.shape) fs)
  DSum _ _ cs -> sum (map (plainIds <<< _.payload) cs)
  _ -> 0

spec :: Spec Unit
spec = do
  describe "the model call descriptions" do
    it "every backticked name in a prompt is a key of its call's shapes" do
      let names p = maybe [] (mapMaybe (map (\m -> String.take (String.length m - 2) (String.drop 1 m))) <<< NEA.toArray) (Regex.match (unsafeRegex "`[a-zA-Z_]+`" global) p)
      for_
        [ { prompt: interpretCall.prompt, keys: descKeys interpretCall.request.desc <> descKeys interpretCall.wire.desc }
        , { prompt: speakCall.prompt, keys: descKeys speakCall.request.desc <> descKeys speakCall.wire.desc }
        ]
        \c -> for_ (names c.prompt) \n -> elem n c.keys `shouldEqual` true
  describe "Prompts" do
    it "each request has its own task: the joint pass reconciles, a seat speaks" do
      contains (Pattern "reconcile them into CandidateTransactions") interpretPrompt `shouldEqual` true
      contains (Pattern "one to three sentences") speakPrompt `shouldEqual` true

    it "the joint pass's prompt renders its action list and its reply schema from the shapes" do
      contains (Pattern (prose intentShape.desc)) interpretPrompt `shouldEqual` true
      contains (Pattern "{\"tag\":\"Trade\",\"values\":[") interpretPrompt `shouldEqual` true
      contains (Pattern "{\"Trade\"") interpretPrompt `shouldEqual` false
      contains (Pattern (stringify (encodeWith Snapshot intentShape example))) interpretPrompt `shouldEqual` true
      contains (Pattern (prose jointReplyShape.desc)) interpretPrompt `shouldEqual` true
      contains (Pattern "agreed") interpretPrompt `shouldEqual` true

    it "the joint pass's prompt states the joint CandidateTransaction rule screening applies" do
      contains (Pattern "bundles only Transfer and Transform Intents") interpretPrompt `shouldEqual` true

  describe "Reply decoding" do
    it "a joint reply decodes into its CandidateTransactions, as the model states them" do
      case decodeJoint ("{\"candidateTransactions\":[" <> stringify (encodeWith Snapshot proposedJointShape sample) <> "]}") of
        Right [ d ] -> do
          d.author `shouldEqual` Id "US"
          d.signers `shouldEqual` [ Id "CN" ]
          d.agreed `shouldEqual` [ Id "US" ]
        _ -> fail "one CandidateTransaction should decode"

    it "the joint pass's prompt requires the exact ids its payload lists" do
      contains (Pattern "exactly as `names` lists it") interpretPrompt `shouldEqual` true

    it "every id a reply can hold says what it names, except one the engine assigns" do
      plainIds interpretCall.wire.desc `shouldEqual` 0
      sort (kindsNamed interpretCall.wire.desc) `shouldEqual` [ "actor", "exchange", "resource", "rule kind", "template", "transformation" ]

    it "the payload names every kind a reply can hold, from the World played" do
      g <- pacificParts >>= unseated
      let request = jointRequest g.world 1 []
      sort (Object.keys request.names) `shouldEqual` sort (kindsNamed interpretCall.wire.desc)
      Object.lookup "resource" request.names `shouldEqual` Just (map idText (fromFoldable (Map.keys g.world.resources)))

    it "a reply naming what the World lacks gets a correction, and the corrected reply stands" do
      g <- pacificParts >>= unseated
      calls <- liftEffect (Ref.new 0)
      let
        wrong = (jointOf (Id "US") [ Id "CN" ] [ Id "US", Id "CN" ]) { intents = [ Transfer { from: Id "US", to: Id "CN", resource: Id "Energy", quantity: Q.fromInt 10 } ] }
        right = jointOf (Id "US") [ Id "CN" ] [ Id "US", Id "CN" ]
        answer _ input = if contains (Pattern "unknown resource Energy; use one of") input then replyJoint [ right ] else replyJoint [ wrong ]
      j <- single =<< jointWith g (answering calls answer) [ tr "US" "I give CN ten Energy", tr "CN" "I take ten Energy" ]
      j.draft.intents `shouldEqual` right.intents
      liftEffect (Ref.read calls) >>= (_ `shouldEqual` 2)

    it "a reply still naming what the World lacks after its corrections stands, for the engine to refuse" do
      g <- pacificParts >>= unseated
      let wrong = (jointOf (Id "US") [ Id "CN" ] [ Id "US", Id "CN" ]) { intents = [ Transfer { from: Id "US", to: Id "CN", resource: Id "Energy", quantity: Q.fromInt 10 } ] }
      backend <- answerWith (replyJoint [ wrong ])
      j <- single =<< jointWith g backend [ tr "US" "I give CN ten Energy", tr "CN" "I take ten Energy" ]
      j.draft.intents `shouldEqual` wrong.intents

    it "a joint reply can enter an agreement the transcripts describe" do
      let
        entering = Enter { template: "loan", args: [ { name: "principal", value: Q.fromInt 10 } ], parties: [ { role: "lender", actor: Id "US" }, { role: "borrower", actor: Id "CN" } ] }
        proposed = sample { intents = [ entering ] }
      case decodeJoint ("{\"candidateTransactions\":[" <> stringify (encodeWith Snapshot proposedJointShape proposed) <> "]}") of
        Right [ d ] -> d.intents `shouldEqual` [ entering ]
        _ -> fail "one CandidateTransaction should decode"
      contains (Pattern "enter an agreement") interpretPrompt `shouldEqual` true

    it "a reply wrapped in prose or a code fence still decodes" do
      decodeJoint "Here is the joint pass:\n```json\n{\"candidateTransactions\":[]}\n```" `shouldEqual` Right []

    it "a reply with trailing prose containing a brace decodes its first object" do
      decodeJoint "```json\n{\"candidateTransactions\":[]}\n```\nNote: `{}` means none" `shouldEqual` Right []

    it "a brace inside a JSON string does not close the object" do
      decodeText "{\"text\":\"a } and a \\\" quote\"} trailing }" `shouldEqual` Right "a } and a \" quote"

    it "a clarification is not a CandidateTransaction list, so it does not decode" do
      isLeft (decodeJoint "{\"kind\":\"clarify\",\"message\":\"How much energy?\"}") `shouldEqual` true

    it "text that is not JSON does not decode" do
      isLeft (decodeJoint "not json") `shouldEqual` true

    it "a seat reply decodes its text; a reply without text does not" do
      decodeText "{\"text\":\"Exchanges moved.\"}" `shouldEqual` Right "Exchanges moved."
      isLeft (decodeText "{\"candidateTransactions\":[]}") `shouldEqual` true

  describe "Joint pass (§7.4)" do
    it "two transcripts describing one joint CandidateTransaction reconcile into one CandidateTransaction with both as signers" do
      g <- pacificParts >>= unseated
      backend <- answerWith (replyJoint [ jointOf (Id "US") [ Id "CN" ] [ Id "US", Id "CN" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell CN ten energy", tr "CN" "I buy ten energy from the US" ]
      j.author `shouldEqual` Id "US"
      j.signed `shouldEqual` [ Id "US", Id "CN" ]

    it "a one-sided description keeps only its author as signer" do
      g <- pacificParts >>= unseated
      backend <- answerWith (replyJoint [ jointOf (Id "US") [ Id "CN" ] [ Id "US" ] ])
      j <- single =<< jointWith g backend [ tr "US" "I sell CN ten energy", tr "CN" "nothing" ]
      j.signed `shouldEqual` [ Id "US" ]

    it "a reply that does not decode is a Left, which the caller turns into an empty submission" do
      g <- pacificParts >>= unseated
      backend <- answerWith "not a CandidateTransaction list"
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true

    it "a joint reply that does not decode is corrected: the second call carries the request, the previous reply and the decode error, and its valid reply is the result" do
      g <- pacificParts >>= unseated
      calls <- liftEffect (Ref.new [])
      let
        backend = backendOf \system input -> do
          liftEffect (Ref.modify_ (_ <> [ { system, input } ]) calls)
          n <- liftEffect (Ref.read calls)
          pure { text: if length n == 1 then "not json" else replyJoint [ jointOf (Id "US") [ Id "CN" ] [ Id "US", Id "CN" ] ], raw: "" }
      j <- single =<< jointWith g backend [ tr "US" "I sell CN ten energy", tr "CN" "I buy ten energy from the US" ]
      j.signed `shouldEqual` [ Id "US", Id "CN" ]
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
      g <- pacificParts >>= unseated
      counter <- liftEffect (Ref.new 0)
      out <- jointWith g (answering counter (\_ _ -> "not json")) [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 3

    it "a valid first reply makes exactly one call, with no correction" do
      g <- pacificParts >>= unseated
      counter <- liftEffect (Ref.new 0)
      _ <- single =<< jointWith g (answering counter (\_ _ -> replyJoint [ jointOf (Id "US") [ Id "CN" ] [ Id "US" ] ])) [ tr "US" "I sell CN ten energy" ]
      calls <- liftEffect (Ref.read counter)
      calls `shouldEqual` 1

    it "an author who did not speak is dropped" do
      g <- pacificParts >>= unseated
      backend <- answerWith (replyJoint [ jointOf (Id "CN-firm") [ Id "CN" ] [ Id "CN-firm" ] ])
      out <- jointWith g backend [ tr "US" "I sell CN ten energy" ]
      out `shouldEqual` Right []

    it "an empty Intent list, and a signer that is not an Actor, are dropped from the CandidateTransaction" do
      g <- pacificParts >>= unseated
      let bare = (jointOf (Id "US") [ Id "NOBODY", Id "CN" ] [ Id "US" ]) { intents = [] }
      empty <- answerWith (replyJoint [ bare ])
      out <- jointWith g empty [ tr "US" "nothing" ]
      out `shouldEqual` Right []
      named <- answerWith (replyJoint [ jointOf (Id "US") [ Id "NOBODY", Id "CN" ] [ Id "US" ] ])
      j <- single =<< jointWith g named [ tr "US" "I sell CN ten energy" ]
      j.draft.signers `shouldEqual` [ Id "CN" ]

    it "a backend failure is fatal to the call: the error propagates with its cause" do
      g <- pacificParts >>= unseated
      let broken = backendOf (\_ _ -> throwError (error "language model backend failed (broken): spawn pi ENOENT"))
      out <- attempt (jointWith g broken [ tr "US" "buy ten energy" ])
      case out of
        Left e -> message e `shouldEqual` "language model backend failed (broken): spawn pi ENOENT"
        Right _ -> fail "expected the backend failure to reach the caller"

  describe "Evidence and recording" do
    it "each call leaves its evidence: the effect, the model, digests of the template and payload, the raw stream by path, and the decode" do
      g <- pacificParts >>= unseated
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
      g <- pacificParts >>= unseated
      evidence <- liftEffect (Ref.new [])
      let backend = (backendOf \_ _ -> pure { text: "not json", raw: "" }) { evidence = \e -> Ref.modify_ (_ <> [ e ]) evidence }
      out <- jointWith g backend [ tr "US" "buy ten energy" ]
      isLeft out `shouldEqual` true
      evs <- liftEffect (Ref.read evidence)
      map (\e -> contains (Pattern "error: ") e.decoded) evs `shouldEqual` [ true, true, true ]

    it "a recorded turn replays: the recording file reads back, and the replayed CandidateTransaction equals the live one" do
      g <- pacificParts >>= unseated
      turns <- liftEffect (Ref.new [])
      let
        live = (backendOf \_ _ -> pure { text: replyJoint [ tradeTenEnergy ], raw: "" }) { turn = \x -> Ref.modify_ (_ <> [ x ]) turns }
      j1 <- single =<< jointWith g live [ tr "US" "buy ten energy" ]
      rec <- liftEffect (Ref.read turns)
      length rec `shouldEqual` 1
      back <- either (\e -> throwError (error ("the recording should decode: " <> e))) pure (decodeRecording (encodeRecording rec))
      replay <- liftEffect (replayCall back)
      j2 <- single =<< jointWith g (backendOf replay) [ tr "US" "buy ten energy" ]
      j2.signed `shouldEqual` j1.signed
      j2.author `shouldEqual` j1.author

    it "a failed call leaves evidence naming why, and rethrows" do
      g <- pacificParts >>= unseated
      evidence <- liftEffect (Ref.new [])
      let failing = (backendOf \_ _ -> throwError (error "down")) { evidence = \e -> Ref.modify_ (_ <> [ e ]) evidence }
      out <- attempt (jointWith g failing [ tr "US" "buy ten energy" ])
      isLeft out `shouldEqual` true
      evs <- liftEffect (Ref.read evidence)
      map _.decoded evs `shouldEqual` [ "error: the call failed: down" ]

    it "a model call that never returns fails at the backend's timeout" do
      g <- pacificParts >>= unseated
      let hung = (backendOf \_ _ -> makeAff \_ -> pure nonCanceler) { timeoutMs = 5 }
      out <- attempt (jointWith g hung [ tr "US" "buy ten energy" ])
      either (contains (Pattern "timed out") <<< message) (const false) out `shouldEqual` true

    it "a failed call is not recorded" do
      g <- pacificParts >>= unseated
      turns <- liftEffect (Ref.new [])
      let
        failing = (backendOf \_ _ -> throwError (error "down")) { turn = \x -> Ref.modify_ (_ <> [ x ]) turns }
      _ <- attempt (jointWith g failing [ tr "US" "buy ten energy" ])
      rec <- liftEffect (Ref.read turns)
      length rec `shouldEqual` 0

  describe "AI-agent seats" do
    it "a seat speaks its reply text; a reply that does not decode is an empty transcript" do
      g <- pacificParts >>= unseated
      view <- case Map.lookup (Id "CN") g.world.actors of
        Just de -> pure (observe g.world de)
        Nothing -> fail "Germany should be an Actor" *> throwError (error "actor")
      spoken <- answerWith (replyText "I will buy ten energy")
      said <- speak spoken (const "Secure energy.") (Id "CN") view
      said `shouldEqual` "I will buy ten energy"
      silent <- answerWith "not json"
      none <- speak silent (const "Secure energy.") (Id "CN") view
      none `shouldEqual` ""
