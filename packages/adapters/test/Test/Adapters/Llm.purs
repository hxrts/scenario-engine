-- | The language-model adapter without a model (docs/06_simulator.md §14): the prompts each request
-- | sends, how a reply decodes, the `pi` process boundary (a fake `pi` on the PATH answers with a canned
-- | event stream and records its arguments), and the recording that stores and replays typed calls.
-- | No real model is called here; `Test.Adapters.LiveInterpret` is the one billed check.
module Test.Adapters.Llm (spec) where

import Prelude

import Adapters.Llm (decodeJoint, decodeText, fromCall, interpretPrompt, narratePrompt, pi, speakPrompt)
import Adapters.Recording (decodeRecording, encodeRecording, recordingPort, replayPort)
import Control.Monad.Error.Class (throwError)
import Core.Types (Id(..))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.String.CodeUnits as SCU
import Data.Either (Either(..), either, isLeft)
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Handlers.Llm (JointRequest, ModelPort, ProposedDeal)
import Test.Adapters.Files (fileExists, installPi, readFileText, removeTree, restorePath, tempDir, writeFileText)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | The event stream a `pi` run writes: one assistant message whose text is `text`.
streamOf :: String -> String
streamOf text = stringify $ encodeJson
  { type: "message_end"
  , message: { role: "assistant", content: [ { type: "text", text } ] }
  }

-- | A scratch directory with a fake `pi` on the PATH. `body` is the script's shell text, given the
-- | scratch directory. The PATH is restored and the directory removed after the action, pass or fail.
withFakePi :: (String -> String) -> (String -> Aff Unit) -> Aff Unit
withFakePi body act = do
  dir <- liftEffect tempDir
  old <- liftEffect (installPi (dir <> "/bin") (body dir))
  out <- attempt (act dir)
  liftEffect (restorePath old *> removeTree dir)
  either throwError pure out

joint :: JointRequest
joint = { round: 1, transcripts: [ { seat: Id "US", text: "I will buy ten energy" } ], entities: [ Id "US", Id "DE" ], markets: [ Id "energy-atlantic" ] }

-- | A Proposal with no Intents, as the model states it, for the decoding tests.
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
      let
        wrapped = "Here is the joint pass:\n```json\n{\"proposals\":[]}\n```"
      decodeJoint wrapped `shouldEqual` Right []

    it "a clarification is not a Proposal list, so it does not decode" do
      isLeft (decodeJoint "{\"kind\":\"clarify\",\"message\":\"How much energy?\"}") `shouldEqual` true

    it "text that is not JSON does not decode" do
      isLeft (decodeJoint "not json") `shouldEqual` true

    it "a narrator or seat reply decodes its text; a reply without text does not" do
      decodeText "{\"text\":\"Markets moved.\"}" `shouldEqual` Right "Markets moved."
      isLeft (decodeText "{\"proposals\":[]}") `shouldEqual` true

  describe "pi process boundary (no model)" do
    it "pi receives the task as the system prompt and the payload as the input, with no tools or sessions" do
      withFakePi
        (\dir -> "cat '" <> dir <> "/reply.jsonl'\nfor a in \"$@\"; do printf '%s\\n' \"$a\"; done > '" <> dir <> "/args.txt'")
        \dir -> do
          liftEffect (writeFileText (dir <> "/reply.jsonl") (streamOf "{\"proposals\":[]}" <> "\n"))
          ref <- liftEffect (Ref.new [])
          let port = pi { model: "fake/model", rawDir: dir <> "/raw", evidence: \e -> Ref.modify_ (_ <> [ e ]) ref }
          out <- port.interpret joint
          out `shouldEqual` Right []
          args <- liftEffect (readFileText (dir <> "/args.txt"))
          contains (Pattern ("--system-prompt\n" <> interpretPrompt <> "\n")) args `shouldEqual` true
          contains (Pattern "\n--no-tools\n") args `shouldEqual` true
          contains (Pattern "\n--no-session\n") args `shouldEqual` true
          contains (Pattern "\n-p\n{") args `shouldEqual` true
          contains (Pattern "\"round\":1") args `shouldEqual` true
          contains (Pattern "--model\nfake/model\n") args `shouldEqual` true

    it "the reply text is decoded, and each call's evidence is kept: digests, the raw stream by path, and the decode" do
      withFakePi (\dir -> "cat '" <> dir <> "/reply.jsonl'")
        \dir -> do
          liftEffect (writeFileText (dir <> "/reply.jsonl") (streamOf "{\"proposals\":[]}" <> "\n"))
          ref <- liftEffect (Ref.new [])
          let port = pi { model: "fake/model", rawDir: dir <> "/raw", evidence: \e -> Ref.modify_ (_ <> [ e ]) ref }
          _ <- port.interpret joint
          evs <- liftEffect (Ref.read ref)
          map _.effect evs `shouldEqual` [ "interpret" ]
          map _.decoded evs `shouldEqual` [ "ok" ]
          map (\e -> { m: e.model, template: SCU.length e.template, input: SCU.length e.input }) evs `shouldEqual` [ { m: "fake/model", template: 16, input: 16 } ]
          case evs of
            [ e ] -> do
              present <- liftEffect (fileExists e.raw)
              present `shouldEqual` true
            _ -> fail "one call should leave one evidence entry"

    it "a reply the model gave that does not decode is a Left, and its evidence says so" do
      withFakePi (\dir -> "cat '" <> dir <> "/reply.jsonl'")
        \dir -> do
          liftEffect (writeFileText (dir <> "/reply.jsonl") (streamOf "not json" <> "\n"))
          ref <- liftEffect (Ref.new [])
          let port = pi { model: "fake/model", rawDir: dir <> "/raw", evidence: \e -> Ref.modify_ (_ <> [ e ]) ref }
          out <- port.interpret joint
          isLeft out `shouldEqual` true
          evs <- liftEffect (Ref.read ref)
          map (\e -> contains (Pattern "error: ") e.decoded) evs `shouldEqual` [ true ]

    it "a non-zero exit from pi is fatal: the error names the model" do
      withFakePi (const "exit 3")
        \dir -> do
          let port = pi { model: "fake/model", rawDir: dir <> "/raw", evidence: \_ -> pure unit }
          out <- attempt (port.interpret joint)
          case out of
            Left e -> contains (Pattern "language model backend failed (fake/model)") (message e) `shouldEqual` true
            Right _ -> fail "a failed pi run must fail the call"

  describe "Port over a call" do
    it "a failed call is an Aff error, with its message unchanged" do
      let port = fromCall { model: "down", call: \_ _ -> throwError (error "network down"), evidence: \_ -> pure unit }
      out <- attempt (port.narrate { round: 1, events: [] })
      case out of
        Left e -> message e `shouldEqual` "network down"
        Right _ -> fail "the call must fail"

  describe "Recording" do
    it "a recorded run replays each reply from its request, through the file" do
      ref <- liftEffect (Ref.new [])
      let
        live :: ModelPort
        live =
          { model: "live"
          , interpret: \q -> pure (Right [ sample { author = Id (show (length q.transcripts)) } ])
          , narrate: \q -> pure (Right ("front page for round " <> show q.round))
          , speak: \_ -> pure (Right "nothing")
          }
        port = recordingPort live ref
      j1 <- port.interpret joint
      n1 <- port.narrate { round: 3, events: [] }
      rec <- liftEffect (Ref.read ref)
      length rec `shouldEqual` 2
      back <- either (\e -> throwError (error ("the recording should decode: " <> e))) pure (decodeRecording (encodeRecording rec))
      let replay = replayPort back
      j2 <- replay.interpret joint
      map (map _.author) j2 `shouldEqual` map (map _.author) j1
      n2 <- replay.narrate { round: 3, events: [] }
      n2 `shouldEqual` n1
      n2 `shouldEqual` Right "front page for round 3"

    it "a request the recording does not hold is an error, never a model call" do
      out <- attempt ((replayPort []).interpret joint)
      case out of
        Left e -> contains (Pattern "no recorded interpret call") (message e) `shouldEqual` true
        Right _ -> fail "an unrecorded request must not be answered"

    it "a failed call is not recorded" do
      ref <- liftEffect (Ref.new [])
      let
        failing :: ModelPort
        failing = { model: "x", interpret: \_ -> throwError (error "down"), narrate: \_ -> pure (Right ""), speak: \_ -> pure (Right "") }
      _ <- attempt ((recordingPort failing ref).interpret joint)
      rec <- liftEffect (Ref.read ref)
      length rec `shouldEqual` 0

