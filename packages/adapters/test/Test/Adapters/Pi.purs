-- | The `pi` process boundary, with no model (docs/07_simulator.md §14): a fake `pi` on the PATH answers
-- | with a canned event stream and records its arguments. The prompts, the decoding, and the evidence
-- | are the handler's and are tested in `Test.Handlers.Llm`. `Test.Adapters.LiveInterpret` is the one
-- | billed check.
-- | Pins: the raw `pi` event-stream format (docs/07 §5) and the prompt passing on its command line.
module Test.Adapters.Pi (spec) where

import Prelude

import Adapters.Pi (pi)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Node.FS.Sync (exists)
import Test.Adapters.Files (installPi, readFileText, removeTree, restorePath, tempDir, writeFileText)
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

spec :: Spec Unit
spec = describe "pi process boundary (no model)" do
  it "pi receives the system prompt and the input, with no tools or sessions" do
    withFakePi
      (\dir -> "cat '" <> dir <> "/reply.jsonl'\nfor a in \"$@\"; do printf '%s\\n' \"$a\"; done > '" <> dir <> "/args.txt'")
      \dir -> do
        liftEffect (writeFileText (dir <> "/reply.jsonl") (streamOf "{\"candidateTransactions\":[]}" <> "\n"))
        _ <- pi { model: "fake/model", rawDir: dir <> "/raw" } "the system task" "{\"round\":1}"
        args <- liftEffect (readFileText (dir <> "/args.txt"))
        contains (Pattern "--system-prompt\nthe system task\n") args `shouldEqual` true
        contains (Pattern "\n--no-tools\n") args `shouldEqual` true
        contains (Pattern "\n--no-session\n") args `shouldEqual` true
        contains (Pattern "\n-p\n{\"round\":1}\n") args `shouldEqual` true
        contains (Pattern "--model\nfake/model\n") args `shouldEqual` true

  it "the reply is the final assistant message's text; the raw event stream is saved and its path returned" do
    withFakePi (\dir -> "cat '" <> dir <> "/reply.jsonl'")
      \dir -> do
        liftEffect (writeFileText (dir <> "/reply.jsonl") (streamOf "first" <> "\n" <> streamOf "{\"text\":\"second\"}" <> "\n"))
        out <- pi { model: "fake/model", rawDir: dir <> "/raw" } "system" "input"
        out.text `shouldEqual` "{\"text\":\"second\"}"
        present <- liftEffect (exists out.raw)
        present `shouldEqual` true
        raw <- liftEffect (readFileText out.raw)
        contains (Pattern "message_end") raw `shouldEqual` true

  it "a stream with no assistant message is a failed call, never an empty reply" do
    withFakePi (const "echo '{\"type\":\"something_new\"}'")
      \dir -> do
        out <- attempt (pi { model: "fake/model", rawDir: dir <> "/raw" } "system" "input")
        case out of
          Left e -> contains (Pattern "no assistant message") (message e) `shouldEqual` true
          Right _ -> fail "a stream with no assistant message must fail the call"

  it "a non-zero exit from pi is fatal: the error names the model" do
    withFakePi (const "exit 3")
      \dir -> do
        out <- attempt (pi { model: "fake/model", rawDir: dir <> "/raw" } "system" "input")
        case out of
          Left e -> contains (Pattern "language model backend failed (fake/model)") (message e) `shouldEqual` true
          Right _ -> fail "a failed pi run must fail the call"
