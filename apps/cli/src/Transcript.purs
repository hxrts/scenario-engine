-- | `just _premises` (after `Compile`): writes the capstone's joint-pass recording,
-- | `capstone/transcript.json` under the setting root. Each exchange the DSL defines
-- | (`Settings.Meridian.Capstone.jointLines`) runs through the real handler (prompt, decode, evidence) over a
-- | stub model that answers with the DSL's reply text (`jointLineFor`); the handler reports each exchange, which is kept. Each Intent set is
-- | checked against the DSL's expectation before anything is written; a mismatch exits 1 and writes
-- | nothing. No model is called: the file is generated, never hand-maintained.
module Transcript (main) where

import Prelude

import Adapters.Digest (sha256)
import Adapters.Filesystem (fileOps)
import Settings.Tree (settingRoot)
import Control.Monad.Error.Class (throwError)
import Core.Types (Id(..))
import Data.Argonaut.Core (stringify)
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Signatures (InterpretF(..))
import Handlers.Llm.Joint (interpreter)
import Handlers.Llm.Backend (backend)
import Handlers.Recording (encodeRecording)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Settings.Meridian.Capstone (game, jointLineFor, jointLines, matchesExpect)
import Settings.Load (loadFrom)
import Settings.Layout (capstoneTranscriptFile)
import Settings.Meridian.Names (meridianDir)
import Exit (fail)

-- | The stub model's answer to a joint call: the reply the DSL gives the transcript the payload names.
answer :: String -> String -> Aff { text :: String, raw :: String }
answer _ input = case jointLineFor input of
  Just l -> pure { text: stringify l.reply, raw: "" }
  Nothing -> throwError (error ("the capstone defines no reply for: " <> input))

main :: Effect Unit
main = runAff_ (either (\e -> fail ("transcript: " <> message e)) (const (pure unit))) do
  parts <- liftEffect (loadFrom fileOps meridianDir) >>= either (throwError <<< error) pure
  g <- either (throwError <<< error <<< show) pure (game parts)
  let w = g.world
  ref <- liftEffect (Ref.new [])
  let llm = (backend { model: "capstone-stub", call: answer, digest: sha256 }) { exchange = \x -> Ref.modify_ (_ <> [ x ]) ref }
  for_ jointLines \l -> do
    out <- interpreter llm w 1 [ { seat: Id "US", text: l.text } ]
    case out of
      Left why -> throwError (error ("the capstone exchange \"" <> l.text <> "\" did not decode: " <> why))
      Right js
        | matchesExpect l js -> pure unit
        | otherwise -> throwError (error ("the capstone exchange \"" <> l.text <> "\" does not give the Intents the DSL expects"))
  rec <- liftEffect (Ref.read ref)
  root <- liftEffect (settingRoot fileOps)
  liftEffect do
    writeTextFile UTF8 (root <> capstoneTranscriptFile) (encodeRecording rec)
    log ("wrote " <> show (length rec) <> " capstone exchanges to " <> root <> capstoneTranscriptFile)
