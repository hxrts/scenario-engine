-- | `just _premises` (after the compile): the capstone's joint-pass recording, `capstone/transcript.json` under the
-- | premise root. Each turn the DSL defines (`Premises.Meridian.Capstone.jointLines`) runs through `INTERPRET`.
-- | Each Intent set is checked against the DSL's expectation before anything is written; a mismatch writes
-- | nothing. No model is called: the file is generated, never hand-maintained.
module Cli.Transcript (capstonePreparedRun, checkTurns, writeRecording) where

import Prelude

import Laws.Types (Id(..))
import Data.Array (foldM)
import Data.Either (Either(..))
import Effects.Signatures (CONSOLE, FILES, INTERPRET, InterpretF(..), _interpret)
import Effects.System (files, writeFile, writeLine)
import Assembly.Types (PreparedRun)
import Run (Run, lift)
import Premises.Layout (capstoneTranscriptFile)
import Premises.Load (loadFrom)
import Premises.Meridian.Capstone (game, jointLines, matchesExpect)
import Premises.Meridian.Names (meridianDir)
import Premises.Tree (premiseRoot)
import Type.Row (type (+))

capstonePreparedRun :: forall r. Run (FILES + r) (Either String PreparedRun)
capstonePreparedRun = loadFrom files meridianDir <#> \loaded -> case loaded of
  Left e -> Left e
  Right parts -> case game parts of
    Left e -> Left (show e)
    Right g -> Right g

-- | Every turn decodes and gives the Intents the DSL expects.
checkTurns :: forall r. Run (INTERPRET + r) (Either String Unit)
checkTurns = foldM step (Right unit) jointLines
  where
  step (Left e) _ = pure (Left e)
  step (Right _) l = lift _interpret (Interpret 1 [ { seat: Id "US", text: l.text } ] identity) <#> case _ of
    Left why -> Left ("the capstone turn \"" <> l.text <> "\" did not decode: " <> why)
    Right js
      | matchesExpect l js -> Right unit
      | otherwise -> Left ("the capstone turn \"" <> l.text <> "\" does not give the Intents the DSL expects")

-- | Write the recording, given its text and how many turns it holds.
writeRecording :: forall r. String -> Int -> Run (FILES + CONSOLE + r) Unit
writeRecording text count = do
  root <- premiseRoot files
  writeFile (root <> capstoneTranscriptFile) text
  writeLine ("wrote " <> show count <> " capstone turns to " <> root <> capstoneTranscriptFile)
