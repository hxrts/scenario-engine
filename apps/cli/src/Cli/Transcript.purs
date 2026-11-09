-- | `just _settings` (after the compile): the capstone's joint-pass recording, `capstone/transcript.json` under the
-- | setting root. Each exchange the DSL defines (`Settings.Meridian.Capstone.jointLines`) runs through `INTERPRET`.
-- | Each Intent set is checked against the DSL's expectation before anything is written; a mismatch writes
-- | nothing. No model is called: the file is generated, never hand-maintained.
module Cli.Transcript (capstoneGame, checkExchanges, writeRecording) where

import Prelude

import Laws.Types (Id(..))
import Data.Array (foldM)
import Data.Either (Either(..))
import Effects.Signatures (CONSOLE, FILES, INTERPRET, InterpretF(..), _interpret)
import Effects.System (files, writeFile, writeLine)
import Game.Types (Game)
import Run (Run, lift)
import Settings.Layout (capstoneTranscriptFile)
import Settings.Load (loadFrom)
import Settings.Meridian.Capstone (game, jointLines, matchesExpect)
import Settings.Meridian.Names (meridianDir)
import Settings.Tree (settingRoot)
import Type.Row (type (+))

capstoneGame :: forall r. Run (FILES + r) (Either String Game)
capstoneGame = loadFrom files meridianDir <#> \loaded -> case loaded of
  Left e -> Left e
  Right parts -> case game parts of
    Left e -> Left (show e)
    Right g -> Right g

-- | Every exchange decodes and gives the Intents the DSL expects.
checkExchanges :: forall r. Run (INTERPRET + r) (Either String Unit)
checkExchanges = foldM step (Right unit) jointLines
  where
  step (Left e) _ = pure (Left e)
  step (Right _) l = lift _interpret (Interpret 1 [ { seat: Id "US", text: l.text } ] identity) <#> case _ of
    Left why -> Left ("the capstone exchange \"" <> l.text <> "\" did not decode: " <> why)
    Right js
      | matchesExpect l js -> Right unit
      | otherwise -> Left ("the capstone exchange \"" <> l.text <> "\" does not give the Intents the DSL expects")

-- | Write the recording, given its text and how many exchanges it holds.
writeRecording :: forall r. String -> Int -> Run (FILES + CONSOLE + r) Unit
writeRecording text count = do
  root <- settingRoot files
  writeFile (root <> capstoneTranscriptFile) text
  writeLine ("wrote " <> show count <> " capstone exchanges to " <> root <> capstoneTranscriptFile)
