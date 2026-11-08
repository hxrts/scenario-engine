-- | `just _premises` (after `Compile`): writes the capstone's joint-pass recording,
-- | `capstone/transcript.json` under the premise root. Each exchange the DSL defines
-- | (`Premises.Capstone.jointLines`) runs through the real handler (prompt, decode, evidence) over a
-- | stub model that answers with the DSL's reply text (`jointLineFor`); the handler reports each exchange, which is kept. Each Intent set is
-- | checked against the DSL's expectation before anything is written; a mismatch exits 1 and writes
-- | nothing. No model is called: the file is generated, never hand-maintained.
module Transcript (main) where

import Prelude

import Adapters.Digest (sha256)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readPremise, premiseRoot)
import Control.Monad.Error.Class (throwError)
import Core.Types (Id(..))
import Core.World (WorldState)
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
import Handlers.Llm (interpreter, backend)
import Handlers.Recording (encodeRecording)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Process (exit')
import Game.Assemble (assemble)
import Game.Premise (Premise(..))
import Premises.Capstone (jointLineFor, jointLines, matchesExpect, seatBindings)
import Premises.Load (loadPremise)

-- | The capstone's world: the shipped Meridian at its default configuration, assembled with the capstone's seats.
worldOf :: Aff WorldState
worldOf = do
  parts <- liftEffect (readPremise fileOps "meridian/v0") >>= either (throwError <<< error) pure >>= (either (throwError <<< error) pure <<< loadPremise)
  case assemble (GameboardPremise parts) seatBindings [] of
    Right game -> pure game.world
    Left e -> throwError (error (show e))

-- | The stub model's answer to a joint call: the reply the DSL gives the transcript the payload names.
answer :: String -> String -> Aff { text :: String, raw :: String }
answer _ input = case jointLineFor input of
  Just l -> pure { text: stringify l.reply, raw: "" }
  Nothing -> throwError (error ("the capstone defines no reply for: " <> input))

main :: Effect Unit
main = runAff_ (either (\e -> log ("transcript: " <> message e) *> exit' 1) (const (pure unit))) do
  w <- worldOf
  ref <- liftEffect (Ref.new [])
  let llm = (backend { model: "capstone-stub", call: answer, digest: sha256 }) { exchange = \x -> Ref.modify_ (_ <> [ x ]) ref }
  for_ jointLines \l -> do
    out <- interpreter llm w (Interpret 1 [ { seat: Id "US", text: l.text } ] identity)
    case out of
      Left why -> throwError (error ("the capstone exchange \"" <> l.text <> "\" did not decode: " <> why))
      Right js
        | matchesExpect l js -> pure unit
        | otherwise -> throwError (error ("the capstone exchange \"" <> l.text <> "\" does not give the Intents the DSL expects"))
  rec <- liftEffect (Ref.read ref)
  root <- liftEffect (premiseRoot fileOps)
  liftEffect do
    writeTextFile UTF8 (root <> "capstone/transcript.json") (encodeRecording rec)
    log ("wrote " <> show (length rec) <> " capstone exchanges to " <> root <> "capstone/transcript.json")
