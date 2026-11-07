-- | `just _scenarios` (after `Compile`): writes the capstone's joint-pass recording,
-- | `capstone/transcript.json` under the scenario root. Each exchange the DSL defines
-- | (`Scenarios.Capstone.jointLines`) runs through the real handler (prompt, decode, evidence) over a
-- | stub model that answers with the DSL's reply text; the handler reports each exchange, which is kept. Each Intent set is
-- | checked against the DSL's expectation before anything is written; a mismatch exits 1 and writes
-- | nothing. No model is called: the file is generated, never hand-maintained.
module Transcript (main) where

import Prelude

import Adapters.Scenarios (readScenario, scenariosRoot)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Types (Id(..))
import Core.World (WorldState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, length)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effects.Signatures (InterpretF(..))
import Handlers.Llm (interpreter)
import Handlers.Recording (encodeRecording)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Process (exit')
import Scenarios.Capstone (jointLines)
import Scenarios.Load (load)

-- | The capstone's world: the shipped Meridian at its default configuration.
worldOf :: Aff WorldState
worldOf = do
  loaded <- liftEffect (readScenario "meridian/v0") >>= either (throwError <<< error) pure >>= (either (throwError <<< error) pure <<< load)
  case instantiate loaded.gameboard loaded.config of
    Right initial -> pure initial.world
    Left _ -> throwError (error "instantiation")

-- | The stub model's answer to a joint call: the reply the DSL gives the transcript the payload names.
answer :: String -> String -> Aff { text :: String, raw :: String }
answer _ input = case find (\l -> contains (Pattern l.text) input) jointLines of
  Just l -> pure { text: stringify l.reply, raw: "" }
  Nothing -> throwError (error ("the capstone defines no reply for: " <> input))

main :: Effect Unit
main = runAff_ (either (\e -> log ("transcript: " <> message e) *> exit' 1) (const (pure unit))) do
  w <- worldOf
  ref <- liftEffect (Ref.new [])
  let backend = { model: "capstone-stub", call: answer, evidence: const (pure unit), exchange: \x -> Ref.modify_ (_ <> [ x ]) ref }
  for_ jointLines \l -> do
    out <- interpreter backend w (Interpret 1 [ { seat: Id "US", text: l.text } ] identity)
    case out of
      Left why -> throwError (error ("the capstone exchange \"" <> l.text <> "\" did not decode: " <> why))
      Right js
        | map (\j -> stringify (encodeJson j.draft.intents)) js == [ stringify (encodeJson l.expect) ] -> pure unit
        | otherwise -> throwError (error ("the capstone exchange \"" <> l.text <> "\" does not give the Intents the DSL expects"))
  rec <- liftEffect (Ref.read ref)
  root <- liftEffect scenariosRoot
  liftEffect do
    writeTextFile UTF8 (root <> "capstone/transcript.json") (encodeRecording rec)
    log ("wrote " <> show (length rec) <> " capstone exchanges to " <> root <> "capstone/transcript.json")
