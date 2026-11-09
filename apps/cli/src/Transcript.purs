-- | The composition root of the capstone recording (`Cli.Transcript`): the real handler over a stub model that
-- | answers with the DSL's reply text. The handler reports each exchange, which is kept here and written.
module Transcript (main) where

import Prelude

import Adapters.Digest (sha256)
import Adapters.Exit (fail)
import Cli.Stack (runInterpret, runSystem)
import Cli.Transcript (capstoneGame, checkExchanges, writeRecording)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (stringify)
import Data.Array (length)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Handlers.Llm.Backend (backend)
import Handlers.Recording (encodeRecording)
import Settings.Meridian.Capstone (jointLineFor)

-- | The stub model's answer to a joint call: the reply the DSL gives the transcript the payload names.
answer :: String -> String -> Aff { text :: String, raw :: String }
answer _ input = case jointLineFor input of
  Just l -> pure { text: stringify l.reply, raw: "" }
  Nothing -> throwError (error ("the capstone defines no reply for: " <> input))

main :: Effect Unit
main = runAff_ (either (\e -> fail ("transcript: " <> message e)) (const (pure unit))) do
  g <- runSystem capstoneGame >>= orThrow
  ref <- liftEffect (Ref.new [])
  let llm = (backend { model: "capstone-stub", call: answer, digest: sha256 }) { exchange = \x -> Ref.modify_ (_ <> [ x ]) ref }
  runInterpret llm g.world checkExchanges >>= orThrow
  rec <- liftEffect (Ref.read ref)
  runSystem (writeRecording (encodeRecording rec) (length rec))
  where
  orThrow :: forall a. Either String a -> Aff a
  orThrow = either (throwError <<< error) pure
