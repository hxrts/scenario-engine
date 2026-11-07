-- | The terminal (docs/07_architecture.md): one line of input after a prompt, and a line of output.
-- | The raw primitives only; the prompts and what is said are the handler's and the app's. Console.js
-- | holds the readline interface.
module Adapters.Console (readLine, writeLine, closeInput) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import readLineImpl :: String -> EffectFnAff String

-- | One line of input after the prompt text, trimmed. Fails when input ends before it is answered.
readLine :: String -> Aff String
readLine question = fromEffectFnAff (readLineImpl question)

foreign import writeLine :: String -> Effect Unit

-- | End the input: a pending prompt fails, and a later `readLine` opens the terminal again.
foreign import closeInput :: Effect Unit
