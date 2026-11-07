-- | The `pi` process (docs/01_overview.md §10): the raw model call. One ephemeral, tool-less `pi`
-- | process per call, given a system prompt and an input text, returning the reply text and the path
-- | of its raw event stream. This adapter knows nothing of prompts, JSON, or evidence: those are the
-- | handler's (`Handlers.Llm`). The model ID is the only configuration.
module Adapters.Pi (defaultModel, pi) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (Error, error, message)

defaultModel :: String
defaultModel = "openrouter/deepseek/deepseek-v4-flash-latest"

foreign import piImpl :: String -> String -> String -> String -> EffectFnAff { text :: String, raw :: String }

-- | One call: the system prompt and the input in, the reply text and the raw stream's path out. A
-- | backend failure (a missing `pi`, a non-zero exit, an unwritable raw directory) is fatal to the
-- | run: it is rethrown with the model and the cause named.
pi :: { model :: String, rawDir :: String } -> String -> String -> Aff { text :: String, raw :: String }
pi c system input = fromEffectFnAff (piImpl c.model c.rawDir system input) `catchError` backendFailed c.model

backendFailed :: forall a. String -> Error -> Aff a
backendFailed model e = throwError (error ("language model backend failed (" <> model <> "): " <> message e))
