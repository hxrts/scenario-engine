-- | The language-model backend: one raw call, its evidence and timeout, and the corrections a reply that does not decode gets. Model output is only a candidate; the engine checks it when it resolves the round.
module Handlers.Llm.Backend
  ( Call
  , Evidence
  , Backend
  , backend
  , defaultTimeoutMs
  , Key
  , maxCorrections
  , seatText
  , send
  , within
  , ask
  , correction
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parallel, sequential)
import Core.Shape (Format(..), Shape, encodeWith)
import Core.Types (EntityId, Id(..))
import Data.Argonaut.Core (Json, stringify)
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.Exception (Error, error, message)
import Effect.Class (liftEffect)
import Handlers.Llm.Wire (correctionShape)
import Handlers.Recording (Exchange, encodeReply)

-- | One raw model call: the system prompt and the JSON payload in, the reply text and the path of
-- | its raw event stream out. A failed call is an Aff error.
type Call = String -> String -> Aff { text :: String, raw :: String }

-- | Evidence by pointer, appended for every call: the effect, the model, digests of the prompt
-- | template and the JSON payload, the path of the raw event stream, and whether the reply decoded.
type Evidence = { effect :: String, model :: String, template :: String, input :: String, raw :: String, decoded :: String }

-- | The model backend a handler interprets over: the raw call, the model name for evidence, the
-- | digest device that hashes a template or payload for evidence, and the sinks for each call's
-- | evidence and its typed exchange (the recording, when one is kept).
type Backend = { model :: String, call :: Call, digest :: String -> String, timeoutMs :: Int, evidence :: Evidence -> Effect Unit, exchange :: Exchange -> Effect Unit }

-- | A backend with the default timeout and sinks that keep nothing; a composition root overrides what it keeps.
backend :: { model :: String, call :: Call, digest :: String -> String } -> Backend
backend b = { model: b.model, call: b.call, digest: b.digest, timeoutMs: defaultTimeoutMs, evidence: const (pure unit), exchange: const (pure unit) }

-- | How long one model call may take before the run fails.
defaultTimeoutMs :: Int
defaultTimeoutMs = 300000

-- | What a call concerns, for the recording: its round and the seats whose transcripts it carries.
type Key = { round :: Int, seats :: Array String }

-- | The joint pass makes at most this many corrections after its first call (docs/04 §11).
maxCorrections :: Int
maxCorrections = 2

seatText :: EntityId -> String
seatText (Id s) = s

-- | One call with its payload already encoded: the payload goes to the backend, the reply decodes,
-- | and the call's evidence and typed exchange are kept. A failed call or one that times out leaves
-- | evidence naming why, keeps no exchange, and rethrows.
send :: forall a. Backend -> String -> Key -> String -> Json -> Shape a -> (String -> Either String a) -> Aff { text :: String, decoded :: Either String a }
send b effect key template request replyShape decode = do
  let input = stringify request
  let evidenceOf raw result = b.evidence { effect, model: b.model, template: b.digest template, input: b.digest input, raw, decoded: result }
  out <- within b (b.call template input) >>= case _ of
    Right r -> pure r
    Left err -> do
      liftEffect (evidenceOf "" ("error: the call failed: " <> message err))
      throwError err
  let decoded = decode out.text
  liftEffect do
    evidenceOf out.raw (either ("error: " <> _) (const "ok") decoded)
    b.exchange { effect, round: key.round, seats: key.seats, request, reply: encodeReply replyShape decoded }
  pure { text: out.text, decoded }

-- | Run a call under the backend's timeout: the first of its result and the timer. The loser is cancelled.
within :: forall a. Backend -> Aff a -> Aff (Either Error a)
within b call = sequential (parallel (attempt call) <|> parallel timer)
  where
  timer = delay (Milliseconds (toNumber b.timeoutMs)) $> Left (error ("the model call timed out after " <> show b.timeoutMs <> " ms"))

-- | One call: the payload is encoded through its shape, and the reply decodes.
ask :: forall p a. Backend -> String -> Key -> String -> Shape p -> p -> Shape a -> (String -> Either String a) -> Aff (Either String a)
ask b effect key template requestShape payload replyShape decode =
  _.decoded <$> send b effect key template (encodeWith Snapshot requestShape payload) replyShape decode

-- | A correction's payload: the original request, the previous reply's text, and the decode error as
-- | the decoder reports it. The system prompt is the same as the first call's.
correction :: Json -> String -> String -> Json
correction request previous err = encodeWith Snapshot correctionShape { request, previous_reply: previous, error: err }
