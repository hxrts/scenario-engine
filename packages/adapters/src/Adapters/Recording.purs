-- | Recorded model calls (docs/08_coverage.md): a wrapper that keeps every typed request with its typed
-- | reply, and a port that answers from such a recording without a model. A recording is a list of
-- | `{ effect, request, reply }` entries in the Argonaut encoding of the domain types. A replayed call
-- | matches its request structurally (the same encoded request); a request the recording does not hold
-- | is an error, never a model call.
module Adapters.Recording
  ( Exchange
  , Recording
  , recordingPort
  , replayPort
  , encodeRecording
  , decodeRecording
  ) where

import Prelude

import Board.Decode.Common (readJson)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Handlers.Llm (ModelPort)

-- | One model call: the effect it served, the encoded typed request, and the encoded typed reply.
type Exchange = { effect :: String, request :: Json, reply :: Json }

type Recording = Array Exchange

-- | The reply's encoding: an error text, or the typed value. Both keys are always written.
type ReplyJson = { error :: Maybe String, value :: Maybe Json }

encodeReply :: forall a. EncodeJson a => Either String a -> Json
encodeReply r = encodeJson $ case r of
  Left e -> { error: Just e, value: Nothing } :: ReplyJson
  Right a -> { error: Nothing, value: Just (encodeJson a) }

decodeReply :: forall a. DecodeJson a => Json -> Either String (Either String a)
decodeReply j = do
  o <- lmap printJsonDecodeError (decodeJson j :: Either JsonDecodeError ReplyJson)
  case o.error, o.value of
    Just e, _ -> pure (Left e)
    Nothing, Just v -> Right <$> lmap printJsonDecodeError (decodeJson v)
    Nothing, Nothing -> Left "a reply must carry an error or a value"

-- | A port that keeps every call it makes: each request, encoded, with the reply the wrapped port
-- | gave. Failed calls (Aff errors) are not kept.
recordingPort :: ModelPort -> Ref Recording -> ModelPort
recordingPort p ref =
  { model: p.model
  , interpret: \req -> do
      out <- p.interpret req
      keep "interpret" (encodeJson req) (encodeReply out)
      pure out
  , narrate: \req -> do
      out <- p.narrate req
      keep "narrate" (encodeJson req) (encodeReply out)
      pure out
  , speak: \req -> do
      out <- p.speak req
      keep "speak" (encodeJson req) (encodeReply out)
      pure out
  }
  where
  keep effect request reply = liftEffect (Ref.modify_ (_ <> [ { effect, request, reply } ]) ref)

-- | A port that answers from a recording: the first entry of the same effect whose request encodes
-- | the same. No entry is an error. Nothing calls a model.
replayPort :: Recording -> ModelPort
replayPort rec =
  { model: "replay"
  , interpret: answer "interpret"
  , narrate: answer "narrate"
  , speak: answer "speak"
  }
  where
  answer :: forall req rep. EncodeJson req => DecodeJson rep => String -> req -> Aff (Either String rep)
  answer effect req = case find (\x -> x.effect == effect && x.request == encodeJson req) rec of
    Nothing -> throwError (error ("no recorded " <> effect <> " call matches this request"))
    Just x -> case decodeReply x.reply of
      Left why -> throwError (error ("the recorded " <> effect <> " reply does not decode: " <> why))
      Right reply -> pure reply

-- | The recording file's text: indented JSON, with a final newline.
encodeRecording :: Recording -> String
encodeRecording rec = stringifyWithIndent 1 (encodeJson rec) <> "\n"

decodeRecording :: String -> Either String Recording
decodeRecording = readJson
