-- | Test support for the model handler (docs/08_coverage.md): backends that stand in for the `pi`
-- | process. A stub answers each call with a reply text, so the handler's prompts, decoding, and
-- | fallbacks all run; `answering` builds those texts from typed replies. `replayCall` answers from a
-- | recorded capstone exchange by its request, and `decodeRecording` reads the recording file.
module Test.Handlers.Stub
  ( answering
  , backendOf
  , dealOf
  , decodeRecording
  , replayCall
  , replyJoint
  , replyText
  , tradeTenEnergy
  ) where

import Prelude

import Board.Decode.Common (readJson)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (fromObject, stringify, toObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Core.Num as Q
import Core.Types (EntityId, Id(..), Intent(..))
import Handlers.Llm (Backend, Call)
import Handlers.Llm.Wire (ProposedDeal)
import Handlers.Recording (Exchange, Recording)

-- | A backend over a raw call: the model is named "stub", and its evidence and exchanges are discarded.
backendOf :: Call -> Backend
backendOf call = { model: "stub", call, evidence: \_ -> pure unit, exchange: \_ -> pure unit }

-- | A backend that answers each call with `answer system input` and counts the calls in `counter`.
answering :: Ref Int -> (String -> String -> String) -> Backend
answering counter answer = backendOf \system input -> do
  liftEffect (Ref.modify_ (_ + 1) counter)
  pure { text: answer system input, raw: "" }

-- | A Proposal that transfers ten Energy from `author` to Germany, with the signers and agreed seats given.
dealOf :: EntityId -> Array EntityId -> Array EntityId -> ProposedDeal
dealOf author signers agreed =
  { author, intents: [ Transfer { from: author, to: Id "DE", resource: Id "E", quantity: Q.fromInt 10 } ], signers, agreed }

-- | The Proposal that trades ten Energy in the Atlantic market.
tradeTenEnergy :: ProposedDeal
tradeTenEnergy =
  { author: Id "US"
  , intents: [ Trade { market: Id "energy-atlantic", quantity: Q.fromInt 10, limitPrice: Nothing } ]
  , signers: []
  , agreed: [ Id "US" ]
  }

-- | The reply text of a joint pass with these Proposals, as the model writes it.
replyJoint :: Array ProposedDeal -> String
replyJoint ps = stringify (encodeJson { proposals: ps })

-- | The reply text of a narrator or seat with this text.
replyText :: String -> String
replyText s = stringify (encodeJson { text: s })

-- | A recorded exchange's reply, as the model's text: the recorded value under the key the
-- | effect's reply uses. An error reply has no value, so its text does not decode.
replyTextOf :: Exchange -> String
replyTextOf x = case toObject x.reply >>= Object.lookup "value" of
  Nothing -> ""
  Just v -> stringify (fromObject (Object.singleton key v))
  where
  key = if x.effect == "interpret" then "proposals" else "text"

-- | A call answered from the recording: the first exchange whose request is the payload given. The
-- | prompt and the decoding still run; only the model's text is replayed. No entry is an error.
replayCall :: Recording -> Call
replayCall rec _ input = case jsonParser input of
  Left why -> throwError (error ("the payload is not JSON: " <> why))
  Right request -> case find (\x -> x.request == request) rec of
    Nothing -> throwError (error "no recorded call matches this request")
    Just x -> pure { text: replyTextOf x, raw: "" }

-- | The recording file's text, decoded.
decodeRecording :: String -> Either String Recording
decodeRecording = readJson
