-- | Test support for the model handler: backends that stand in for the `pi`
-- | process. A stub answers each call with a reply text, so the handler's prompts, decoding, and
-- | fallbacks all run; `answering` builds those texts from typed replies. `replayCall` answers from the
-- | recorded capstone exchanges in their recorded order, checking each call's effect, round and seats
-- | (never its prompt text), and `decodeRecording` reads the recording file.
module Test.Handlers.Stub
  ( answering
  , backendOf
  , digest
  , dealOf
  , decodeRecording
  , replayCall
  , replyJoint
  , replyText
  , tradeTenEnergy
  ) where

import Prelude

import Schema.Decode.Common (readJson)
import Control.Monad.Error.Class (throwError)
import Core.Shape (Format(..), arrayOf, decodeWith, encodeWith, stringShape)
import Data.Argonaut.Core (stringify, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (uncons)
import Data.Either (Either(..), either, note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Core.Num as Q
import Core.Types (EntityId, Id(..), IntentF(..))
import Handlers.Llm (Backend, Call)
import Core.Proposal (ProposedDeal, proposedDealShape)
import Handlers.Llm.Wire (jointReplyShape, textReplyShape)
import Handlers.Recording (Exchange, Recording)

-- | The stub digest: sixteen characters, as the adapter's is, with no hashing.
digest :: String -> String
digest = const "stub-digest-0000"

-- | A backend over a raw call: the model is named "stub", and its evidence and exchanges are discarded.
backendOf :: Call -> Backend
backendOf call = { model: "stub", call, digest, evidence: \_ -> pure unit, exchange: \_ -> pure unit }

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
replyJoint ps = stringify (encodeWith Snapshot jointReplyShape { proposals: ps })

-- | The reply text of a narrator or seat with this text.
replyText :: String -> String
replyText s = stringify (encodeWith Snapshot textReplyShape { text: s })

-- | A recorded exchange's reply, as the model's text: the recorded value under the key the
-- | effect's reply uses. An error reply has no value, so its text does not decode.
replyTextOf :: Exchange -> String
replyTextOf x = case toObject x.reply >>= Object.lookup "value" of
  Nothing -> ""
  Just v
    | x.effect == "interpret" -> either (const "") (\ps -> replyJoint ps) (decodeWith Snapshot (arrayOf proposedDealShape) v)
    | otherwise -> either (const "") replyText (decodeWith Snapshot stringShape v)

-- | What a call is keyed on: its effect, round and seats. The payload says them: a joint pass
-- | carries `transcripts`, a narrator `events`, and a seat `actor`.
type Key = { effect :: String, round :: Int, seats :: Array String }

keyOfPayload :: String -> Either String Key
keyOfPayload input = do
  j <- jsonParser input
  o0 <- note "the payload is not an object" (toObject j)
  -- a joint pass correction carries the original request under `request` (Handlers.Llm.joint)
  let o = fromMaybe o0 (Object.lookup "request" o0 >>= toObject)
  round <- note "the payload has no round" (Object.lookup "round" o >>= toNumber >>= Int.fromNumber)
  case Object.lookup "transcripts" o, Object.lookup "actor" o of
    Just ts, _ -> do
      seats <- note "the transcripts are not a list" (toArray ts)
      names <- traverse (\t -> note "a transcript has no seat" (toObject t >>= Object.lookup "seat" >>= toString)) seats
      pure { effect: "interpret", round, seats: names }
    _, Just a -> pure { effect: "seat:speak", round, seats: [ fromMaybe "" (toString a) ] }
    _, _ -> pure { effect: "narrate", round, seats: [] }

keyOfExchange :: Exchange -> Key
keyOfExchange x = { effect: x.effect, round: x.round, seats: x.seats }

-- | A replay of a recording: each call answers with the next recorded exchange, which must be the
-- | same effect, round and seats. The prompt text plays no part in the match. A call past the end of
-- | the recording, or one whose key is not the next recorded key, is an error and is not answered.
replayCall :: Recording -> Effect Call
replayCall rec = do
  next <- Ref.new rec
  pure \_ input -> case keyOfPayload input of
    Left why -> throwError (error ("the payload is not a recorded call: " <> why))
    Right k -> do
      queue <- liftEffect (Ref.read next)
      case uncons queue of
        Nothing -> throwError (error "no recorded call matches this request: the recording is spent")
        Just { head: x, tail }
          | keyOfExchange x /= k -> throwError (error ("no recorded call matches this request: the next recorded call is " <> show (keyOfExchange x) <> ", not " <> show k))
          | otherwise -> do
              liftEffect (Ref.write tail next)
              pure { text: replyTextOf x, raw: "" }

-- | The recording file's text, decoded.
decodeRecording :: String -> Either String Recording
decodeRecording = readJson
