-- | The joint pass (docs/04 §11): reads a round's transcripts together into candidate Transactions.
module Handlers.Llm.Joint
  ( joint
  , interpreter
  , exchanges
  , jointRequest
  , unknownNames
  ) where

import Prelude

import Laws.World (WorldFields, namesOfKind, WorldState, templateTerms)

import Core.Shape (allValues, kindsNamed, namesIn, Format(..), encodeWith)
import Data.Argonaut.Core (Json)
import Data.Array (elem, filter, mapMaybe, nub, null)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Laws.Types (RefKind, ExchangeId, RoundIndex, Rule)
import Core.Map (values)
import Schema.Joint (JointCandidateTransaction, ProposedJoint, Transcript, jointCandidateTransactions)
import Data.Either (Either(..))
import Data.Map as Map
import Effect.Aff (Aff)
import Handlers.Llm.Call (interpretCall)
import Handlers.Llm.Wire (JointRequest)
import Handlers.Llm.Backend (Backend, maxCorrections, send, correction)

-- | The joint pass: one call, then up to `maxCorrections` corrections while the reply does not decode.
-- | The first reply that decodes is the result. If the last correction does not decode either, the
-- | result is that decode error (the caller tells the speakers and submits nothing).
joint :: Backend -> JointRequest -> Aff (Either String (Array ProposedJoint))
joint b request = go 0 original
  where
  original = encodeWith Snapshot interpretCall.request request
  go n payload = do
    out <- send b interpretCall (interpretCall.key request) payload
    case out.decoded of
      Right r -> case unknownNames request (encodeWith Snapshot interpretCall.wire r) of
        bad
          | not (null bad) && n < maxCorrections -> go (n + 1) (correction original out.text (joinWith "; " bad))
          -- after the corrections the reply stands, and the engine refuses what it cannot use, on the record
          | otherwise -> pure (Right r.candidateTransactions)
      Left err
        | n < maxCorrections -> go (n + 1) (correction original out.text err)
        | otherwise -> pure (Left err)

-- | The round's transcripts to candidate Transactions, one joint pass (`joint`). A reply that still does not
-- | decode after the corrections is a `Left` (the caller tells the speakers and submits nothing).
-- | Entries the engine could not use are dropped by the joint pass rule (`Laws.candidate Transaction.jointProposals`).
-- | `w` supplies only the ids and agreement templates, which no round changes, so the initial world serves.
interpreter :: Backend -> Array Rule -> WorldState -> RoundIndex -> Array Transcript -> Aff (Either String (Array JointCandidateTransaction))
interpreter b rules w round transcripts = do
  let
    request :: JointRequest
    request = jointRequest w rules round transcripts
  r <- joint b request
  pure (map (jointCandidateTransactions (\s -> Map.member s w.actors) transcripts) r)

-- | The one way a joint pass's payload is built. Its names are the World's, for every kind the reply's description
-- | can name, so any Premise supplies them and none can leave a kind out; and the Rules a seat can enact, each in its
-- | declared form, for the reply to copy.
jointRequest :: forall r. Record (WorldFields r) -> Array Rule -> RoundIndex -> Array Transcript -> JointRequest
jointRequest w rules round transcripts = { round, transcripts, names: Object.fromFoldable (map entry kinds), templates: map templateTerms w.templates, rules }
  where
  kinds = filter (\k -> show k `elem` kindsNamed interpretCall.wire.desc) (allValues :: Array RefKind)
  entry k = Tuple (show k) (namesOfKind w k)

-- | What a reply names that its payload does not offer, each with the names it could have used.
unknownNames :: JointRequest -> Json -> Array String
unknownNames request reply = nub (mapMaybe unknown (namesIn interpretCall.wire.desc reply))
  where
  unknown (Tuple kind name) = case Object.lookup kind request.names of
    Just names | not (name `elem` names) -> Just ("unknown " <> kind <> " " <> name <> "; use one of " <> joinWith ", " names)
    _ -> Nothing

exchanges :: WorldState -> Array ExchangeId
exchanges w = map _.id (values w.exchanges)
