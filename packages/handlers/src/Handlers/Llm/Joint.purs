-- | The joint pass (docs/04 §11): reads a round's transcripts together into candidate Transactions.
module Handlers.Llm.Joint
  ( joint
  , interpreter
  , exchanges
  ) where

import Prelude
import Core.Map (values)
import Schema.Joint (JointCandidateTransaction, ProposedJoint, Transcript, jointCandidateTransactions)
import Core.Shape (Format(..), encodeWith)
import Laws.Types (ExchangeId, RoundIndex)
import Laws.World (WorldState, templateTerms)
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
      Right r -> pure (Right r.candidateTransactions)
      Left err
        | n < maxCorrections -> go (n + 1) (correction original out.text err)
        | otherwise -> pure (Left err)

-- | The round's transcripts to candidate Transactions, one joint pass (`joint`). A reply that still does not
-- | decode after the corrections is a `Left` (the caller tells the speakers and submits nothing).
-- | Entries the engine could not use are dropped by the joint pass rule (`Laws.candidate Transaction.jointProposals`).
-- | `w` supplies only the ids and agreement templates, which no round changes, so the initial world serves.
interpreter :: Backend -> WorldState -> RoundIndex -> Array Transcript -> Aff (Either String (Array JointCandidateTransaction))
interpreter b w round transcripts = do
  let
    request :: JointRequest
    request = { round, transcripts, actors: map _.id (values w.actors), exchanges: exchanges w, resources: map _.id (values w.resources), transformations: map _.id (values w.transformations), templates: map templateTerms w.templates }
  r <- joint b request
  pure (map (jointCandidateTransactions (\s -> Map.member s w.actors) transcripts) r)

exchanges :: WorldState -> Array ExchangeId
exchanges w = map _.id (values w.exchanges)
