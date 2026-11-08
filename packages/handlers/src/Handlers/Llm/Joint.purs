-- | The joint pass (docs/04 §11): reads a round's transcripts together into Proposals.
module Handlers.Llm.Joint
  ( joint
  , interpreter
  , markets
  ) where

import Prelude
import Core.Map (values)
import Schema.Joint (ProposedJoint, jointProposals, jointReplyShape)
import Core.Shape (Format(..), encodeWith)
import Core.Types (MarketId)
import Core.World (WorldState)
import Data.Either (Either(..))
import Data.Map as Map
import Effect.Aff (Aff)
import Effects.Signatures (InterpretF(..))
import Handlers.Llm.Wire (JointRequest, decodeJointReply, jointRequestShape)
import Handlers.Llm.Prompt (interpretPrompt)
import Handlers.Llm.Backend (Backend, Key, maxCorrections, seatText, send, correction)

-- | The joint pass: one call, then up to `maxCorrections` corrections while the reply does not decode.
-- | The first reply that decodes is the result. If the last correction does not decode either, the
-- | result is that decode error (the caller tells the speakers and submits nothing).
joint :: Backend -> Key -> JointRequest -> Aff (Either String (Array ProposedJoint))
joint b key request = go 0 original
  where
  original = encodeWith Snapshot jointRequestShape request
  go n payload = do
    out <- send b "interpret" key interpretPrompt payload jointReplyShape decodeJointReply
    case out.decoded of
      Right r -> pure (Right r.proposals)
      Left err
        | n < maxCorrections -> go (n + 1) (correction original out.text err)
        | otherwise -> pure (Left err)

-- | The round's transcripts to Proposals, one joint pass (`joint`). A reply that still does not
-- | decode after the corrections is a `Left` (the caller tells the speakers and submits nothing).
-- | Entries the engine could not use are dropped by the joint pass rule (`Core.Proposal.jointProposals`).
-- | `w` supplies only the Entity and Market ids, which no round changes, so the initial world serves.
interpreter :: Backend -> WorldState -> InterpretF ~> Aff
interpreter b w (Interpret round transcripts k) = k <$> do
  let
    request :: JointRequest
    request = { round, transcripts, entities: map _.id (values w.entities), markets: markets w }
    key = { round, seats: map (\t -> seatText t.seat) transcripts }
  r <- joint b key request
  pure (map (jointProposals (\s -> Map.member s w.entities) transcripts) r)

markets :: WorldState -> Array MarketId
markets w = map _.id (values w.markets)
