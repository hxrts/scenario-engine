-- | The joint pass's values (docs/04 §11): a seat's transcript, a Proposal as the model states it, the
-- | rule that reconciles those, and how the result is numbered. Pure; the handlers and the round runner
-- | run the effects.
module Schema.Joint
  ( Transcript
  , JointProposal
  , ProposedJoint
  , transcriptShape
  , proposedJointShape
  , jointReplyShape
  , numberJoint
  , jointProposalsOf
  , jointProposals
  ) where

import Prelude

import Core.Proposal (ProposalBody, proposalAt)
import Core.Shape (Shape, arrayOf, field, recordOf, requiredOf, stringShape, (&.))
import Core.Num (Q)
import Core.Types (ProposalCore, MarketId, EntityId, Intent, Proposal, RoundIndex, idShape, intentShape)
import Data.Array (elem, filter, mapMaybe, nub, null)
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (mapAccumL)

-- | One seat's transcript of a round: its id and what it said.
type Transcript = { seat :: EntityId, text :: String }

transcriptShape :: Shape Transcript
transcriptShape = recordOf "Transcript"
  ( field @"seat" "the seat that spoke" idShape
      &. field @"text" "what it said" stringShape
  )

-- | A Proposal reconciled by the joint pass: its author, its body (Intents, and the signers the joint Proposal
-- | asks for besides the author), and the seats whose transcripts describe it. The engine signs for
-- | `signed` (and the author) and lapses the Proposal when a signer is missing.
type JointProposal = { author :: EntityId, draft :: ProposalBody, signed :: Array EntityId }

-- | One Proposal as the model states it in a joint pass's reply, before the engine's checks: its
-- | author, its Intents, the other signers the joint Proposal asks for, and the seats whose transcripts describe
-- | it. `jointProposals` reconciles these into JointProposals.
type ProposedJoint = Record (ProposalCore MarketId Q (author :: EntityId, agreed :: Array EntityId))

proposedJointShape :: Shape ProposedJoint
proposedJointShape = recordOf "ProposedJoint"
  ( field @"author" "the seat that submits it" idShape
      &. field @"intents" "the Intents" (arrayOf intentShape)
      &. field @"signers" "the other Entities the joint Proposal asks for" (arrayOf idShape)
      &. field @"agreed" "the seats whose transcripts describe it" (arrayOf idShape)
  )

-- | The Proposals of a reply; one malformed entry fails only the reply it is in.
proposedJointsShape :: Shape (Array ProposedJoint)
proposedJointsShape = arrayOf proposedJointShape

-- | A joint pass's reply: one object whose `proposals` are the Proposals as the model states them.
jointReplyShape :: Shape { proposals :: Array ProposedJoint }
jointReplyShape = recordOf "JointReply"
  ( field @"proposals" "the Proposals the transcripts describe" (requiredOf proposedJointsShape)
  )

-- | The joint pass's Proposals, each numbered after the author's `own` Proposals of the round and
-- | counted across the joint reply, ids assigned as `proposalsOf` assigns a seat's own, so that
-- | replay assigns the same ids.
numberJoint :: RoundIndex -> Map EntityId Int -> Array JointProposal -> Array { proposal :: Proposal, signed :: Array EntityId }
numberJoint r own joint = (mapAccumL step own joint).value
  where
  step seen j =
    let
      n = fromMaybe 0 (Map.lookup j.author seen)
    in
      { accum: Map.insert j.author (n + 1) seen
      , value: { proposal: proposalAt r j.author n j.draft, signed: j.signed }
      }

-- | A failed joint reply yields no Proposals: the round proceeds with the seats' own Proposals.
jointProposalsOf :: forall a. Either String (Array a) -> Array a
jointProposalsOf = either (const []) identity

-- | The joint pass's rule (docs/04 §11): a Proposal whose author did not speak, and a Proposal with no
-- | Intents, are dropped. Signers are kept only when `known` says they are Entities of the World, and
-- | the author is never its own signer. Only speakers can be recorded as signing: `signed` is the
-- | author and those of the agreed Entities who spoke.
jointProposals :: (EntityId -> Boolean) -> Array Transcript -> Array ProposedJoint -> Array JointProposal
jointProposals known transcripts = mapMaybe joint
  where
  speakers = map _.seat transcripts

  joint p
    | not (p.author `elem` speakers) = Nothing
    | null p.intents = Nothing
    | otherwise = Just
        { author: p.author
        , draft: { intents: p.intents, signers: filter (\s -> s /= p.author && known s) p.signers, procedure: Nothing }
        , signed: nub ([ p.author ] <> filter (\s -> s `elem` speakers) p.agreed)
        }
