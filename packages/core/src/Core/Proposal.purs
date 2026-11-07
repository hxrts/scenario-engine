-- | Proposal assembly rules (docs/02_type_system.md §3.3, §8.3): how a round's Proposals are
-- | numbered and identified, which Proposals an Entity is asked to sign, and how the signatures
-- | attach to them. Pure; the round runner (`Effects.Round`) calls these and runs the effects.
module Core.Proposal
  ( ProposalBody
  , proposalsOf
  , numberJoint
  , jointProposalsOf
  , signableBy
  , assembleSigned
  , submissionOrder
  ) where

import Prelude

import Core.Query (isAggregate)
import Core.Types (CollectiveProcedureId, Entity, EntityId, Intent, Proposal, ProposalId, RoundIndex, SignedProposal, proposalId, submissionIndex)
import Data.Array (concatMap, elem, filter, mapWithIndex)
import Data.Either (Either, either)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))

-- | What a Proposal says before its id and author are assigned (a policy's Draft has this shape).
type ProposalBody = { intents :: Array Intent, signers :: Array EntityId, procedure :: Maybe CollectiveProcedureId }

-- | The Proposals of one author in a round. Each takes the id of its index in the author's
-- | submission, so the same drafts always get the same ids (replay depends on this).
proposalsOf :: RoundIndex -> EntityId -> Array ProposalBody -> Array Proposal
proposalsOf r author = mapWithIndex \i d -> proposalAt r author i d

proposalAt :: RoundIndex -> EntityId -> Int -> ProposalBody -> Proposal
proposalAt r author index d =
  { id: proposalId r author index, author, intents: d.intents, signers: d.signers, procedure: d.procedure }

-- | The joint pass's Proposals, each with its author's index counted across the joint reply, ids
-- | assigned as `proposalsOf` assigns a seat's own, so that replay assigns the same ids.
numberJoint :: RoundIndex -> Array { author :: EntityId, draft :: ProposalBody, signed :: Array EntityId } -> Array { proposal :: Proposal, signed :: Array EntityId }
numberJoint r joint = (mapAccumL step Map.empty joint).value
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

-- | The Proposals an Entity is asked to sign: those that name it as a signer, that it did not
-- | author, and only when it is not an Aggregate.
signableBy :: Entity -> Array Proposal -> Array Proposal
signableBy e = filter (\p -> e.id `elem` p.signers && p.author /= e.id && not (isAggregate e))

-- | Each Proposal with the Entities that signed it: the Entities whose Sign named its id, in the
-- | order given, then the joint pass's signers for it.
assembleSigned :: Array Proposal -> Array { entity :: EntityId, ids :: Array ProposalId } -> Array { proposal :: Proposal, signed :: Array EntityId } -> Array SignedProposal
assembleSigned proposals signatures jointNumbered = map
  (\p -> { proposal: p, signatures: map _.entity (filter (\s -> p.id `elem` s.ids) signatures) <> jointSigned p.id })
  proposals
  where
  jointSigned pid = concatMap _.signed (filter (\j -> j.proposal.id == pid) jointNumbered)

-- | The canonical order of SignedProposals: by author, then by the numeric index of the author's
-- | submission (so index 10 follows index 2), then by id. The engine commits in this order and
-- | replay seats answer in it.
submissionOrder :: SignedProposal -> Tuple String (Tuple Int String)
submissionOrder { proposal: p } = Tuple (show p.author) (Tuple index (show p.id))
  where
  index = fromMaybe 0 (submissionIndex p.id)
