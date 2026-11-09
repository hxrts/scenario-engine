-- | Proposal assembly rules (docs/02_type_system.md §3.3, §8.3): how a round's Proposals are
-- | numbered and identified, which Proposals an Entity is asked to sign, and how the signatures
-- | attach to them. Pure; the round runner
-- | (`Effects.Round`) calls these and runs the effects.
module Laws.Proposal
  ( ProposalBody
  , proposalsOf
  , proposalAt
  , signableBy
  , assembleSigned
  , submissionOrder
  ) where

import Prelude

import Laws.Query (isAggregate)
import Laws.Types (Entity, EntityId, MarketId, Proposal, ProposalF, ProposalId, RoundIndex, SignedProposal, proposalId, submissionIndex)
import Core.Num (Q)
import Data.Array (concatMap, elem, filter, mapWithIndex, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

-- | What a Proposal says before its id and author are assigned: a Strategy's draft, a seat's
-- | submission, and a joint-pass draft all have this shape (`ProposalF` at `MarketId` and `Q`).
type ProposalBody = ProposalF MarketId Q

-- | The Proposals of one author in a round. Each takes the id of its index in the author's
-- | submission, so the same drafts always get the same ids (replay depends on this).
proposalsOf :: RoundIndex -> EntityId -> Array ProposalBody -> Array Proposal
proposalsOf r author = mapWithIndex \i d -> proposalAt r author i d

proposalAt :: RoundIndex -> EntityId -> Int -> ProposalBody -> Proposal
proposalAt r author index d =
  { id: proposalId r author index, author, intents: d.intents, signers: d.signers, procedure: d.procedure }

-- | The Proposals an Entity is asked to sign: those that name it as a signer, that it did not
-- | author, and only when it is not an Aggregate.
signableBy :: Entity -> Array Proposal -> Array Proposal
signableBy e = filter (\p -> e.id `elem` p.signers && p.author /= e.id && not (isAggregate e))

-- | Each Proposal with the Entities that signed it: the Entities whose Sign named its id and the
-- | joint pass's signers for it, once each and in id order, so a replay assembles the same list.
assembleSigned :: Array Proposal -> Array { entity :: EntityId, ids :: Array ProposalId } -> Array { proposal :: Proposal, signed :: Array EntityId } -> Array SignedProposal
assembleSigned proposals signatures jointNumbered = map
  (\p -> { proposal: p, signatures: sort (nub (map _.entity (filter (\s -> p.id `elem` s.ids) signatures) <> jointSigned p.id)) })
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
