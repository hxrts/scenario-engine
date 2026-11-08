-- | Proposal assembly rules (docs/02_type_system.md §3.3, §8.3): how a round's Proposals are
-- | numbered and identified, which Proposals an Entity is asked to sign, and how the signatures
-- | attach to them. Also the joint pass's domain values (`Transcript`, `JointProposal`). Pure; the
-- | round runner (`Effects.Round`) calls these and runs the effects.
module Core.Proposal
  ( ProposalBody
  , Transcript
  , JointProposal
  , ProposedDeal
  , transcriptShape
  , proposedDealShape
  , proposalsOf
  , proposalAt
  , numberJoint
  , jointProposalsOf
  , jointProposals
  , signableBy
  , assembleSigned
  , submissionOrder
  , proposalBodyShape
  ) where

import Prelude

import Core.Query (isAggregate)
import Core.Shape (Shape, arrayOf, fieldOf, object, optionalOf, stringShape)
import Core.Types (Entity, EntityId, Intent, MarketId, Proposal, ProposalF, ProposalId, RoundIndex, SignedProposal, idShape, intentShape, proposalId, submissionIndex)
import Core.Num (Q)
import Data.Array (concatMap, elem, filter, mapMaybe, mapWithIndex, nub, null, sort)
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))

-- | What a Proposal says before its id and author are assigned: a Strategy's draft, a seat's
-- | submission, and a joint-pass draft all have this shape (`ProposalF` at `MarketId` and `Q`).
type ProposalBody = ProposalF MarketId Q

-- | A ProposalBody in the snapshot form. A record synonym cannot carry a class instance, so this is a
-- | value; a body that crosses a boundary is encoded through `Core.Shape.codec` over it.
proposalBodyShape :: Shape ProposalBody
proposalBodyShape = object "ProposalBody"
  ( (\i s p -> { intents: i, signers: s, procedure: p })
      <$> fieldOf "intents" "the Intents" (arrayOf intentShape) _.intents
      <*> fieldOf "signers" "the Entities asked to sign" (arrayOf idShape) _.signers
      <*> fieldOf "procedure" "the collective procedure, if any" (optionalOf idShape) _.procedure
  )

-- | One seat's transcript of a round: its id and what it said.
type Transcript = { seat :: EntityId, text :: String }

transcriptShape :: Shape Transcript
transcriptShape = object "Transcript"
  ( (\s t -> { seat: s, text: t })
      <$> fieldOf "seat" "the seat that spoke" idShape _.seat
      <*> fieldOf "text" "what it said" stringShape _.text
  )

-- | A Proposal reconciled by the joint pass: its author, its body (Intents, and the signers the deal
-- | asks for besides the author), and the seats whose transcripts describe it. The engine signs for
-- | `signed` (and the author) and lapses the Proposal when a signer is missing.
type JointProposal = { author :: EntityId, draft :: ProposalBody, signed :: Array EntityId }

-- | One Proposal as the model states it in a joint pass's reply, before the engine's checks: its
-- | author, its Intents, the other signers the deal asks for, and the seats whose transcripts describe
-- | it. `jointProposals` reconciles these into JointProposals.
type ProposedDeal = { author :: EntityId, intents :: Array Intent, signers :: Array EntityId, agreed :: Array EntityId }

proposedDealShape :: Shape ProposedDeal
proposedDealShape = object "ProposedDeal"
  ( (\a i s g -> { author: a, intents: i, signers: s, agreed: g })
      <$> fieldOf "author" "the seat that submits it" idShape _.author
      <*> fieldOf "intents" "the Intents" (arrayOf intentShape) _.intents
      <*> fieldOf "signers" "the other Entities the deal asks for" (arrayOf idShape) _.signers
      <*> fieldOf "agreed" "the seats whose transcripts describe it" (arrayOf idShape) _.agreed
  )

-- | The Proposals of one author in a round. Each takes the id of its index in the author's
-- | submission, so the same drafts always get the same ids (replay depends on this).
proposalsOf :: RoundIndex -> EntityId -> Array ProposalBody -> Array Proposal
proposalsOf r author = mapWithIndex \i d -> proposalAt r author i d

proposalAt :: RoundIndex -> EntityId -> Int -> ProposalBody -> Proposal
proposalAt r author index d =
  { id: proposalId r author index, author, intents: d.intents, signers: d.signers, procedure: d.procedure }

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
jointProposals :: (EntityId -> Boolean) -> Array Transcript -> Array ProposedDeal -> Array JointProposal
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
