-- | candidate Transaction assembly rules (docs/02_kernel.md §3.3, §8.3): how a round's candidate Transactions are
-- | numbered and identified, which candidate Transactions an Actor is asked to sign, and how the signatures
-- | attach to them. Pure; the round runner
-- | (`Effects.Round`) calls these and runs the effects.
module Laws.CandidateTransaction
  ( CandidateTransactionBody
  , candidateTransactionsOf
  , candidateTransactionAt
  , signableBy
  , assembleSigned
  , submissionOrder
  ) where

import Prelude

import Laws.Query (isAggregate)
import Laws.Types (Actor, ActorId, ExchangeId, CandidateTransaction, CandidateTransactionF, CandidateTransactionId, RoundIndex, SignedCandidateTransaction, candidateTransactionId, submissionIndex)
import Core.Num (Q)
import Data.Array (concatMap, elem, filter, mapWithIndex, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

-- | What a candidate Transaction says before its id and author are assigned: a StrategyRules's draft, a seat's
-- | submission, and a joint-pass draft all have this shape (`ProposalF` at `ExchangeId` and `Q`).
type CandidateTransactionBody = CandidateTransactionF ExchangeId Q

-- | The candidate Transactions of one author in a round. Each takes the id of its index in the author's
-- | submission, so the same drafts always get the same ids (replay depends on this).
candidateTransactionsOf :: RoundIndex -> ActorId -> Array CandidateTransactionBody -> Array CandidateTransaction
candidateTransactionsOf r author = mapWithIndex \i d -> candidateTransactionAt r author i d

candidateTransactionAt :: RoundIndex -> ActorId -> Int -> CandidateTransactionBody -> CandidateTransaction
candidateTransactionAt r author index d =
  { id: candidateTransactionId r author index, author, intents: d.intents, signers: d.signers, procedure: d.procedure }

-- | The candidate Transactions an Actor is asked to sign: those that name it as a signer, that it did not
-- | author, and only when it is not an Aggregate.
signableBy :: Actor -> Array CandidateTransaction -> Array CandidateTransaction
signableBy e = filter (\p -> e.id `elem` p.signers && p.author /= e.id && not (isAggregate e))

-- | Each candidate Transaction with the Actors that signed it: the Actors whose Sign named its id and the
-- | joint pass's signers for it, once each and in id order, so a replay assembles the same list.
assembleSigned :: Array CandidateTransaction -> Array { actor :: ActorId, ids :: Array CandidateTransactionId } -> Array { candidateTransaction :: CandidateTransaction, signed :: Array ActorId } -> Array SignedCandidateTransaction
assembleSigned candidateTransactions signatures jointNumbered = map
  (\p -> { candidateTransaction: p, signatures: sort (nub (map _.actor (filter (\s -> p.id `elem` s.ids) signatures) <> jointSigned p.id)) })
  candidateTransactions
  where
  jointSigned pid = concatMap _.signed (filter (\j -> j.candidateTransaction.id == pid) jointNumbered)

-- | The canonical order of SignedProposals: by author, then by the numeric index of the author's
-- | submission (so index 10 follows index 2), then by id. The engine commits in this order and
-- | replay seats answer in it.
submissionOrder :: SignedCandidateTransaction -> Tuple String (Tuple Int String)
submissionOrder { candidateTransaction: p } = Tuple (show p.author) (Tuple index (show p.id))
  where
  index = fromMaybe 0 (submissionIndex p.id)
