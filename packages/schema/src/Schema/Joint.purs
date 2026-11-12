-- | The joint pass's values (docs/04 §11): a seat's transcript, a candidate Transaction as the model states it, the
-- | rule that reconciles those, and how the result is numbered. Pure; the handlers and the round runner
-- | run the effects.
module Schema.Joint
  ( Transcript
  , JointCandidateTransaction
  , ProposedJoint
  , transcriptShape
  , proposedJointShape
  , jointReplyShape
  , numberJoint
  , jointCandidateTransactionsOf
  , jointCandidateTransactions
  ) where

import Prelude

import Laws.CandidateTransaction (CandidateTransactionBody, candidateTransactionAt)
import Core.Shape (recordShape, Shape, arrayOf, field, fieldOf, object, optionalOf, recordOf, requiredOf, stringShape, (&.))
import Core.Num (Q)
import Laws.Types (refIdShape, ActorId, ExchangeId, CandidateTransaction, CandidateTransactionCore, RoundIndex, idShape, intentShape)
import Data.Array (elem, filter, mapMaybe, nub, null)
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (mapAccumL)

-- | One seat's transcript of a round: its id and what it said.
-- | One seat's words for a round. `enteredBy` names who typed them when that was not the seat itself (a facilitator
-- | entering a seat's text in a live run); absent, the seat spoke.
type Transcript = { seat :: ActorId, text :: String, enteredBy :: Maybe String }

transcriptShape :: Shape Transcript
transcriptShape = object "Transcript" ado
  seat <- fieldOf "seat" "the seat that spoke" idShape _.seat
  text <- fieldOf "text" "what it said" stringShape _.text
  enteredBy <- fieldOf "enteredBy" "who typed the text, when not the seat itself" (optionalOf stringShape) _.enteredBy
  in { seat, text, enteredBy }

-- | A candidate Transaction reconciled by the joint pass: its author, its body (Intents, and the signers the joint candidate Transaction
-- | asks for besides the author), and the seats whose transcripts describe it. The engine signs for
-- | `signed` (and the author) and lapses the candidate Transaction when a signer is missing.
type JointCandidateTransaction = { author :: ActorId, draft :: CandidateTransactionBody, signed :: Array ActorId }

-- | One candidate Transaction as the model states it in a joint pass's reply, before the engine's checks: its
-- | author, its Intents, the other signers the joint candidate Transaction asks for, and the seats whose transcripts describe
-- | it. `jointProposals` reconciles these into JointProposals.
type ProposedJoint = Record (CandidateTransactionCore ExchangeId Q (author :: ActorId, agreed :: Array ActorId))

proposedJointShape :: Shape ProposedJoint
proposedJointShape = recordOf "ProposedJoint"
  ( field @"author" "the seat that submits it" refIdShape
      &. field @"intents" "the Intents" (arrayOf intentShape)
      &. field @"signers" "the other Actors the joint CandidateTransaction asks for" (arrayOf refIdShape)
      &. field @"agreed" "the seats whose transcripts describe it" (arrayOf refIdShape)
  )

-- | The candidate Transactions of a reply; one malformed entry fails only the reply it is in.
proposedJointsShape :: Shape (Array ProposedJoint)
proposedJointsShape = arrayOf proposedJointShape

-- | A joint pass's reply: one object whose `proposals` are the candidate Transactions as the model states them.
jointReplyShape :: Shape { candidateTransactions :: Array ProposedJoint }
jointReplyShape = recordOf "JointReply"
  ( field @"candidateTransactions" "the CandidateTransactions the transcripts describe" (requiredOf proposedJointsShape)
  )

-- | The joint pass's candidate Transactions, each numbered after the author's `own` candidate Transactions of the round and
-- | counted across the joint reply, ids assigned as `proposalsOf` assigns a seat's own, so that
-- | replay assigns the same ids.
numberJoint :: RoundIndex -> Map ActorId Int -> Array JointCandidateTransaction -> Array { candidateTransaction :: CandidateTransaction, signed :: Array ActorId }
numberJoint r own joint = (mapAccumL step own joint).value
  where
  step seen j =
    let
      n = fromMaybe 0 (Map.lookup j.author seen)
    in
      { accum: Map.insert j.author (n + 1) seen
      , value: { candidateTransaction: candidateTransactionAt r j.author n j.draft, signed: j.signed }
      }

-- | A failed joint reply yields no candidate Transactions: the round proceeds with the seats' own candidate Transactions.
jointCandidateTransactionsOf :: forall a. Either String (Array a) -> Array a
jointCandidateTransactionsOf = either (const []) identity

-- | The joint pass's rule (docs/04 §11): a candidate Transaction whose author did not speak, and a candidate Transaction with no
-- | Intents, are dropped. Signers are kept only when `known` says they are Actors of the World, and
-- | the author is never its own signer. Only speakers can be recorded as signing: `signed` is the
-- | author and those of the agreed Actors who spoke.
jointCandidateTransactions :: (ActorId -> Boolean) -> Array Transcript -> Array ProposedJoint -> Array JointCandidateTransaction
jointCandidateTransactions known transcripts = mapMaybe joint
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
