-- | The impure vocabulary (docs/02_type_system.md §8.4): the effect rows and their functor types.
-- | Signatures only; nothing here implements an effect. Handlers in `handlers` interpret them.
module Effects.Signatures
  ( SEAT_INPUT
  , SeatInputF(..)
  , RECORD
  , RecordF(..)
  , INTERPRET
  , InterpretF(..)
  , Transcript
  , JointProposal
  , NARRATE
  , NarrateF(..)
  , CLOCK
  , ClockF(..)
  , Deadline(..)
  ) where

import Prelude

import Core.Types (EntityId, ProposalId, RoundIndex)
import Data.Either (Either)
import Core.World (RoundResult, View)
import Policy.Interpret (Draft)

-- | A seat's Proposals in Propose, its signatures in Sign, and its transcript for the round in
-- | Speak (the seat's words, read by the round's joint pass; "" when it says nothing).
data SeatInputF a
  = Propose EntityId View (Array Draft -> a)
  | Sign EntityId View (Array ProposalId -> a)
  | Speak EntityId View (String -> a)

derive instance Functor SeatInputF

type SEAT_INPUT r = (seatInput :: SeatInputF | r)

-- | Append a round's result to the run's records.
data RecordF a = Record RoundResult a

derive instance Functor RecordF

type RECORD r = (record :: RecordF | r)

-- | One seat's transcript of the round: its id and what it said.
type Transcript = { seat :: EntityId, text :: String }

-- | A Proposal reconciled by the joint pass: its author, its Draft (Intents, and the signers
-- | the deal asks for besides the author), and the seats whose transcripts describe it. The
-- | engine signs for `signed` (and the author) and lapses the Proposal when a signer is missing.
type JointProposal = { author :: EntityId, draft :: Draft, signed :: Array EntityId }

-- | One joint pass over a round's transcripts (live play): the Proposals they reconcile into.
data InterpretF a = Interpret RoundIndex (Array Transcript) (Either String (Array JointProposal) -> a)

derive instance Functor InterpretF

type INTERPRET r = (interpret :: InterpretF | r)

-- | Resolved events to the front page and balance-sheet text; narration adds no facts.
data NarrateF a = Narrate RoundResult (String -> a)

derive instance Functor NarrateF

type NARRATE r = (narrate :: NarrateF | r)

-- | Session deadlines (live play only): wait until the propose or sign deadline passes.
data Deadline = ProposeDeadline | SignDeadline

derive instance Eq Deadline

instance Show Deadline where
  show ProposeDeadline = "propose"
  show SignDeadline = "sign"

data ClockF a = Await RoundIndex Deadline a

derive instance Functor ClockF

type CLOCK r = (clock :: ClockF | r)
