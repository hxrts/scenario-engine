-- | The impure vocabulary (docs/02_type_system.md §8.4): the effect rows and their functor types.
-- | Signatures only; nothing here implements an effect. Handlers in `handlers` interpret them.
module Effects.Signatures
  ( SEAT_INPUT
  , SeatInputF(..)
  , RECORD
  , RecordF(..)
  , INTERPRET
  , InterpretF(..)
  , Interpretation(..)
  , NARRATE
  , NarrateF(..)
  , CLOCK
  , ClockF(..)
  , Deadline(..)
  ) where

import Prelude

import Core.Types (EntityId, ProposalId, RoundIndex)
import Core.World (RoundResult, View)
import Policy.Interpret (Draft)

-- | A seat's Proposals in Propose and its signatures in Sign.
data SeatInputF a
  = Propose EntityId View (Array Draft -> a)
  | Sign EntityId View (Array ProposalId -> a)

derive instance Functor SeatInputF

type SEAT_INPUT r = (seatInput :: SeatInputF | r)

-- | Append a round's result to the run's records.
data RecordF a = Record RoundResult a

derive instance Functor RecordF

type RECORD r = (record :: RecordF | r)

-- | Transcript to candidate Intents (live play). The interpreter proposes; the player confirms.
data Interpretation
  = Understood (Array Draft) (Array ProposalId) -- Proposals to publish, and Proposals to sign
  | Clarify String -- a materially ambiguous action, returned as a question
  | Inadmissible String -- claimed resources or authority the Actor lacks

data InterpretF a = Interpret EntityId View String (Interpretation -> a)

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
