-- | One run's assembled state (docs/04 §9): the Game a runner plays. A Game is data:
-- | the initial World State, the in-world Strategies, the rounds still to play, and one SeatBinding per
-- | seat. Every runner (the simulator, live play) reads the same Game.
module Assembly.Types
  ( Game
  , SeatBinding(..)
  , ScriptRound
  , Bindings
  ) where

import Laws.CandidateTransaction (CandidateTransactionBody)
import Laws.StrategyRules (InWorld, StrategyRules)
import Laws.Types (ActorId, CandidateTransactionId, RoundIndex)
import Laws.World (Manifest, WorldState)
import Data.Map (Map)

-- | What answers a seat's Propose and Sign steps. Agent and Human seats are answered by live play; a
-- | simulation rejects them (`Assembly.Error.NotSimulable`).
data SeatBinding
  = StrategyRules StrategyRules -- a declarative StrategyRules, interpreted by `strategy`
  | Script (Array ScriptRound) -- literal candidate Transactions and signatures per round
  | Agent -- a model call per round (live play only)
  | Human -- a person's transcript per round (live play only)

-- | A seat's literal answer for one round: the candidate Transaction bodies it submits and the candidate Transactions it signs.
-- | The runner assigns the author (`proposalAt`), so a script cannot submit as another seat.
type ScriptRound = { round :: RoundIndex, propose :: Array CandidateTransactionBody, sign :: Array CandidateTransactionId }

-- | The seat bindings of a game, by seat: exactly the configured seats (`Assembly.Assemble.assemble`
-- | rejects anything else). A seat that should stay silent is bound explicitly, `Script []`.
type Bindings = Map ActorId SeatBinding

-- | A game ready to run: the manifest, the initial world, the in-world Strategies, the rounds to play,
-- | and the seats.
type Game =
  { manifest :: Manifest
  , world :: WorldState
  , inWorld :: InWorld
  , rounds :: Int
  , seats :: Bindings
  }

