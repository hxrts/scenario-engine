-- | One run's assembled state (docs/04 §8): the Game a runner plays. A Game is data:
-- | the initial World State, the in-world Strategies, the rounds still to play, and one SeatBinding per
-- | seat. Every runner (the simulator, live play) reads the same Game.
module Game.Types
  ( Game
  , SeatBinding(..)
  , ScriptRound
  , Bindings
  ) where

import Core.Proposal (ProposalBody)
import Core.Strategy (InWorld, Strategy)
import Core.Types (EntityId, ProposalId, RoundIndex)
import Core.World (Manifest, WorldState)
import Data.Map (Map)

-- | What answers a seat's Propose and Sign steps. Agent and Human seats are answered by live play; a
-- | simulation rejects them (`Game.Error.NotSimulable`).
data SeatBinding
  = Strategy Strategy -- a declarative Strategy, interpreted by `policy`
  | Script (Array ScriptRound) -- literal Proposals and signatures per round
  | Agent -- a model call per round (live play only)
  | Human -- a person's transcript per round (live play only)

-- | A seat's literal answer for one round: the Proposal bodies it submits and the Proposals it signs.
-- | The runner assigns the author (`proposalAt`), so a script cannot submit as another seat.
type ScriptRound = { round :: RoundIndex, propose :: Array ProposalBody, sign :: Array ProposalId }

-- | The seat bindings of a game, by seat: exactly the configured seats (`Game.Assemble.assemble`
-- | rejects anything else). A seat that should stay silent is bound explicitly, `Script []`.
type Bindings = Map EntityId SeatBinding

-- | A game ready to run: the manifest, the initial world, the in-world Strategies, the rounds to play,
-- | and the seats.
type Game =
  { manifest :: Manifest
  , world :: WorldState
  , inWorld :: InWorld
  , rounds :: Int
  , seats :: Bindings
  }

