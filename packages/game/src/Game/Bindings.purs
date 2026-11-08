-- | The bindings a runner plays a game with when no person or model takes part: Strategies, and
-- | scripts read from Proposals already written down (a literal premise's rounds, a stored run's records).
module Game.Bindings
  ( strategyBindings
  , scriptBindings
  ) where

import Prelude

import Core.Proposal (submissionOrder)
import Core.Strategy (Strategy)
import Core.Types (EntityId, RoundIndex, SignedProposal)
import Data.Array (concatMap, elem, filter, nub, sortWith)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Game.Types (Bindings, ScriptRound, SeatBinding(..))

-- | Each named seat bound to its Strategy.
strategyBindings :: Array (Tuple EntityId Strategy) -> Bindings
strategyBindings = Map.fromFoldable <<< map (map Strategy)

-- | The Script binding of every Entity that authors or signs a Proposal in the rounds. A seat's
-- | Proposals are in submission order, since `proposalsOf` numbers them by position.
scriptBindings :: forall r. Array { round :: RoundIndex, inputs :: Array SignedProposal | r } -> Bindings
scriptBindings rounds = Map.fromFoldable (map (\e -> Tuple e (Script (map (scriptRound e) rounds))) mentioned)
  where
  mentioned :: Array EntityId
  mentioned = nub (concatMap (\r -> concatMap (\s -> [ s.proposal.author ] <> s.signatures) r.inputs) rounds)

  scriptRound :: EntityId -> { round :: RoundIndex, inputs :: Array SignedProposal | r } -> ScriptRound
  scriptRound e r =
    { round: r.round
    , propose: map (\s -> { intents: s.proposal.intents, signers: s.proposal.signers, procedure: s.proposal.procedure }) (sortWith submissionOrder (filter (\s -> s.proposal.author == e) r.inputs))
    , sign: map _.proposal.id (filter (\s -> e `elem` s.signatures) r.inputs)
    }
