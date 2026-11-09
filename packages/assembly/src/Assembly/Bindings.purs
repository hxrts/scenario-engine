-- | The bindings a runner plays a game with when no person or model takes part: Strategies, and
-- | scripts read from candidate Transactions already written down (a literal premise's rounds, a stored run's records).
module Assembly.Bindings
  ( strategyRulesBindings
  , scriptBindings
  ) where

import Prelude

import Laws.CandidateTransaction (submissionOrder)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorId, RoundIndex, SignedCandidateTransaction)
import Data.Array (concatMap, elem, filter, nub, sortWith)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Assembly.Types (Bindings, ScriptRound, SeatBinding(..))

-- | Each named seat bound to its StrategyRules.
strategyRulesBindings :: Array (Tuple ActorId StrategyRules) -> Bindings
strategyRulesBindings = Map.fromFoldable <<< map (map StrategyRules)

-- | The Script binding of every Actor that authors or signs a candidate Transaction in the rounds. A seat's
-- | candidate Transactions are in submission order, since `proposalsOf` numbers them by position.
scriptBindings :: forall r. Array { round :: RoundIndex, inputs :: Array SignedCandidateTransaction | r } -> Bindings
scriptBindings rounds = Map.fromFoldable (map (\e -> Tuple e (Script (map (scriptRound e) rounds))) mentioned)
  where
  mentioned :: Array ActorId
  mentioned = nub (concatMap (\r -> concatMap (\s -> [ s.candidateTransaction.author ] <> s.signatures) r.inputs) rounds)

  scriptRound :: ActorId -> { round :: RoundIndex, inputs :: Array SignedCandidateTransaction | r } -> ScriptRound
  scriptRound e r =
    { round: r.round
    , propose: map (\s -> { intents: s.candidateTransaction.intents, signers: s.candidateTransaction.signers, procedure: s.candidateTransaction.procedure }) (sortWith submissionOrder (filter (\s -> s.candidateTransaction.author == e) r.inputs))
    , sign: map _.candidateTransaction.id (filter (\s -> e `elem` s.signatures) r.inputs)
    }
