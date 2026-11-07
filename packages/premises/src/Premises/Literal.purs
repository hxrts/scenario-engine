-- | A literal premise (a fixture, docs/05_test_instance.md) as a Game: its World written out in full,
-- | and each seat that proposes or signs in its rounds given those answers as a Script binding. A
-- | literal premise plays through `simulate` like any other game, so its expectations are checked
-- | against the records the engine writes (`Checks`), not against the premise.
module Premises.Literal
  ( literalGame
  ) where

import Prelude

import Core.Types (EntityId, SignedProposal)
import Data.Array (concatMap, elem, filter, length, nub)
import Data.Either (Either)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Game.Assemble (assemble)
import Game.Error (GameError)
import Game.Premise (Premise(..))
import Game.Types (Bindings, Game, ScriptRound, SeatBinding(..))
import Schema.Premise.Fixture (Literal, Round)

-- | The game a literal premise plays: no overrides, its seats bound to their Script answers.
literalGame :: Literal -> Either GameError Game
literalGame l = assemble (LiteralPremise { world: l.world, rounds: length l.rounds }) (literalBindings l.rounds) []

-- | The Script binding of every Entity that authors or signs a Proposal in the rounds: its Proposal
-- | bodies in the order written, and the ids it signs, in each round.
literalBindings :: Array Round -> Bindings
literalBindings rounds = Map.fromFoldable (map (\e -> Tuple e (Script (map (scriptRound e) rounds))) entities)
  where
  entities :: Array EntityId
  entities = nub (concatMap (\r -> concatMap (\s -> [ s.proposal.author ] <> s.signatures) r.inputs) rounds)

  scriptRound :: EntityId -> Round -> ScriptRound
  scriptRound e r =
    { round: r.round
    , propose: map (\s -> { intents: s.proposal.intents, signers: s.proposal.signers, procedure: s.proposal.procedure }) (filter (\s -> s.proposal.author == e) r.inputs)
    , sign: map _.proposal.id (filter (signsIt e) r.inputs)
    }

  signsIt :: EntityId -> SignedProposal -> Boolean
  signsIt e s = e `elem` s.signatures
