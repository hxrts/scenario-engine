-- | A literal premise (a fixture, docs/05_test_instance.md) as a Game: its World written out in full,
-- | and each seat that proposes or signs in its rounds given those answers as a Script binding. A
-- | literal premise plays through `simulate` like any other game, so its expectations are checked
-- | against the records the engine writes (`Checks`), not against the premise.
module Premises.Literal
  ( literalGame
  ) where

import Data.Array (length)
import Data.Either (Either)
import Game.Assemble (assemble)
import Game.Bindings (scriptBindings)
import Game.Error (GameError)
import Game.Premise (Premise(..))
import Game.Types (Game)
import Schema.Premise.Fixture (Literal)

-- | The game a literal premise plays: no overrides, its seats bound to their Script answers.
literalGame :: Literal -> Either GameError Game
literalGame l = assemble (LiteralPremise { world: l.world, rounds: length l.rounds }) (scriptBindings l.rounds) []
