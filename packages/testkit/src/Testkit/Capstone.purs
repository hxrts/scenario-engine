-- | The capstone run as the test suites read it: the Meridian premise assembled with the capstone's seat
-- | bindings (and its counterfactual's), its expectations and objectives, and the run it produces. The
-- | values come from `Premises.Capstone`; nothing decodes `capstone/run.json`. Test support only.
module Testkit.Capstone (Capstone, load, runOf) where

import Prelude

import Core.Draw (Seed(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Game.Assemble (assemble, withRounds)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Bindings, Game)
import Premises.Capstone as C
import Premises.Capstone (ObjectiveExpect, RoundExpect)
import Simulator.Simulate (SimRun)
import Testkit.Meridian (orThrow, premise, run)

type Capstone =
  { rounds :: Int
  , playerSeed :: Seed
  , game :: Game
  , counterfactual :: Game
  , expectations :: Array RoundExpect
  , objectives :: Array ObjectiveExpect
  , counterfactualObjectives :: Array ObjectiveExpect
  }

-- | The capstone's game and its counterfactual, with their expectations and objectives.
load :: Aff Capstone
load = do
  p <- premise
  game <- assembled p C.seatBindings
  counterfactual <- assembled p C.counterfactualBindings
  pure
    { rounds: C.rounds
    , playerSeed: Seed C.playerSeed
    , game
    , counterfactual
    , expectations: C.expectations
    , objectives: C.baseObjectives
    , counterfactualObjectives: C.counterfactualObjectives
    }
  where
  assembled :: GameboardParts -> Bindings -> Aff Game
  assembled p bindings = orThrow (map (withRounds (Just C.rounds)) (assemble (GameboardPremise p) bindings []))

-- | Play a game to its end under a player seed; a simulation or engine failure is an error.
runOf :: Seed -> Game -> Aff SimRun
runOf = run
