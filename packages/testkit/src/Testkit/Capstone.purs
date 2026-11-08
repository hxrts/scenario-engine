-- | The capstone run as the test suites read it: the Meridian setting assembled with the capstone's seat
-- | bindings (and its counterfactual's), its expectations and objectives, and the run it produces. The
-- | values come from `Settings.Meridian.Capstone`. Test support only.
module Testkit.Capstone (Capstone, load) where

import Prelude

import Core.Draw (Seed(..))
import Effect.Aff (Aff)
import Game.Types (Game)
import Settings.Meridian.Capstone as C
import Settings.Meridian.Capstone (ObjectiveExpect, RoundExpect)
import Testkit.Meridian (orThrow, setting)

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
  p <- setting
  game <- orThrow (C.game p)
  counterfactual <- orThrow (C.counterfactualGame p)
  pure
    { rounds: C.rounds
    , playerSeed: Seed C.playerSeed
    , game
    , counterfactual
    , expectations: C.expectations
    , objectives: C.baseObjectives
    , counterfactualObjectives: C.counterfactualObjectives
    }

