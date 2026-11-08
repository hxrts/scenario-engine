-- | The capstone run as the test suites read it: the Meridian setting assembled with the capstone's seat
-- | bindings (and its counterfactual's), its expectations and objectives, and the run it produces. The
-- | values come from `Settings.Meridian.Capstone`; nothing decodes `capstone/run.json`. Test support only.
module Testkit.Capstone (Capstone, load) where

import Prelude

import Core.Draw (Seed(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Game.Assemble (assemble, withRounds)
import Game.Setting (GameboardParts, Setting(..))
import Game.Types (Bindings, Game)
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
  assembled p bindings = orThrow (map (withRounds (Just C.rounds)) (assemble (GameboardSetting p) bindings []))

