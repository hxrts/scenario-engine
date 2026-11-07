-- | The capstone run as the test suites read it: the Meridian premise assembled with the capstone's seat
-- | bindings (and its counterfactual's), its expectations and objectives, and the run it produces. The
-- | values come from `Premises.Capstone`; nothing decodes `capstone/run.json`. Test support only.
module Testkit.Capstone (Capstone, RoundExpect, ObjectiveExpect, load, runOf) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Types (EntityId, Id(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Game.Assemble (assemble, withRounds)
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Bindings, Game)
import Premises.Capstone as C
import Schema.Decode.Rounds (Expect, decodeExpect)
import Schema.Gameboard (Outcome(..))
import Simulator.Simulate (SimRun, simulate)
import Testkit.Meridian (orThrow, premise)

type RoundExpect = { round :: Int, expect :: Expect }

-- | An objective the run must end with, by its Entity and its outcome.
type ObjectiveExpect = { entity :: EntityId, outcome :: Outcome }

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
  expectations <- orThrow (traverse roundExpect C.expectations)
  pure
    { rounds: C.rounds
    , playerSeed: Seed C.playerSeed
    , game
    , counterfactual
    , expectations
    , objectives: map objective C.baseObjectives
    , counterfactualObjectives: map objective C.counterfactualObjectives
    }
  where
  assembled :: GameboardParts -> Bindings -> Aff Game
  assembled p bindings = orThrow (map (withRounds (Just C.rounds)) (assemble (GameboardPremise p) bindings []))

  roundExpect :: Json -> Either JsonDecodeError RoundExpect
  roundExpect j = do
    o <- decodeJson j :: Either JsonDecodeError { round :: Int, expect :: Json }
    e <- decodeExpect o.expect
    pure { round: o.round, expect: e }

  objective o = { entity: Id o.entity, outcome: outcomeOf o.outcome }

  outcomeOf :: String -> Outcome
  outcomeOf = case _ of
    "met" -> Met
    "notMet" -> NotMet
    _ -> NotEvaluated

-- | Play a game to its end under a player seed; a simulation or engine failure is an error.
runOf :: Seed -> Game -> Aff SimRun
runOf seed g = do
  r <- orThrow (simulate seed g)
  case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r
