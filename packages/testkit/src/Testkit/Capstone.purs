-- | The capstone run file (`packages/scenarios/capstone/run.json`) as the test suites read it: its
-- | decoded form, the spec it plays, its counterfactual, and the run it produces. Test support only.
module Testkit.Capstone (load, specOf, counterfactualOf, runOf) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Adapters.Filesystem (fileOps)
import Premises.Load (loadRun)
import Premises.Tree (readRootFile)
import Simulator.Capstone (Capstone, decodeCapstone)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, simulate)

-- | The capstone run file, decoded.
load :: Aff Capstone
load = do
  file <- liftEffect (map (_ >>= loadRun) (readRootFile fileOps "capstone/run.json"))
  case file of
    Left e -> throwError (error e)
    Right j -> case decodeCapstone j of
      Left e -> throwError (error e)
      Right cap -> pure cap

-- | The spec the capstone run plays: its seats, player seed and round count.
specOf :: Capstone -> SimulationSpec
specOf cap = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }

-- | The counterfactual: the same seats, with the named seats replaced.
counterfactualOf :: Capstone -> SimulationSpec
counterfactualOf cap = (specOf cap) { seats = Map.union (map Custom cap.counterfactual) (map Custom cap.seats) }

-- | Play a spec to its end; a simulation or engine failure is an error.
runOf :: Context -> SimulationSpec -> Aff Run
runOf ctx s = case simulate ctx s of
  Left e -> throwError (error (show e))
  Right r -> case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r
