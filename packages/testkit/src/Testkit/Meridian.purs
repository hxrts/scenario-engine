-- | The Meridian fixture the analysis and simulator test suites share: the scenario loaded with its
-- | presets, and the fifteen seats every test plays. Test support only; no suite's source depends on it.
module Testkit.Meridian (context, seats) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Adapters.Filesystem (fileOps)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Premises.Load (load, loadPresets)
import Premises.Tree (readRootFile, readPremise)
import Simulator.Simulate (Context)

-- | The Meridian scenario, its configuration and presets, as a simulation context.
context :: Aff Context
context = do
  loaded <- liftEffect (map (_ >>= load) (readPremise fileOps "meridian/v0"))
  presets <- liftEffect (map (_ >>= loadPresets) (readRootFile fileOps "presets.json"))
  case loaded, presets of
    Right { gameboard, config }, Right ps -> pure { gameboard, config, presets: ps }
    Left e, _ -> throwError (error e)
    _, Left e -> throwError (error e)

-- | The Meridian seats every test plays.
seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]
