-- | Counterfactuals, player-seed runs and agency (docs/07_simulator.md §10.3, §11). Every
-- | experiment composes the pure `simulate`; none has its own run loop. The views present the results.
module Experiments.Agency
  ( difference
  , counterfactual
  , playerSeedRuns
  , agency
  ) where

import Prelude

import Data.Tuple (Tuple(..))

import Core.Draw (Seed(..))

import Laws.Types (ActorId)
import Laws.World (WorldState)

import Data.Either (Either, hush)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import Assembly.Error (AssemblyError)
import Assembly.Types (PreparedRun)
import Handlers.Simulate (SimRun, simulate)
import Views.Agency (AgencyProfile, agencyProfile)
import Assembly.Assemble (assembleCast, defaultCast)
import Assembly.Composition (CompositionParts)
import Data.Map (Map)
import Schema.Cast (Role(..))

import Views.Difference (Difference, differenceOfWorlds)

finalWorld :: SimRun -> Maybe WorldState
finalWorld r = hush r.final

-- | The difference of two runs' final states; a run that did not finish has no difference.
difference :: SimRun -> SimRun -> Difference
difference base other = case finalWorld base, finalWorld other of
  Just a, Just b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }

-- | Change exactly one thing and replay with both seeds fixed (§10.3).
counterfactual :: Seed -> PreparedRun -> (PreparedRun -> PreparedRun) -> Either AssemblyError { base :: SimRun, variant :: SimRun, difference :: Difference }
counterfactual seed game change = do
  base <- simulate seed game
  counterfactualFrom seed base game change

-- | The same, over a base run already played: only the variant is run.
counterfactualFrom :: Seed -> SimRun -> PreparedRun -> (PreparedRun -> PreparedRun) -> Either AssemblyError { base :: SimRun, variant :: SimRun, difference :: Difference }
counterfactualFrom seed base game change = do
  variant <- simulate seed (change game)
  pure { base, variant, difference: difference base variant }

-- | Player-seed distribution: the game seed fixed, the player seed varied (§11).
playerSeedRuns :: PreparedRun -> Array Int -> Either AssemblyError (Array SimRun)
playerSeedRuns game seeds = traverse (\s -> simulate (Seed s) game) seeds

-- | For each seat with an alternative, replay with that seat alone cast to it, everything else fixed: the baseline
-- | preset's cast, and both seeds. Each seat keeps its routine. An unknown preset is an error, not a silent baseline.
agency :: CompositionParts -> Seed -> String -> Map ActorId String -> Either AssemblyError (Array AgencyProfile)
agency p seed baseline alternatives = do
  base <- assembleCast p (defaultCast p baseline) [] Nothing >>= simulate seed
  traverse (profile base) (Map.toUnfoldable alternatives)
  where
  profile base (Tuple explicitActor alternative) = do
    variant <- assembleCast p (Map.insert explicitActor (CastPreset alternative) (defaultCast p baseline)) [] Nothing >>= simulate seed
    pure (agencyProfile p.composition explicitActor alternative { rounds: base.records } { rounds: variant.records })
