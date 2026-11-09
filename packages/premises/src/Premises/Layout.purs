-- | The compiled tree's layout under the premise root: the compiled compositions and Premises, and
-- | where the fixtures and the capstone are. It names no premise.
module Premises.Layout where

import Prelude ((<>))

-- Files of a composition's directory --------------------------------------------------------

compositionFile :: String
compositionFile = "composition.json"

instantiationFile :: String
instantiationFile = "instantiation.json"

presetsFile :: String
presetsFile = "presets.json"

-- Paths under the premise root ----------------------------------------------------------------

compositionsDir :: String
compositionsDir = "compositions"

premisesDir :: String
premisesDir = "premises"

-- | A composition's directory under the premise root (`compositions/meridian/v0`).
compositionDir :: String -> String -> String
compositionDir id version = compositionsDir <> "/" <> id <> "/" <> version

-- | A Premise's file under the premise root (`premises/meridian-energy/v0.json`).
premiseFilePath :: String -> String -> String
premiseFilePath id version = premisesDir <> "/" <> id <> "/" <> version <> ".json"

fixturesDir :: String
fixturesDir = "fixtures"

capstoneDir :: String
capstoneDir = "capstone"

-- | A compiled fixture's path.
fixturePath :: String -> String
fixturePath name = fixturesDir <> "/" <> name <> ".json"

capstoneTranscriptFile :: String
capstoneTranscriptFile = capstoneDir <> "/transcript.json"
