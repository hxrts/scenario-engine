-- | The compiled tree's layout under the premise root: the section files of a premise directory, and
-- | where the fixtures and the capstone are. It names no premise.
module Premises.Layout where

import Prelude ((<>))

-- Files (the premise package's JSON sections and the Gameboard's instantiation) ----------

premiseFile :: String
premiseFile = "premise.json"

resourcesFile :: String
resourcesFile = "resources.json"

marketsFile :: String
marketsFile = "markets.json"

transformationsFile :: String
transformationsFile = "transformations.json"

circulationFile :: String
circulationFile = "circulation.json"

entitiesFile :: String
entitiesFile = "entities.json"

representationsFile :: String
representationsFile = "representations.json"

gameRulesFile :: String
gameRulesFile = "game-rules.json"

policiesFile :: String
policiesFile = "policies.json"

instantiationFile :: String
instantiationFile = "instantiation.json"

presetsFile :: String
presetsFile = "presets.json"

-- Paths under the premise root ----------------------------------------------------------------

fixturesDir :: String
fixturesDir = "fixtures"

capstoneDir :: String
capstoneDir = "capstone"

-- | A compiled fixture's path.
fixturePath :: String -> String
fixturePath name = fixturesDir <> "/" <> name <> ".json"

capstoneRunFile :: String
capstoneRunFile = capstoneDir <> "/run.json"

capstoneTranscriptFile :: String
capstoneTranscriptFile = capstoneDir <> "/transcript.json"
