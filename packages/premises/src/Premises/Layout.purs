-- | The compiled tree's layout under the premise root: the section files of a premise directory, and
-- | where the fixtures and the capstone are. It names no premise.
module Premises.Layout where

import Prelude ((<>))

-- Files (the premise package's JSON sections and the possible World's instantiation) ----------

premiseFile :: String
premiseFile = "premise.json"

resourcesFile :: String
resourcesFile = "resources.json"

exchangesFile :: String
exchangesFile = "exchanges.json"

transformationsFile :: String
transformationsFile = "transformations.json"

circulationFile :: String
circulationFile = "circulation.json"

actorsFile :: String
actorsFile = "actors.json"

representationsFile :: String
representationsFile = "representations.json"

gameRulesFile :: String
gameRulesFile = "game-rules.json"

strategiesFile :: String
strategiesFile = "strategies.json"

instantiationFile :: String
instantiationFile = "instantiation.json"

presetsFile :: String
presetsFile = "presets.json"

-- | A premise's directory under the premise root (`meridian/v0`).
premiseDir :: String -> String -> String
premiseDir id version = id <> "/" <> version

-- Paths under the premise root ----------------------------------------------------------------

fixturesDir :: String
fixturesDir = "fixtures"

capstoneDir :: String
capstoneDir = "capstone"

-- | A compiled fixture's path.
fixturePath :: String -> String
fixturePath name = fixturesDir <> "/" <> name <> ".json"

capstoneTranscriptFile :: String
capstoneTranscriptFile = capstoneDir <> "/transcript.json"
