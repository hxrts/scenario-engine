-- | The compiled tree's layout under the premise root: the compiled compositions and Premises, and
-- | where the fixtures are. It names no premise.
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

-- | The compositions in the order a launch offers them, the first its default (`compositions/order.json`):
-- | each `<id>/<version>`. Written from `Premises.Catalog`.
orderFile :: String
orderFile = compositionsDir <> "/order.json"

-- | A composition's directory under the premise root (`compositions/pacific/v0`).
compositionDir :: String -> String -> String
compositionDir id version = compositionsDir <> "/" <> id <> "/" <> version

-- | A Premise's file under the premise root (`premises/bilateral-economy/v0.json`).
premiseFilePath :: String -> String -> String
premiseFilePath id version = premisesDir <> "/" <> id <> "/" <> version <> ".json"

fixturesDir :: String
fixturesDir = "fixtures"

-- | A compiled fixture's path.
fixturePath :: String -> String
fixturePath name = fixturesDir <> "/" <> name <> ".json"

