-- | The seat-policy presets offered by the launch form. The server has no endpoint for them, so
-- | this list is kept in step with `packages/scenarios/src/Scenarios/Presets.purs` by a test
-- | (`apps/dashboard/test`) that reads the compiled `presets.json`.
module Dashboard.Presets (presetNames) where

presetNames :: Array String
presetNames =
  [ "Passive"
  , "Conservative"
  , "Aggressive"
  , "Accumulator"
  , "Trader"
  , "Cooperative"
  , "Opportunistic"
  , "Investor"
  , "CrisisResponse"
  , "RandomValid"
  ]
