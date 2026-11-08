-- | The names of the seat-policy presets (docs/06_simulator.md §4.2), shared by every composition, and
-- | the ones a caller plays when it names none. The strategies are `Premises.Kit.Presets`, a function
-- | of each composition's binding.
module Premises.Presets (presetId, presetNames, defaultPresets) where

-- | Each preset's name.
presetId
  :: { passive :: String
     , conservative :: String
     , aggressive :: String
     , accumulator :: String
     , trader :: String
     , cooperative :: String
     , opportunistic :: String
     , investor :: String
     , crisisResponse :: String
     , randomValid :: String
     , deployer :: String
     }
presetId =
  { passive: "Passive"
  , conservative: "Conservative"
  , aggressive: "Aggressive"
  , accumulator: "Accumulator"
  , trader: "Trader"
  , cooperative: "Cooperative"
  , opportunistic: "Opportunistic"
  , investor: "Investor"
  , crisisResponse: "CrisisResponse"
  , randomValid: "RandomValid"
  , deployer: "Deployer"
  }

-- | The preset names, in the order the launch form offers them.
presetNames :: Array String
presetNames =
  [ presetId.passive
  , presetId.conservative
  , presetId.aggressive
  , presetId.accumulator
  , presetId.trader
  , presetId.cooperative
  , presetId.opportunistic
  , presetId.investor
  , presetId.crisisResponse
  , presetId.randomValid
  , presetId.deployer
  ]

-- | The presets a caller plays when it names none: a command-line run, and the agency analysis's
-- | baseline and alternative.
defaultPresets :: { run :: String, agencyBaseline :: String, agencyAlternative :: String }
defaultPresets = { run: presetId.passive, agencyBaseline: presetId.conservative, agencyAlternative: presetId.aggressive }
