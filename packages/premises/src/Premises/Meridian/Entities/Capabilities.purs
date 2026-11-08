-- | Meridian's shared capability sets: the named lists each Entity's capabilities are
-- | built from. A market to mayTrade in, the military powers of a government, the legislative
-- | powers, and the shipping facility. Each Actor's list is the concatenation of the sets it
-- | holds.
module Premises.Meridian.Entities.Capabilities
  ( dealer
  , allied
  , military
  , shipping
  , government
  ) where

import Prelude ((<>))

import Core.Types (Capability)
import Schema.Build.Base (mayTrade, mayTransfer, mayTransform)
import Premises.Meridian.Names as N
import Premises.Rules (mayEnactAccessBan, mayEnactTariff)

-- | Trading in one Energy market, and holding Money and Energy.
dealer :: String -> Array Capability
dealer m = [ mayTrade m, mayTransfer N.money, mayTransfer N.energy ]

-- | Trading in the defence-goods Market: the US and its allies (DE, UK, JP), who are listed in
-- | its access rule (Meridian.purs `defenceAccess`).
allied :: Array Capability
allied = [ mayTrade N.defenceGoods ]

-- | A government's military powers. `procurement` is the Actor's own procurement facility.
military :: String -> Array Capability
military procurement =
  [ mayTransform N.militaryDeployment, mayTransform N.majorMilitaryAction, mayTransform N.publicAttention, mayTransform N.humanitarianAid, mayTransfer N.mil, mayTransform procurement ]

-- | Enacting tariffs and access bans.
legislate :: Array Capability
legislate = [ mayEnactTariff, mayEnactAccessBan ]

-- | Running the shipping facility.
shipping :: Array Capability
shipping = [ mayTransform N.energyShipping ]

-- | A government: trading in one Energy market, its military powers with its procurement
-- | facility, its legislative powers, and the shipping facility.
government :: String -> String -> Array Capability
government market procurement = dealer market <> military procurement <> legislate <> shipping
