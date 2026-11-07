-- | Meridian's shared capability sets: the named lists each Entity's capabilities are
-- | built from. A market to trade in, the military powers of a government, the legislative
-- | powers, and the shipping facility. Each Actor's list is the concatenation of the sets it
-- | holds.
module Premises.Meridian.Entities.Capabilities (dealer, allied, military, legislate, shipping, government) where

import Prelude ((<>))

import Schema.Build.Base (Capability, trade, transfer, transform, enactTariff, enactAccessBan)
import Premises.Names as N

-- | Trading in one Energy market, and holding Money and Energy.
dealer :: String -> Array Capability
dealer m = [ trade m, transfer N.money, transfer N.energy ]

-- | Trading in the defence-goods Market: the US and its allies (DE, UK, JP), who are listed in
-- | its access rule (Meridian.purs `defenceAccess`).
allied :: Array Capability
allied = [ trade N.defenceGoods ]

-- | A government's military powers. `procurement` is the Actor's own procurement facility.
military :: String -> Array Capability
military procurement =
  [ transform N.militaryDeployment, transform N.majorMilitaryAction, transform N.publicAttention, transform N.humanitarianAid, transfer N.mil, transform procurement ]

-- | Enacting tariffs and access bans.
legislate :: Array Capability
legislate = [ enactTariff, enactAccessBan ]

-- | Running the shipping facility.
shipping :: Array Capability
shipping = [ transform N.energyShipping ]

-- | A government: trading in one Energy market, its military powers with its procurement
-- | facility, its legislative powers, and the shipping facility.
government :: String -> String -> Array Capability
government market procurement = dealer market <> military procurement <> legislate <> shipping
