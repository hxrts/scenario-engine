-- | Meridian's shared capability sets: the named lists each Entity's capabilities are
-- | built from. A market to trade in, the military powers of a government, the legislative
-- | powers, and the shipping facility. Each Actor's list is the concatenation of the sets it
-- | holds, in the order the JSON has always carried.
module Scenarios.Meridian.Entities.Capabilities (dealer, allied, military, legislate, shipping) where

import DSL.Base (Capability, trade, transfer, transform, enactTariff, enactAccessBan)

-- | Trading in one Energy market, and holding Money and Energy.
dealer :: String -> Array Capability
dealer m = [ trade m, transfer "M", transfer "E" ]

-- | Trading in the defence-goods Market: the US and its allies (DE, UK, JP), who are listed in
-- | its access rule (Meridian.purs `defenceAccess`).
allied :: Array Capability
allied = [ trade "defence-goods" ]

-- | A government's military powers. `procurement` is the Actor's own procurement facility.
military :: String -> Array Capability
military procurement =
  [ transform "military-deployment", transform "major-military-action", transform "public-attention", transform "humanitarian-aid", transfer "Mil", transform procurement ]

-- | Enacting tariffs and access bans.
legislate :: Array Capability
legislate = [ enactTariff, enactAccessBan ]

-- | Running the shipping facility.
shipping :: Array Capability
shipping = [ transform "energy-shipping" ]
