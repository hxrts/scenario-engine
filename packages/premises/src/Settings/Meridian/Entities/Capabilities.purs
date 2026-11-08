-- | Meridian's shared capability sets: the named lists each Entity's capabilities are
-- | built from. A market to mayTrade in, the military powers of a government, the legislative
-- | powers, and the shipping facility. Each Actor's list is the concatenation of the sets it
-- | holds.
module Settings.Meridian.Entities.Capabilities
  ( dealer
  , allied
  , defenceAccess
  , military
  , shipping
  , government
  ) where

import Prelude ((<>))

import Data.Array (elem)

import Core.Types (Capability)
import Schema.Build.Base (mayTrade, mayTransfer, mayTransform)
import Settings.Meridian.Names (n)
import Settings.Meridian.Rules (accessBan, tariff)

-- | Trading in one Energy market, and holding Money and Energy.
dealer :: String -> Array Capability
dealer m = [ mayTrade m, mayTransfer n.money, mayTransfer n.energy ]

-- | Who may trade Military: the US and its allies, Germany, the UK, and Japan. The Market's access list
-- | and each Entity's capability both read this one list.
defenceAccess :: Array String
defenceAccess = [ n.us, n.de, n.uk, n.jp ]

-- | The defence-goods trading capability, for an Entity `defenceAccess` lists.
allied :: String -> Array Capability
allied e = if elem e defenceAccess then [ mayTrade n.defenceGoods ] else []

-- | A government's military powers. `procurement` is the Actor's own procurement facility.
military :: String -> Array Capability
military procurement =
  [ mayTransform n.militaryDeployment, mayTransform n.majorMilitaryAction, mayTransform n.publicAttention, mayTransform n.humanitarianAid, mayTransfer n.mil, mayTransform procurement ]

-- | Enacting tariffs and access bans.
legislate :: Array Capability
legislate = [ tariff.may, accessBan.may ]

-- | Running the shipping facility.
shipping :: Array Capability
shipping = [ mayTransform n.energyShipping ]

-- | A government: trading in one Energy market, its military powers with its procurement
-- | facility, its legislative powers, and the shipping facility.
government :: String -> String -> Array Capability
government market procurement = dealer market <> military procurement <> legislate <> shipping
