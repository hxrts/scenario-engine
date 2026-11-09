-- | Meridian's shared capability sets: the named lists each Actor's capabilities are
-- | built from. An exchange to mayTrade in, the military powers of a government, the legislative
-- | powers, and the shipping transformation. Each Actor's list is the concatenation of the sets it
-- | holds.
module Premises.Meridian.Actors.Capabilities
  ( dealer
  , allied
  , defenceAccess
  , military
  , shipping
  , government
  ) where

import Prelude ((<>))

import Data.Array (elem)

import Laws.Types (Capability, ActorId, ExchangeId, TransformationId)
import Schema.Build.Base (mayTrade, mayTransfer, mayTransform)
import Premises.Meridian.Names (n)
import Premises.Meridian.Rules (accessBan, tariff)

-- | Trading in one Energy exchange, and holding Money and Energy.
dealer :: ExchangeId -> Array Capability
dealer m = [ mayTrade m, mayTransfer n.money, mayTransfer n.energy ]

-- | Who may trade Military: the US and its allies, Germany, the UK, and Japan. The Exchange's access list
-- | and each Actor's capability both read this one list.
defenceAccess :: Array ActorId
defenceAccess = [ n.us, n.de, n.uk, n.jp ]

-- | The defence-goods trading capability, for an Actor `defenceAccess` lists.
allied :: ActorId -> Array Capability
allied e = if elem e defenceAccess then [ mayTrade n.defenceGoods ] else []

-- | A government's military powers. `procurement` is the Actor's own procurement transformation.
military :: TransformationId -> Array Capability
military procurement =
  [ mayTransform n.militaryDeployment, mayTransform n.majorMilitaryAction, mayTransform n.publicAttention, mayTransform n.humanitarianAid, mayTransfer n.mil, mayTransform procurement ]

-- | Enacting tariffs and access bans.
legislate :: Array Capability
legislate = [ tariff.may, accessBan.may ]

-- | Running the shipping transformation.
shipping :: Array Capability
shipping = [ mayTransform n.energyShipping ]

-- | A government: trading in one Energy exchange, its military powers with its procurement
-- | transformation, its legislative powers, and the shipping transformation.
government :: ExchangeId -> TransformationId -> Array Capability
government exchange procurement = dealer exchange <> military procurement <> legislate <> shipping
