-- | The Resources a Setting commonly declares.
module Settings.Kit.Resources
  ( capacity
  ) where

import Rules.Types (ResourceDefinition, Transferability(..))
import Data.Maybe (Maybe(..))
import Schema.Build.World (noResource)
import Schema.Gameboard (capacityCategory)

-- | A capacity: a Resource held for a Transformation to draw on, never moved and never decaying. A Setting can
-- | make one tradeable by changing its transferability and giving it a Market.
capacity :: ResourceDefinition
capacity = noResource { transferability = NonTransferable, category = Just capacityCategory }
