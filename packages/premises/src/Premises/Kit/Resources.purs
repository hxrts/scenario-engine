-- | The Resources a Premise commonly declares.
module Premises.Kit.Resources
  ( capacity
  ) where

import Laws.Types (ResourceDefinition, Transferability(..))
import Data.Maybe (Maybe(..))
import Schema.Build.World (noResource)
import Schema.PossibleWorld (capacityCategory)

-- | A capacity: a Resource held for a Transformation to draw on, never moved and never decaying. A Premise can
-- | make one tradeable by changing its transferability and giving it an Exchange.
capacity :: ResourceDefinition
capacity = noResource { transferability = NonTransferable, category = Just capacityCategory }
