-- | The Resources a Premise commonly declares.
module Premises.Kit.Resources
  ( capacity
  , competence
  ) where

import Laws.Types (ResourceDefinition, Transferability(..))
import Data.Maybe (Maybe(..))
import Schema.Build.World (noResource)
import Schema.Composition (capacityCategory, competenceCategory)

-- | A capacity: a Resource held for a Transformation to draw on, never moved and never decaying. A Premise can
-- | make one tradeable by changing its transferability and giving it an Exchange.
capacity :: ResourceDefinition
capacity = noResource { transferability = NonTransferable, category = Just capacityCategory }

-- | A competence: what a workforce knows how to run. Drawn like a capacity and never moved, but in its own
-- | category, since a Premise has it learned by production and lost to decay.
competence :: ResourceDefinition
competence = noResource { transferability = NonTransferable, category = Just competenceCategory }
