-- | How a CLI program ends on an error: the message, then exit status 1.
module Exit (fail, usageError) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (exit')

-- | Print the message and exit 1.
fail :: forall a. String -> Effect a
fail e = log e *> exit' 1

-- | A usage error: why, then the usage lines, and exit 1.
usageError :: forall a. Array String -> String -> Effect a
usageError lines why = do
  log ("error: " <> why)
  for_ lines log
  exit' 1
