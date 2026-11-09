-- | How the DSL's builders take a name: an `Id` of the right kind, as a setting's typed `Names` give, or a
-- | String, as a test's inline names and a generic Kit do. An `Id` of the wrong kind has no instance, so naming a
-- | Market where an Entity goes fails to compile.
module Schema.Build.Named
  ( class Named
  , named
  ) where

import Prelude

import Core.Types (Id(..))

class Named a t where
  named :: a -> Id t

instance Named String t where
  named = Id

instance Named (Id t) t where
  named = identity
