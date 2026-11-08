-- | The setting spelling of each constructor beside its snapshot one (docs/07 §5). Setting files are
-- | authored by hand, so both spellings stay; each pair is written once, here, and `settingEnum` builds
-- | the setting form of an enum from its snapshot shape. The tests check the pairs against the
-- | snapshot shapes' constructors.
module Schema.Setting.Tags
  ( TagPair
  , transferabilityTags
  , capabilityTags
  , settingEnum
  ) where

import Prelude

import Core.Shape (Format(..), Shape, bare, settingSum)
import Data.Argonaut.Core (fromArray, fromObject, fromString)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)

-- | One constructor's tag in each format, and its note.
type TagPair = { snapshot :: String, setting :: String, desc :: String }

tradeKindTag :: String
tradeKindTag = "trade"

transferKindTag :: String
transferKindTag = "transfer"

transformKindTag :: String
transformKindTag = "transform"

enactKindTag :: String
enactKindTag = "enact"

transferabilityTags :: Array TagPair
transferabilityTags =
  [ { snapshot: "Free", setting: "Free", desc: "tradable and transferable" }
  , { snapshot: "Restricted", setting: "Restricted", desc: "transferred only by an Entity with the matching transfer capability" }
  , { snapshot: "NonTransferable", setting: "NonTransferable", desc: "never moves between Entities" }
  ]

capabilityTags :: { trade :: TagPair, transfer :: TagPair, transform :: TagPair, enact :: TagPair }
capabilityTags =
  { trade: { snapshot: "MayTrade", setting: tradeKindTag, desc: "may trade in the Market" }
  , transfer: { snapshot: "MayTransfer", setting: transferKindTag, desc: "may transfer the Resource" }
  , transform: { snapshot: "MayTransform", setting: transformKindTag, desc: "may run the transformation" }
  , enact: { snapshot: "MayEnact", setting: enactKindTag, desc: "may enact a Rule of the kind" }
  }

-- | The setting form of an enum: each value is the one its snapshot tag decodes to, written as a bare
-- | string in its setting spelling. A pair naming no constructor of the snapshot shape is a programming
-- | error, so building it crashes.
settingEnum :: forall a. Eq a => String -> Shape a -> Array TagPair -> Shape a
settingEnum name snapshot pairs = settingSum name (map ctor pairs)
  where
  ctor p = bare { tag: p.setting, desc: p.desc } (valueOf p)
  valueOf p = case (snapshot.codec Snapshot).dec (fromObject (Object.fromFoldable [ Tuple "tag" (fromString p.snapshot), Tuple "values" (fromArray []) ])) of
    Right x -> x
    Left why -> unsafeCrashWith (name <> ": " <> why)
