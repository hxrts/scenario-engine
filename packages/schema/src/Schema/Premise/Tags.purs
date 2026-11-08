-- | The premise spelling of each constructor beside its snapshot one (docs/07 §5). Premise files are
-- | authored by hand, so both spellings stay; each pair is written once, here, and `premiseEnum` builds
-- | the premise form of an enum from its snapshot shape. The tests check the pairs against the
-- | snapshot shapes' constructors.
module Schema.Premise.Tags
  ( TagPair
  , transferabilityTags
  , ruleKindTags
  , capabilityTags
  , ruleTags
  , premiseEnum
  ) where

import Prelude

import Core.Shape (Format(..), Shape, bare, premiseSum)
import Data.Argonaut.Core (fromArray, fromObject, fromString)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)

-- | One constructor's tag in each format, and its note.
type TagPair = { snapshot :: String, premise :: String, desc :: String }

tariffTag :: String
tariffTag = "tariff"

accessBanTag :: String
accessBanTag = "accessBan"

withholdingTag :: String
withholdingTag = "withholding"

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
  [ { snapshot: "Free", premise: "Free", desc: "tradable and transferable" }
  , { snapshot: "Restricted", premise: "Restricted", desc: "held only by the Entities that may trade it" }
  , { snapshot: "NonTransferable", premise: "NonTransferable", desc: "never moves between Entities" }
  ]

ruleKindTags :: Array TagPair
ruleKindTags =
  [ { snapshot: "TariffRule", premise: tariffTag, desc: "" }
  , { snapshot: "AccessBanRule", premise: accessBanTag, desc: "" }
  , { snapshot: "WithholdingRule", premise: withholdingTag, desc: "" }
  ]

capabilityTags :: { trade :: TagPair, transfer :: TagPair, transform :: TagPair, enact :: TagPair }
capabilityTags =
  { trade: { snapshot: "MayTrade", premise: tradeKindTag, desc: "may trade in the Market" }
  , transfer: { snapshot: "MayTransfer", premise: transferKindTag, desc: "may transfer the Resource" }
  , transform: { snapshot: "MayTransform", premise: transformKindTag, desc: "may run the transformation" }
  , enact: { snapshot: "MayEnact", premise: enactKindTag, desc: "may enact a Rule of the kind" }
  }

ruleTags :: { tariff :: TagPair, accessBan :: TagPair, withholding :: TagPair }
ruleTags =
  { tariff: { snapshot: "Tariff", premise: tariffTag, desc: "a tariff on the origin's goods, set by the jurisdiction" }
  , accessBan: { snapshot: "AccessBan", premise: accessBanTag, desc: "a ban on shipments from the jurisdiction to the target" }
  , withholding: { snapshot: "Withholding", premise: withholdingTag, desc: "units of a capacity withheld" }
  }

-- | The premise form of an enum: each value is the one its snapshot tag decodes to, written as a bare
-- | string in its premise spelling. A pair naming no constructor of the snapshot shape is a programming
-- | error, so building it crashes.
premiseEnum :: forall a. Eq a => String -> Shape a -> Array TagPair -> Shape a
premiseEnum name snapshot pairs = premiseSum name (map ctor pairs)
  where
  ctor p = bare { tag: p.premise, desc: p.desc } (valueOf p)
  valueOf p = case (snapshot.codec Snapshot).dec (fromObject (Object.fromFoldable [ Tuple "tag" (fromString p.snapshot), Tuple "values" (fromArray []) ])) of
    Right x -> x
    Left why -> unsafeCrashWith (name <> ": " <> why)
