-- | The tag grammar of the premise format (docs/03_gameboard.md, docs/02_type_system.md §4.4): the
-- | capability, fallback, measure, span, and share-source tags, and the decision language's variant
-- | tags and sentinels. A tag is `kind` or `kind:argument`. The builders
-- | (`Schema.Build.*`) write these strings from the constants here, and the decoders in
-- | `Schema.Decode.*` match on the same constants, so the two sides cannot drift apart.
module Schema.Tags
  ( separator
  , tag
  , Split
  , splitTag
  , argOf
  , isBare
  -- capability kinds
  , tradeKind
  , transferKind
  , transformKind
  , enactKind
  , tariffKind
  , accessBanKind
  , withholdingKind
  -- fallback kinds
  , coverageKind
  , policyKind
  , omissionKind
  -- measure kinds
  , holdingKind
  , capacityKind
  , shortfallKind
  , shortfallRoundsKind
  , payeeReceiptsKind
  , volumeKind
  -- span kinds
  , finalKind
  , everyRoundKind
  , cumulativeKind
  -- share source kinds
  , salesKind
  , payeeKind
  -- decision language: expressions
  , paramTag
  , roundTag
  , priceTag
  , holdingTag
  , factorTag
  , mulTag
  , subTag
  , maxTag
  -- conditions
  , gtTag
  , geTag
  , leTag
  , eqTag
  , notTag
  , transformedLastRoundTag
  , restrictedLastRoundTag
  , restrictionInForceTag
  -- templates
  , tradeTag
  , routeTag
  , transformTag
  , transferTag
  , procedureTag
  , enactTariffTag
  , reciprocateTag
  , dealTag
  , chooseTag
  , enactAccessBanTag
  , enactWithholdingTag
  , repealTag
  , noActionTag
  -- sign rules
  , proceduresTag
  , facilityTag
  , allTag
  , signIfTag
  -- sentinels
  , homeMarket
  , selfEntity
  ) where

import Prelude

import Data.Array (head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)

-- | Between a tag's kind and its argument.
separator :: String
separator = ":"

-- | `tag kind argument`, e.g. `tag tradeKind "energy-atlantic"` is `trade:energy-atlantic`.
tag :: String -> String -> String
tag kind arg = kind <> separator <> arg

-- | A tag split at its first separator. `arg` is absent for a bare kind such as `omission`.
type Split = { kind :: String, arg :: Maybe String }

splitTag :: String -> Split
splitTag s = case split (Pattern separator) s of
  [ k ] -> { kind: k, arg: Nothing }
  parts -> { kind: fromMaybe "" (head parts), arg: Just (joinWith separator (fromMaybe [] (tail parts))) }

-- | The argument of a tag of the given kind: Nothing for another kind, or for a bare kind.
argOf :: String -> String -> Maybe String
argOf kind s = case splitTag s of
  { kind: k, arg } | k == kind -> arg
  _ -> Nothing

-- | Whether a tag is the given kind with no argument.
isBare :: String -> String -> Boolean
isBare kind s = case splitTag s of
  { kind: k, arg: Nothing } -> k == kind
  _ -> false

-- Capabilities: `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`, `enact:<rule>`
tradeKind :: String
tradeKind = "trade"

transferKind :: String
transferKind = "transfer"

transformKind :: String
transformKind = "transform"

enactKind :: String
enactKind = "enact"

tariffKind :: String
tariffKind = "tariff"

accessBanKind :: String
accessBanKind = "accessBan"

withholdingKind :: String
withholdingKind = "withholding"

-- Fallbacks: `coverage:<aggregate>`, `policy:<policy>`, `omission`
coverageKind :: String
coverageKind = "coverage"

policyKind :: String
policyKind = "policy"

omissionKind :: String
omissionKind = "omission"

-- Measures: `holding:<resource>`, `capacity:<capacity>`, `shortfall:<resource>`,
-- `shortfallRounds:<resource>`, `payeeReceipts`, `volume:<transformation>`
holdingKind :: String
holdingKind = "holding"

capacityKind :: String
capacityKind = "capacity"

shortfallKind :: String
shortfallKind = "shortfall"

shortfallRoundsKind :: String
shortfallRoundsKind = "shortfallRounds"

payeeReceiptsKind :: String
payeeReceiptsKind = "payeeReceipts"

volumeKind :: String
volumeKind = "volume"

-- Spans (bare): `final`, `everyRound`, `cumulative`
finalKind :: String
finalKind = "final"

everyRoundKind :: String
everyRoundKind = "everyRound"

cumulativeKind :: String
cumulativeKind = "cumulative"

-- Share sources: `sales:<resource>`, `payee` (bare)
salesKind :: String
salesKind = "sales"

payeeKind :: String
payeeKind = "payee"

-- Decision language (docs/02_type_system.md §4.4) -------------------------------------------
paramTag :: String
paramTag = "param"

roundTag :: String
roundTag = "round"

priceTag :: String
priceTag = "price"

holdingTag :: String
holdingTag = "holding"

factorTag :: String
factorTag = "factor"

mulTag :: String
mulTag = "mul"

subTag :: String
subTag = "sub"

maxTag :: String
maxTag = "max"

gtTag :: String
gtTag = "gt"

geTag :: String
geTag = "ge"

leTag :: String
leTag = "le"

eqTag :: String
eqTag = "eq"

notTag :: String
notTag = "not"

transformedLastRoundTag :: String
transformedLastRoundTag = "transformedLastRound"

restrictedLastRoundTag :: String
restrictedLastRoundTag = "restrictedLastRound"

restrictionInForceTag :: String
restrictionInForceTag = "restrictionInForce"

tradeTag :: String
tradeTag = "trade"

-- | A routed order: no Market named, only the pair (docs/02 §5.2).
routeTag :: String
routeTag = "route"

transformTag :: String
transformTag = "transform"

transferTag :: String
transferTag = "transfer"

procedureTag :: String
procedureTag = "procedure"

enactTariffTag :: String
enactTariffTag = "enactTariff"

reciprocateTag :: String
reciprocateTag = "reciprocate"

dealTag :: String
dealTag = "deal"

chooseTag :: String
chooseTag = "choose"

enactAccessBanTag :: String
enactAccessBanTag = "enactAccessBan"

enactWithholdingTag :: String
enactWithholdingTag = "enactWithholding"

repealTag :: String
repealTag = "repeal"

noActionTag :: String
noActionTag = "noAction"

proceduresTag :: String
proceduresTag = "procedures"

facilityTag :: String
facilityTag = "facility"

allTag :: String
allTag = "all"

signIfTag :: String
signIfTag = "signIf"

-- | The Entity's home market, as a Market name.
homeMarket :: String
homeMarket = "$home"

-- | The acting Entity, as an Entity name, at every entity position.
selfEntity :: String
selfEntity = "self"
