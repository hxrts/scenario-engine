-- | The decision language's JSON vocabulary (docs/02_type_system.md §4.4): the variant tags of
-- | expressions, conditions, templates and sign rules, and the two sentinel names. The DSL
-- | builders (packages/scenarios, DSL.Decision) emit these constants and `Policy.Program` decodes
-- | them, so the two sides cannot drift apart.
module Policy.Tags
  ( -- expressions
    paramTag
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
