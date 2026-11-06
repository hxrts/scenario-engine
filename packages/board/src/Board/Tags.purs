-- | The string grammar of Gameboard tags (docs/03_gameboard.md): the capability, fallback,
-- | measure, span, and share-source tags. A tag is `kind` or `kind:argument`. The DSL
-- | (packages/scenarios) builds these strings from the constants here, and the decoders
-- | in `Board.Decode` and `Board.Gameboard` match on the same constants, so the two sides
-- | cannot drift apart.
module Board.Tags
  ( separator
  , tag
  , Split
  , splitTag
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
