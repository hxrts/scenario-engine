-- | Decoders for the two tagged unions of the schema: a Rule (`accessBan`, `tariff`,
-- | `withholding`) and an Intent (`trade`, `transfer`, `transform`, `enact`).
module Board.Decode.Intents
  ( rule
  , intent
  ) where

import Prelude

import Board.Decode.Common (oneKey, unknownKind)
import Board.Tags (accessBanKind, enactKind, tariffKind, tradeKind, transferKind, transformKind, withholdingKind)
import Core.Types (Id(..), Intent, IntentF(..), Rule, RuleF(..), RuleChangeF(..))
import Data.Argonaut.Core (Json, isNull)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- | A Rule: `{ "accessBan": ... }`, `{ "tariff": ... }`, or `{ "withholding": ... }`.
rule :: Json -> Either JsonDecodeError Rule
rule j = do
  Tuple kind body <- oneKey "rule" j
  x <- decodeJson body
  if kind == accessBanKind then do
    jurisdiction <- x .: "jurisdiction"
    target <- x .: "target"
    pure (AccessBan { jurisdiction: Id jurisdiction, target: Id target })
  else if kind == tariffKind then do
    jurisdiction <- x .: "jurisdiction"
    origin <- x .: "origin"
    rate <- x .: "rate"
    pure (Tariff { jurisdiction: Id jurisdiction, origin: Id origin, rate })
  else if kind == withholdingKind then do
    capacity <- x .: "capacity"
    units <- x .: "units"
    pure (Withholding { capacity: Id capacity, units })
  else Left (unknownKind "rule" kind)

-- | An Intent: exactly one of `trade`, `transfer`, `transform`, or `enact` (a Rule). A trade's
-- | `market` is a Market id, or `null` for a routed order (docs/02 §5.2), which names its pair
-- | by `base` and `quote` instead.
intent :: Json -> Either JsonDecodeError Intent
intent j = do
  Tuple kind body <- oneKey "intent" j
  if kind == tradeKind then do
    x <- decodeJson body
    m <- x .: "market"
    quantity <- x .: "quantity"
    limitPrice <- x .:? "limit"
    if isNull m then do
      base <- x .: "base"
      quote <- x .: "quote"
      pure (Route { pair: { base: Id base, quote: Id quote }, quantity, limitPrice })
    else do
      market <- decodeJson m
      pure (Trade { market: Id market, quantity, limitPrice })
  else if kind == transferKind then do
    x <- decodeJson body
    from <- x .: "from"
    to <- x .: "to"
    r <- x .: "resource"
    quantity <- x .: "quantity"
    pure (Transfer { from: Id from, to: Id to, resource: Id r, quantity })
  else if kind == transformKind then do
    x <- decodeJson body
    by <- x .: "by"
    tr <- x .: "transformation"
    quantity <- x .: "quantity"
    pure (Transform { by: Id by, transformation: Id tr, quantity })
  else if kind == enactKind then ChangeRule <<< Enact <$> rule body
  else Left (unknownKind "intent" kind)
