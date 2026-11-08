-- | The premise format of an Intent, a Rule, and a Capability (docs/03 §5; the fixture and the
-- | Gameboard's `capabilities`). Each is a `Shape` value: the premise tags live here, privately,
-- | and nowhere else spells them.
module Schema.Premise.Intent
  ( intentShape
  , ruleShape
  , capabilityShape
  ) where

import Prelude

import Core.Num (Q)
import Core.Shape (Format(..), PCtor, Shape, TagForm(..), Desc(..), bare, encodeWith, fieldOf, kindArg, object, oneKey, premiseSum, qShape)
import Core.Types (Capability(..), Id(..), Intent, IntentF(..), RuleChangeF(..), RuleF(..), RuleKind(..), idShape)
import Data.Argonaut.Core (fromObject, fromString, isNull, jsonNull, toObject)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | The premise tags. Private: the sums below are the only place they are written or read.
tradeTag :: String
tradeTag = "trade"

transferTag :: String
transferTag = "transfer"

transformTag :: String
transformTag = "transform"

enactTag :: String
enactTag = "enact"

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

-- | The Rule, under `enact`: `{ "tariff": ... }`, `{ "accessBan": ... }`, `{ "withholding": ... }`.
ruleShape :: Shape (RuleF Q)
ruleShape = premiseSum "Rule"
  [ oneKey { tag: tariffTag, desc: "a tariff on the origin's goods, set by the jurisdiction" } tariffBody
      ( case _ of
          Tariff r -> Just r
          _ -> Nothing
      )
      Tariff
  , oneKey { tag: accessBanTag, desc: "a ban on shipments from the jurisdiction to the target" } accessBanBody
      ( case _ of
          AccessBan r -> Just r
          _ -> Nothing
      )
      AccessBan
  , oneKey { tag: withholdingTag, desc: "units of a capacity withheld" } withholdingBody
      ( case _ of
          Withholding r -> Just r
          _ -> Nothing
      )
      Withholding
  ]
  where
  tariffBody = object "Tariff"
    ( mk3 <$> fieldOf "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction
        <*> fieldOf "origin" "the Entity whose goods it taxes" idShape _.origin
        <*> fieldOf "rate" "the rate" qShape _.rate
    )
    where
    mk3 j o r = { jurisdiction: j, origin: o, rate: r }
  accessBanBody = object "AccessBan"
    ( mk2 <$> fieldOf "jurisdiction" "the Entity that enacts it" idShape _.jurisdiction
        <*> fieldOf "target" "the Entity the ban is against" idShape _.target
    )
    where
    mk2 j t = { jurisdiction: j, target: t }
  withholdingBody = object "Withholding"
    ( mk2 <$> fieldOf "capacity" "the capacity withheld" idShape _.capacity
        <*> fieldOf "units" "units withheld" qShape _.units
    )
    where
    mk2 c u = { capacity: c, units: u }

-- | An Intent. A `trade` with a null market is a routed order (docs/02 §5.2): it names its pair by
-- | `base` and `quote`. A Repeal has no premise tag (Phase 2), so it does not encode.
intentShape :: Shape Intent
intentShape = premiseSum "Intent"
  [ tradeCtor
  , oneKey { tag: transferTag, desc: "a Resource given from one Entity to another" } transferBody
      ( case _ of
          Transfer r -> Just r
          _ -> Nothing
      )
      Transfer
  , oneKey { tag: transformTag, desc: "a run of a transformation" } transformBody
      ( case _ of
          Transform r -> Just r
          _ -> Nothing
      )
      Transform
  , oneKey { tag: enactTag, desc: "enact a Rule" } ruleShape
      ( case _ of
          ChangeRule (Enact r) -> Just r
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact r))
  ]
  where
  transferBody = object "Transfer"
    ( mk4 <$> fieldOf "from" "the giving Entity" idShape _.from
        <*> fieldOf "to" "the receiving Entity" idShape _.to
        <*> fieldOf "resource" "the Resource given" idShape _.resource
        <*> fieldOf "quantity" "the quantity" qShape _.quantity
    )
    where
    mk4 f t r q' = { from: f, to: t, resource: r, quantity: q' }
  transformBody = object "Transform"
    ( mk3 <$> fieldOf "by" "the Entity that runs it" idShape _.by
        <*> fieldOf "transformation" "the transformation" idShape _.transformation
        <*> fieldOf "quantity" "the quantity run" qShape _.quantity
    )
    where
    mk3 b t q' = { by: b, transformation: t, quantity: q' }

-- | The Intent body under `trade`, written as its market (or null) and its amounts.
tradeCtor :: PCtor Intent
tradeCtor =
  { tag: tradeTag
  , desc: "an order in a Market, or a routed order on a pair of Resources"
  , payload: DRec "TradeBody" []
  , form: OneKey
  , project: \x -> case x of
      Trade r -> Just (wrapBody (writeTrade r))
      Route r -> Just (wrapBody (writeRoute r))
      _ -> Nothing
  , inject: \j -> case toObject j of
      Just o -> case Object.toUnfoldable o of
        [ Tuple k body ] | k == tradeTag -> Just (readTrade body)
        _ -> Nothing
      Nothing -> Nothing
  }
  where
  wrapBody b = fromObject (Object.singleton tradeTag b)
  writeTrade r = fromObject $ Object.fromFoldable $ [ Tuple "market" (fromString (unId r.market)), Tuple "quantity" (encodeWith Premise qShape r.quantity) ] <> optionalLimit r.limitPrice
  writeRoute r = fromObject $ Object.fromFoldable $ [ Tuple "market" jsonNull, Tuple "base" (fromString (unId r.pair.base)), Tuple "quote" (fromString (unId r.pair.quote)), Tuple "quantity" (encodeWith Premise qShape r.quantity) ] <> optionalLimit r.limitPrice
  optionalLimit = case _ of
    Nothing -> []
    Just l -> [ Tuple "limit" (encodeWith Premise qShape l) ]
  readTrade body = do
    o <- note "expected an object" (toObject body)
    case Object.lookup "market" o of
      Nothing -> Left "missing key market"
      Just m | isNull m -> readRoute o
      Just m -> readTradeMarket m o
  readTradeMarket m o = do
    market <- (idShape.codec Premise).dec m
    quantity <- lookupQ "quantity" o
    limitPrice <- lookupOptQ "limit" o
    pure (Trade { market, quantity, limitPrice })
  readRoute o = do
    base <- lookupId "base" o
    quote <- lookupId "quote" o
    quantity <- lookupQ "quantity" o
    limitPrice <- lookupOptQ "limit" o
    pure (Route { pair: { base, quote }, quantity, limitPrice })
  lookupQ key o = case Object.lookup key o of
    Just j -> (qShape.codec Premise).dec j
    Nothing -> Left ("missing key " <> key)
  lookupOptQ key o = case Object.lookup key o of
    Just j -> if isNull j then Right Nothing else map Just ((qShape.codec Premise).dec j)
    Nothing -> Right Nothing
  lookupId key o = case Object.lookup key o of
    Just j -> (idShape.codec Premise).dec j
    Nothing -> Left ("missing key " <> key)

unId :: forall t. Id t -> String
unId (Id s) = s

-- | A Capability: `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`, or
-- | `enact:<tariff|accessBan|withholding>`.
capabilityShape :: Shape Capability
capabilityShape = premiseSum "Capability"
  [ kindArg { tag: tradeKindTag, desc: "may trade in the Market" } idShape
      ( case _ of
          MayTrade m -> Just m
          _ -> Nothing
      )
      MayTrade
  , kindArg { tag: transferKindTag, desc: "may transfer the Resource" } idShape
      ( case _ of
          MayTransfer r -> Just r
          _ -> Nothing
      )
      MayTransfer
  , kindArg { tag: transformKindTag, desc: "may run the transformation" } idShape
      ( case _ of
          MayTransform t -> Just t
          _ -> Nothing
      )
      MayTransform
  , kindArg { tag: enactKindTag, desc: "may enact a Rule of the kind" } ruleKindShape
      ( case _ of
          MayEnact k -> Just k
          _ -> Nothing
      )
      MayEnact
  ]

ruleKindShape :: Shape RuleKind
ruleKindShape = premiseSum "RuleKind"
  [ bare { tag: tariffTag, desc: "" } TariffRule (_ == TariffRule)
  , bare { tag: accessBanTag, desc: "" } AccessBanRule (_ == AccessBanRule)
  , bare { tag: withholdingTag, desc: "" } WithholdingRule (_ == WithholdingRule)
  ]
