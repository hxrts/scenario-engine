-- | The premise format of an Intent, a Rule, and a Capability (docs/03 §5; the fixture and the
-- | Gameboard's `capabilities`). Each is a `Shape` value: the premise tags live here, privately,
-- | and nowhere else spells them.
module Schema.Premise.Intent
  ( intentShape
  , ruleShape
  , ruleBody
  , capabilityShape
  ) where

import Prelude

import Core.Num (Q, qShape)
import Core.Shape (Shape, atBare, atKindArg, atOneKey, fieldOf, genericPremiseSum, nameShape, object, omittedWhen, oneKey, optionalOf, partialIso, premiseSum, recordShape, requiredOf)
import Core.Types (Capability, Direction(..), EntityId, FlowEffect, Intent, IntentF(..), MarketId, Pair, ResourceId, RuleChangeF(..), RuleF, idShape, transferBody)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..))
import Schema.Premise.Tags (capabilityTags)

-- | The Intent tags. Private: the sums below are the only place they are written or read.
tradeTag :: String
tradeTag = "trade"

transferTag :: String
transferTag = "transfer"

transformTag :: String
transformTag = "transform"

enactTag :: String
enactTag = "enact"

repealTag :: String
repealTag = "repeal"

-- | A Rule's direction, written by name.
directionShape :: Shape Direction
directionShape = nameShape "Direction" "direction: inbound, outbound or eitherWay" dirName [ Inbound, Outbound, EitherWay ]
  where
  dirName = case _ of
    Inbound -> "inbound"
    Outbound -> "outbound"
    EitherWay -> "eitherWay"

-- | What a flows Rule does, over its filling.
effectShapeOf :: forall v. Shape v -> Shape (FlowEffect v)
effectShapeOf v = genericPremiseSum "FlowEffect"
  { "Levy": atOneKey "levy" "a levy at this rate of the flow" v
  , "Block": atBare { tag: "block", desc: "the flow is refused" }
  }

-- | A Rule, under `enact`. `governor` is how the governor is written: templates default it to the acting Entity.
ruleBody :: forall v. Shape EntityId -> Shape v -> Shape (RuleF v)
ruleBody governor v = genericPremiseSum "Rule"
  { "FlowRule": atOneKey "flows" "a Rule on Transfers between two groups"
      (recordShape "FlowRule" { kind: idShape, scope: recordShape "FlowScope" { governor, counterparty: idShape, direction: directionShape }, effect: effectShapeOf v })
  , "CapacityRule": atOneKey "capacity" "a Rule reducing one capacity" (recordShape "CapacityRule" { kind: idShape, capacity: idShape, reduce: v })
  }

ruleShape :: Shape (RuleF Q)
ruleShape = ruleBody idShape qShape

-- | An Intent. A `trade` with a null market is a routed order (docs/02 §5.2): it names its pair by
-- | `base` and `quote`.
intentShape :: Shape Intent
intentShape = premiseSum "Intent"
  [ oneKey tradeTag "an order in a Market, or a routed order on a pair of Resources" tradeBody
      ( case _ of
          Trade r -> Just (Left r)
          Route r -> Just (Right r)
          _ -> Nothing
      )
      (either Trade Route)
  , oneKey transferTag "a Resource given from one Entity to another" (transferBody qShape)
      ( case _ of
          Transfer r -> Just r
          _ -> Nothing
      )
      Transfer
  , oneKey transformTag "a run of a transformation" transformBody
      ( case _ of
          Transform r -> Just r
          _ -> Nothing
      )
      Transform
  , oneKey enactTag "enact a Rule" ruleShape
      ( case _ of
          ChangeRule (Enact r) -> Just r
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact r))
  , oneKey repealTag "repeal a Rule by the id the engine gave it" repealBody repealOf (\r -> ChangeRule (Repeal r.rule))
  ]
  where
  repealBody = object "Repeal" ((\r -> { rule: r }) <$> fieldOf "rule" "the Rule, by the id the engine gave it" idShape _.rule)
  repealOf (ChangeRule (Repeal r)) = Just { rule: r }
  repealOf _ = Nothing
  transformBody = object "Transform"
    ( mk3 <$> fieldOf "by" "the Entity that runs it" idShape _.by
        <*> fieldOf "transformation" "the transformation" idShape _.transformation
        <*> fieldOf "quantity" "the quantity run" qShape _.quantity
    )
    where
    mk3 b t q' = { by: b, transformation: t, quantity: q' }

type TradeRec = { market :: MarketId, quantity :: Q, limitPrice :: Maybe Q }

type RouteRec = { pair :: Pair, quantity :: Q, limitPrice :: Maybe Q }

-- | The body under `trade`, as written: its market (null for a routed order), the pair of a routed
-- | order, and its amounts.
type TradeBody =
  { market :: Maybe MarketId
  , base :: Maybe ResourceId
  , quote :: Maybe ResourceId
  , quantity :: Q
  , limit :: Maybe Q
  }

-- | A Market order, or a routed order when the market is null.
tradeBody :: Shape (Either TradeRec RouteRec)
tradeBody = partialIso bodyShape read write
  where
  bodyShape = object "TradeBody"
    ( mk <$> fieldOf "market" "the Market; null for a routed order" (requiredOf (omittedWhen (const false) (optionalOf idShape))) _.market
        <*> fieldOf "base" "the base Resource of a routed order" (optionalOf idShape) _.base
        <*> fieldOf "quote" "the quote Resource of a routed order" (optionalOf idShape) _.quote
        <*> fieldOf "quantity" "the quantity" qShape _.quantity
        <*> fieldOf "limit" "the limit price, optional for a Market order and required for a routed order" (optionalOf qShape) _.limit
    )
    where
    mk m b qt qn l = { market: m, base: b, quote: qt, quantity: qn, limit: l }

  read :: TradeBody -> Either String (Either TradeRec RouteRec)
  read b = case b.market of
    Just market -> Right (Left { market, quantity: b.quantity, limitPrice: b.limit })
    Nothing -> do
      base <- note "missing key base" b.base
      quote <- note "missing key quote" b.quote
      pure (Right { pair: { base, quote }, quantity: b.quantity, limitPrice: b.limit })

  write :: Either TradeRec RouteRec -> TradeBody
  write = case _ of
    Left r -> { market: Just r.market, base: Nothing, quote: Nothing, quantity: r.quantity, limit: r.limitPrice }
    Right r -> { market: Nothing, base: Just r.pair.base, quote: Just r.pair.quote, quantity: r.quantity, limit: r.limitPrice }

-- | A Capability: `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`, or
-- | `enact:<kind>`, a kind the premise declares.
capabilityShape :: Shape Capability
capabilityShape = genericPremiseSum "Capability"
  { "MayTrade": atKindArg { tag: capabilityTags.trade.premise, desc: capabilityTags.trade.desc } idShape
  , "MayTransfer": atKindArg { tag: capabilityTags.transfer.premise, desc: capabilityTags.transfer.desc } idShape
  , "MayTransform": atKindArg { tag: capabilityTags.transform.premise, desc: capabilityTags.transform.desc } idShape
  , "MayEnact": atKindArg { tag: capabilityTags.enact.premise, desc: capabilityTags.enact.desc } idShape
  }

