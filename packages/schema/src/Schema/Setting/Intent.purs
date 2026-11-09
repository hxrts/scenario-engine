-- | The setting format of an Intent, a Rule, and a Capability (docs/03 §5; the fixture and the
-- | Gameboard's `capabilities`). Each is a `Shape` value: the setting tags live here, privately,
-- | and nowhere else spells them.
module Schema.Setting.Intent
  ( intentShape
  , ruleShape
  , ruleBody
  , capabilityShape
  ) where

import Core.Sum (ctor)
import Prelude

import Core.Num (Q, qShape)
import Core.Shape (Shape, atBare, atKindArg, atOneKey, fieldOf, genericSettingSum, listOf, nameShape, object, omittedWhen, oneKey, optionalOf, partialIso, recordShape, refined, requiredOf, settingSum)
import Core.Types (transformBody, repealBody, Capability, Direction(..), EntityId, FlowEffect, Intent, LevyBasis, LevyRecipient(..), IntentF(..), MarketId, Pair, ResourceId, RuleChangeF(..), RuleF, idShape, transferBody)
import Data.Array (null)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..))

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

-- | What a levy is charged on: `perUnit`, or `{ adValorem: <market> }`.
basisShape :: Shape LevyBasis
basisShape = genericSettingSum "LevyBasis"
  { "PerUnit": atBare { tag: "perUnit", desc: "per unit moved" }
  , "AdValorem": atOneKey "adValorem" "on the value moved, at this Market's price" idShape
  }

recipientShape :: Shape LevyRecipient
recipientShape = nameShape "LevyRecipient" "who receives it: destination, governor or sink" recipient [ LevyToDestination, LevyToGovernor, LevyToSink ]
  where
  recipient = case _ of
    LevyToDestination -> "destination"
    LevyToGovernor -> "governor"
    LevyToSink -> "sink"

-- | What a flows Rule does, over its filling.
effectShapeOf :: forall v. Shape v -> Shape (FlowEffect v)
effectShapeOf v = genericSettingSum "FlowEffect"
  { "Levy": atOneKey "levy" "a levy on the flow: its rate, its basis, the Resource it is paid in, and who receives it" (recordShape "Levy" { rate: v, basis: basisShape, in: idShape, to: recipientShape })
  , "Block": atBare { tag: "block", desc: "the flow is refused" }
  }

-- | A Rule, under `enact`. `governor` is how the governor is written: templates default it to the acting Entity.
ruleBody :: forall v. Shape EntityId -> Shape v -> Shape (RuleF v)
ruleBody governor v = genericSettingSum "Rule"
  { "FlowRule": atOneKey "flows" "a Rule on Transfers between two groups"
      (recordShape "FlowRule" { kind: idShape, scope: recordShape "FlowScope" { governor, counterparty: idShape, direction: directionShape, over: refined "a flows Rule covers at least one Resource" (not <<< null) (listOf idShape) }, effect: effectShapeOf v })
  , "HoldingRule": atOneKey "holding" "a Rule reducing what the holder of a Resource can have drawn" (recordShape "HoldingRule" { kind: idShape, resource: idShape, reduce: v })
  }

ruleShape :: Shape (RuleF Q)
ruleShape = ruleBody idShape qShape

-- | An Intent. A `trade` with a null market is a routed order (docs/02 §5.2): it names its pair by
-- | `base` and `quote`.
intentShape :: Shape Intent
intentShape = settingSum "Intent"
  [ oneKey tradeTag "an order in a Market, or a routed order on a pair of Resources" tradeBody
      ( case _ of
          Trade r -> Just (Left r)
          Route r -> Just (Right r)
          _ -> Nothing
      )
      (either Trade Route)
  , oneKey transferTag "a Resource given from one Entity to another" (transferBody qShape) (ctor @"Transfer")
      Transfer
  , oneKey transformTag "a run of a transformation" (transformBody qShape) (ctor @"Transform")
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
  repealOf (ChangeRule (Repeal r)) = Just { rule: r }
  repealOf _ = Nothing

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
-- | `enact:<kind>`, a kind the setting declares.
capabilityShape :: Shape Capability
capabilityShape = genericSettingSum "Capability"
  { "MayTrade": atKindArg { tag: tradeTag, desc: "may trade in the Market" } idShape
  , "MayTransfer": atKindArg { tag: transferTag, desc: "may transfer the Resource" } idShape
  , "MayTransform": atKindArg { tag: transformTag, desc: "may run the transformation" } idShape
  , "MayEnact": atKindArg { tag: enactTag, desc: "may enact a Rule of the kind" } idShape
  }

