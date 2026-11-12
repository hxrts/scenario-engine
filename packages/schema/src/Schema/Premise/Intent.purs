-- | The premise format of an Intent, a Rule, and a Capability (docs/03 §5; the fixture and the
-- | composition's `capabilities`). Each is a `Shape` value: the premise tags live here, privately,
-- | and nowhere else spells them.
module Schema.Premise.Intent
  ( intentShape
  , ruleShape
  , ruleBody
  , capabilityShape
  ) where

import Core.Sum (ctor)
import Prelude

import Core.Num (Q, qShape)
import Core.Shape (Shape, defaultsTo, field, recordOf, (&.), atBare, atKindArg, atOneKey, fieldOf, genericNameShape, genericPremiseSum, listOf, orList, camelName, object, omittedWhen, oneKey, optionalOf, partialIso, recordShape, refined, requiredOf, premiseSum)
import Laws.Types (enterBody, transformBody, repealBody, Capability, Direction, ActorId, FlowEffect, Intent, LevyBasis, LevyRecipient(..), LevyPayer(..), IntentF(..), ExchangeId, Pair, ResourceId, RuleChangeF(..), RuleF, idShape, transferBody)
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
directionShape = genericNameShape "Direction" (\ns -> "direction: " <> orList ns) camelName

-- | What a levy is charged on: `perUnit`, or `{ adValorem: <exchange> }`.
basisShape :: Shape LevyBasis
basisShape = genericPremiseSum "LevyBasis"
  { "PerUnit": atBare { tag: "perUnit", desc: "per unit moved" }
  , "AdValorem": atOneKey "adValorem" "on the value moved, at this Exchange's price" idShape
  }

recipientShape :: Shape LevyRecipient
recipientShape = genericNameShape "LevyRecipient" (\ns -> "who receives it: " <> orList ns) recipient
  where
  recipient = case _ of
    LevyToDestination -> "destination"
    LevyToGovernor -> "governor"
    LevyToSink -> "sink"

payerShape :: Shape LevyPayer
payerShape = genericNameShape "LevyPayer" (\ns -> "who pays it: " <> orList ns) payer
  where
  payer = case _ of
    PaidBySender -> "sender"
    PaidByReceiver -> "receiver"

-- | What a flows Rule does, over its filling.
effectShapeOf :: forall v. Shape v -> Shape (FlowEffect v)
effectShapeOf v = genericPremiseSum "FlowEffect"
  { "Levy": atOneKey "levy" "a levy on the flow: its rate, its basis, the Resource it is paid in, and who receives it" (recordOf "Levy" (field @"rate" "its rate" v &. field @"basis" "what it is charged on" basisShape &. field @"in" "the Resource it is paid in" idShape &. field @"to" "who receives it" recipientShape &. field @"payer" "who pays it (default the sender)" (defaultsTo PaidBySender payerShape)))
  , "Block": atBare { tag: "block", desc: "the flow is refused" }
  }

-- | A Rule, under `enact`. `governor` is how the governor is written: templates default it to the acting Actor.
ruleBody :: forall v. Shape ActorId -> Shape v -> Shape (RuleF v)
ruleBody governor v = genericPremiseSum "Rule"
  { "FlowRule": atOneKey "flows" "a Rule on Transfers between two groups"
      (recordShape "FlowRule" { kind: idShape, scope: recordShape "FlowScope" { governor, counterparty: idShape, direction: directionShape, over: refined "a flows Rule covers at least one Resource" (not <<< null) (listOf idShape) }, effect: effectShapeOf v })
  , "HoldingRule": atOneKey "holding" "a Rule reducing what the holder of a Resource can have drawn" (recordShape "HoldingRule" { kind: idShape, resource: idShape, reduce: v })
  }

ruleShape :: Shape (RuleF Q)
ruleShape = ruleBody idShape qShape

-- | An Intent. A `trade` with a null exchange is a routed order (docs/02 §5.2): it names its pair by
-- | `base` and `quote`.
intentShape :: Shape Intent
intentShape = premiseSum "Intent"
  [ oneKey tradeTag "an order in an Exchange, or a routed order on a pair of Resources" tradeBody
      ( case _ of
          Trade r -> Just (Left r)
          Route r -> Just (Right r)
          _ -> Nothing
      )
      (either Trade Route)
  , oneKey transferTag "a Resource given from one Actor to another" (transferBody qShape) (ctor @"Transfer")
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
  , oneKey "enter" "enter an agreement; every party named must sign" (enterBody qShape) (ctor @"Enter") Enter
  ]
  where
  repealOf (ChangeRule (Repeal r)) = Just { rule: r }
  repealOf _ = Nothing

type TradeRec = { exchange :: ExchangeId, quantity :: Q, limitPrice :: Maybe Q }

type RouteRec = { pair :: Pair, quantity :: Q, limitPrice :: Maybe Q }

-- | The body under `trade`, as written: its exchange (null for a routed order), the pair of a routed
-- | order, and its amounts.
type TradeBody =
  { exchange :: Maybe ExchangeId
  , base :: Maybe ResourceId
  , quote :: Maybe ResourceId
  , quantity :: Q
  , limit :: Maybe Q
  }

-- | An Exchange order, or a routed order when the exchange is null.
tradeBody :: Shape (Either TradeRec RouteRec)
tradeBody = partialIso bodyShape read write
  where
  bodyShape = object "TradeBody"
    ( mk <$> fieldOf "exchange" "the Exchange; null for a routed order" (requiredOf (omittedWhen (const false) (optionalOf idShape))) _.exchange
        <*> fieldOf "base" "the base Resource of a routed order" (optionalOf idShape) _.base
        <*> fieldOf "quote" "the quote Resource of a routed order" (optionalOf idShape) _.quote
        <*> fieldOf "quantity" "the quantity" qShape _.quantity
        <*> fieldOf "limit" "the limit price, optional for an Exchange order and required for a routed order" (optionalOf qShape) _.limit
    )
    where
    mk m b qt qn l = { exchange: m, base: b, quote: qt, quantity: qn, limit: l }

  read :: TradeBody -> Either String (Either TradeRec RouteRec)
  read b = case b.exchange of
    Just exchange -> Right (Left { exchange, quantity: b.quantity, limitPrice: b.limit })
    Nothing -> do
      base <- note "missing key base" b.base
      quote <- note "missing key quote" b.quote
      pure (Right { pair: { base, quote }, quantity: b.quantity, limitPrice: b.limit })

  write :: Either TradeRec RouteRec -> TradeBody
  write = case _ of
    Left r -> { exchange: Just r.exchange, base: Nothing, quote: Nothing, quantity: r.quantity, limit: r.limitPrice }
    Right r -> { exchange: Nothing, base: Just r.pair.base, quote: Just r.pair.quote, quantity: r.quantity, limit: r.limitPrice }

-- | A Capability: `trade:<exchange>`, `transfer:<resource>`, `transform:<transformation>`, or
-- | `enact:<kind>`, a kind the premise declares.
capabilityShape :: Shape Capability
capabilityShape = genericPremiseSum "Capability"
  { "MayTrade": atKindArg { tag: tradeTag, desc: "may trade in the Exchange" } idShape
  , "MayTransfer": atKindArg { tag: transferTag, desc: "may transfer the Resource" } idShape
  , "MayTransform": atKindArg { tag: transformTag, desc: "may run the transformation" } idShape
  , "MayEnact": atKindArg { tag: enactTag, desc: "may enact a Rule of the kind" } idShape
  }

