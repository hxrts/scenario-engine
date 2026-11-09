-- | The setting format of a Strategy (docs/02_type_system.md §4.4): its expressions, conditions,
-- | Proposal templates, sign rules and clauses, and the presets file that holds the seat Strategies.
-- | The setting tags are private to this module. `self` is implicit where the setting does not
-- | write it: the jurisdiction or Entity that runs an Intent, and a transform's `by`.
module Schema.Setting.Strategy
  ( strategyShape
  , presetsShape
  , policiesShape
  , decodeStrategy
  , encodeStrategy
  ) where

import Partial.Unsafe (unsafeCrashWith)
import Core.Sum (ctor)
import Prelude

import Rules.Query (transfersOf, transformsOf)
import Core.Shape (genericSettingSum, atOneKey, atEmbedded, iso, partialIso, Format(..), Shape, arrayOf, atLiteral, decodeJsonWith, embedded, encodeWith, field, fieldOf, keyedOf, object, omittedWhen, oneKey, optionalOf, pairOf, recordOf, recordShape, recursive, requiredMaybe, settingSum, stringShape, unitShape, (&.))
import Core.Num (qShape)
import Rules.Strategy (Clause, Cond, Expr, IntentTemplate, MarketRef, RecordQuery, SignRule, Strategy, Template(..), selfId)
import Schema.Setting.Intent (ruleBody)
import Schema.Setting.Query (legQueryShape)
import Rules.Types (CollectiveProcedureId, EntityId, IntentF(..), ResourceId, TransformationId, idShape, jointIntent, ownTransformBody, repealBody, transferBody)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple)

-- The setting tags used more than once. Private to this module.
noActionT :: String
noActionT = "noAction"

-- | An Entity that defaults to the acting one, and is not written when it is.
selfIdShape :: Shape EntityId
selfIdShape = idShape { absent = (_ == selfId), missing = Just selfId }

-- | A Market reference: a bare Market id, or a selector over the accessible Markets for a Resource.
marketRefShape :: Shape MarketRef
marketRefShape = genericSettingSum "MarketRef"
  { "Named": atLiteral idShape
  , "Each": atOneKey "each" "every accessible Market for the Resource" idShape
  , "Cheapest": atOneKey "cheapest" "the accessible Market with the lowest price for the Resource" idShape
  , "Costliest": atOneKey "costliest" "the accessible Market with the highest price for the Resource" idShape
  }

-- | An expression: a quantity literal (a bare string), or a oneKey form.
exprShape :: Shape Expr
exprShape = recursive "Expr" \e -> genericSettingSum "Expr"
  { "Lit": atLiteral qShape
  , "Param": atOneKey "param" "a setting parameter" stringShape
  , "Round": atOneKey "round" "the current round" unitShape
  , "Price": atOneKey "price" "the beginning-of-round price of a Market" marketRefShape
  , "Holding": atOneKey "holding" "the Entity's own holding of a Resource" idShape
  , "Factor": atOneKey "factor" "the stored factor of a behaviour domain" idShape
  , "Mul": atOneKey "mul" "a product" (pairOf e e)
  , "Sub": atOneKey "sub" "a difference" (pairOf e e)
  , "Max": atOneKey "max" "the larger of two" (pairOf e e)
  }

-- | A condition.
condShape :: Shape Cond
condShape = recursive "Cond" \c -> genericSettingSum "Cond"
  { "Gt": atOneKey "gt" "greater than" (pairOf exprShape exprShape)
  , "Ge": atOneKey "ge" "at least" (pairOf exprShape exprShape)
  , "Le": atOneKey "le" "at most" (pairOf exprShape exprShape)
  , "Eq": atOneKey "eq" "equal to" (pairOf exprShape exprShape)
  , "Not": atOneKey "not" "not" c
  , "Recorded": atOneKey "recorded" "what a query of the record reads is at least the threshold" recordedBody
  }
  where
  recordedBody = recordOf "Recorded"
    ( field @"query" "what to read from the record" recordQueryShape
        &. field @"atLeast" "the threshold" exprShape
    )

-- | A question of the record.
recordQueryShape :: Shape RecordQuery
recordQueryShape = genericSettingSum "RecordQuery"
  { "OfLegs": atOneKey "legs" "a query of the settlement legs" legQueryShape
  , "RulesEnacted": atOneKey "rulesEnacted" "the Rules a governor enacted against a counterparty at this round's boundary" (recordShape "RulesEnacted" { governor: idShape, counterparty: idShape })
  , "RulesInForce": atOneKey "rulesInForce" "the Rules in force a governor governs" (recordShape "RulesInForce" { governor: idShape })
  }

type TransformRec = { transformation :: TransformationId, quantity :: Expr }

-- | A Transfer as a plain object: how a joint Proposal writes its transfers.
transferObject :: Shape { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }
transferObject = transferBody exprShape

-- | A transform as a plain object: how a joint Proposal writes its transforms, run by the acting Entity.
transformObject :: Shape TransformRec
transformObject = ownTransformBody exprShape

-- | An Intent a Strategy writes as a template. `self` is implicit: the jurisdiction of an enactment,
-- | and the `by` of a transform.
intentTemplateShape :: Shape IntentTemplate
intentTemplateShape = genericSettingSum "Intent template"
  { "Trade": atOneKey "trade" "an order in a Market" tradeBody
  , "Route": atOneKey "route" "a routed order on a pair" routeBody
  , "Transfer": atOneKey "transfer" "a Resource given by the Entity" transferObject
  , "Transform": atOneKey "transform" "a run of a transformation by the acting Entity" selfTransformShape
  , "ChangeRule": atEmbedded ruleChangeShape
  }
  where
  ruleChangeShape = genericSettingSum "Rule change"
    { "Enact": atOneKey "enact" "enact a Rule, with the acting Entity as its governor by default" (ruleBody selfIdShape exprShape)
    , "Repeal": atOneKey "repeal" "repeal a Rule the author governs" (iso _.rule (\r -> { rule: r }) repealBody)
    }
  selfTransformShape = partialIso transformObject
    (\r -> Right { by: selfId, transformation: r.transformation, quantity: r.quantity })
    ( \r ->
        if r.by == selfId then { transformation: r.transformation, quantity: r.quantity }
        else unsafeCrashWith ("a template's Transform must be run by self, not " <> show r.by)
    )
  tradeBody = object "Trade"
    ( mk3 <$> fieldOf "market" "the Market, by id or by selector" marketRefShape _.market
        <*> fieldOf "quantity" "the quantity" exprShape _.quantity
        <*> fieldOf "limit" "the limit price, if any" (optionalOf exprShape) _.limitPrice
    )
    where
    mk3 m q l = { market: m, quantity: q, limitPrice: l }
  routeBody = object "Route"
    ( mk4 <$> fieldOf "base" "the base Resource" idShape _.pair.base
        <*> fieldOf "quote" "the quote Resource" idShape _.pair.quote
        <*> fieldOf "quantity" "the quantity" exprShape _.quantity
        <*> fieldOf "limit" "the limit price, which a routed order must state" (requiredMaybe exprShape) _.limitPrice
    )
    where
    mk4 b qu q l = { pair: { base: b, quote: qu }, quantity: q, limitPrice: l }

-- | A Procedure's Intents: each a template Intent. A `noAction` among them contributes nothing.
procedureIntentsShape :: Shape (Array IntentTemplate)
procedureIntentsShape = iso Array.catMaybes (map Just) (arrayOf procedureIntent)
  where
  procedureIntent = settingSum "Intent template"
    [ oneKey noActionT "contributes nothing" unitShape
        ( case _ of
            Nothing -> Just unit
            Just _ -> Nothing
        )
        (const Nothing)
    , embedded intentTemplateShape identity Just
    ]

-- | Whether a joint Proposal's Intents are its transfers, then its transforms, all run by the acting Entity.
jointShaped :: Array IntentTemplate -> Boolean
jointShaped xs = Array.all jointIntent xs && Array.all isSelfTransform fs && xs == ts <> fs
  where
  ts = Array.filter isTransfer xs
  fs = Array.filter isTransform xs
  isTransfer = isJust <<< ctor @"Transfer"
  isTransform = isJust <<< ctor @"Transform"
  isSelfTransform = maybe false (\r -> r.by == selfId) <<< ctor @"Transform"

-- | The Proposal templates and the choices among them.
templateShape :: Shape Template
templateShape = recursive "Template" \t -> settingSum "Template"
  [ oneKey "choose" "one option, picked by a seeded draw" (arrayOf t)
      ( \x -> case x of
          Choose ts -> Just ts
          _ -> Nothing
      )
      Choose
  , oneKey noActionT "propose nothing" unitShape
      ( \x -> case x of
          NoAction -> Just unit
          _ -> Nothing
      )
      (const NoAction)
  , oneKey "mirror" "enact, turned back on its governor, each Rule of a RulesEnacted query" mirrorBody
      ( \x -> case x of
          Mirror r -> Just r
          _ -> Nothing
      )
      Mirror
  , oneKey "joint" "a Proposal with no procedure: its transfers, then its transforms" jointBody
      jointOf
      (\r -> ProposalT { signers: r.signers, intents: r.intents, procedure: Nothing })
  , oneKey "procedure" "a Procedure's Proposal, under its collective procedure" procedureBody
      procedureOf
      (\r -> ProposalT { signers: r.signers, intents: r.intents, procedure: Just r.id })
  , embedded intentTemplateShape
      ( \x -> case x of
          IntentT i -> Just i
          _ -> Nothing
      )
      IntentT
  ]
  where
  mirrorBody = recordShape "Mirror" { governor: idShape, counterparty: idShape }

  jointOf :: Template -> Maybe { signers :: Array EntityId, intents :: Array IntentTemplate }
  jointOf = case _ of
    ProposalT p | Nothing <- p.procedure, jointShaped p.intents -> Just { signers: p.signers, intents: p.intents }
    _ -> Nothing
  jointBody = object "Joint"
    ( mk3 <$> fieldOf "signers" "the Entities asked to sign" (arrayOf idShape) _.signers
        <*> fieldOf "transfers" "the Transfers" (omittedWhen (const false) (arrayOf transferObject)) (\r -> transfersOf r.intents)
        <*> fieldOf "transforms" "the Transforms, if any" (omittedWhen Array.null (arrayOf transformObject)) (\r -> map (\x -> { transformation: x.transformation, quantity: x.quantity }) (transformsOf r.intents))
    )
    where
    mk3 s ts fs = { signers: s, intents: map Transfer ts <> map (\r -> Transform { by: selfId, transformation: r.transformation, quantity: r.quantity }) fs }

  procedureOf :: Template -> Maybe { id :: CollectiveProcedureId, signers :: Array EntityId, intents :: Array IntentTemplate }
  procedureOf = case _ of
    ProposalT p | Just pid <- p.procedure -> Just { id: pid, signers: p.signers, intents: p.intents }
    _ -> Nothing
  procedureBody = object "Procedure"
    ( mk3p <$> fieldOf "id" "the collective procedure" idShape _.id
        <*> fieldOf "signers" "the Entities asked to sign" (arrayOf idShape) _.signers
        <*> fieldOf "intents" "the Intents" procedureIntentsShape _.intents
    )
    where
    mk3p i s x = { id: i, signers: s, intents: x }

-- | A sign rule: which addressed Proposals the Entity signs.
signRuleShape :: Shape SignRule
signRuleShape = recursive "SignRule" \s -> genericSettingSum "SignRule"
  { "SignProcedures": atOneKey "procedures" "the collective procedures this Entity signs under" (arrayOf idShape)
  , "SignConversion": atOneKey "facility" "Proposals ordering this Transformation" idShape
  , "SignAll": atOneKey "all" "every addressed Proposal" unitShape
  , "SignIf": atOneKey "signIf" "the inner rule, while every condition holds" (signIfBody s)
  }
  where
  signIfBody s = recordOf "SignIf"
    ( field @"when" "the conditions" (omittedWhen (const false) (arrayOf condShape))
        &. field @"rule" "the inner sign rule" s
    )

-- | A clause: when every condition holds, propose its templates.
clauseShape :: Shape Clause
clauseShape = recordOf "Clause"
  ( field @"when" "the conditions" (omittedWhen (const false) (arrayOf condShape))
      &. field @"propose" "the templates proposed" (arrayOf templateShape)
  )

-- | A Strategy: its clauses, and its sign rules.
strategyShape :: Shape Strategy
strategyShape = recordOf "Strategy"
  ( field @"rules" "the clauses" (omittedWhen Array.null (arrayOf clauseShape))
      &. field @"sign" "the sign rules" (omittedWhen Array.null (arrayOf signRuleShape))
  )

-- | The policies file: the Gameboard's Policy catalog, each Policy's name and its Strategy.
policiesShape :: Shape (Array (Tuple String Strategy))
policiesShape = object "Policies" (identity <$> fieldOf "policies" "the Policy catalog" (keyedOf strategyShape) identity)

-- | The presets file: each seat preset's name and its Strategy, in the order the file lists them.
presetsShape :: Shape (Array (Tuple String Strategy))
presetsShape = keyedOf strategyShape

decodeStrategy :: Json -> Either JsonDecodeError Strategy
decodeStrategy = decodeJsonWith Authored strategyShape

encodeStrategy :: Strategy -> Json
encodeStrategy = encodeWith Authored strategyShape
