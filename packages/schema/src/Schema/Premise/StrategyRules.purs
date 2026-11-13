-- | The premise format of a StrategyRules (docs/02_kernel.md §4.4): its expressions, conditions,
-- | candidate Transaction templates, sign rules and clauses, and the presets file that holds the seat Strategies.
-- | The premise tags are private to this module. `self` is implicit where the premise does not
-- | write it: the reserveParty or Actor that runs an Intent, and a transform's `by`.
module Schema.Premise.StrategyRules
  ( strategyRulesShape
  , presetsShape
  , strategiesShape
  , decodeStrategyRules
  , encodeStrategyRules
  ) where

import Partial.Unsafe (unsafeCrashWith)
import Core.Sum (ctor)
import Prelude

import Laws.Query (transfersOf, transformsOf)
import Core.Shape (defaultsTo, genericPremiseSum, atOneKey, atEmbedded, iso, partialIso, Format(..), Shape, arrayOf, atLiteral, decodeJsonWith, embedded, encodeWith, field, fieldOf, keyedOf, object, omittedWhen, oneKey, optionalOf, pairOf, recordOf, recordShape, recursive, requiredMaybe, premiseSum, stringShape, unitShape, (&.))
import Core.Num (qShape)
import Laws.StrategyRules (Clause, Cond, Expr, IntentTemplate, ExchangeRef, RecordQuery, SignRule, StrategyRules, Template(..), selfId)
import Schema.Premise.Intent (ruleBody)
import Schema.Premise.Query (legQueryShape)
import Laws.Types (enterBody, CollectiveProcedureId, ActorId, IntentF(..), ResourceId, TransformationId, idShape, jointIntent, ownTransformBody, repealBody, transferBody)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple)

-- The premise tags used more than once. Private to this module.
noActionT :: String
noActionT = "noAction"

-- | An Actor that defaults to the acting one, and is not written when it is.
selfIdShape :: Shape ActorId
selfIdShape = idShape { absent = (_ == selfId), missing = Just selfId }

-- | An Exchange reference: a bare Exchange id, or a selector over the accessible Exchanges for a Resource.
exchangeRefShape :: Shape ExchangeRef
exchangeRefShape = genericPremiseSum "ExchangeRef"
  { "Named": atLiteral idShape
  , "Each": atOneKey "each" "every accessible Exchange for the Resource" idShape
  , "Cheapest": atOneKey "cheapest" "the accessible Exchange with the lowest price for the Resource" idShape
  , "Costliest": atOneKey "costliest" "the accessible Exchange with the highest price for the Resource" idShape
  }

-- | An expression: a quantity literal (a bare string), or a oneKey form.
exprShape :: Shape Expr
exprShape = recursive "Expr" \e -> genericPremiseSum "Expr"
  { "Lit": atLiteral qShape
  , "Param": atOneKey "param" "a premise parameter" stringShape
  , "Round": atOneKey "round" "the current round" unitShape
  , "Price": atOneKey "price" "the beginning-of-round price of an Exchange" exchangeRefShape
  , "Holding": atOneKey "holding" "the Actor's own holding of a Resource" idShape
  , "Factor": atOneKey "factor" "the stored factor of a behaviour domain" idShape
  , "Due": atOneKey "due" "the fixed amount of a Resource the Actor's recurring flows take from it each round" idShape
  , "Scheduled": atOneKey "scheduled" "what scheduled Flows still pending will deliver of a Resource to the Actor" idShape
  , "Planned": atOneKey "planned" "what the Transforms drafted earlier this round consume of a Resource" idShape
  , "Mul": atOneKey "mul" "a product" (pairOf e e)
  , "Sub": atOneKey "sub" "a difference" (pairOf e e)
  , "Max": atOneKey "max" "the larger of two" (pairOf e e)
  , "Div": atOneKey "div" "a quotient, zero when the divisor is zero" (pairOf e e)
  }

-- | A condition.
condShape :: Shape Cond
condShape = recursive "Cond" \c -> genericPremiseSum "Cond"
  { "Gt": atOneKey "gt" "greater than" (pairOf exprShape exprShape)
  , "Ge": atOneKey "ge" "at least" (pairOf exprShape exprShape)
  , "Le": atOneKey "le" "at most" (pairOf exprShape exprShape)
  , "Eq": atOneKey "eq" "equal to" (pairOf exprShape exprShape)
  , "Not": atOneKey "not" "not" c
  , "Recorded": atOneKey "recorded" "what a query of the record reads is at least the threshold" recordedBody
  , "Acting": atOneKey "acting" "the acting Actor is this one" idShape
  }
  where
  recordedBody = recordOf "Recorded"
    ( field @"query" "what to read from the record" recordQueryShape
        &. field @"atLeast" "the threshold" exprShape
    )

-- | A question of the record.
recordQueryShape :: Shape RecordQuery
recordQueryShape = genericPremiseSum "RecordQuery"
  { "OfLegs": atOneKey "legs" "a query of the settlement legs" legQueryShape
  , "RulesEnacted": atOneKey "rulesEnacted" "the Rules a governor enacted against a counterparty at this round's boundary" (recordShape "RulesEnacted" { governor: idShape, counterparty: idShape })
  , "RulesInForce": atOneKey "rulesInForce" "the Rules in force a governor governs, of the kinds named or of any" (recordShape "RulesInForce" { governor: idShape, kinds: defaultsTo [] (arrayOf idShape) })
  }

type TransformRec = { transformation :: TransformationId, quantity :: Expr }

-- | A Transfer as a plain object: how a joint candidate Transaction writes its transfers.
transferObject :: Shape { from :: ActorId, to :: ActorId, resource :: ResourceId, quantity :: Expr }
transferObject = transferBody exprShape

-- | A transform as a plain object: how a joint candidate Transaction writes its transforms, run by the acting Actor.
transformObject :: Shape TransformRec
transformObject = ownTransformBody exprShape

-- | An Intent a StrategyRules writes as a template. `self` is implicit: the reserveParty of an enactment,
-- | and the `by` of a transform.
intentTemplateShape :: Shape IntentTemplate
intentTemplateShape = genericPremiseSum "Intent template"
  { "Trade": atOneKey "trade" "an order in an Exchange" tradeBody
  , "Route": atOneKey "route" "a routed order on a pair" routeBody
  , "Transfer": atOneKey "transfer" "a Resource given by the Actor" transferObject
  , "Transform": atOneKey "transform" "a run of a transformation by the acting Actor" selfTransformShape
  , "ChangeRule": atEmbedded ruleChangeShape
  , "Enter": atOneKey "enter" "enter an agreement; every party named must sign" (enterBody exprShape)
  }
  where
  ruleChangeShape = genericPremiseSum "Rule change"
    { "Enact": atOneKey "enact" "enact a Rule, with the acting Actor as its governor by default" (ruleBody selfIdShape exprShape)
    , "Repeal": atOneKey "repeal" "repeal a Rule the author governs" (iso _.rule (\r -> { rule: r }) repealBody)
    }
  selfTransformShape = partialIso transformObject
    (\r -> Right { by: selfId, transformation: r.transformation, quantity: r.quantity })
    ( \r ->
        if r.by == selfId then { transformation: r.transformation, quantity: r.quantity }
        else unsafeCrashWith ("a template's Transform must be run by self, not " <> show r.by)
    )
  tradeBody = object "Trade"
    ( mk3 <$> fieldOf "exchange" "the Exchange, by id or by selector" exchangeRefShape _.exchange
        <*> fieldOf "quantity" "the quantity" exprShape _.quantity
        <*> fieldOf "limit" "the limit price, if any" (optionalOf exprShape) _.limitPrice
    )
    where
    mk3 m q l = { exchange: m, quantity: q, limitPrice: l }
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
  procedureIntent = premiseSum "Intent template"
    [ oneKey noActionT "contributes nothing" unitShape
        ( case _ of
            Nothing -> Just unit
            Just _ -> Nothing
        )
        (const Nothing)
    , embedded intentTemplateShape identity Just
    ]

-- | Whether a joint candidate Transaction's Intents are its transfers, then its transforms, all run by the acting Actor,
-- | then the agreements it enters.
jointShaped :: Array IntentTemplate -> Boolean
jointShaped xs = Array.all jointIntent xs && Array.all isSelfTransform fs && xs == ts <> fs <> es
  where
  ts = Array.filter isTransfer xs
  fs = Array.filter isTransform xs
  es = Array.filter (isJust <<< ctor @"Enter") xs
  isTransfer = isJust <<< ctor @"Transfer"
  isTransform = isJust <<< ctor @"Transform"
  isSelfTransform = maybe false (\r -> r.by == selfId) <<< ctor @"Transform"

-- | The candidate Transaction templates and the choices among them.
templateShape :: Shape Template
templateShape = recursive "Template" \t -> premiseSum "Template"
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
  , oneKey "joint" "a CandidateTransaction with no procedure: its transfers, then its transforms, then the agreements it enters" jointBody
      jointOf
      (\r -> CandidateTransactionT { signers: r.signers, intents: r.intents, procedure: Nothing })
  , oneKey "procedure" "a Procedure's CandidateTransaction, under its collective procedure" procedureBody
      procedureOf
      (\r -> CandidateTransactionT { signers: r.signers, intents: r.intents, procedure: Just r.id })
  , embedded intentTemplateShape
      ( \x -> case x of
          IntentT i -> Just i
          _ -> Nothing
      )
      IntentT
  ]
  where
  mirrorBody = recordShape "Mirror" { governor: idShape, counterparty: idShape }

  jointOf :: Template -> Maybe { signers :: Array ActorId, intents :: Array IntentTemplate }
  jointOf = case _ of
    CandidateTransactionT p | Nothing <- p.procedure, jointShaped p.intents -> Just { signers: p.signers, intents: p.intents }
    _ -> Nothing
  jointBody = object "Joint"
    ( mk4 <$> fieldOf "signers" "the Actors asked to sign" (arrayOf idShape) _.signers
        <*> fieldOf "transfers" "the Transfers" (omittedWhen (const false) (arrayOf transferObject)) (\r -> transfersOf r.intents)
        <*> fieldOf "transforms" "the Transforms, if any" (omittedWhen Array.null (arrayOf transformObject)) (\r -> map (\x -> { transformation: x.transformation, quantity: x.quantity }) (transformsOf r.intents))
        <*> fieldOf "enters" "the agreements it enters, if any" (omittedWhen Array.null (arrayOf (enterBody exprShape))) (\r -> Array.mapMaybe (ctor @"Enter") r.intents)
    )
    where
    mk4 s ts fs es = { signers: s, intents: map Transfer ts <> map (\r -> Transform { by: selfId, transformation: r.transformation, quantity: r.quantity }) fs <> map Enter es }

  procedureOf :: Template -> Maybe { id :: CollectiveProcedureId, signers :: Array ActorId, intents :: Array IntentTemplate }
  procedureOf = case _ of
    CandidateTransactionT p | Just pid <- p.procedure -> Just { id: pid, signers: p.signers, intents: p.intents }
    _ -> Nothing
  procedureBody = object "Procedure"
    ( mk3p <$> fieldOf "id" "the collective procedure" idShape _.id
        <*> fieldOf "signers" "the Actors asked to sign" (arrayOf idShape) _.signers
        <*> fieldOf "intents" "the Intents" procedureIntentsShape _.intents
    )
    where
    mk3p i s x = { id: i, signers: s, intents: x }

-- | A sign rule: which addressed candidate Transactions the Actor signs.
signRuleShape :: Shape SignRule
signRuleShape = recursive "SignRule" \s -> genericPremiseSum "SignRule"
  { "SignProcedures": atOneKey "procedures" "the collective procedures this Actor signs under" (arrayOf idShape)
  , "SignTransformation": atOneKey "transformation" "CandidateTransactions ordering this Transformation" idShape
  , "SignAgreements": atOneKey "agreements" "CandidateTransactions entering one of these templates" (arrayOf stringShape)
  , "SignAll": atOneKey "all" "every addressed CandidateTransaction" unitShape
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

-- | A StrategyRules: its clauses, and its sign rules.
strategyRulesShape :: Shape StrategyRules
strategyRulesShape = recordOf "StrategyRules"
  ( field @"rules" "the clauses" (omittedWhen Array.null (arrayOf clauseShape))
      &. field @"sign" "the sign rules" (omittedWhen Array.null (arrayOf signRuleShape))
  )

-- | The strategies file: the composition's Strategy catalog, each Strategy's name and its StrategyRules.
strategiesShape :: Shape (Array (Tuple String StrategyRules))
strategiesShape = object "Strategies" (identity <$> fieldOf "strategies" "the Strategy catalog" (keyedOf strategyRulesShape) identity)

-- | The presets file: each seat preset's name and its StrategyRules, in the order the file lists them.
presetsShape :: Shape (Array (Tuple String StrategyRules))
presetsShape = keyedOf strategyRulesShape

decodeStrategyRules :: Json -> Either JsonDecodeError StrategyRules
decodeStrategyRules = decodeJsonWith Authored strategyRulesShape

encodeStrategyRules :: StrategyRules -> Json
encodeStrategyRules = encodeWith Authored strategyRulesShape
