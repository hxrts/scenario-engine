-- | The premise format of a Strategy (docs/02_type_system.md §4.4): its expressions, conditions,
-- | Proposal templates, sign rules and clauses, and the presets file that holds the seat Strategies.
-- | The premise tags are private to this module. `self` is implicit where the premise does not
-- | write it: the jurisdiction or Entity that runs an Intent, and a transform's `by`.
module Schema.Premise.Strategy
  ( strategyShape
  , presetsShape
  , policiesShape
  , decodeStrategy
  , encodeStrategy
  ) where

import Prelude

import Core.Query (transfersOf, transformsOf)
import Core.Shape (Format(..), Shape, arrayOf, atLiteral, atOneKey, decodeJsonWith, embedded, encodeWith, fieldOf, genericPremiseSum, iso, keyedOf, object, omittedWhen, oneKey, optionalOf, pairOf, premiseSum, recursive, requiredMaybe, stringShape, unitShape)
import Core.Num (qShape)
import Core.Strategy (Clause, Cond, Expr, IntentTemplate, MarketRef, SignRule, Strategy, Template(..), selfName)
import Core.Types (CollectiveProcedureId, EntityId, Id(..), IntentF(..), ResourceId, RuleChangeF(..), RuleF(..), TransformationId, dealIntent, idShape, ownAccessBanBody, ownTariffBody, transferBody, withholdingBody)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

-- The premise tags used more than once. Private to this module.
noActionT :: String
noActionT = "noAction"

-- | The entity a Strategy's `self` names.
selfId :: EntityId
selfId = Id selfName

-- | A Market reference: a bare Market id, or a selector over the accessible Markets for a Resource.
marketRefShape :: Shape MarketRef
marketRefShape = genericPremiseSum "MarketRef"
  { "Named": atLiteral idShape
  , "Each": atOneKey "each" "every accessible Market for the Resource" idShape
  , "Cheapest": atOneKey "cheapest" "the accessible Market with the lowest price for the Resource" idShape
  , "Costliest": atOneKey "costliest" "the accessible Market with the highest price for the Resource" idShape
  }

-- | An expression: a quantity literal (a bare string), or a oneKey form.
exprShape :: Shape Expr
exprShape = recursive "Expr" \e -> genericPremiseSum "Expr"
  { "Lit": atLiteral qShape
  , "Param": atOneKey "param" "a premise parameter" stringShape
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
condShape = recursive "Cond" \c -> genericPremiseSum "Cond"
  { "Gt": atOneKey "gt" "greater than" (pairOf exprShape exprShape)
  , "Ge": atOneKey "ge" "at least" (pairOf exprShape exprShape)
  , "Le": atOneKey "le" "at most" (pairOf exprShape exprShape)
  , "Eq": atOneKey "eq" "equal to" (pairOf exprShape exprShape)
  , "Not": atOneKey "not" "not" c
  , "TransformedLastRound": atOneKey "transformedLastRound" "a transformation ran at least the amount last round" transformedBody
  , "RestrictedLastRound": atOneKey "restrictedLastRound" "a restriction was enacted against the Entity last round" restrictedBody
  , "RestrictionInForce": atOneKey "restrictionInForce" "a restriction by the jurisdiction is in force" restrictionBody
  }
  where
  transformedBody = object "TransformedLastRound"
    ( mk3 <$> fieldOf "by" "the jurisdiction" idShape _.by
        <*> fieldOf "transformation" "the transformation" idShape _.transformation
        <*> fieldOf "atLeast" "the threshold" exprShape _.atLeast
    )
    where
    mk3 b t a = { by: b, transformation: t, atLeast: a }
  restrictedBody = object "RestrictedLastRound"
    ( mk2 <$> fieldOf "by" "the jurisdiction" idShape _.by
        <*> fieldOf "target" "the Entity restricted" idShape _.target
    )
    where
    mk2 b t = { by: b, target: t }
  restrictionBody = object "RestrictionInForce" (mk1 <$> fieldOf "by" "the jurisdiction" idShape _.by)
    where
    mk1 b = { by: b }

type TransformRec = { transformation :: TransformationId, quantity :: Expr }

-- | A Transfer as a plain object: how a Deal writes its transfers.
transferObject :: Shape { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }
transferObject = transferBody exprShape

-- | A transform as a plain object: how a Deal writes its transforms, run by the acting Entity.
transformObject :: Shape TransformRec
transformObject = object "Transform"
  ( mk2 <$> fieldOf "transformation" "the transformation" idShape _.transformation
      <*> fieldOf "quantity" "the quantity run" exprShape _.quantity
  )
  where
  mk2 t q = { transformation: t, quantity: q }

-- | An Intent a Strategy writes as a template. `self` is implicit: the jurisdiction of an enactment,
-- | and the `by` of a transform.
intentTemplateShape :: Shape IntentTemplate
intentTemplateShape = premiseSum "Intent template"
  [ oneKey "trade" "an order in a Market" tradeBody
      ( \x -> case x of
          Trade r -> Just r
          _ -> Nothing
      )
      Trade
  , oneKey "route" "a routed order on a pair" routeBody
      ( \x -> case x of
          Route r -> Just r
          _ -> Nothing
      )
      Route
  , oneKey "transform" "a run of a transformation by the acting Entity" transformObject
      ( \x -> case x of
          Transform r | r.by == selfId -> Just { transformation: r.transformation, quantity: r.quantity }
          _ -> Nothing
      )
      (\r -> Transform { by: selfId, transformation: r.transformation, quantity: r.quantity })
  , oneKey "transfer" "a Resource given by the Entity" transferObject
      ( \x -> case x of
          Transfer r -> Just r
          _ -> Nothing
      )
      Transfer
  , oneKey "enactTariff" "a tariff set by the acting Entity" (ownTariffBody exprShape)
      ( \x -> case x of
          ChangeRule (Enact (Tariff r)) | r.jurisdiction == selfId -> Just { origin: r.origin, rate: r.rate }
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (Tariff { jurisdiction: selfId, origin: r.origin, rate: r.rate })))
  , oneKey "enactAccessBan" "a ban set by the acting Entity" ownAccessBanBody
      ( \x -> case x of
          ChangeRule (Enact (AccessBan r)) | r.jurisdiction == selfId -> Just { target: r.target }
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (AccessBan { jurisdiction: selfId, target: r.target })))
  , oneKey "enactWithholding" "units of a capacity withheld by the author" (withholdingBody exprShape)
      ( \x -> case x of
          ChangeRule (Enact (Withholding r)) -> Just r
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (Withholding r)))
  , oneKey "repeal" "repeal a Rule the author governs" repealBody
      ( \x -> case x of
          ChangeRule (Repeal r) -> Just { rule: r }
          _ -> Nothing
      )
      (\r -> ChangeRule (Repeal r.rule))
  ]
  where
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
  repealBody = object "Repeal" (mk1 <$> fieldOf "rule" "the Rule, by the id the engine gave it" idShape _.rule)
    where
    mk1 r = { rule: r }

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

-- | Whether a Deal's Intents are its transfers, then its transforms, all run by the acting Entity.
dealShaped :: Array IntentTemplate -> Boolean
dealShaped xs = Array.all dealIntent xs && Array.all isSelfTransform fs && xs == ts <> fs
  where
  ts = Array.filter isTransfer xs
  fs = Array.filter isTransform xs
  isTransfer = case _ of
    Transfer _ -> true
    _ -> false
  isTransform = case _ of
    Transform _ -> true
    _ -> false
  isSelfTransform = case _ of
    Transform r -> r.by == selfId
    _ -> false

-- | The Proposal templates and the choices among them.
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
  , oneKey "reciprocate" "enact the restriction enacted against this Entity last round" reciprocateBody
      ( \x -> case x of
          ReciprocateLastRound r -> Just r
          _ -> Nothing
      )
      ReciprocateLastRound
  , oneKey "deal" "a Proposal with no procedure: its transfers, then its transforms" dealBody
      dealOf
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
  reciprocateBody = object "Reciprocate" (mk1 <$> fieldOf "target" "the Entity that restricted this one" idShape _.target)
    where
    mk1 x = { target: x }

  dealOf :: Template -> Maybe { signers :: Array EntityId, intents :: Array IntentTemplate }
  dealOf = case _ of
    ProposalT p | Nothing <- p.procedure, dealShaped p.intents -> Just { signers: p.signers, intents: p.intents }
    _ -> Nothing
  dealBody = object "Deal"
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
signRuleShape = recursive "SignRule" \s -> genericPremiseSum "SignRule"
  { "SignProcedures": atOneKey "procedures" "the collective procedures this Entity signs under" (arrayOf idShape)
  , "SignFacility": atOneKey "facility" "Proposals ordering this facility" idShape
  , "SignAll": atOneKey "all" "every addressed Proposal" unitShape
  , "SignIf": atOneKey "signIf" "the inner rule, while every condition holds" (signIfBody s)
  }
  where
  signIfBody s = object "SignIf"
    ( mk2 <$> fieldOf "when" "the conditions" (omittedWhen (const false) (arrayOf condShape)) _.when
        <*> fieldOf "rule" "the inner sign rule" s _.rule
    )
    where
    mk2 w r = { when: w, rule: r }

-- | A clause: when every condition holds, propose its templates.
clauseShape :: Shape Clause
clauseShape = object "Clause"
  ( mk <$> fieldOf "when" "the conditions" (omittedWhen (const false) (arrayOf condShape)) _.when
      <*> fieldOf "propose" "the templates proposed" (arrayOf templateShape) _.propose
  )
  where
  mk w p = { when: w, propose: p }

-- | A Strategy: its clauses, and its sign rules.
strategyShape :: Shape Strategy
strategyShape = object "Strategy"
  ( mk <$> fieldOf "rules" "the clauses" (omittedWhen Array.null (arrayOf clauseShape)) _.rules
      <*> fieldOf "sign" "the sign rules" (omittedWhen Array.null (arrayOf signRuleShape)) _.sign
  )
  where
  mk r s = { rules: r, sign: s }

-- | The policies file: the Gameboard's Policy catalog, each Policy's name and its Strategy.
policiesShape :: Shape (Array (Tuple String Strategy))
policiesShape = object "Policies" (identity <$> fieldOf "policies" "the Policy catalog" (keyedOf strategyShape) identity)

-- | The presets file: each seat preset's name and its Strategy, in the order the file lists them.
presetsShape :: Shape (Array (Tuple String Strategy))
presetsShape = keyedOf strategyShape

decodeStrategy :: Json -> Either JsonDecodeError Strategy
decodeStrategy = decodeJsonWith Premise strategyShape

encodeStrategy :: Strategy -> Json
encodeStrategy = encodeWith Premise strategyShape
