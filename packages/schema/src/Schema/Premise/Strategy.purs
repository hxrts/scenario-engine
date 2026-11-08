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

import Core.Shape (Desc(..), Format(..), PCtor, Shape, arrayOf, decodeJsonWith, decodeWith, embedded, encodeWith, fieldOf, keyedOf, literal, object, omittedWhen, oneKey, optionalOf, pairOf, premiseSum, qShape, recursive, stringShape, unitShape)
import Core.Strategy (Clause, Cond(..), Expr(..), IntentTemplate, MarketRef(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (CollectiveProcedureId, EntityId, Id(..), IntentF(..), ResourceId, RuleChangeF(..), RuleF(..), TransformationId, idShape)
import Data.Argonaut.Core (Json, fromArray, jsonNull, toArray, toObject)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array as Array
import Foreign.Object as Object
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- The premise tags. Private to this module.
paramT :: String
paramT = "param"

roundT :: String
roundT = "round"

priceT :: String
priceT = "price"

holdingT :: String
holdingT = "holding"

factorT :: String
factorT = "factor"

mulT :: String
mulT = "mul"

subT :: String
subT = "sub"

maxT :: String
maxT = "max"

gtT :: String
gtT = "gt"

geT :: String
geT = "ge"

leT :: String
leT = "le"

eqT :: String
eqT = "eq"

notT :: String
notT = "not"

transformedLastRoundT :: String
transformedLastRoundT = "transformedLastRound"

restrictedLastRoundT :: String
restrictedLastRoundT = "restrictedLastRound"

restrictionInForceT :: String
restrictionInForceT = "restrictionInForce"

tradeT :: String
tradeT = "trade"

routeT :: String
routeT = "route"

transformT :: String
transformT = "transform"

transferT :: String
transferT = "transfer"

procedureT :: String
procedureT = "procedure"

enactTariffT :: String
enactTariffT = "enactTariff"

reciprocateT :: String
reciprocateT = "reciprocate"

dealT :: String
dealT = "deal"

chooseT :: String
chooseT = "choose"

enactAccessBanT :: String
enactAccessBanT = "enactAccessBan"

enactWithholdingT :: String
enactWithholdingT = "enactWithholding"

repealT :: String
repealT = "repeal"

noActionT :: String
noActionT = "noAction"

proceduresT :: String
proceduresT = "procedures"

facilityT :: String
facilityT = "facility"

allT :: String
allT = "all"

signIfT :: String
signIfT = "signIf"

eachT :: String
eachT = "each"

cheapestT :: String
cheapestT = "cheapest"

costliestT :: String
costliestT = "costliest"

-- | A tagged form: `{ "<tag>": payload }`.
tagged :: forall a b. String -> String -> Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
tagged t desc = oneKey { tag: t, desc }

-- | The entity a Strategy's `self` names.
selfId :: EntityId
selfId = Id selfName

-- | A Market reference: a bare Market id, or a selector over the accessible Markets for a Resource.
marketRefShape :: Shape MarketRef
marketRefShape = premiseSum "MarketRef"
  [ tagged eachT "every accessible Market for the Resource" idShape
      ( \x -> case x of
          Each r -> Just r
          _ -> Nothing
      )
      Each
  , tagged cheapestT "the accessible Market with the lowest price for the Resource" idShape
      ( \x -> case x of
          Cheapest r -> Just r
          _ -> Nothing
      )
      Cheapest
  , tagged costliestT "the accessible Market with the highest price for the Resource" idShape
      ( \x -> case x of
          Costliest r -> Just r
          _ -> Nothing
      )
      Costliest
  , literal idShape
      ( \x -> case x of
          Named m -> Just m
          _ -> Nothing
      )
      Named
  ]

-- | An expression: a quantity literal (a bare string), or a tagged form.
exprShape :: Shape Expr
exprShape = recursive "Expr" \e -> premiseSum "Expr"
  [ tagged paramT "a premise parameter" stringShape
      ( \x -> case x of
          Param s -> Just s
          _ -> Nothing
      )
      Param
  , tagged roundT "the current round" unitShape
      ( \x -> case x of
          Round -> Just unit
          _ -> Nothing
      )
      (const Round)
  , tagged priceT "the beginning-of-round price of a Market" marketRefShape
      ( \x -> case x of
          Price m -> Just m
          _ -> Nothing
      )
      Price
  , tagged holdingT "the Entity's own holding of a Resource" idShape
      ( \x -> case x of
          Holding r -> Just r
          _ -> Nothing
      )
      Holding
  , tagged factorT "the stored factor of a behaviour domain" idShape
      ( \x -> case x of
          Factor d -> Just d
          _ -> Nothing
      )
      Factor
  , tagged mulT "a product" (pairOf e e)
      ( \x -> case x of
          Mul a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Mul a b)
  , tagged subT "a difference" (pairOf e e)
      ( \x -> case x of
          Sub a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Sub a b)
  , tagged maxT "the larger of two" (pairOf e e)
      ( \x -> case x of
          Max a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Max a b)
  , literal qShape
      ( \x -> case x of
          Lit a -> Just a
          _ -> Nothing
      )
      Lit
  ]

-- | A condition.
condShape :: Shape Cond
condShape = recursive "Cond" \c -> premiseSum "Cond"
  [ tagged gtT "greater than" (pairOf exprShape exprShape)
      ( \x -> case x of
          Gt a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Gt a b)
  , tagged geT "at least" (pairOf exprShape exprShape)
      ( \x -> case x of
          Ge a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Ge a b)
  , tagged leT "at most" (pairOf exprShape exprShape)
      ( \x -> case x of
          Le a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Le a b)
  , tagged eqT "equal to" (pairOf exprShape exprShape)
      ( \x -> case x of
          Eq a b -> Just (Tuple a b)
          _ -> Nothing
      )
      (\(Tuple a b) -> Eq a b)
  , tagged notT "not" c
      ( \x -> case x of
          Not y -> Just y
          _ -> Nothing
      )
      Not
  , tagged transformedLastRoundT "a transformation ran at least the amount last round" transformedBody
      ( \x -> case x of
          TransformedLastRound r -> Just r
          _ -> Nothing
      )
      TransformedLastRound
  , tagged restrictedLastRoundT "a restriction was enacted against the Entity last round" restrictedBody
      ( \x -> case x of
          RestrictedLastRound r -> Just r
          _ -> Nothing
      )
      RestrictedLastRound
  , tagged restrictionInForceT "a restriction by the jurisdiction is in force" restrictionBody
      ( \x -> case x of
          RestrictionInForce r -> Just r
          _ -> Nothing
      )
      RestrictionInForce
  ]
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

type TransferRec = { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }

type TransformRec = { transformation :: TransformationId, quantity :: Expr }

-- | A Transfer as a plain object: how a Deal writes its transfers.
transferObject :: Shape TransferRec
transferObject = object "Transfer"
  ( mk4 <$> fieldOf "from" "the giving Entity" idShape _.from
      <*> fieldOf "to" "the receiving Entity" idShape _.to
      <*> fieldOf "resource" "the Resource given" idShape _.resource
      <*> fieldOf "quantity" "the quantity" exprShape _.quantity
  )
  where
  mk4 f t r q = { from: f, to: t, resource: r, quantity: q }

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
  [ tagged tradeT "an order in a Market" tradeBody
      ( \x -> case x of
          Trade r -> Just r
          _ -> Nothing
      )
      Trade
  , tagged routeT "a routed order on a pair" routeBody
      ( \x -> case x of
          Route r -> Just r
          _ -> Nothing
      )
      Route
  , tagged transformT "a run of a transformation by the acting Entity" transformObject
      ( \x -> case x of
          Transform r | r.by == selfId -> Just { transformation: r.transformation, quantity: r.quantity }
          _ -> Nothing
      )
      (\r -> Transform { by: selfId, transformation: r.transformation, quantity: r.quantity })
  , tagged transferT "a Resource given by the Entity" transferObject
      ( \x -> case x of
          Transfer r -> Just r
          _ -> Nothing
      )
      Transfer
  , tagged enactTariffT "a tariff set by the acting Entity" tariffBody
      ( \x -> case x of
          ChangeRule (Enact (Tariff r)) | r.jurisdiction == selfId -> Just { origin: r.origin, rate: r.rate }
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (Tariff { jurisdiction: selfId, origin: r.origin, rate: r.rate })))
  , tagged enactAccessBanT "a ban set by the acting Entity" banBody
      ( \x -> case x of
          ChangeRule (Enact (AccessBan r)) | r.jurisdiction == selfId -> Just { target: r.target }
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (AccessBan { jurisdiction: selfId, target: r.target })))
  , tagged enactWithholdingT "units of a capacity withheld by the author" withholdingBody
      ( \x -> case x of
          ChangeRule (Enact (Withholding r)) -> Just r
          _ -> Nothing
      )
      (\r -> ChangeRule (Enact (Withholding r)))
  , tagged repealT "repeal a Rule the author governs" repealBody
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
        <*> fieldOf "limit" "the limit price, if any" (optionalOf exprShape) _.limitPrice
    )
    where
    mk4 b qu q l = { pair: { base: b, quote: qu }, quantity: q, limitPrice: l }
  tariffBody = object "EnactTariff"
    ( mk2 <$> fieldOf "origin" "the Entity whose goods it taxes" idShape _.origin
        <*> fieldOf "rate" "the rate" exprShape _.rate
    )
    where
    mk2 o r = { origin: o, rate: r }
  banBody = object "EnactAccessBan" (mk1 <$> fieldOf "target" "the Entity the ban is against" idShape _.target)
    where
    mk1 t = { target: t }
  withholdingBody = object "EnactWithholding"
    ( mk2 <$> fieldOf "capacity" "the capacity withheld" idShape _.capacity
        <*> fieldOf "units" "units withheld per round" exprShape _.units
    )
    where
    mk2 c u = { capacity: c, units: u }
  repealBody = object "Repeal" (mk1 <$> fieldOf "rule" "the Rule, by the id the engine gave it" idShape _.rule)
    where
    mk1 r = { rule: r }

-- | A Procedure's Intents: each a template Intent. A `noAction` among them contributes nothing.
isNoAction :: Json -> Boolean
isNoAction j = case toObject j of
  Just o -> Object.lookup noActionT o == Just jsonNull
  Nothing -> false

procedureIntentsShape :: Shape (Array IntentTemplate)
procedureIntentsShape =
  { desc: DArray intentTemplateShape.desc
  , codec: \_ ->
      { enc: \xs -> fromArray (map (encodeWith Premise intentTemplateShape) xs)
      , dec: \j -> do
          xs <- note "expected a list of Intents" (toArray j)
          map Array.catMaybes (traverse readOne xs)
      }
  , absent: Array.null
  , missing: Just []
  }
  where
  readOne j
    | isNoAction j = Right Nothing
    | otherwise = map Just (decodeWith Premise intentTemplateShape j)

-- | Whether a Deal's Intents are its transfers, then its transforms, all run by the acting Entity.
dealShaped :: Array IntentTemplate -> Boolean
dealShaped xs = Array.all isSelfTransform fs && xs == ts <> fs
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
  [ tagged chooseT "one option, picked by a seeded draw" (arrayOf t)
      ( \x -> case x of
          Choose ts -> Just ts
          _ -> Nothing
      )
      Choose
  , tagged noActionT "propose nothing" unitShape
      ( \x -> case x of
          NoAction -> Just unit
          _ -> Nothing
      )
      (const NoAction)
  , tagged reciprocateT "enact the restriction enacted against this Entity last round" reciprocateBody
      ( \x -> case x of
          ReciprocateLastRound r -> Just r
          _ -> Nothing
      )
      ReciprocateLastRound
  , tagged dealT "a Proposal with no procedure: its transfers, then its transforms" dealBody
      dealOf
      (\r -> ProposalT { signers: r.signers, intents: r.intents, procedure: Nothing })
  , tagged procedureT "a Procedure's Proposal, under its collective procedure" procedureBody
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
        <*> fieldOf "transforms" "the Transforms, if any" (omittedWhen Array.null (arrayOf transformObject)) (\r -> transformsOf r.intents)
    )
    where
    mk3 s ts fs = { signers: s, intents: map Transfer ts <> map (\r -> Transform { by: selfId, transformation: r.transformation, quantity: r.quantity }) fs }
  transfersOf xs = Array.mapMaybe
    ( case _ of
        Transfer r -> Just r
        _ -> Nothing
    )
    xs
  transformsOf xs = Array.mapMaybe
    ( case _ of
        Transform r -> Just { transformation: r.transformation, quantity: r.quantity }
        _ -> Nothing
    )
    xs

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
signRuleShape = recursive "SignRule" \s -> premiseSum "SignRule"
  [ tagged proceduresT "the collective procedures this Entity signs under" (arrayOf idShape)
      ( \x -> case x of
          SignProcedures ps -> Just ps
          _ -> Nothing
      )
      SignProcedures
  , tagged facilityT "Proposals ordering this facility" idShape
      ( \x -> case x of
          SignFacility f -> Just f
          _ -> Nothing
      )
      SignFacility
  , tagged allT "every addressed Proposal" unitShape
      ( \x -> case x of
          SignAll -> Just unit
          _ -> Nothing
      )
      (const SignAll)
  , tagged signIfT "the inner rule, while every condition holds" (signIfBody s)
      ( \x -> case x of
          SignIf r -> Just r
          _ -> Nothing
      )
      SignIf
  ]
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
