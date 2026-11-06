-- | The shared decision language (docs/02_type_system.md §4.4). One interpreter runs NPC
-- | Policies, Aggregate baselines, and seat-policy presets. The language holds only what the
-- | reference Policies and presets use.
module Policy.Program
  ( Program
  , Clause
  , Cond(..)
  , Expr(..)
  , Template(..)
  , SignRule(..)
  , Refs
  , decodeProgram
  , programRefs
  , programParams
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (BehaviorDomain, CapacityRef, CollectiveProcedureId, EntityId, Id(..), MarketId, Pair, ResourceId, RuleId, TransformationId)
import Data.Argonaut.Core (Json, fromString, isNull, toArray, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Array (concatMap, filter, mapMaybe)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Clauses propose; sign rules choose which addressed Proposals to sign.
type Program = { rules :: Array Clause, sign :: Array SignRule }

-- | Every condition must hold for the clause's templates to be proposed.
type Clause = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a scenario parameter (checked against the Gameboard at load time)
  | Round -- the current round
  | Price MarketId -- beginning-of-round marginal price
  | Holding ResourceId -- the Entity's own holding
  | Factor BehaviorDomain -- the stored Representation factor, 1 when none
  | Mul Expr Expr
  | Sub Expr Expr
  | Max Expr Expr

data Cond
  = Gt Expr Expr
  | Ge Expr Expr
  | Le Expr Expr
  | Eq Expr Expr
  | Not Cond
  | TransformedLastRound { by :: EntityId, transformation :: TransformationId, atLeast :: Q }
  | RestrictedLastRound { by :: EntityId, target :: EntityId } -- a restriction enacted last round
  | RestrictionInForce { by :: EntityId } -- an AccessBan or Tariff by that jurisdiction is in force

data Template
  = TradeT { market :: MarketId, quantity :: Expr, limit :: Maybe Expr }
  | RouteT { pair :: Pair, quantity :: Expr, limit :: Maybe Expr } -- best execution: no Market named (docs/02 §5.2)
  | TransformT { transformation :: TransformationId, quantity :: Expr }
  | Procedure { procedure :: CollectiveProcedureId, signers :: Array EntityId, intents :: Array Template } -- intents: trades, transforms, tariffs, reciprocations only
  | EnactTariff { origin :: EntityId, rate :: Expr }
  | ReciprocateLastRound { target :: EntityId } -- enact the restriction `target` enacted against this Entity last round
  | Deal { signers :: Array EntityId, transfers :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }, transforms :: Array { transformation :: TransformationId, quantity :: Expr } } -- `self` names the author; a deal bundles Transfers and Transforms (docs/02 §3)
  | Choose (Array Template) -- one option, picked by a seeded draw
  | EnactAccessBan { target :: EntityId } -- a ban on shipments from the Entity's jurisdiction into `target`
  | EnactWithholding { capacity :: CapacityRef, units :: Expr } -- withhold units of a capacity the Entity owns
  | RepealRule { rule :: RuleId } -- repeal a Rule the Entity governs, by the id the engine assigned it
  | NoAction -- propose nothing (an option for Choose)

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignFacility TransformationId -- Proposals ordering this facility (e.g. shipments)
  | SignAll
  | SignIf { when :: Array Cond, rule :: SignRule } -- the inner rule, while every condition holds

-- Decoding ---------------------------------------------------------------------

decodeProgram :: Json -> Either JsonDecodeError Program
decodeProgram j = do
  o <- decodeJson j
  rules <- o .:? "rules" .!= [] >>= traverse clause
  sign <- o .:? "sign" .!= [] >>= traverse signRule
  pure { rules, sign }

clause :: Json -> Either JsonDecodeError Clause
clause j = do
  o <- decodeJson j
  when <- o .:? "when" .!= [] >>= traverse cond
  propose <- o .: "propose" >>= traverse template
  pure { when, propose }

-- | Every name a Program refers to, for the load-time check against a Gameboard. `self` and
-- | `$home` are not listed: they are resolved by the interpreter.
type Refs =
  { entities :: Array String
  , markets :: Array String
  , resources :: Array String
  , transformations :: Array String
  , parameters :: Array String
  }

-- | One name a Program refers to.
data Ref
  = EntityName String
  | MarketName String
  | ResourceName String
  | TransformationName String
  | ParamName String

derive instance Eq Ref

programRefs :: Program -> Refs
programRefs p =
  { entities: mapMaybe entityName refs
  , markets: mapMaybe marketName refs
  , resources: mapMaybe resourceName refs
  , transformations: mapMaybe transformationName refs
  , parameters: mapMaybe paramName refs
  }
  where
  refs = concatMap clauseRefs p.rules <> concatMap signRefs p.sign

  entityName = case _ of
    EntityName s -> Just s
    _ -> Nothing

  marketName = case _ of
    MarketName s -> Just s
    _ -> Nothing

  resourceName = case _ of
    ResourceName s -> Just s
    _ -> Nothing

  transformationName = case _ of
    TransformationName s -> Just s
    _ -> Nothing

  paramName = case _ of
    ParamName s -> Just s
    _ -> Nothing

  clauseRefs r = concatMap condRefs r.when <> concatMap templateRefs r.propose

  condRefs = case _ of
    Gt a b -> exprRefs a <> exprRefs b
    Ge a b -> exprRefs a <> exprRefs b
    Le a b -> exprRefs a <> exprRefs b
    Eq a b -> exprRefs a <> exprRefs b
    Not c -> condRefs c
    TransformedLastRound x -> [ EntityName (unwrapId x.by), TransformationName (unwrapId x.transformation) ]
    RestrictedLastRound x -> [ EntityName (unwrapId x.by), EntityName (unwrapId x.target) ]
    RestrictionInForce x -> [ EntityName (unwrapId x.by) ]

  templateRefs = case _ of
    TradeT x -> [ MarketName (unwrapId x.market) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limit
    RouteT x -> [ ResourceName (unwrapId x.pair.base), ResourceName (unwrapId x.pair.quote) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limit
    TransformT x -> [ TransformationName (unwrapId x.transformation) ] <> exprRefs x.quantity
    Procedure x -> map (EntityName <<< unwrapId) x.signers <> concatMap templateRefs x.intents
    EnactTariff x -> [ EntityName (unwrapId x.origin) ] <> exprRefs x.rate
    ReciprocateLastRound x -> [ EntityName (unwrapId x.target) ]
    Deal x -> map (EntityName <<< unwrapId) x.signers <> concatMap transferRefs x.transfers <> concatMap transformRefs x.transforms
    Choose xs -> concatMap templateRefs xs
    EnactAccessBan x -> [ EntityName (unwrapId x.target) ]
    EnactWithholding x -> exprRefs x.units
    RepealRule _ -> []
    NoAction -> []

  transferRefs t =
    [ ResourceName (unwrapId t.resource) ]
      <> filter (\r -> r /= EntityName "self") [ EntityName (unwrapId t.from), EntityName (unwrapId t.to) ]
      <> exprRefs t.quantity

  transformRefs t = [ TransformationName (unwrapId t.transformation) ] <> exprRefs t.quantity

  signRefs = case _ of
    SignFacility t -> [ TransformationName (unwrapId t) ]
    SignIf x -> concatMap condRefs x.when <> signRefs x.rule
    _ -> []

  exprRefs = case _ of
    Lit _ -> []
    Param name -> [ ParamName name ]
    Round -> []
    Price m -> [ MarketName (unwrapId m) ]
    Holding r -> [ ResourceName (unwrapId r) ]
    Factor d -> [ ResourceName (unwrapId d) ]
    Mul a b -> exprRefs a <> exprRefs b
    Sub a b -> exprRefs a <> exprRefs b
    Max a b -> exprRefs a <> exprRefs b

  unwrapId :: forall t. Id t -> String
  unwrapId (Id s) = s

-- | The scenario parameters a Program reads.
programParams :: Program -> Array String
programParams = _.parameters <<< programRefs

-- | `{ "<tag>": body }`: exactly one key names the variant. `kind` names what is decoded, for
-- | the error. Several keys, or none, is a `Named` error carrying the offending value.
tagged :: String -> Json -> Either JsonDecodeError (Tuple String Json)
tagged kind j = do
  o <- note (TypeMismatch "object") (toObject j)
  case Object.toUnfoldable o of
    [ t ] -> Right t
    _ -> Left (Named kind (UnexpectedValue j))

-- | An unknown tag: a `Named` error carrying the tag.
unknownTag :: String -> String -> JsonDecodeError
unknownTag kind tag = Named kind (UnexpectedValue (fromString tag))

expr :: Json -> Either JsonDecodeError Expr
expr j = case toString j of
  Just _ -> Lit <$> decodeJson j
  Nothing -> tagged "expression" j >>= \(Tuple tag body) -> case tag of
    "param" -> Param <$> decodeJson body
    "round" -> pure Round
    "price" -> Price <<< Id <$> decodeJson body
    "holding" -> Holding <<< Id <$> decodeJson body
    "factor" -> Factor <<< Id <$> decodeJson body
    "mul" -> pair Mul body
    "sub" -> pair Sub body
    "max" -> pair Max body
    _ -> Left (unknownTag "expression" tag)
  where
  pair f body = case toArray body of
    Just [ a, b ] -> f <$> expr a <*> expr b
    _ -> Left (Named "expression operands" (UnexpectedValue body))

cond :: Json -> Either JsonDecodeError Cond
cond j = tagged "condition" j >>= \(Tuple tag body) -> case tag of
  "gt" -> pair Gt body
  "ge" -> pair Ge body
  "le" -> pair Le body
  "eq" -> pair Eq body
  "not" -> Not <$> cond body
  "transformedLastRound" -> do
    o <- decodeJson body
    by <- o .: "by"
    t <- o .: "transformation"
    atLeast <- o .: "atLeast"
    pure (TransformedLastRound { by: Id by, transformation: Id t, atLeast })
  "restrictedLastRound" -> do
    o <- decodeJson body
    by <- o .: "by"
    target <- o .: "target"
    pure (RestrictedLastRound { by: Id by, target: Id target })
  "restrictionInForce" -> do
    o <- decodeJson body
    by <- o .: "by"
    pure (RestrictionInForce { by: Id by })
  _ -> Left (unknownTag "condition" tag)
  where
  pair f body = case toArray body of
    Just [ a, b ] -> f <$> expr a <*> expr b
    _ -> Left (Named "condition operands" (UnexpectedValue body))

template :: Json -> Either JsonDecodeError Template
template j = tagged "template" j >>= \(Tuple tag body) -> case tag of
  "choose" -> Choose <$> (decodeJson body >>= traverse template)
  "noAction" -> pure NoAction
  _ -> object tag body

-- | A template inside a Procedure: only Intents may be proposed there. Nested Procedures,
-- | Deals and Choose are rejected, since a Procedure's Proposal carries only Intents.
procedureIntent :: Json -> Either JsonDecodeError Template
procedureIntent j = do
  t <- template j
  case t of
    Procedure _ -> Left (Named "procedure intent" (UnexpectedValue j))
    Deal _ -> Left (Named "procedure intent" (UnexpectedValue j))
    Choose _ -> Left (Named "procedure intent" (UnexpectedValue j))
    _ -> pure t

object :: String -> Json -> Either JsonDecodeError Template
object tag body = do
  o <- decodeJson body
  case tag of
    "trade" -> do
      m <- o .: "market"
      quantity <- o .: "quantity" >>= expr
      limit <- o .:? "limit" >>= traverse expr
      if isNull m then do
        base <- o .: "base"
        quote <- o .: "quote"
        pure (RouteT { pair: { base: Id base, quote: Id quote }, quantity, limit })
      else do
        market <- decodeJson m
        pure (TradeT { market: Id market, quantity, limit })
    "transform" -> do
      t <- o .: "transformation"
      quantity <- o .: "quantity" >>= expr
      pure (TransformT { transformation: Id t, quantity })
    "procedure" -> do
      p <- o .: "id"
      signers <- o .: "signers"
      intents <- o .: "intents" >>= traverse procedureIntent
      pure (Procedure { procedure: Id p, signers: map Id signers, intents })
    "enactTariff" -> do
      origin <- o .: "origin"
      rate <- o .: "rate" >>= expr
      pure (EnactTariff { origin: Id origin, rate })
    "deal" -> do
      signers <- o .: "signers"
      transfers <- o .: "transfers" >>= traverse \t -> do
        x <- decodeJson t
        from <- x .: "from"
        to <- x .: "to"
        resource <- x .: "resource"
        quantity <- x .: "quantity" >>= expr
        pure { from: Id from, to: Id to, resource: Id resource, quantity }
      transforms <- (o .:? "transforms" .!= []) >>= traverse \t -> do
        x <- decodeJson t
        transformation <- x .: "transformation"
        quantity <- x .: "quantity" >>= expr
        pure { transformation: Id transformation, quantity }
      pure (Deal { signers: map Id signers, transfers, transforms })
    "reciprocate" -> do
      target <- o .: "target"
      pure (ReciprocateLastRound { target: Id target })
    "enactAccessBan" -> do
      target <- o .: "target"
      pure (EnactAccessBan { target: Id target })
    "enactWithholding" -> do
      capacity <- o .: "capacity"
      units <- o .: "units" >>= expr
      pure (EnactWithholding { capacity: Id capacity, units })
    "repeal" -> do
      rule <- o .: "rule"
      pure (RepealRule { rule: Id rule })
    _ -> Left (unknownTag "template" tag)

signRule :: Json -> Either JsonDecodeError SignRule
signRule j = tagged "sign rule" j >>= \(Tuple tag body) -> case tag of
  "procedures" -> SignProcedures <<< map Id <$> decodeJson body
  "facility" -> SignFacility <<< Id <$> decodeJson body
  "all" -> pure SignAll
  "signIf" -> do
    o <- decodeJson body
    when <- o .: "when" >>= traverse cond
    rule <- o .: "rule" >>= signRule
    pure (SignIf { when, rule })
  _ -> Left (unknownTag "sign rule" tag)
