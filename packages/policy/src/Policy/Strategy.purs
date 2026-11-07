-- | The shared decision language (docs/02_type_system.md §4.4). One interpreter runs NPC
-- | Policies, Aggregate baselines, and seat-policy presets. The language holds only what the
-- | reference Policies and presets use. The JSON vocabulary is `Policy.Tags`.
module Policy.Strategy
  ( Strategy
  , Clause
  , Cond(..)
  , Expr(..)
  , Template(..)
  , TransferTemplate
  , SignRule(..)
  , Refs
  , decodeStrategy
  , strategyRefs
  , strategyParams
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (BehaviorDomain, CapacityRef, CollectiveProcedureId, EntityId, Id(..), MarketId, Pair, ResourceId, RuleId, TransformationId)
import Data.Argonaut.Core (Json, fromString, toArray, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Array (concatMap, find, mapMaybe)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Policy.Tags as T

-- | Clauses propose; sign rules choose which addressed Proposals to sign.
type Strategy = { rules :: Array Clause, sign :: Array SignRule }

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
  | TransformedLastRound { by :: EntityId, transformation :: TransformationId, atLeast :: Expr }
  | RestrictedLastRound { by :: EntityId, target :: EntityId } -- a restriction enacted last round
  | RestrictionInForce { by :: EntityId } -- an AccessBan or Tariff by that jurisdiction is in force

-- | A Transfer a template carries: the agent `from` gives `quantity` to `to`.
type TransferTemplate = { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr }

data Template
  = TradeT { market :: MarketId, quantity :: Expr, limit :: Maybe Expr }
  | RouteT { pair :: Pair, quantity :: Expr, limit :: Maybe Expr } -- best execution: no Market named (docs/02 §5.2)
  | TransformT { transformation :: TransformationId, quantity :: Expr }
  | TransferT TransferTemplate -- an Intent a procedure may carry (docs/05 §12.1)
  | Procedure { procedure :: CollectiveProcedureId, signers :: Array EntityId, intents :: Array Template } -- intents: trades, transforms, transfers, tariffs, reciprocations only
  | EnactTariff { origin :: EntityId, rate :: Expr }
  | ReciprocateLastRound { target :: EntityId } -- enact the restriction `target` enacted against this Entity last round
  | Deal { signers :: Array EntityId, transfers :: Array TransferTemplate, transforms :: Array { transformation :: TransformationId, quantity :: Expr } } -- `self` names the author; a deal bundles Transfers and Transforms (docs/02 §3)
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

decodeStrategy :: Json -> Either JsonDecodeError Strategy
decodeStrategy j = do
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

-- | Every name a Strategy refers to, for the load-time check against a Gameboard. `self` and
-- | `$home` are not listed: the interpreter resolves them at every position they can appear.
type Refs =
  { entities :: Array String
  , markets :: Array String
  , resources :: Array String
  , transformations :: Array String
  , parameters :: Array String
  }

-- | One name a Strategy refers to.
data Ref
  = EntityName String
  | MarketName String
  | ResourceName String
  | TransformationName String
  | ParamName String

derive instance Eq Ref

strategyRefs :: Strategy -> Refs
strategyRefs p =
  { entities: mapMaybe entityName refs
  , markets: mapMaybe marketName refs
  , resources: mapMaybe resourceName refs
  , transformations: mapMaybe transformationName refs
  , parameters: mapMaybe paramName refs
  }
  where
  refs = concatMap clauseRefs p.rules <> concatMap signRefs p.sign

  -- `self` is never a name to check: every entity position resolves it to the acting Entity
  entityName = case _ of
    EntityName s | s /= T.selfEntity -> Just s
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
    TransformedLastRound x -> [ EntityName (unwrapId x.by), TransformationName (unwrapId x.transformation) ] <> exprRefs x.atLeast
    RestrictedLastRound x -> [ EntityName (unwrapId x.by), EntityName (unwrapId x.target) ]
    RestrictionInForce x -> [ EntityName (unwrapId x.by) ]

  templateRefs = case _ of
    TradeT x -> [ MarketName (unwrapId x.market) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limit
    RouteT x -> [ ResourceName (unwrapId x.pair.base), ResourceName (unwrapId x.pair.quote) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limit
    TransformT x -> [ TransformationName (unwrapId x.transformation) ] <> exprRefs x.quantity
    TransferT x -> transferRefs x
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
    [ ResourceName (unwrapId t.resource), EntityName (unwrapId t.from), EntityName (unwrapId t.to) ]
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

-- | The scenario parameters a Strategy reads.
strategyParams :: Strategy -> Array String
strategyParams = _.parameters <<< strategyRefs

-- | `{ "<tag>": body }`: exactly one key names the variant. `kind` names what is decoded, for
-- | the error. Several keys, or none, is a `Named` error carrying the offending value.
tagged :: String -> Json -> Either JsonDecodeError (Tuple String Json)
tagged kind j = do
  o <- note (TypeMismatch "object") (toObject j)
  case Object.toUnfoldable o of
    [ t ] -> Right t
    _ -> Left (Named kind (UnexpectedValue j))

-- | The decoder a tag names, applied to its body. An unknown tag is a `Named` error carrying it.
dispatch :: forall a. String -> Array (Tuple String (Json -> Either JsonDecodeError a)) -> Tuple String Json -> Either JsonDecodeError a
dispatch kind table (Tuple tag body) = case find (\(Tuple k _) -> k == tag) table of
  Just (Tuple _ decode) -> decode body
  Nothing -> Left (Named kind (UnexpectedValue (fromString tag)))

-- | A binary operator's two operands, each an expression.
operands :: forall a. String -> (Expr -> Expr -> a) -> Json -> Either JsonDecodeError a
operands kind f body = case toArray body of
  Just [ a, b ] -> f <$> expr a <*> expr b
  _ -> Left (Named kind (UnexpectedValue body))

-- | A quantity: a literal, or a tagged expression.
expr :: Json -> Either JsonDecodeError Expr
expr j = case toString j of
  Just _ -> Lit <$> decodeJson j
  Nothing -> tagged "expression" j >>= dispatch "expression"
    [ Tuple T.paramTag (\b -> Param <$> decodeJson b)
    , Tuple T.roundTag (\_ -> pure Round)
    , Tuple T.priceTag (\b -> Price <<< Id <$> decodeJson b)
    , Tuple T.holdingTag (\b -> Holding <<< Id <$> decodeJson b)
    , Tuple T.factorTag (\b -> Factor <<< Id <$> decodeJson b)
    , Tuple T.mulTag (operands "expression operands" Mul)
    , Tuple T.subTag (operands "expression operands" Sub)
    , Tuple T.maxTag (operands "expression operands" Max)
    ]

cond :: Json -> Either JsonDecodeError Cond
cond j = tagged "condition" j >>= dispatch "condition"
  [ Tuple T.gtTag (operands "condition operands" Gt)
  , Tuple T.geTag (operands "condition operands" Ge)
  , Tuple T.leTag (operands "condition operands" Le)
  , Tuple T.eqTag (operands "condition operands" Eq)
  , Tuple T.notTag (\b -> Not <$> cond b)
  , Tuple T.transformedLastRoundTag transformedLastRound
  , Tuple T.restrictedLastRoundTag restrictedLastRound
  , Tuple T.restrictionInForceTag restrictionInForce
  ]
  where
  transformedLastRound b = do
    o <- decodeJson b
    by <- o .: "by"
    t <- o .: "transformation"
    atLeast <- o .: "atLeast" >>= expr
    pure (TransformedLastRound { by: Id by, transformation: Id t, atLeast })
  restrictedLastRound b = do
    o <- decodeJson b
    by <- o .: "by"
    target <- o .: "target"
    pure (RestrictedLastRound { by: Id by, target: Id target })
  restrictionInForce b = do
    o <- decodeJson b
    by <- o .: "by"
    pure (RestrictionInForce { by: Id by })

template :: Json -> Either JsonDecodeError Template
template j = tagged "template" j >>= dispatch "template"
  [ Tuple T.chooseTag (\b -> Choose <$> (decodeJson b >>= traverse template))
  , Tuple T.noActionTag (\_ -> pure NoAction)
  , Tuple T.tradeTag trade
  , Tuple T.routeTag route
  , Tuple T.transformTag transform
  , Tuple T.transferTag (\b -> TransferT <$> transferSpec b)
  , Tuple T.procedureTag procedure
  , Tuple T.enactTariffTag enactTariff
  , Tuple T.dealTag deal
  , Tuple T.reciprocateTag reciprocate
  , Tuple T.enactAccessBanTag enactAccessBan
  , Tuple T.enactWithholdingTag enactWithholding
  , Tuple T.repealTag repeal
  ]
  where
  trade b = do
    o <- decodeJson b
    m <- o .: "market"
    quantity <- o .: "quantity" >>= expr
    limit <- o .:? "limit" >>= traverse expr
    market <- decodeJson m
    pure (TradeT { market: Id market, quantity, limit })
  transform b = do
    o <- decodeJson b
    t <- o .: "transformation"
    quantity <- o .: "quantity" >>= expr
    pure (TransformT { transformation: Id t, quantity })
  procedure b = do
    o <- decodeJson b
    p <- o .: "id"
    signers <- o .: "signers"
    intents <- o .: "intents" >>= traverse procedureIntent
    pure (Procedure { procedure: Id p, signers: map Id signers, intents })
  enactTariff b = do
    o <- decodeJson b
    origin <- o .: "origin"
    rate <- o .: "rate" >>= expr
    pure (EnactTariff { origin: Id origin, rate })
  deal b = do
    o <- decodeJson b
    signers <- o .: "signers"
    transfers <- o .: "transfers" >>= traverse transferSpec
    transforms <- (o .:? "transforms" .!= []) >>= traverse \t -> do
      x <- decodeJson t
      transformation <- x .: "transformation"
      quantity <- x .: "quantity" >>= expr
      pure { transformation: Id transformation, quantity }
    pure (Deal { signers: map Id signers, transfers, transforms })
  reciprocate b = do
    o <- decodeJson b
    target <- o .: "target"
    pure (ReciprocateLastRound { target: Id target })
  enactAccessBan b = do
    o <- decodeJson b
    target <- o .: "target"
    pure (EnactAccessBan { target: Id target })
  enactWithholding b = do
    o <- decodeJson b
    capacity <- o .: "capacity"
    units <- o .: "units" >>= expr
    pure (EnactWithholding { capacity: Id capacity, units })
  repeal b = do
    o <- decodeJson b
    rule <- o .: "rule"
    pure (RepealRule { rule: Id rule })

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

-- | One transfer: `from` gives `quantity` of `resource` to `to`. A Deal's transfers and a
-- | standalone transfer decode alike.
transferSpec :: Json -> Either JsonDecodeError TransferTemplate
transferSpec j = do
  o <- decodeJson j
  from <- o .: "from"
  to <- o .: "to"
  resource <- o .: "resource"
  quantity <- o .: "quantity" >>= expr
  pure { from: Id from, to: Id to, resource: Id resource, quantity }

-- | A routed order: the pair, the quantity and the optional limit (no Market is named).
route :: Json -> Either JsonDecodeError Template
route b = do
  o <- decodeJson b
  base <- o .: "base"
  quote <- o .: "quote"
  quantity <- o .: "quantity" >>= expr
  limit <- o .:? "limit" >>= traverse expr
  pure (RouteT { pair: { base: Id base, quote: Id quote }, quantity, limit })

signRule :: Json -> Either JsonDecodeError SignRule
signRule j = tagged "sign rule" j >>= dispatch "sign rule"
  [ Tuple T.proceduresTag (\b -> SignProcedures <<< map Id <$> decodeJson b)
  , Tuple T.facilityTag (\b -> SignFacility <<< Id <$> decodeJson b)
  , Tuple T.allTag (\_ -> pure SignAll)
  , Tuple T.signIfTag signIf
  ]
  where
  signIf b = do
    o <- decodeJson b
    when <- o .: "when" >>= traverse cond
    rule <- o .: "rule" >>= signRule
    pure (SignIf { when, rule })
