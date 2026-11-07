-- | The Strategy decoder (docs/02_type_system.md §4.4): the premise format's JSON for Strategies,
-- | with the vocabulary of `Schema.Tags`. Decoding is pure, and a Strategy decodes to the
-- | `Core.Strategy` AST.
module Schema.Decode.Strategy
  ( decodeStrategy
  ) where

import Prelude

import Core.Strategy (Clause, Cond(..), Expr(..), Strategy, SignRule(..), Template(..))
import Core.Types (Id(..), IntentF(..), RuleChangeF(..), RuleF(..))
import Data.Argonaut.Core (Json, fromString, toArray, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Either (Either(..), note)
import Data.Array (concat, find)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Schema.Tags as T

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
  , Tuple T.tradeTag (\b -> IntentT <$> trade b)
  , Tuple T.routeTag (\b -> IntentT <$> route b)
  , Tuple T.transformTag (\b -> IntentT <$> transform b)
  , Tuple T.transferTag (\b -> IntentT <$> transferSpec b)
  , Tuple T.procedureTag procedure
  , Tuple T.enactTariffTag (\b -> IntentT <$> enactTariff b)
  , Tuple T.dealTag deal
  , Tuple T.reciprocateTag reciprocate
  , Tuple T.enactAccessBanTag (\b -> IntentT <$> enactAccessBan b)
  , Tuple T.enactWithholdingTag (\b -> IntentT <$> enactWithholding b)
  , Tuple T.repealTag (\b -> IntentT <$> repeal b)
  ]
  where
  -- `self` names the acting Entity in an entity position; `Policy.Interpret` resolves it
  self = Id T.selfEntity
  trade b = do
    o <- decodeJson b
    m <- o .: "market"
    quantity <- o .: "quantity" >>= expr
    limitPrice <- o .:? "limit" >>= traverse expr
    market <- decodeJson m
    pure (Trade { market: Id market, quantity, limitPrice })
  transform b = do
    o <- decodeJson b
    t <- o .: "transformation"
    quantity <- o .: "quantity" >>= expr
    pure (Transform { by: self, transformation: Id t, quantity })
  -- a Procedure's Proposal: its signers, its id, and its Intents (see `procedureIntent`)
  procedure b = do
    o <- decodeJson b
    p <- o .: "id"
    signers <- o .: "signers"
    intents <- o .: "intents" >>= traverse procedureIntent
    pure (ProposalT { signers: map Id signers, intents: concat intents, procedure: Just (Id p) })
  enactTariff b = do
    o <- decodeJson b
    origin <- o .: "origin"
    rate <- o .: "rate" >>= expr
    pure (ChangeRule (Enact (Tariff { jurisdiction: self, origin: Id origin, rate })))
  deal b = do
    o <- decodeJson b
    signers <- o .: "signers"
    transfers <- o .: "transfers" >>= traverse transferSpec
    transforms <- (o .:? "transforms" .!= []) >>= traverse \t -> do
      x <- decodeJson t
      transformation <- x .: "transformation"
      quantity <- x .: "quantity" >>= expr
      pure (Transform { by: self, transformation: Id transformation, quantity })
    pure (ProposalT { signers: map Id signers, intents: transfers <> transforms, procedure: Nothing })
  reciprocate b = do
    o <- decodeJson b
    target <- o .: "target"
    pure (ReciprocateLastRound { target: Id target })
  enactAccessBan b = do
    o <- decodeJson b
    target <- o .: "target"
    pure (ChangeRule (Enact (AccessBan { jurisdiction: self, target: Id target })))
  enactWithholding b = do
    o <- decodeJson b
    capacity <- o .: "capacity"
    units <- o .: "units" >>= expr
    pure (ChangeRule (Enact (Withholding { capacity: Id capacity, units })))
  repeal b = do
    o <- decodeJson b
    rule <- o .: "rule"
    pure (ChangeRule (Repeal (Id rule)))

-- | An Intent a Procedure may carry. A Procedure's Proposal holds Intents only, so a nested Procedure,
-- | a Deal, a Choose and a reciprocation are rejected. NoAction yields nothing and is dropped.
procedureIntent :: Json -> Either JsonDecodeError (Array (IntentF Expr))
procedureIntent j = do
  t <- template j
  case t of
    IntentT i -> pure [ i ]
    NoAction -> pure []
    _ -> Left (Named "procedure intent" (UnexpectedValue j))

-- | One transfer: `from` gives `quantity` of `resource` to `to`. A Deal's transfers and a
-- | standalone transfer decode alike.
transferSpec :: Json -> Either JsonDecodeError (IntentF Expr)
transferSpec j = do
  o <- decodeJson j
  from <- o .: "from"
  to <- o .: "to"
  resource <- o .: "resource"
  quantity <- o .: "quantity" >>= expr
  pure (Transfer { from: Id from, to: Id to, resource: Id resource, quantity })

-- | A routed order: the pair, the quantity and the optional limit (no Market is named).
route :: Json -> Either JsonDecodeError (IntentF Expr)
route b = do
  o <- decodeJson b
  base <- o .: "base"
  quote <- o .: "quote"
  quantity <- o .: "quantity" >>= expr
  limitPrice <- o .:? "limit" >>= traverse expr
  pure (Route { pair: { base: Id base, quote: Id quote }, quantity, limitPrice })

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
