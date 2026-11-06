-- | The shared decision language (docs/02_type_system.md §4.4). One interpreter runs NPC
-- | Policies, Aggregate baselines, and seat-policy presets. The language holds only what the
-- | reference Policies and presets use.
module Policy.Program
  ( Program
  , Rule
  , Cond(..)
  , Expr(..)
  , Template(..)
  , SignRule(..)
  , decodeProgram
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (BehaviorDomain, CollectiveProcedureId, EntityId, Id(..), MarketId, ResourceId, TransformationId)
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Array (head)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Rules propose; sign rules choose which addressed Proposals to sign.
type Program = { rules :: Array Rule, sign :: Array SignRule }

-- | Every condition must hold for the rule's templates to be proposed.
type Rule = { when :: Array Cond, propose :: Array Template }

data Expr
  = Lit Q
  | Param String -- a binding or scenario parameter
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
  | TransformT { transformation :: TransformationId, quantity :: Expr }
  | Procedure { procedure :: CollectiveProcedureId, signers :: Array EntityId, intents :: Array Template }
  | EnactTariff { origin :: EntityId, rate :: Expr }
  | ReciprocateLastRound { target :: EntityId } -- enact the restriction `target` enacted against this Entity last round
  | Deal { signers :: Array EntityId, transfers :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Expr } } -- `self` names the author
  | Choose (Array Template) -- one option, picked by a seeded draw
  | Nothing' -- propose nothing (an option for Choose)

data SignRule
  = SignProcedures (Array CollectiveProcedureId)
  | SignFacility TransformationId -- Proposals ordering this facility (e.g. shipments)
  | SignAll

-- Decoding ---------------------------------------------------------------------

decodeProgram :: Json -> Either JsonDecodeError Program
decodeProgram j = do
  o <- decodeJson j
  rules <- o .:? "rules" .!= [] >>= traverse rule
  sign <- o .:? "sign" .!= [] >>= traverse signRule
  pure { rules, sign }

rule :: Json -> Either JsonDecodeError Rule
rule j = do
  o <- decodeJson j
  when <- o .:? "when" .!= [] >>= traverse cond
  propose <- o .: "propose" >>= traverse template
  pure { when, propose }

-- | `{ "<tag>": body }`
tagged :: Json -> Either JsonDecodeError (Tuple String Json)
tagged j = do
  o <- note (TypeMismatch "tagged object") (toObject j)
  note (TypeMismatch "one key") (head (Object.toUnfoldable o))

expr :: Json -> Either JsonDecodeError Expr
expr j = case toString j of
  Just _ -> Lit <$> decodeJson j
  Nothing -> tagged j >>= \(Tuple tag body) -> case tag of
    "param" -> Param <$> decodeJson body
    "round" -> pure Round
    "price" -> Price <<< Id <$> decodeJson body
    "holding" -> Holding <<< Id <$> decodeJson body
    "factor" -> Factor <<< Id <$> decodeJson body
    "mul" -> pair Mul body
    "sub" -> pair Sub body
    "max" -> pair Max body
    _ -> Left (TypeMismatch ("expression " <> tag))
  where
  pair f body = case toArray body of
    Just [ a, b ] -> f <$> expr a <*> expr b
    _ -> Left (TypeMismatch "two operands")

cond :: Json -> Either JsonDecodeError Cond
cond j = tagged j >>= \(Tuple tag body) -> case tag of
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
  _ -> Left (TypeMismatch ("condition " <> tag))
  where
  pair f body = case toArray body of
    Just [ a, b ] -> f <$> expr a <*> expr b
    _ -> Left (TypeMismatch "two operands")

template :: Json -> Either JsonDecodeError Template
template j = tagged j >>= \(Tuple tag body) -> case tag of
  "choose" -> Choose <$> (decodeJson body >>= traverse template)
  "none" -> pure Nothing'
  _ -> object tag body

object :: String -> Json -> Either JsonDecodeError Template
object tag body = do
  o <- decodeJson body
  case tag of
    "trade" -> do
      m <- o .: "market"
      quantity <- o .: "quantity" >>= expr
      limit <- o .:? "limit" >>= traverse expr
      pure (TradeT { market: Id m, quantity, limit })
    "transform" -> do
      t <- o .: "transformation"
      quantity <- o .: "quantity" >>= expr
      pure (TransformT { transformation: Id t, quantity })
    "procedure" -> do
      p <- o .: "id"
      signers <- o .: "signers"
      intents <- o .: "intents" >>= traverse template
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
      pure (Deal { signers: map Id signers, transfers })
    "reciprocate" -> do
      target <- o .: "target"
      pure (ReciprocateLastRound { target: Id target })
    _ -> Left (TypeMismatch ("template " <> tag))

signRule :: Json -> Either JsonDecodeError SignRule
signRule j = tagged j >>= \(Tuple tag body) -> case tag of
  "procedures" -> SignProcedures <<< map Id <$> decodeJson body
  "facility" -> SignFacility <<< Id <$> decodeJson body
  "all" -> pure SignAll
  _ -> Left (TypeMismatch ("sign rule " <> tag))

