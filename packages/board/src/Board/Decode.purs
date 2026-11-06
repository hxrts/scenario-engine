-- | JSON decoding for scenario data. The schema mirrors the core types one to one; this
-- | module adds decoding only, never a second model.
module Board.Decode
  ( readJson
  , readJsonWith
  , decodeWorld
  , decodeEntity
  , decodeRound
  , capability
  , intent
  , rule
  , holdings
  , toMap
  , Round
  , Expect
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (CollectiveProcedure, Representation, CostFunction, Market, ResourceDefinition, Transformation, Capability(..), Entity, EntityKind(..), Controller(..), Id(..), Intent(..), Rule(..), RuleChange(..), RuleKind(..), SignedProposal, Transferability(..), proposalId)
import Core.World (Circulation, ShareSource(..), WorldState)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, fromJsonString, parseJson, printJsonDecodeError, (.:), (.:?), (.!=))
import Data.Bifunctor (lmap)
import Data.Array (mapWithIndex, groupBy, sortWith, concat)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Expect =
  { accounts :: Object (Object Q)
  , reserves :: Object (Object Q)
  , deliveries :: Array { entity :: String, resource :: String, quantity :: Q }
  , rejected :: Array String
  , lapsed :: Array String
  , factors :: Object (Object Q)
  , scheduled :: Int
  , procedures :: Array { author :: String, support :: Q, executed :: Boolean }
  }

type Round = { inputs :: Array SignedProposal, expect :: Expect }

-- | A World State and its rounds, from the compact scenario schema.
-- | Parse and decode JSON text, with a readable error.
readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap printJsonDecodeError <<< fromJsonString

-- | Parse JSON text and decode it with a custom decoder.
readJsonWith :: forall a. (Json -> Either JsonDecodeError a) -> String -> Either String a
readJsonWith decode = lmap printJsonDecodeError <<< (parseJson >=> decode)

decodeWorld :: Json -> Either JsonDecodeError { name :: String, world :: WorldState, rounds :: Array Round }
decodeWorld j = do
  o <- decodeJson j
  name <- o .:? "name" .!= ""
  round <- o .:? "round" .!= 1
  numeraire <- o .: "numeraire"
  resources <- o .: "resources" >>= traverseKeys resource
  markets <- o .: "markets" >>= traverseKeys market
  transformations <- o .: "transformations" >>= traverseKeys transformation
  transport <- o .:? "transport" .!= Object.empty >>= traverse (pure <<< Id)
  jurisdiction <- o .:? "jurisdiction" .!= Object.empty
  entities <- o .: "entities" >>= traverseKeys decodeEntity
  rules <- o .:? "rules" .!= [] >>= traverse rule
  seed <- o .:? "seed" .!= 26021
  representations <- o .:? "representations" .!= [] >>= traverse representation
  circulation <- o .:? "circulation" >>= traverse decodeCirculation
  roundsJson <- o .:? "rounds" >>= case _ of
    Just rs -> pure rs
    Nothing -> pure [ j ]
  rounds <- traverse (\(Tuple k rj) -> decodeRound (round + k) rj) (mapWithIndex Tuple roundsJson)
  pure
    { name
    , rounds
    , world:
        { round
        , seed: Seed seed
        , entities: toMap entities
        , resources: toMap resources
        , markets: toMap markets
        , transformations: toMap transformations
        , representations
        , numeraire: Id numeraire
        , transport: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) v) (Object.toUnfoldable transport :: Array _))
        , jurisdiction: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) (Id v)) (Object.toUnfoldable jurisdiction :: Array (Tuple String String)))
        , rules: Map.fromFoldable (mapWithIndex (\i r -> Tuple (Id ("rule-" <> show i)) r) rules)
        , scheduled: Map.empty
        , deliveries: []
        , factors: Map.empty
        , circulation: fromMaybe emptyCirculation circulation
        , events: []
        }
    }
  where
  traverseKeys :: forall a. (String -> Json -> Either JsonDecodeError a) -> Object Json -> Either JsonDecodeError (Object a)
  traverseKeys = traverseWithIndex
  emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }

decodeRound :: Int -> Json -> Either JsonDecodeError Round
decodeRound round j = do
  o <- decodeJson j
  proposals <- o .:? "proposals" .!= []
  inputs <- concat <$> traverse signed (groupByAuthor proposals)
  expect <- o .:? "expect" >>= maybe (pure noExpect) decodeExpect
  pure { inputs, expect }
  where
  groupByAuthor ps = map toArray (groupBy (\a b -> a.author == b.author) (sortWith _.author ps))
  signed group = traverse (\(Tuple i p) -> proposal round i p) (mapWithIndex Tuple group)
  noExpect = { accounts: Object.empty, reserves: Object.empty, deliveries: [], rejected: [], lapsed: [], factors: Object.empty, scheduled: 0, procedures: [] }

decodeExpect :: Json -> Either JsonDecodeError Expect
decodeExpect j = do
  o <- decodeJson j
  accounts <- o .:? "accounts" .!= Object.empty
  reserves <- o .:? "reserves" .!= Object.empty
  deliveries <- o .:? "deliveries" .!= []
  rejected <- o .:? "rejected" .!= []
  lapsed <- o .:? "lapsed" .!= []
  factors <- o .:? "factors" .!= Object.empty
  scheduled <- o .:? "scheduled" .!= 0
  procedures <- o .:? "procedures" .!= []
  pure { accounts, reserves, deliveries, rejected, lapsed, factors, scheduled, procedures }

resource :: String -> Json -> Either JsonDecodeError ResourceDefinition
resource id j = do
  o <- decodeJson j
  t <- o .: "transferability"
  transferability <- case t of
    "Free" -> Right Free
    "Restricted" -> Right Restricted
    "NonTransferable" -> Right NonTransferable
    _ -> Left (TypeMismatch "transferability")
  reservable <- o .:? "reservable" .!= false
  decay <- o .:? "decay"
  pure { id: Id id, unit: id, transferability, decay: map (\rate -> { rate }) decay, reservable }

costFn :: Json -> Either JsonDecodeError CostFunction
costFn j = do
  o <- decodeJson j
  base <- o .: "base"
  slope <- o .: "slope"
  lo <- o .: "lo"
  hi <- o .: "hi"
  pure { base, slope, domain: { lo, hi } }

toMap :: forall t a. Object a -> Map.Map (Id t) a
toMap = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v) <<< (Object.toUnfoldable :: _ -> Array _)

holdings :: forall t. Object Q -> Map.Map (Id t) Q
holdings = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v) <<< (Object.toUnfoldable :: _ -> Array _)

market :: String -> Json -> Either JsonDecodeError Market
market id j = do
  o <- decodeJson j
  base <- o .: "base"
  quote <- o .: "quote"
  cost <- o .: "cost" >>= costFn
  reserves <- o .: "reserves"
  access <- o .: "access"
  position <- o .:? "position" .!= Q.zero
  pure { id: Id id, base: Id base, quote: Id quote, cost, position, reserves: holdings reserves, access: map Id access }

transformation :: String -> Json -> Either JsonDecodeError Transformation
transformation id j = do
  o <- decodeJson j
  inputs <- o .:? "inputs" .!= Object.empty
  outputs <- o .:? "outputs" .!= Object.empty
  cost <- o .:? "cost" >>= traverse costFn
  capacity <- o .:? "capacity"
  payee <- o .:? "payee"
  delay <- o .:? "delay" .!= 0
  minimum <- o .:? "minimum" .!= Q.zero
  maximum <- o .:? "maximum"
  capacityUse <- o .:? "capacityUse" .!= Q.fromInt 1
  ownerSigns <- o .:? "ownerSigns" .!= false
  effects <- o .:? "effects" .!= [] >>= traverse \e -> do
    x <- decodeJson e
    c <- x .: "capacity"
    holder <- x .:? "holder"
    perUnit <- x .: "perUnit"
    rounds <- x .:? "rounds"
    pure { capacity: Id c, holder: map Id holder, perUnit, rounds }
  pure { id: Id id, inputs: holdings inputs, outputs: holdings outputs, cost, capacity: map Id capacity, payee: map Id payee, delay, minimum, maximum, capacityUse, ownerSigns, effects, authority: MayTransform (Id id) }

capability :: String -> Either JsonDecodeError Capability
capability s = case split (Pattern ":") s of
  [ "trade", m ] -> Right (MayTrade (Id m))
  [ "transfer", r ] -> Right (MayTransfer (Id r))
  [ "transform", t ] -> Right (MayTransform (Id t))
  [ "enact", "tariff" ] -> Right (MayEnact TariffRule)
  [ "enact", "accessBan" ] -> Right (MayEnact AccessBanRule)
  [ "enact", "withholding" ] -> Right (MayEnact WithholdingRule)
  _ -> Left (TypeMismatch ("capability " <> s))

decodeEntity :: String -> Json -> Either JsonDecodeError Entity
decodeEntity id j = do
  o <- decodeJson j
  accounts <- o .: "accounts"
  caps <- o .: "capabilities" >>= traverse capability
  capacities <- o .:? "capacities" .!= Object.empty
  kind <- o .:? "aggregate" >>= case _ of
    Nothing -> pure (Actor (Seat (Id id)))
    Just a -> do
      policy <- a .: "policy"
      members <- a .:? "members" .!= []
      procs <- a .:? "procedures" .!= Object.empty >>= traverseWithIndex (const procedure)
      pure (Aggregate { policy: Id policy, members: map Id members, procedures: toMap procs })
  pure { id: Id id, accounts: holdings accounts, capacities: holdings capacities, capabilities: caps, kind }

procedure :: Json -> Either JsonDecodeError CollectiveProcedure
procedure j = do
  o <- decodeJson j
  weights <- o .: "weights"
  residualWeight <- o .: "residualWeight"
  threshold <- o .: "threshold"
  authorizes <- o .: "authorizes" >>= traverse capability
  pure { weights: holdings weights, residualWeight, threshold, authorizes }

representation :: Json -> Either JsonDecodeError Representation
representation j = do
  o <- decodeJson j
  id <- o .: "id"
  actor <- o .: "actor"
  aggregate <- o .: "aggregate"
  domain <- o .: "domain"
  weight <- o .: "weight"
  reference <- o .: "reference"
  shrinkage <- o .: "shrinkage"
  bound <- o .:? "bound"
  noiseSpread <- o .:? "noiseSpread" .!= Q.zero
  pure
    { id: Id id
    , actor: Id actor
    , aggregate: Id aggregate
    , domain: Id domain
    , weight
    , estimator: { reference, shrinkage, deviationBound: map (\b -> { lo: Q.neg b, hi: b }) bound, noiseSpread }
    }

decodeCirculation :: Json -> Either JsonDecodeError Circulation
decodeCirculation j = do
  o <- decodeJson j
  production <- o .:? "production" .!= [] >>= traverse \x -> do
    p <- decodeJson x
    e <- p .: "entity"
    r <- p .: "resource"
    c <- p .: "capacity"
    pure { entity: Id e, resource: Id r, capacity: Id c }
  flows <- o .:? "flows" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    from <- f .: "from"
    to <- f .: "to"
    r <- f .: "resource"
    amount <- f .: "amount"
    pure { from: Id from, to: Id to, resource: Id r, amount }
  shares <- o .:? "shares" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    from <- f .: "from"
    to <- f .: "to"
    share <- f .: "share"
    src <- f .: "source"
    source <- case split (Pattern ":") src of
      [ "sales", r ] -> Right (SalesOf (Id r))
      [ "payee" ] -> Right PayeeReceipts
      _ -> Left (TypeMismatch "share source")
    pure { from: Id from, to: Id to, share, source }
  requirements <- o .:? "requirements" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    e <- f .: "entity"
    r <- f .: "resource"
    amount <- f .:? "amount"
    penalty <- f .:? "penalty" .!= Q.zero
    pure { entity: Id e, resource: Id r, amount, penalty }
  pure { production, flows, shares, requirements }

rule :: Json -> Either JsonDecodeError Rule
rule j = do
  o <- decodeJson j
  case Object.lookup "accessBan" o, Object.lookup "tariff" o of
    Just b, _ -> do
      x <- decodeJson b
      jurisdiction <- x .: "jurisdiction"
      target <- x .: "target"
      pure (AccessBan { jurisdiction: Id jurisdiction, target: Id target })
    _, Just t -> do
      x <- decodeJson t
      jurisdiction <- x .: "jurisdiction"
      origin <- x .: "origin"
      rate <- x .: "rate"
      pure (Tariff { jurisdiction: Id jurisdiction, origin: Id origin, rate })
    _, _ -> Left (TypeMismatch "rule")

intent :: Json -> Either JsonDecodeError Intent
intent j = do
  o <- decodeJson j
  case Object.lookup "trade" o, Object.lookup "transfer" o, Object.lookup "transform" o of
    Just t, _, _ -> do
      x <- decodeJson t
      m <- x .: "market"
      quantity <- x .: "quantity"
      limitPrice <- x .:? "limit"
      pure (Trade { market: Id m, quantity, limitPrice })
    _, Just t, _ -> do
      x <- decodeJson t
      from <- x .: "from"
      to <- x .: "to"
      r <- x .: "resource"
      quantity <- x .: "quantity"
      pure (Transfer { from: Id from, to: Id to, resource: Id r, quantity })
    _, _, Just t -> do
      x <- decodeJson t
      by <- x .: "by"
      tr <- x .: "transformation"
      quantity <- x .: "quantity"
      pure (Transform { by: Id by, transformation: Id tr, quantity })
    _, _, _ -> case Object.lookup "enact" o of
      Just r -> ChangeRule <<< Enact <$> rule r
      Nothing -> Left (TypeMismatch "intent")

proposal :: Int -> Int -> { author :: String, intents :: Array Json, signers :: Maybe (Array String), signatures :: Maybe (Array String), procedure :: Maybe String } -> Either JsonDecodeError SignedProposal
proposal round i p = do
  intents <- traverse intent p.intents
  let author = Id p.author
  pure
    { proposal: { id: proposalId round author i, author, intents, signers: map Id (unwrapArr p.signers), procedure: map Id p.procedure }
    , signatures: map Id (unwrapArr p.signatures)
    }
  where
  unwrapArr = case _ of
    Just xs -> xs
    Nothing -> []

