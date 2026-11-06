-- | Decodes the compact fixture JSON in packages/scenarios/fixtures and runs it through
-- | the engine. One generic golden runner for every engine-level fixture.
module Test.Core.Fixture (Fixture, Expect, load, run, Outcome) where

import Prelude

import Core.Draw (Seed(..))
import Core.Engine (EngineError, resolveRound)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (CostFunction, Market, ResourceDefinition, Transformation, Capability(..), Entity, EntityKind(..), Controller(..), Id(..), Intent(..), Rule(..), RuleKind(..), SignedProposal, Transferability(..), proposalId)
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, printJsonDecodeError, (.:), (.:?), (.!=))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapWithIndex, groupBy, sortWith, concat)
import Data.Array.NonEmpty (toArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Expect =
  { accounts :: Object (Object Q)
  , reserves :: Object (Object Q)
  , deliveries :: Array { entity :: String, resource :: String, quantity :: Q }
  , rejected :: Array String
  , lapsed :: Array String
  }

type Fixture = { name :: String, world :: WorldState, inputs :: Array SignedProposal, expect :: Expect }

type Outcome = Either EngineError RoundResult

load :: String -> Effect (Either String Fixture)
load name = do
  text <- readTextFile UTF8 ("packages/scenarios/fixtures/" <> name <> ".json")
  pure (jsonParser text >>= (lmap printJsonDecodeError <<< decodeFixture))

run :: Fixture -> Outcome
run f = resolveRound f.world f.inputs

decodeFixture :: Json -> Either JsonDecodeError Fixture
decodeFixture j = do
  o <- decodeJson j
  name <- o .: "name"
  round <- o .: "round"
  numeraire <- o .: "numeraire"
  resources <- o .: "resources" >>= traverseKeys resource
  markets <- o .: "markets" >>= traverseKeys market
  transformations <- o .: "transformations" >>= traverseKeys transformation
  transport <- o .: "transport" >>= traverse (pure <<< Id)
  entities <- o .: "entities" >>= traverseKeys entity
  rules <- o .: "rules" >>= traverse rule
  proposals <- o .: "proposals"
  inputs <- concat <$> traverse (signed round) (groupByAuthor proposals)
  expect <- o .: "expect" >>= decodeExpect
  pure
    { name
    , expect
    , inputs
    , world:
        { round
        , seed: Seed 26021
        , entities: toMap entities
        , resources: toMap resources
        , markets: toMap markets
        , transformations: toMap transformations
        , representations: []
        , numeraire: Id numeraire
        , transport: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) v) (Object.toUnfoldable transport :: Array _))
        , jurisdiction: Map.empty
        , rules: Map.fromFoldable (mapWithIndex (\i r -> Tuple (Id ("rule-" <> show i)) r) rules)
        , scheduled: Map.empty
        , deliveries: []
        , factors: Map.empty
        , events: []
        }
    }
  where
  toMap :: forall t a. Object a -> Map.Map (Id t) a
  toMap = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v) <<< (Object.toUnfoldable :: _ -> Array _)

  traverseKeys :: forall a. (String -> Json -> Either JsonDecodeError a) -> Object Json -> Either JsonDecodeError (Object a)
  traverseKeys = traverseWithIndex
  groupByAuthor ps = map toArray (groupBy (\a b -> a.author == b.author) (sortWith _.author ps))
  signed round group = traverse (\(Tuple i p) -> proposal round i p) (mapWithIndex Tuple group)

decodeExpect :: Json -> Either JsonDecodeError Expect
decodeExpect j = do
  o <- decodeJson j
  accounts <- o .:? "accounts" .!= Object.empty
  reserves <- o .:? "reserves" .!= Object.empty
  deliveries <- o .:? "deliveries" .!= []
  rejected <- o .:? "rejected" .!= []
  lapsed <- o .:? "lapsed" .!= []
  pure { accounts, reserves, deliveries, rejected, lapsed }

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
  pure { id: Id id, unit: id, transferability, decay: Nothing, reservable }

costFn :: Json -> Either JsonDecodeError CostFunction
costFn j = do
  o <- decodeJson j
  base <- o .: "base"
  slope <- o .: "slope"
  lo <- o .: "lo"
  hi <- o .: "hi"
  pure { base, slope, domain: { lo, hi } }

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
  pure { id: Id id, base: Id base, quote: Id quote, cost, position: Q.zero, reserves: holdings reserves, access: map Id access }

transformation :: String -> Json -> Either JsonDecodeError Transformation
transformation id j = do
  o <- decodeJson j
  inputs <- o .: "inputs"
  outputs <- o .: "outputs"
  cost <- o .:? "cost" >>= traverse costFn
  capacity <- o .: "capacity"
  payee <- o .:? "payee"
  delay <- o .: "delay"
  pure { id: Id id, inputs: holdings inputs, outputs: holdings outputs, cost, capacity: Id capacity, payee: map Id payee, delay, authority: MayTransform (Id id) }

capability :: String -> Either JsonDecodeError Capability
capability s = case split (Pattern ":") s of
  [ "trade", m ] -> Right (MayTrade (Id m))
  [ "transfer", r ] -> Right (MayTransfer (Id r))
  [ "transform", t ] -> Right (MayTransform (Id t))
  [ "enact", "tariff" ] -> Right (MayEnact TariffRule)
  [ "enact", "accessBan" ] -> Right (MayEnact AccessBanRule)
  [ "enact", "withholding" ] -> Right (MayEnact WithholdingRule)
  _ -> Left (TypeMismatch ("capability " <> s))

entity :: String -> Json -> Either JsonDecodeError Entity
entity id j = do
  o <- decodeJson j
  accounts <- o .: "accounts"
  caps <- o .: "capabilities" >>= traverse capability
  capacities <- o .:? "capacities" .!= Object.empty
  pure { id: Id id, accounts: holdings accounts, reserved: Map.empty, capacities: holdings capacities, capabilities: caps, kind: Actor (Seat (Id id)) }

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
    _, _, _ -> Left (TypeMismatch "intent")

proposal :: Int -> Int -> { author :: String, intents :: Array Json, signers :: Maybe (Array String), signatures :: Maybe (Array String) } -> Either JsonDecodeError SignedProposal
proposal round i p = do
  intents <- traverse intent p.intents
  let author = Id p.author
  pure
    { proposal: { id: proposalId round author i, author, intents, signers: map Id (unwrapArr p.signers), procedure: Nothing }
    , signatures: map Id (unwrapArr p.signatures)
    }
  where
  unwrapArr = case _ of
    Just xs -> xs
    Nothing -> []

