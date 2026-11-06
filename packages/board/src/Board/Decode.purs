-- | JSON decoding for scenario data. The schema mirrors the core types one to one; this
-- | module adds decoding only, never a second model.
-- |
-- | Tagged unions (a rule, an intent, a fixture's `expect` entry) are objects with exactly
-- | one key. Several keys, or an unknown key, is a decode error that names the kind decoded.
module Board.Decode
  ( readJson
  , readJsonWith
  , decodeWorld
  , decodeFixture
  , decodeEntity
  , decodeRound
  , decodeExpect
  , capability
  , intent
  , rule
  , holdings
  , toMap
  , Round
  , Expect
  , Delivery
  , ProposalRef
  , ProcedureExpect
  ) where

import Prelude

import Board.Tags (accessBanKind, enactKind, payeeKind, salesKind, splitTag, tariffKind, tradeKind, transferKind, transformKind, withholdingKind)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (CollectiveProcedure, Representation, CostFunction, Market, ResourceDefinition, Transformation, Capability(..), Entity, EntityKind(..), Controller(..), Id(..), Intent(..), Rule(..), RuleChange(..), RuleKind(..), SignedProposal, Transferability(..), proposalId)
import Core.World (Circulation, ShareSource(..), WorldState)
import Data.Argonaut.Core (Json, fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, fromJsonString, parseJson, printJsonDecodeError, (.:), (.:?), (.!=))
import Data.Bifunctor (lmap)
import Data.Array (mapWithIndex, groupBy, sortWith, concat)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | A proposal named in an expectation: its author and its position among that author's
-- | Proposals in the round (0 for the first). Written as the author's name or as
-- | `{ "author": ..., "index": ... }`.
type ProposalRef = { author :: String, index :: Int }

type Delivery = { entity :: String, resource :: String, quantity :: Q }

type ProcedureExpect = { proposal :: ProposalRef, support :: Q, executed :: Boolean }

-- | The checks a round makes. Every field is optional: only the fields given are checked, so
-- | `deliveries: []` asserts that nothing was delivered, and an absent field asserts nothing.
type Expect =
  { accounts :: Maybe (Object (Object Q))
  , reserves :: Maybe (Object (Object Q))
  , deliveries :: Maybe (Array Delivery)
  , rejected :: Maybe (Array ProposalRef)
  , lapsed :: Maybe (Array ProposalRef)
  , factors :: Maybe (Object (Object Q))
  , scheduled :: Maybe Int
  , procedures :: Maybe (Array ProcedureExpect)
  }

type Round = { inputs :: Array SignedProposal, expect :: Expect }

-- | The World State a document describes, its name, and its rounds.
type Document = { name :: String, world :: WorldState, rounds :: Array Round }

-- | Parse and decode JSON text, with a readable error.
readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap printJsonDecodeError <<< fromJsonString

-- | Parse JSON text and decode it with a custom decoder.
readJsonWith :: forall a. (Json -> Either JsonDecodeError a) -> String -> Either String a
readJsonWith decode = lmap printJsonDecodeError <<< (parseJson >=> decode)

-- | An error for a value whose kind is not one the schema defines.
unknownKind :: String -> String -> JsonDecodeError
unknownKind kind value = Named kind (UnexpectedValue (fromString value))

-- | A tagged object `{ "<kind>": body }`: exactly one key names the variant.
oneKey :: String -> Json -> Either JsonDecodeError (Tuple String Json)
oneKey kind j = do
  o <- decodeJson j
  case Object.toUnfoldable (o :: Object Json) of
    [ t ] -> Right t
    _ -> Left (Named kind (UnexpectedValue j))

-- | A Gameboard document: its World State (the seed is set by the instantiation) and rounds.
decodeWorld :: Json -> Either JsonDecodeError Document
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
        -- a Gameboard has no seed: the instantiation sets it (`decodeFixture` reads it)
        , seed: Seed 0
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

-- | A fixture: a Gameboard document whose World State carries its own seed.
decodeFixture :: Json -> Either JsonDecodeError Document
decodeFixture j = do
  o <- decodeJson j
  seed <- o .: "seed"
  doc <- decodeWorld j
  pure doc { world = doc.world { seed = Seed seed } }

decodeRound :: Int -> Json -> Either JsonDecodeError Round
decodeRound round j = do
  o <- decodeJson j
  proposals <- o .:? "proposals" .!= []
  inputs <- concat <$> traverse signed (groupByAuthor proposals)
  expect <- o .:? "expect" >>= traverse decodeExpect
  pure { inputs, expect: fromMaybe noExpect expect }
  where
  groupByAuthor ps = map toArray (groupBy (\a b -> a.author == b.author) (sortWith _.author ps))
  signed group = traverse (\(Tuple i p) -> proposal round i p) (mapWithIndex Tuple group)
  noExpect = { accounts: Nothing, reserves: Nothing, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, factors: Nothing, scheduled: Nothing, procedures: Nothing }

decodeExpect :: Json -> Either JsonDecodeError Expect
decodeExpect j = do
  o <- decodeJson j
  accounts <- o .:? "accounts"
  reserves <- o .:? "reserves"
  deliveries <- o .:? "deliveries" >>= traverse (traverse delivery)
  rejected <- o .:? "rejected" >>= traverse (traverse proposalRef)
  lapsed <- o .:? "lapsed" >>= traverse (traverse proposalRef)
  factors <- o .:? "factors"
  scheduled <- o .:? "scheduled"
  procedures <- o .:? "procedures" >>= traverse (traverse procedureExpect)
  pure { accounts, reserves, deliveries, rejected, lapsed, factors, scheduled, procedures }
  where
  delivery x = do
    d <- decodeJson x
    entity <- d .: "entity"
    res <- d .: "resource"
    quantity <- d .: "quantity"
    pure { entity, resource: res, quantity }
  procedureExpect x = do
    ref <- proposalRef x
    d <- decodeJson x
    support <- d .: "support"
    executed <- d .: "executed"
    pure { proposal: ref, support, executed }

-- | `"US"` is the first Proposal by US; `{ "author": "US", "index": 1 }` names the second.
proposalRef :: Json -> Either JsonDecodeError ProposalRef
proposalRef j = case toString j of
  Just author -> Right { author, index: 0 }
  Nothing -> do
    o <- decodeJson j
    author <- o .: "author"
    index <- o .:? "index" .!= 0
    pure { author, index }

resource :: String -> Json -> Either JsonDecodeError ResourceDefinition
resource id j = do
  o <- decodeJson j
  t <- o .: "transferability"
  transferability <- case t of
    "Free" -> Right Free
    "Restricted" -> Right Restricted
    "NonTransferable" -> Right NonTransferable
    _ -> Left (unknownKind "transferability" t)
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
  reserves <- o .:? "reserves" .!= Object.empty
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
  pure { id: Id id, inputs: holdings inputs, outputs: holdings outputs, reserves: holdings reserves, cost, capacity: map Id capacity, payee: map Id payee, delay, minimum, maximum, capacityUse, ownerSigns, effects }

-- | A capability tag: `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`,
-- | or `enact:<tariff|accessBan|withholding>`.
capability :: String -> Either JsonDecodeError Capability
capability s = case splitTag s of
  { kind, arg: Just a } | kind == tradeKind -> Right (MayTrade (Id a))
  { kind, arg: Just a } | kind == transferKind -> Right (MayTransfer (Id a))
  { kind, arg: Just a } | kind == transformKind -> Right (MayTransform (Id a))
  { kind, arg: Just a } | kind == enactKind -> enactment a
  _ -> Left (unknownKind "capability" s)
  where
  enactment a
    | a == tariffKind = Right (MayEnact TariffRule)
    | a == accessBanKind = Right (MayEnact AccessBanRule)
    | a == withholdingKind = Right (MayEnact WithholdingRule)
    | otherwise = Left (unknownKind "capability" s)

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
    source <- case splitTag src of
      { kind, arg: Just r } | kind == salesKind -> Right (SalesOf (Id r))
      { kind, arg: Nothing } | kind == payeeKind -> Right PayeeReceipts
      _ -> Left (unknownKind "share source" src)
    pure { from: Id from, to: Id to, share, source }
  requirements <- o .:? "requirements" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    e <- f .: "entity"
    r <- f .: "resource"
    amount <- f .:? "amount"
    penalty <- f .:? "penalty" .!= Q.zero
    pure { entity: Id e, resource: Id r, amount, penalty }
  pure { production, flows, shares, requirements }

-- | A Rule: `{ "accessBan": ... }`, `{ "tariff": ... }`, or `{ "withholding": ... }`.
rule :: Json -> Either JsonDecodeError Rule
rule j = do
  Tuple kind body <- oneKey "rule" j
  x <- decodeJson body
  if kind == accessBanKind then do
    jurisdiction <- x .: "jurisdiction"
    target <- x .: "target"
    pure (AccessBan { jurisdiction: Id jurisdiction, target: Id target })
  else if kind == tariffKind then do
    jurisdiction <- x .: "jurisdiction"
    origin <- x .: "origin"
    rate <- x .: "rate"
    pure (Tariff { jurisdiction: Id jurisdiction, origin: Id origin, rate })
  else if kind == withholdingKind then do
    capacity <- x .: "capacity"
    units <- x .: "units"
    pure (Withholding { capacity: Id capacity, units })
  else Left (unknownKind "rule" kind)

-- | An Intent: exactly one of `trade`, `transfer`, `transform`, or `enact` (a Rule).
intent :: Json -> Either JsonDecodeError Intent
intent j = do
  Tuple kind body <- oneKey "intent" j
  if kind == tradeKind then do
    x <- decodeJson body
    m <- x .: "market"
    quantity <- x .: "quantity"
    limitPrice <- x .:? "limit"
    pure (Trade { market: Id m, quantity, limitPrice })
  else if kind == transferKind then do
    x <- decodeJson body
    from <- x .: "from"
    to <- x .: "to"
    r <- x .: "resource"
    quantity <- x .: "quantity"
    pure (Transfer { from: Id from, to: Id to, resource: Id r, quantity })
  else if kind == transformKind then do
    x <- decodeJson body
    by <- x .: "by"
    tr <- x .: "transformation"
    quantity <- x .: "quantity"
    pure (Transform { by: Id by, transformation: Id tr, quantity })
  else if kind == enactKind then ChangeRule <<< Enact <$> rule body
  else Left (unknownKind "intent" kind)

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
