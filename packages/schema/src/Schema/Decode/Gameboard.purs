-- | The premise format's Gameboard and Instantiation Configuration decoders (docs/03_gameboard.md).
-- | The Gameboard's Policy catalog is decoded here too, in the same pass (`decodeStrategy`), so
-- | a Gameboard value is complete when it leaves this module.
module Schema.Decode.Gameboard
  ( decodeGameboard
  , decodeConfig
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (Id(..))
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Schema.Decode.Common (decodeEstimator, toMap)
import Schema.Premise.Strategy (decodeStrategy)
import Schema.Decode.World (decodeWorld)
import Schema.Gameboard (ActorDefinition, Fallback(..), Gameboard, InstantiationConfig, Objective, Criterion, MetricRef, Measure(..), Span(..), Bound(..), Scheme)
import Schema.Tags (argOf, capacityKind, cumulativeKind, coverageKind, everyRoundKind, finalKind, holdingKind, isBare, omissionKind, payeeReceiptsKind, policyKind, shortfallKind, shortfallRoundsKind, volumeKind)

decodeGameboard :: Json -> Either JsonDecodeError Gameboard
decodeGameboard j = do
  o <- decodeJson j
  id <- o .: "id"
  version <- o .: "version"
  { world } <- decodeWorld j
  defs <- o .: "entities" >>= traverseWithIndex \k v -> do
    x <- decodeJson v
    maybeDef <- x .:? "definition"
    traverse (definition k) maybeDef
  schemes <- o .:? "schemes" .!= [] >>= traverse scheme
  policies <- o .:? "policies" .!= Object.empty >>= traverseWithIndex policy
  parameters <- o .:? "parameters" .!= Object.empty
  horizon <- o .: "horizon"
  rounds <- o .: "roundRange" >>= roundRange
  label <- o .:? "label"
  pure
    { id
    , version
    , world
    , actors: Map.mapMaybe identity (toMap defs)
    , schemes
    , policies: toMap policies
    , parameters
    , horizon
    , rounds
    , label
    }
  where
  policy name j' = lmap (Named ("policy " <> name)) (decodeStrategy j')
  definition _ j' = do
    d <- decodeJson j'
    required <- d .:? "required" .!= false
    seat <- d .:? "seat" .!= true
    npc <- d .:? "npc" .!= []
    fallback <- d .:? "fallback" >>= traverse decodeFallback
    objective <- d .:? "objective" >>= traverse decodeObjective
    description <- d .:? "description" .!= ""
    dependency <- d .:? "dependency" .!= ""
    pure { required, seat, npc: map Id npc, fallback, objective, description, dependency }
  scheme j' = do
    s <- decodeJson j'
    id <- s .: "id"
    eligible <- s .: "eligibleActors"
    target <- s .: "target"
    domain <- s .: "domain"
    estimator <- s .: "estimator" >>= decodeEstimator
    pure
      { id
      , eligibleActors: map Id eligible
      , target: Id target
      , domain: Id domain
      , estimator
      }

-- | `[min, max]`: the supported round counts, with 1 <= min <= max.
roundRange :: Json -> Either JsonDecodeError { min :: Int, max :: Int }
roundRange j = do
  xs <- decodeJson j
  case xs of
    [ a, b ] | a >= 1 && a <= b -> Right { min: a, max: b }
    _ -> Left (Named "roundRange" (UnexpectedValue j))

-- | A fallback tag: `coverage:<aggregate>`, `policy:<policy>`, or `omission`.
decodeFallback :: Json -> Either JsonDecodeError Fallback
decodeFallback j = do
  s <- decodeJson j
  if isBare omissionKind s then Right PermittedOmission
  else case argOf coverageKind s, argOf policyKind s of
    Just a, _ -> Right (ImplicitCoverage (Id a))
    _, Just a -> Right (InstitutionalPolicy (Id a))
    _, _ -> Left (Named "fallback" (UnexpectedValue j))

decodeObjective :: Json -> Either JsonDecodeError Objective
decodeObjective j = do
  o <- decodeJson j
  description <- o .: "description"
  criteria <- o .: "criteria" >>= traverse \c -> do
    x <- decodeJson c
    metric <- x .: "metric" >>= metricRef
    over <- x .:? "over" .!= finalKind >>= span
    atLeast <- x .: "atLeast" >>= \b -> case decodeJson b of
      Right q -> Right (Fixed q)
      Left _ -> Of <$> metricRef b
    pure { metric, over, atLeast }
  pure { description, criteria }
  where
  span s
    | s == finalKind = Right Final
    | s == everyRoundKind = Right EveryRound
    | s == cumulativeKind = Right Cumulative
    | otherwise = Left (Named "span" (UnexpectedValue (fromString s)))
  metricRef j' = do
    m <- decodeJson j'
    entities <- m .: "entities"
    negated <- m .:? "negated" .!= false
    s <- m .: "measure"
    measure <- measureOf s
    pure { entities: map Id entities, measure, negated }
  measureOf s
    | Just r <- argOf holdingKind s = Right (HoldingOf (Id r))
    | Just c <- argOf capacityKind s = Right (CapacityAmount (Id c))
    | Just r <- argOf shortfallKind s = Right (ShortfallOf (Id r))
    | Just r <- argOf shortfallRoundsKind s = Right (ShortfallRounds (Id r))
    | isBare payeeReceiptsKind s = Right PayeeReceipts
    | Just t <- argOf volumeKind s = Right (VolumeOf (Id t))
    | otherwise = Left (Named "measure" (UnexpectedValue (fromString s)))

decodeConfig :: Json -> Either JsonDecodeError InstantiationConfig
decodeConfig j = do
  o <- decodeJson j
  seed <- o .: "seed"
  rounds <- o .: "rounds"
  seats <- o .:? "seats" .!= Object.empty
  npcs <- o .:? "npcs" .!= Object.empty
  fallbacks <- o .:? "fallbacks" .!= Object.empty >>= traverse decodeFallback
  reps <- o .:? "representations" .!= [] >>= traverse \r -> do
    x <- decodeJson r
    s <- x .: "scheme"
    actor <- x .: "actor"
    weight <- x .:? "weight" .!= Q.fromInt 1
    pure { scheme: s, actor: Id actor, weight }
  pure { seed: Seed seed, rounds, seats: toMap seats, npcs: map Id (toMap npcs), fallbacks: toMap fallbacks, representations: reps }
