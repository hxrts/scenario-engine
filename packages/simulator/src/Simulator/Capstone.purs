-- | The capstone run file (`packages/scenarios/capstone/run.json`, authored in
-- | `Scenarios.Capstone`): its seat strategies, counterfactual seats, round expectations and
-- | objective outcomes, decoded through the Policy and Board decoders. No Proposal or override
-- | is built here.
module Simulator.Capstone
  ( Capstone
  , ObjectiveExpect
  , RoundExpect
  , decodeCapstone
  ) where

import Prelude

import Board.Decode.Common (decodeError)
import Board.Decode.Rounds (Expect, decodeExpect)
import Board.Gameboard.Schema (Outcome(..))
import Core.Draw (Seed(..))
import Core.Types (EntityId, Id(..))
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Core.Strategy (Strategy)
import Policy.Strategy (decodeStrategy)

type RoundExpect = { round :: Int, expect :: Expect }

-- | An objective the run must end with, by its Entity and its outcome.
type ObjectiveExpect = { entity :: EntityId, outcome :: Outcome }

type Capstone =
  { rounds :: Int
  , playerSeed :: Seed
  , seats :: Map EntityId Strategy
  , counterfactual :: Map EntityId Strategy -- replaces the named seats in the counterfactual run
  , expectations :: Array RoundExpect
  , objectives :: Array ObjectiveExpect
  , counterfactualObjectives :: Array ObjectiveExpect
  }

-- | The run file's JSON, decoded. A malformed part is an error that names it.
decodeCapstone :: Json -> Either String Capstone
decodeCapstone j = do
  o <- note "capstone: the run file must be an object" (toObject j)
  rounds <- field o "rounds" int
  playerSeed <- Seed <$> field o "playerSeed" int
  seats <- field o "seats" strategies
  counterfactual <- field o "counterfactual" strategies
  expectations <- field o "expectations" (\x -> arrayOf roundExpect x)
  objectives <- field o "objectives" (\x -> arrayOf objective x)
  counterfactualObjectives <- field o "counterfactualObjectives" (\x -> arrayOf objective x)
  pure { rounds, playerSeed, seats, counterfactual, expectations, objectives, counterfactualObjectives }

field :: forall a. Object Json -> String -> (Json -> Either String a) -> Either String a
field o k decode = case Object.lookup k o of
  Just v -> decode v
  Nothing -> Left ("capstone: missing " <> k)

int :: Json -> Either String Int
int j = decodeError (decodeJson j)

strategies :: Json -> Either String (Map EntityId Strategy)
strategies j = do
  o <- note "capstone: seats must be an object of strategies" (toObject j)
  let entries = Object.toUnfoldable o :: Array (Tuple String Json)
  kvs <- traverse (\(Tuple e p) -> Tuple (Id e) <$> decodeError (decodeStrategy p)) entries
  pure (Map.fromFoldable kvs)

roundExpect :: Json -> Either String RoundExpect
roundExpect x = do
  o <- note "capstone: an expectation must be an object" (toObject x)
  round <- field o "round" int
  expect <- field o "expect" (\v -> decodeError (decodeExpect v))
  pure { round, expect }

objective :: Json -> Either String ObjectiveExpect
objective x = do
  o <- note "capstone: an objective must be an object" (toObject x)
  entity <- field o "entity" (\v -> note "capstone: objective entity must be a string" (toString v))
  outcome <- field o "outcome" (\v -> note "capstone: objective outcome must be a string" (toString v) >>= outcomeOf)
  pure { entity: Id entity, outcome }

-- | The DSL's spelling of an outcome: "met", "notMet" or "notEvaluated".
outcomeOf :: String -> Either String Outcome
outcomeOf = case _ of
  "met" -> Right Met
  "notMet" -> Right NotMet
  "notEvaluated" -> Right NotEvaluated
  other -> Left ("capstone: unknown objective outcome " <> other)

arrayOf :: forall a. (Json -> Either String a) -> Json -> Either String (Array a)
arrayOf f j = do
  xs <- note "capstone: expected an array" (toArray j)
  traverse f xs
