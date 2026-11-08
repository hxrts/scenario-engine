-- | The shared path for a literal setting (fixture): it plays through `simulate` with its Proposals
-- | and signatures as Script bindings, and the records are checked against the setting's rounds. Every
-- | suite that checks a fixture (the golden tests, the views, experiments and simulator suites) calls this.
module Testkit.Fixture (fixture, checkLiteral) where

import Prelude

import Adapters.Filesystem (fileOps)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Settings.Load (loadLiteral)
import Settings.Layout (fixturePath)
import Settings.Tree (readRootFile)
import Test.Spec.Assertions (fail, shouldEqual)

import Checks.Round (Failure(..), Field(..), checkRound)
import Checks.Run (checkRun)
import Core.Types (EntityId, ProposalId, SignedProposal)
import Core.World (RoundResult)
import Data.Array (concat, index, length, mapWithIndex, sort, sortWith, head)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import Settings.Literal (literalGame)
import Schema.Setting.Fixture (Literal, Round)
import Handlers.Simulate (SimRun, simulate)

-- | Every failure of a literal setting played through the simulator: an invariant broken in any round,
-- | an expectation the records do not meet, a round whose Proposals the engine did not receive as
-- | written, or a round the engine stopped at. An empty list is a passing fixture.
checkLiteral :: Literal -> Array Failure
checkLiteral l = case literalGame l of
  Left e -> [ Stopped first (show e) ]
  Right g -> case simulate g.world.seed g of
    Left e -> [ Stopped first (show e) ]
    Right sim -> concat
      [ checkRun l.world sim.records
      , concat (mapWithIndex (\k r -> maybe [] (roundChecks r) (index sim.records k)) l.rounds)
      , stopped sim
      ]
  where
  first = fromMaybe 1 (map _.round (head l.rounds))

  stopped :: SimRun -> Array Failure
  stopped sim = case sim.final of
    Left e -> [ Stopped (first + length sim.records) (show e) ]
    Right _ -> []

-- | One round's expectation against its record, and the Proposals the record holds against those scripted.
roundChecks :: Round -> RoundResult -> Array Failure
roundChecks r rr = checkRound r.expect rr <> inputsCheck
  where
  inputsCheck =
    if summary rr.record.inputs == summary r.inputs then []
    else [ Mismatch r.round Inputs (show (summary rr.record.inputs)) (show (summary r.inputs)) ]

-- | The Proposals of a round by id, each with the sorted Entities that signed it.
summary :: Array SignedProposal -> Array (Tuple ProposalId (Array EntityId))
summary xs = sortWith (show <<< fst) (map (\s -> Tuple s.proposal.id (sort s.signatures)) xs)

-- | A named fixture file, played through the shared checker: every expectation holds.
fixture :: String -> Aff Unit
fixture name = liftEffect (map (_ >>= loadLiteral name) (readRootFile fileOps (fixturePath name))) >>= case _ of
  Left e -> fail (show e)
  Right f -> checkLiteral f `shouldEqual` []
