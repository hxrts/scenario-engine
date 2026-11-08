-- | Builders for a golden fixture's rounds and expectations (docs/05_test_instance.md §21–§27):
-- | Proposals and their references, deliveries, procedure outcomes, the `expect` checks, and the
-- | Rules in force from the start of a world. Every expected value is a literal; a key the check
-- | does not make is `Nothing`, so it is not written. An `expect` is `noExpect` with the keys it
-- | checks given by record update: `noExpect { accounts = Just (keyed [ ... ]) }`.
module Schema.Build.Fixture
  ( noExpect
  , proposal
  , ref
  , refAt
  , procedureDoc
  , noRound
  ) where

import Core.Num (Q)
import Core.Types (Id(..), Intent)
import Schema.Premise.Fixture (ExpectDoc, ProcedureDoc, ProposalDoc, ProposalRefDoc, RoundDoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | An expectation that checks nothing: every key is absent.
noExpect :: ExpectDoc
noExpect = { accounts: Nothing, reserves: Nothing, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, factors: Nothing, scheduled: Nothing, procedures: Nothing }

-- | An unsigned Proposal with no signers, signatures or procedure. Those keys are given by record update.
proposal :: { author :: String, intents :: Array Intent } -> ProposalDoc
proposal p = { author: Id p.author, intents: p.intents, signers: Nothing, signatures: Nothing, procedure: Nothing }

-- | A Proposal named in an expectation: its author's first Proposal in the round.
ref :: String -> ProposalRefDoc
ref author = Left (Id author)

-- | The Proposal at a position among its author's Proposals in the round.
refAt :: String -> Int -> ProposalRefDoc
refAt author index = Right { author: Id author, index: Just index }

-- | A procedure's expected outcome: the Proposal it names, the weight behind it, and whether it ran.
procedureDoc :: ProposalRefDoc -> Q -> Boolean -> ProcedureDoc
procedureDoc named support executed = { proposal: named, support, executed }

-- | A round that receives nothing and checks nothing. A round's keys are given by record update.
noRound :: RoundDoc
noRound = { proposals: Nothing, expect: Nothing }

-- | Rules in force from the start of a world.
