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
  , noRound
  , accessBanRule
  , tariffRule
  , withholdingRule
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (Id(..), IntentF, RuleF(..))
import Schema.Premise.Fixture (ExpectDoc, ProposalDoc, ProposalRefDoc, RoundDoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | An expectation that checks nothing: every key is absent.
noExpect :: ExpectDoc
noExpect = { accounts: Nothing, reserves: Nothing, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, factors: Nothing, scheduled: Nothing, procedures: Nothing }

-- | An unsigned Proposal with no signers, signatures or procedure. Those keys are given by record update.
proposal :: { author :: String, intents :: Array (IntentF Q) } -> ProposalDoc
proposal p = { author: p.author, intents: p.intents, signers: Nothing, signatures: Nothing, procedure: Nothing }

-- | A Proposal named in an expectation: its author's first Proposal in the round.
ref :: String -> ProposalRefDoc
ref author = Left author

-- | The Proposal at a position among its author's Proposals in the round.
refAt :: String -> Int -> ProposalRefDoc
refAt author index = Right { author, index: Just index }

-- | A round that receives nothing and checks nothing. A round's keys are given by record update.
noRound :: RoundDoc
noRound = { proposals: Nothing, expect: Nothing }

-- | Rules in force from the start of a world.
accessBanRule :: { jurisdiction :: String, target :: String } -> RuleF Q
accessBanRule r = AccessBan { jurisdiction: Id r.jurisdiction, target: Id r.target }

tariffRule :: { jurisdiction :: String, origin :: String, rate :: Q } -> RuleF Q
tariffRule r = Tariff { jurisdiction: Id r.jurisdiction, origin: Id r.origin, rate: r.rate }

withholdingRule :: { capacity :: String, units :: Q } -> RuleF Q
withholdingRule r = Withholding { capacity: Id r.capacity, units: r.units }
