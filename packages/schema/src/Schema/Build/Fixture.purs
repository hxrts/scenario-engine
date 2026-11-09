-- | Builders for a golden fixture's rounds and expectations (docs/05_test_instance.md §21–§27):
-- | Proposals and their references, procedure outcomes, and the `expect` checks. Every expected value
-- | is a literal. An `expect` is `noExpect` with the keys it checks given by record update:
-- | `noExpect { accounts = balances [ Tuple N.us (keyed [ N.money := 260 ]) ] }`.
module Schema.Build.Fixture
  ( module Exports
  , balances
  , proposal
  , ref
  , procedureOutcome
  , noRound
  ) where

import Prelude (map, (<<<))

import Core.Num (Q)
import Laws.Types (EntityTag, Id, Intent, SignedProposal)
import Schema.Build.Named (class Named, named)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Setting.Fixture (RoundSpec, unnumbered)
import Schema.Round (ProcedureOf, ProposalRef)
import Schema.Round (noExpect) as Exports

-- | An expectation's balances, from each key's literals: each is a leaf asserted.
balances :: forall k a b. Named k a => Array (Tuple k (Map (Id b) Q)) -> Map (Id a) (Map (Id b) (Maybe Q))
balances = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (named k) (map Just v))

-- | An unsigned Proposal with no signers, signatures or procedure, not yet numbered. Those keys are
-- | given by record update (`p { signatures = [ ... ], proposal { signers = [ ... ] } }`).
proposal :: forall a. Named a EntityTag => { author :: a, intents :: Array Intent } -> SignedProposal
proposal p = { proposal: { id: unnumbered, author: named p.author, intents: p.intents, signers: [], procedure: Nothing }, signatures: [] }

-- | A Proposal named in an expectation: its author's first Proposal in the round.
ref :: forall a. Named a EntityTag => a -> ProposalRef
ref author = { author: named author, index: 0 }

-- | A procedure's expected outcome: the Proposal it names, the weight behind it, and whether it ran.
procedureOutcome :: ProposalRef -> Q -> Boolean -> ProcedureOf ProposalRef
procedureOutcome named support executed = { proposal: named, support, executed }

-- | A round that receives nothing and checks nothing. A round's keys are given by record update.
noRound :: RoundSpec
noRound = { proposals: Nothing, expect: Nothing }
