-- | Builders for a golden fixture's rounds and expectations (docs/05_pacific.md §9.3):
-- | candidate Transactions and their references, procedure outcomes, and the `expect` checks. Every expected value
-- | is a literal. An `expect` is `noExpect` with the keys it checks given by record update:
-- | `noExpect { accounts = balances [ Tuple N.us (keyed [ N.money := 260 ]) ] }`.
module Schema.Build.Fixture
  ( module Exports
  , balances
  , candidateTransaction
  , ref
  , procedureOutcome
  , noRound
  ) where

import Prelude (map, (<<<))

import Core.Num (Q)
import Laws.Types (ActorTag, Id, Intent, SignedCandidateTransaction)
import Schema.Build.Named (class Named, named)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Premise.Fixture (RoundSpec, unnumbered)
import Schema.Round (ProcedureOf, CandidateTransactionRef)
import Schema.Round (noExpect) as Exports

-- | An expectation's balances, from each key's literals: each is a leaf asserted.
balances :: forall k a b. Named k a => Array (Tuple k (Map (Id b) Q)) -> Map (Id a) (Map (Id b) (Maybe Q))
balances = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (named k) (map Just v))

-- | An unsigned candidate Transaction with no signers, signatures or procedure, not yet numbered. Those keys are
-- | given by record update (`p { signatures = [ ... ], proposal { signers = [ ... ] } }`).
candidateTransaction :: forall a. Named a ActorTag => { author :: a, intents :: Array Intent } -> SignedCandidateTransaction
candidateTransaction p = { candidateTransaction: { id: unnumbered, author: named p.author, intents: p.intents, signers: [], procedure: Nothing }, signatures: [] }

-- | A candidate Transaction named in an expectation: its author's first candidate Transaction in the round.
ref :: forall a. Named a ActorTag => a -> CandidateTransactionRef
ref author = { author: named author, index: 0 }

-- | A procedure's expected outcome: the candidate Transaction it names, the weight behind it, and whether it ran.
procedureOutcome :: CandidateTransactionRef -> Q -> Boolean -> ProcedureOf CandidateTransactionRef
procedureOutcome named support executed = { candidateTransaction: named, support, executed }

-- | A round that receives nothing and checks nothing. A round's keys are given by record update.
noRound :: RoundSpec
noRound = { candidateTransactions: Nothing, expect: Nothing }
