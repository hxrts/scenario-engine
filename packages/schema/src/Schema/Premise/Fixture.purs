-- | The premise format of a literal premise (docs/05_pacific.md §7), as three parts: the
-- | `premise` (its seed, name and World), the `bindings` (each round's candidate Transactions, the answers a seat
-- | plays as a Script binding, in order from the World's round), and the `expectations` (the checks a
-- | round makes, each naming its round). A round with no expectation checks nothing. The file's
-- | candidate Transactions are `SignedProposal`s with no id yet: a candidate Transaction's id depends on its round and position,
-- | and `toRound` numbers them. `decodeLiteral` yields the engine-level `Literal`.
module Schema.Premise.Fixture
  ( Literal
  , Round
  , Fixture
  , Binding
  , RoundSpec
  , unnumbered
  , fixtureShape
  , candidateTransactionShape
  , roundShape
  , toRound
  , decodeLiteral
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q, qShape)
import Core.Shape (Format(..), RecF, Shape, arrayOf, boolShape, contramapRec, decodeJsonWith, defaultsTo, embedded, emptyByDefault, field, fieldOf, intShape, iso, listOf, literal, object, optionalOf, recordOf, premiseSum, stringShape, (&.))
import Laws.Types (Id(..), CandidateTransactionId, RoundIndex, SignedCandidateTransaction, idMap, idShape, candidateTransactionId)
import Laws.World (UnseededWorld, WorldState, seeded)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Array (concat, elem, find, groupBy, length, mapWithIndex, nub, range, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Schema.Premise.Intent (intentShape)
import Schema.Premise.World (PremiseWorld, worldRec)
import Schema.Recurring as Recurring
import Schema.Round (Authored, Delivery, Expect, ProcedureOf, CandidateTransactionRef, atRound, noExpect)

-- | A round as the engine plays it: its round index, its signed candidate Transactions, and its checks.
type Round = { round :: RoundIndex, inputs :: Array SignedCandidateTransaction, expect :: Expect }

-- | A literal premise, decoded: its name, its seeded World, and its rounds.
type Literal = { name :: String, world :: WorldState, rounds :: Array Round }

-- | One round's candidate Transactions, as the seat answers it (a Script binding's `propose` and `sign`, derived at load).
type Binding = { candidateTransactions :: Array SignedCandidateTransaction }

-- | The fixture file: the premise (seed, name, World), the bindings, and the expectations.
type Fixture =
  { seed :: Int
  , name :: String
  , world :: UnseededWorld
  , premiseAuthored :: Recurring.Authored -- what only the document holds
  , bindings :: Array Binding
  , expectations :: Array { round :: Int, expect :: Authored }
  }

-- | One round as an author gives it: its candidate Transactions and its checks.
type RoundSpec = { candidateTransactions :: Maybe (Array SignedCandidateTransaction), expect :: Maybe Authored }

-- | The id of a candidate Transaction not yet numbered.
unnumbered :: CandidateTransactionId
unnumbered = Id ""

-- Shapes --------------------------------------------------------------------------------------

deliveryShape :: Shape Delivery
deliveryShape = recordOf "Delivery"
  ( field @"actor" "the Actor that receives" idShape
      &. field @"resource" "the Resource" idShape
      &. field @"quantity" "the quantity" qShape
  )

-- | A candidate Transaction by its author and its position among the author's candidate Transactions; the position is omitted when 0.
candidateTransactionAtRec :: RecF CandidateTransactionRef CandidateTransactionRef
candidateTransactionAtRec = mk <$> fieldOf "author" "the author" idShape _.author
  <*> fieldOf "index" "the position among the author's CandidateTransactions (default 0)" (defaultsTo 0 intShape) _.index
  where
  mk a i = { author: a, index: i }

-- | A proposal reference: a bare author name (the first candidate Transaction), or an object with an author and an optional index.
candidateTransactionRefShape :: Shape CandidateTransactionRef
candidateTransactionRefShape = premiseSum "CandidateTransactionRef"
  [ literal idShape (\r -> if r.index == 0 then Just r.author else Nothing) (\a -> { author: a, index: 0 })
  , embedded (object "CandidateTransactionAt" candidateTransactionAtRec) Just identity
  ]

-- | A fixture candidate Transaction, with no id: `toRound` numbers it. Signers and signatures default to none.
candidateTransactionShape :: Shape SignedCandidateTransaction
candidateTransactionShape = object "CandidateTransaction"
  ( mk <$> fieldOf "author" "the author" idShape _.candidateTransaction.author
      <*> fieldOf "intents" "the Intents" (listOf intentShape) _.candidateTransaction.intents
      <*> fieldOf "signers" "the Actors that must sign it" (arrayOf idShape) _.candidateTransaction.signers
      <*> fieldOf "signatures" "the Actors that signed it" (arrayOf idShape) _.signatures
      <*> fieldOf "procedure" "the collective procedure it is put to" (optionalOf idShape) _.candidateTransaction.procedure
  )
  where
  mk a i s g p = { candidateTransaction: { id: unnumbered, author: a, intents: i, signers: s, procedure: p }, signatures: g }

-- | A procedure's outcome: the candidate Transaction it names is flattened into the object, so it is written as an
-- | object even when it names the first candidate Transaction.
procedureShape :: Shape (ProcedureOf CandidateTransactionRef)
procedureShape = object "ProcedureOutcome"
  ( mk <$> contramapRec _.candidateTransaction candidateTransactionAtRec
      <*> fieldOf "support" "the weight behind it" qShape _.support
      <*> fieldOf "executed" "whether it ran" boolShape _.executed
  )
  where
  mk r s e = { candidateTransaction: r, support: s, executed: e }

-- | A two-level balance map, written sparsely: only a leaf asserted is written.
balances :: forall a b. Shape (Map (Id a) (Map (Id b) (Maybe Q)))
balances = emptyByDefault (idMap (iso (map Just) (Map.mapMaybe identity) (idMap qShape)))

expectShape :: Shape Authored
expectShape = recordOf "Expect"
  ( field @"accounts" "the balances, by Actor and Resource" balances
      &. field @"reserves" "the reserves, by Exchange and Resource" balances
      &. field @"deliveries" "the deliveries made" (optionalOf (listOf deliveryShape))
      &. field @"rejected" "the CandidateTransactions rejected" (optionalOf (listOf candidateTransactionRefShape))
      &. field @"lapsed" "the CandidateTransactions that lapsed" (optionalOf (listOf candidateTransactionRefShape))
      &. field @"factors" "the stored factors, by Actor and domain" balances
      &. field @"scheduled" "the number of changes scheduled" (optionalOf intShape)
      &. field @"procedures" "the outcomes of the procedures" (optionalOf (listOf procedureShape))
  )

roundShape :: Shape RoundSpec
roundShape = recordOf "Round"
  ( field @"candidateTransactions" "the CandidateTransactions the round receives" (optionalOf (listOf candidateTransactionShape))
      &. field @"expect" "the checks the round makes" (optionalOf expectShape)
  )

bindingShape :: Shape Binding
bindingShape = recordOf "Binding"
  ( field @"candidateTransactions" "the CandidateTransactions the round receives, in order" (listOf candidateTransactionShape)
  )

expectationShape :: Shape { round :: Int, expect :: Authored }
expectationShape = recordOf "Expectation"
  ( field @"round" "the round it checks" intShape
      &. field @"expect" "the checks the round makes" expectShape
  )

premiseShape :: Shape { seed :: Int, name :: String, world :: PremiseWorld }
premiseShape = object "Premise"
  ( mk <$> fieldOf "seed" "the game seed of the fixture's World" intShape _.seed
      <*> fieldOf "name" "the premise's name" (defaultsTo "" stringShape) _.name
      <*> contramapRec _.world worldRec
  )
  where
  mk s n w = { seed: s, name: n, world: w }

fixtureShape :: Shape Fixture
fixtureShape = object "Fixture"
  ( mk <$> fieldOf "premise" "the seed and the World" premiseShape (\f -> { seed: f.seed, name: f.name, world: { world: f.world, authored: f.premiseAuthored } })
      <*> fieldOf "bindings" "the rounds' CandidateTransactions, one binding per round, in order" (listOf bindingShape) _.bindings
      <*> fieldOf "expectations" "the checks, each naming its round" (listOf expectationShape) _.expectations
  )
  where
  mk p b e = { seed: p.seed, name: p.name, world: p.world.world, premiseAuthored: p.world.authored, bindings: b, expectations: e }

-- Reading -------------------------------------------------------------------------------------

-- | A literal premise as the engine plays it: its name, its seeded World, and its rounds with their
-- | candidate Transactions numbered.
toLiteral :: Fixture -> Literal
toLiteral f =
  { name: f.name
  , world: seeded (Seed f.seed) f.world
  , rounds: mapWithIndex (\k b -> toRound (f.world.round + k) (roundOf (f.world.round + k) b)) f.bindings
  }
  where
  roundOf r b = { candidateTransactions: Just b.candidateTransactions, expect: map _.expect (find (\x -> x.round == r) f.expectations) }

-- | One round: its candidate Transactions in author order, each numbered within its author's group, and its expectation.
toRound :: RoundIndex -> RoundSpec -> Round
toRound round r = { round, inputs: concat (map (mapWithIndex numbered) groups), expect: atRound round (fromMaybe noExpect r.expect) }
  where
  candidateTransactions = fromMaybe [] r.candidateTransactions
  groups = map toArray (groupBy (\a b -> a.candidateTransaction.author == b.candidateTransaction.author) (sortWith _.candidateTransaction.author candidateTransactions))
  numbered i p = p { candidateTransaction { id = candidateTransactionId round p.candidateTransaction.author i } }

-- | A literal premise, decoded from its JSON (the fixture file's document).
decodeLiteral :: Json -> Either JsonDecodeError Literal
decodeLiteral j = do
  f <- decodeJsonWith Authored fixtureShape j
  let
    named = map _.round f.expectations
    bound = map (\k -> f.world.round + k) (range 0 (length f.bindings - 1))
  if all (\r -> elem r bound) named && length (nub named) == length named then pure (toLiteral f)
  else Left (TypeMismatch "an expectation names a round with no binding, or names one round twice")
