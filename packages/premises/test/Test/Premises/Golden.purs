-- | Literal premises (docs/05_pacific.md §7) through the shared checker. The fixtures are
-- | decoded from the DSL's own JSON, so these tests need no compiled output, and the golden names are
-- | the fixture list itself. Every fixture plays through `simulate` (`Testkit.Fixture.checkLiteral`).
-- | Pins: expected values are those of docs/05 §9, held in the fixture modules.
module Test.Premises.Golden (golden, fixture, failWith) where

import Prelude

import Testkit.Edit (onActor)

import Laws.Engine.Commit (resolveRound)
import Laws.Invariants (check)
import Core.Num as Q

import Laws.Types (Id(..), SignedCandidateTransaction)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array (cons, deleteAt, head, index, length, range, uncons, concatMap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (fst)
import Control.Monad.Error.Class (throwError)
import Effect.Aff (Aff, error)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Premises.Fixtures as Fixtures

import Schema.Premise.Fixture (Literal, decodeLiteral)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Fixture (checkLiteral)

-- | A named fixture, decoded from the DSL output. An unknown name fails the test.
fixture :: String -> Aff Literal
fixture name = case Object.lookup name (Object.fromFoldable Fixtures.fixtures) of
  Nothing -> failWith ("no fixture named " <> name)
  Just j -> case decodeLiteral j of
    Left e -> failWith (printJsonDecodeError e)
    Right d -> pure d { name = name }

-- | Fails the test with the message, at any result type.
failWith :: forall a. String -> Aff a
failWith = throwError <<< error

firstInputs :: Literal -> Array SignedCandidateTransaction
firstInputs f = fromMaybe [] (_.inputs <$> head f.rounds)

golden :: Spec Unit
golden = describe "Golden fixtures" do
  -- every fixture in the DSL's list, C5 included: it plays through the simulator, every candidate Transaction and signature
  -- reaches the records as written, and its noise is tested below
  for_ (map fst Fixtures.fixtures) \name ->
    it name (fixture name >>= \f -> checkLiteral f `shouldEqual` [])
  for_ [ "P-A", "P-H", "P-C" ] \name -> it ("order independence: " <> name) do
    f <- fixture name
    for_ (permutations (firstInputs f)) \inputs ->
      map _.world (resolveRound f.world inputs) `shouldEqual` map _.world (resolveRound f.world (firstInputs f))
  it "a deliberately unbalanced result trips INV-05" do
    f <- fixture "P-A"
    case resolveRound f.world (firstInputs f) of
      Left e -> failWith (show e)
      Right r -> do
        let tampered = r { world = onActor (Id "US") (\e -> e { accounts = Map.insert (Id "USD") (Q.fromInt 1000) e.accounts }) r.world }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-04", "INV-05" ]

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case uncons xs of
  Nothing -> [ [] ]
  Just _ -> concatMap (\i -> map (cons (at i)) (permutations (fromMaybe [] (deleteAt i xs)))) (range 0 (length xs - 1))
  where
  at i = case index xs i of
    Just x -> x
    Nothing -> unsafeCrashWith "index"
