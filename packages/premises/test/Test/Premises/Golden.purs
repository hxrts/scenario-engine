-- | Literal premises (docs/05_test_instance.md Part IV) through the shared checker. The fixtures are
-- | decoded from the DSL's own JSON, so these tests need no compiled output, and the golden names are
-- | the fixture list itself. Every fixture plays through `simulate` (`Testkit.Fixture.checkLiteral`).
module Test.Premises.Golden (golden, fixture, failWith) where

import Prelude

import Core.Draw (Seed(..), draw)
import Core.Engine.Commit (resolveRound)
import Core.Invariants (check)
import Core.Num as Q
import Core.Represent (adjustedTarget)
import Core.Types (Id(..), SignedProposal)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array (cons, deleteAt, head, index, length, range, uncons, concatMap)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Premises.Fixtures as Fixtures
import Premises.Meridian (gameSeed)
import Schema.Premise.Fixture (Literal, decodeLiteral)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Fixture (checkLiteral)

-- | A named fixture, decoded from the DSL output. An unknown name fails the test.
fixture :: String -> Aff Literal
fixture name = case Object.lookup name (Object.fromFoldable Fixtures.fixtures) of
  Nothing -> failWith ("no fixture named " <> name)
  Just j -> case decodeLiteral j of
    Left e -> failWith (printJsonDecodeError e)
    Right d -> pure d { name = name }

-- | Fails the test with the message. `fail` throws, so the recursion is never reached; it lets
-- | an Aff of any type fail without constructing an Error value here.
failWith :: forall a. String -> Aff a
failWith msg = fail msg *> failWith msg

firstInputs :: Literal -> Array SignedProposal
firstInputs f = fromMaybe [] (_.inputs <$> head f.rounds)

golden :: Spec Unit
golden = describe "Golden fixtures" do
  -- every fixture in the DSL's list, C5 included: it plays through the simulator, every Proposal and signature
  -- reaches the records as written, and its noise is tested below
  for_ (map fst Fixtures.fixtures) \name ->
    it name (fixture name >>= \f -> checkLiteral f `shouldEqual` [])
  for_ [ "A", "A2", "F", "G4" ] \name -> it ("order independence: " <> name) do
    f <- fixture name
    for_ (permutations (firstInputs f)) \inputs ->
      map _.world (resolveRound f.world inputs) `shouldEqual` map _.world (resolveRound f.world (firstInputs f))
  it "a deliberately unbalanced result trips INV-05" do
    f <- fixture "A"
    case resolveRound f.world (firstInputs f) of
      Left e -> failWith (show e)
      Right r -> do
        let tampered = r { world = r.world { entities = Map.update (\e -> Just e { accounts = Map.insert (Id "M") (Q.fromInt 1000) e.accounts }) (Id "US") r.world.entities } }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-04", "INV-05" ]
  it "C4: the stored factor replaces the next round's baseline" do
    adjustedTarget (Q.fromInt 32) (q "0.95") `shouldEqual` q "30.4"
  it "C5: seeded noise stays in range, replays, and varies with the seed" do
    f <- fixture "C5"
    let
      factorWith seed = do
        r <- hush (resolveRound f.world { seed = Seed seed } (firstInputs f))
        Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")
      u = draw (Seed gameSeed) { round: f.world.round, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
      eps = Q.mul (q "0.1") (Q.sub (Q.mul (Q.fromInt 2) u) (Q.fromInt 1))
    factorWith gameSeed `shouldEqual` Just (Q.add (Q.fromInt 1) (Q.mul (q "0.25") (Q.add (q "-0.2") eps)))
    map (\t -> t >= Q.fromInt 37 && t <= Q.fromInt 39) (map (adjustedTarget (Q.fromInt 40)) (factorWith gameSeed)) `shouldEqual` Just true
    (factorWith gameSeed /= factorWith 1) `shouldEqual` true
  where
  q s = fromMaybe Q.zero (Q.parse s)

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case uncons xs of
  Nothing -> [ [] ]
  Just _ -> concatMap (\i -> map (cons (at i)) (permutations (fromMaybe [] (deleteAt i xs)))) (range 0 (length xs - 1))
  where
  at i = case index xs i of
    Just x -> x
    Nothing -> unsafeCrashWith "index"
