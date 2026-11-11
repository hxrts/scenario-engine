-- | Literal premises (docs/06_meridian.md Part IV) through the shared checker. The fixtures are
-- | decoded from the DSL's own JSON, so these tests need no compiled output, and the golden names are
-- | the fixture list itself. Every fixture plays through `simulate` (`Testkit.Fixture.checkLiteral`).
-- | Pins: expected values are those of docs/06 Part IV (Fixtures A to G, §21 to §27), held in the fixture modules.
module Test.Premises.Golden (golden, fixture, failWith) where

import Schema.Build.Quantity (q)
import Prelude

import Testkit.Edit (onActor)

import Core.Draw (Seed(..))
import Laws.Engine.Commit (resolveRound)
import Laws.Invariants (check)
import Core.Num as Q
import Laws.Engine.Prepare (observe)
import Laws.Types (Id(..), IntentF(..), SignedCandidateTransaction, idText)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array (cons, deleteAt, head, index, length, range, uncons, concatMap)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for_)
import Data.Tuple (fst)
import Control.Monad.Error.Class (throwError)
import Effect.Aff (Aff, error)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Premises.Fixtures as Fixtures
import Strategy.Interpret (nameOnly, propose)
import Premises.Meridian (gameSeed)
import Premises.Meridian.Names (n)
import Schema.Premise.Fixture (Literal, decodeLiteral)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Fixture (checkLiteral)
import Testkit.Meridian (meridianParts, unseated)

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
  for_ [ "A", "A2", "F", "G4" ] \name -> it ("order independence: " <> name) do
    f <- fixture name
    for_ (permutations (firstInputs f)) \inputs ->
      map _.world (resolveRound f.world inputs) `shouldEqual` map _.world (resolveRound f.world (firstInputs f))
  it "a deliberately unbalanced result trips INV-05" do
    f <- fixture "A"
    case resolveRound f.world (firstInputs f) of
      Left e -> failWith (show e)
      Right r -> do
        let tampered = r { world = onActor (Id "US") (\e -> e { accounts = Map.insert (Id "M") (Q.fromInt 1000) e.accounts }) r.world }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-04", "INV-05" ]
  it "C4: GLOBAL_SMES drafts its high-price baseline times the stored factor" do
    p <- meridianParts
    g <- unseated
    strategyRules <- maybe (failWith "no GLOBAL_SMES strategy") pure (Map.lookup n.strategyGlobalSmes p.composition.strategies)
    actor <- maybe (failWith "no GLOBAL_SMES") pure (Map.lookup n.globalSmes g.world.actors)
    let
      view = (observe g.world actor)
        { prices = Map.singleton n.energyAtlantic (q "3.51")
        , factors = Map.singleton (Id (idText n.energy)) (q "0.95")
        }
    -- docs/06 C4: the baseline above 3.5 M/E is 32 E, and 32 x 0.95 = 30.4
    map _.intents (propose strategyRules { view, params: Object.empty, random: const Q.zero, exchange: nameOnly, planned: [] })
      `shouldEqual` [ [ Trade { exchange: n.energyAtlantic, quantity: q "30.4", limitPrice: Nothing } ] ]
  it "C5: seeded noise stays in range, replays, and varies with the seed" do
    f <- fixture "C5"
    let
      factorWith seed = do
        r <- hush (resolveRound f.world { seed = Seed seed } (firstInputs f))
        Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")
    -- docs/06 §23: the recorded draw for game seed 26021 is u = 0.2158, so ε = −0.0568 and the
    -- factor is 1 + 0.25 × (−0.20 − 0.0568)
    factorWith gameSeed `shouldEqual` Just (q "0.9358")
    map (\t -> t >= Q.fromInt 37 && t <= Q.fromInt 39) (map (Q.mul (Q.fromInt 40)) (factorWith gameSeed)) `shouldEqual` Just true
    (factorWith gameSeed /= factorWith 1) `shouldEqual` true

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case uncons xs of
  Nothing -> [ [] ]
  Just _ -> concatMap (\i -> map (cons (at i)) (permutations (fromMaybe [] (deleteAt i xs)))) (range 0 (length xs - 1))
  where
  at i = case index xs i of
    Just x -> x
    Nothing -> unsafeCrashWith "index"
