-- | Literal settings (docs/05_test_instance.md Part IV) through the shared checker. The fixtures are
-- | decoded from the DSL's own JSON, so these tests need no compiled output, and the golden names are
-- | the fixture list itself. Every fixture plays through `simulate` (`Testkit.Fixture.checkLiteral`).
module Test.Settings.Golden (golden, fixture, failWith) where

import Schema.Build.Quantity (q)
import Prelude

import Testkit.Edit (onEntity)

import Core.Draw (Seed(..))
import Core.Engine.Commit (resolveRound)
import Core.Invariants (check)
import Core.Num as Q
import Core.Engine.Prepare (observe)
import Core.Types (Id(..), IntentF(..), SignedProposal, idText)
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
import Settings.Meridian.Fixtures as Fixtures
import Policy.Interpret (nameOnly, propose)
import Settings.Meridian (gameSeed)
import Settings.Meridian.Names (n)
import Schema.Setting.Fixture (Literal, decodeLiteral)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Fixture (checkLiteral)
import Testkit.Meridian (setting, unseated)

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
        let tampered = r { world = onEntity (Id "US") (\e -> e { accounts = Map.insert (Id "M") (Q.fromInt 1000) e.accounts }) r.world }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-04", "INV-05" ]
  it "C4: GLOBAL_SMES drafts its high-price baseline times the stored factor" do
    p <- setting
    g <- unseated
    strategy <- maybe (failWith "no GLOBAL_SMES policy") pure (Map.lookup n.policyGlobalSmes p.gameboard.policies)
    entity <- maybe (failWith "no GLOBAL_SMES") pure (Map.lookup n.globalSmes g.world.entities)
    let
      view = (observe g.world entity)
        { prices = Map.singleton n.energyAtlantic (q "3.51")
        , factors = Map.singleton (Id (idText n.energy)) (q "0.95")
        }
    -- docs/05 C4: the baseline above 3.5 M/E is 32 E, and 32 x 0.95 = 30.4
    map _.intents (propose strategy { view, params: Object.empty, random: const Q.zero, market: nameOnly })
      `shouldEqual` [ [ Trade { market: n.energyAtlantic, quantity: q "30.4", limitPrice: Nothing } ] ]
  it "C5: seeded noise stays in range, replays, and varies with the seed" do
    f <- fixture "C5"
    let
      factorWith seed = do
        r <- hush (resolveRound f.world { seed = Seed seed } (firstInputs f))
        Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")
    -- docs/05 §23: the recorded draw for game seed 26021 is u = 0.2158, so ε = −0.0568 and the
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
