-- | One generic golden runner: every fixture's expected balances, deliveries, rejections,
-- | and lapses must hold exactly, and every invariant must hold.
module Test.Board.Golden (golden, orderIndependent, unbalanced, representation) where

import Prelude

import Core.Invariants (check)
import Core.Num as Q
import Core.Types (Id(..), proposalId)
import Core.World (Event(..), RoundResult, Target(..), WorldState)
import Data.Array (concatMap, cons, deleteAt, filter, head, index, length, range, sort, sortWith, uncons)
import Core.Engine (prepareRound, resolveRound)
import Core.Draw (Seed(..), draw)
import Core.Represent (adjustedTarget)
import Data.Foldable (foldM)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Foreign.Object as Object
import Board.Decode (Expect)
import Test.Board.Fixture (load, run)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail, shouldEqual)

golden :: Array String -> Spec Unit
golden names = for_ names \name -> it name do
  loaded <- liftEffect (load name)
  f <- case loaded of
    Left e -> throwError (error e)
    Right x -> pure x
  void $ foldM
    ( \w0 rnd -> do
        let w = prepareRound w0
        case run w rnd of
          Left e -> fail (show e) $> w
          Right r -> expectRound w rnd.expect r $> r.world
    )
    f.world
    f.rounds

expectRound :: WorldState -> Expect -> RoundResult -> Aff Unit
expectRound before ex r = do
  let w = r.world
  for_ (Object.toUnfoldable ex.accounts :: Array _) \(Tuple e rs) ->
    for_ (Object.toUnfoldable rs :: Array _) \(Tuple res q) ->
      { entity: e, resource: res, amount: fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id res) x.accounts) }
        `shouldEqual` { entity: e, resource: res, amount: q }
  for_ (Object.toUnfoldable ex.reserves :: Array _) \(Tuple m rs) ->
    for_ (Object.toUnfoldable rs :: Array _) \(Tuple res q) ->
      { market: m, resource: res, amount: fromMaybe Q.zero (Map.lookup (Id m) w.markets >>= \x -> Map.lookup (Id res) x.reserves) }
        `shouldEqual` { market: m, resource: res, amount: q }
  for_ (Object.toUnfoldable ex.factors :: Array _) \(Tuple a ds) ->
    for_ (Object.toUnfoldable ds :: Array _) \(Tuple d q) ->
      { aggregate: a, domain: d, factor: Map.lookup (Id a) w.factors >>= Map.lookup (Id d) }
        `shouldEqual` { aggregate: a, domain: d, factor: Just q }
  let
    pending = filter (\d -> d.round > before.round) w.deliveries
    ids authors = sort (map (\a -> show (proposalId before.round (Id a) 0)) authors)
    rejected = sort
      ( concatMap
          ( case _ of
              Rejected x -> [ show x.proposal ]
              _ -> []
          )
          r.record.events
      )
    lapsed = sort
      ( concatMap
          ( case _ of
              Lapsed x -> [ show x.proposal ]
              _ -> []
          )
          r.record.events
      )
    procedures = concatMap
      ( case _ of
          ProcedureOutcome x -> [ { proposal: show x.proposal, support: x.support, executed: x.executed } ]
          _ -> []
      )
      r.record.events
  when (ex.deliveries /= []) $
    sortWith show
      ( concatMap
          ( \d -> case d.target of
              Holding res -> [ { entity: show d.entity, resource: show res, quantity: d.quantity } ]
              _ -> []
          )
          pending
      )
      `shouldEqual` sortWith show ex.deliveries
  rejected `shouldEqual` ids ex.rejected
  lapsed `shouldEqual` ids ex.lapsed
  procedures `shouldEqual` map (\p -> { proposal: show (proposalId before.round (Id p.author) 0), support: p.support, executed: p.executed }) ex.procedures
  Map.size w.scheduled `shouldEqual` ex.scheduled
  check before r `shouldEqual` []

-- | INV-11: every permutation of a fixture's signed Proposals gives the same result.
orderIndependent :: Array String -> Spec Unit
orderIndependent names = for_ names \name -> it ("order independence: " <> name) do
  loaded <- liftEffect (load name)
  case loaded of
    Left e -> fail e
    Right f -> for_ (permutations (fromMaybe [] (_.inputs <$> head f.rounds))) \inputs ->
      map _.world (resolveRound f.world inputs) `shouldEqual` map _.world (resolveRound f.world (fromMaybe [] (_.inputs <$> head f.rounds)))

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case uncons xs of
  Nothing -> [ [] ]
  Just _ -> concatMap (\i -> map (cons (unsafeIndex' i)) (permutations (deleteAt' i))) (range 0 (length xs - 1))
  where
  unsafeIndex' i = fromMaybe' (index xs i)
  deleteAt' i = fromMaybe [] (deleteAt i xs)
  fromMaybe' = case _ of
    Just x -> x
    Nothing -> unsafeCrashWith "index"

-- | A tampered result (one Entity credited from nowhere) must trip INV-05.
unbalanced :: Spec Unit
unbalanced = it "a deliberately unbalanced result trips INV-05" do
  loaded <- liftEffect (load "A")
  case loaded of
    Left e -> fail e
    Right f -> case resolveRound f.world (fromMaybe [] (_.inputs <$> head f.rounds)) of
      Left e -> fail (show e)
      Right r -> do
        let tampered = r { world = r.world { entities = Map.update (\e -> Just e { accounts = Map.insert (Id "M") (Q.fromInt 1000) e.accounts }) (Id "US") r.world.entities } }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-05" ]

-- | Fixture C4 and C5: applying a stored factor, and seeded sampling noise.
representation :: Spec Unit
representation = do
  it "C4: the stored factor replaces the next round's baseline" do
    adjustedTarget (Q.fromInt 32) (q "0.95") `shouldEqual` q "30.4"
  it "C5: seeded noise stays in range, replays, and varies with the seed" do
    loaded <- liftEffect (load "C5")
    case loaded of
      Left e -> fail e
      Right f -> do
        let
          factorWith seed = do
            r <- hush (resolveRound f.world { seed = Seed seed } (fromMaybe [] (_.inputs <$> head f.rounds)))
            Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")
          target seed = map (adjustedTarget (Q.fromInt 40)) (factorWith seed)
          u = draw (Seed 26021) { round: f.world.round, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
          eps = Q.mul (q "0.1") (Q.sub (Q.mul (Q.fromInt 2) u) (Q.fromInt 1))
        factorWith 26021 `shouldEqual` Just (Q.add (Q.fromInt 1) (Q.mul (q "0.25") (Q.add (q "-0.2") eps)))
        (map (\t -> t >= Q.fromInt 37 && t <= Q.fromInt 39) (target 26021)) `shouldEqual` Just true
        factorWith 26021 `shouldEqual` factorWith 26021
        (factorWith 26021 /= factorWith 1) `shouldEqual` true
  where
  q s = fromMaybe Q.zero (Q.parse s)
