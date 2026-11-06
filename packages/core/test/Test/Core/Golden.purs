-- | One generic golden runner: every fixture's expected balances, deliveries, rejections,
-- | and lapses must hold exactly, and every invariant must hold.
module Test.Core.Golden (golden, orderIndependent, unbalanced) where

import Prelude

import Core.Invariants (check)
import Core.Num as Q
import Core.Types (Id(..), proposalId)
import Core.World (Event(..))
import Data.Array (concatMap, cons, deleteAt, index, length, range, sort, sortWith, uncons)
import Core.Engine (resolveRound)
import Partial.Unsafe (unsafeCrashWith)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Foreign.Object as Object
import Test.Core.Fixture (load, run)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail, shouldEqual)

golden :: Array String -> Spec Unit
golden names = for_ names \name -> it name do
  loaded <- liftEffect (load name)
  f <- case loaded of
    Left e -> throwError (error e)
    Right x -> pure x
  case run f of
    Left e -> fail (show e)
    Right r -> do
      let w = r.world
      for_ (Object.toUnfoldable f.expect.accounts :: Array _) \(Tuple e rs) ->
        for_ (Object.toUnfoldable rs :: Array _) \(Tuple res q) ->
          { entity: e, resource: res, amount: fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id res) x.accounts) }
            `shouldEqual` { entity: e, resource: res, amount: q }
      for_ (Object.toUnfoldable f.expect.reserves :: Array _) \(Tuple m rs) ->
        for_ (Object.toUnfoldable rs :: Array _) \(Tuple res q) ->
          { market: m, resource: res, amount: fromMaybe Q.zero (Map.lookup (Id m) w.markets >>= \x -> Map.lookup (Id res) x.reserves) }
            `shouldEqual` { market: m, resource: res, amount: q }
      sortWith show (map (\d -> { entity: show d.entity, resource: show d.resource, quantity: d.quantity }) w.deliveries)
        `shouldEqual` sortWith show f.expect.deliveries
      let
        ids authors = sort (map (\a -> show (proposalId f.world.round (Id a) 0)) authors)
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
      rejected `shouldEqual` ids f.expect.rejected
      lapsed `shouldEqual` ids f.expect.lapsed
      check f.world r `shouldEqual` []

-- | INV-11: every permutation of a fixture's signed Proposals gives the same result.
orderIndependent :: Array String -> Spec Unit
orderIndependent names = for_ names \name -> it ("order independence: " <> name) do
  loaded <- liftEffect (load name)
  case loaded of
    Left e -> fail e
    Right f -> for_ (permutations f.inputs) \inputs ->
      map _.world (resolveRound f.world inputs) `shouldEqual` map _.world (run f)

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
    Right f -> case run f of
      Left e -> fail (show e)
      Right r -> do
        let tampered = r { world = r.world { entities = Map.update (\e -> Just e { accounts = Map.insert (Id "M") (Q.fromInt 1000) e.accounts }) (Id "US") r.world.entities } }
        map _.invariant (check f.world tampered) `shouldEqual` [ "INV-05" ]
