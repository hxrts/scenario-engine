-- | Pins: the kernel's parts, directly. Expected values are literals.
module Test.Core.Kernel (kernel) where

import Prelude

import Core.Kernel.Post (Account(..), Outcome(..), available, post)
import Core.Kernel.Resolve (Fate(..), Kernel, resolve)
import Core.Kernel.Types (Account(HeldBy, Created, Destroyed)) as K
import Core.Kernel.Types (Amount(..), Bond(..), Condition(..), Coupling, Effect(..), Guarded, Limit(..), Obligation, Party(..), Short(..), noNext)
import Core.Num as Q
import Core.Types (Id(..), ResourceId)
import Core.World (Endpoint(..))
import Data.Array (reverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Core.Build (q)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

m :: ResourceId
m = Id "M"

opening :: Account -> ResourceId -> Q.Q
opening (Ent _) _ = q "10"
opening (Mkt _) _ = q "4"

a :: Endpoint
a = Held (Id "A")

b :: Endpoint
b = Held (Id "B")

kernel :: Spec Unit
kernel = describe "Core.Kernel" do
  describe "Post" do
    it "a leg within what is held moves whole, debiting and crediting" do
      let p = post opening Cap { from: a, to: b, resource: m, owed: q "6" } Map.empty
      p.flow.moved `shouldEqual` q "6"
      p.outcome `shouldEqual` PostedWhole
      available opening p.ledger (Ent (Id "A")) m `shouldEqual` q "4"
      Map.lookup (Tuple (Ent (Id "B")) m) p.ledger `shouldEqual` Just (q "6")
    it "Cap moves what is held and records what was owed" do
      let p = post opening Cap { from: a, to: Sink, resource: m, owed: q "15" } Map.empty
      { owed: p.flow.owed, moved: p.flow.moved, outcome: p.outcome } `shouldEqual` { owed: q "15", moved: q "10", outcome: PostedShort }
    it "Reject moves nothing when the whole cannot move" do
      let p = post opening Reject { from: a, to: b, resource: m, owed: q "15" } Map.empty
      { moved: p.flow.moved, outcome: p.outcome, left: available opening p.ledger (Ent (Id "A")) m } `shouldEqual` { moved: Q.zero, outcome: PostedShort, left: q "10" }
    it "a leg that moves nothing still touches its accounts, which is how a round's new holdings are found" do
      Map.member (Tuple (Ent (Id "B")) m) (post opening Reject { from: a, to: b, resource: m, owed: q "15" } Map.empty).ledger `shouldEqual` true
    it "reads the settlement so far" do
      let
        first = post opening Cap { from: a, to: b, resource: m, owed: q "7" } Map.empty
        second = post opening Cap { from: a, to: b, resource: m, owed: q "7" } first.ledger
      second.flow.moved `shouldEqual` q "3"
    it "the boundary always pays, and a sink credits no account" do
      let
        made = post opening Reject { from: Source, to: a, resource: m, owed: q "99" } Map.empty
        sunk = post opening Cap { from: a, to: Sink, resource: m, owed: q "1" } Map.empty
      made.flow.moved `shouldEqual` q "99"
      Map.size sunk.ledger `shouldEqual` 1
    it "a market reserve caps like any account" do
      (post opening Cap { from: InMarket (Id "X"), to: a, resource: m, owed: q "9" } Map.empty).flow.moved `shouldEqual` q "4"

  describe "Resolve" do
    let
      fates = map (map (\c -> { c: c.coupling, o: c.obligation, fate: c.fate, why: c.reason }))
    it "performs what stands, in phase order, on the settlement so far" do
      -- phase 0 leaves A with 4, so the capped payment of phase 1 moves 4
      let result = resolve env [ cp 1 "z" [ pay "z" "A" "9" Cap [] ], cp 0 "a" [ pay "a" "A" "6" Cap [] ] ]
      map (map (map _.moved <<< _.flow)) (map _.consumed result) `shouldEqual` Right [ Just (q "6"), Just (q "4") ]
    it "a condition's failure lapses the obligation and reports its reason" do
      let failing = pay "x" "A" "1" Cap [ { condition: Consent [ Id "B" ], reason: Just "no" } ]
      fates (map _.consumed (resolve env [ cp 0 "x" [ failing ] ])) `shouldEqual` Right [ { c: 0, o: 0, fate: Lapsed, why: Just "no" } ]
    it "a mutually dependent coupling with one failing party performs nothing" do
      -- B cannot pay 50: its obligation is short, A's then lapses in the next pass, and no leg posts
      let
        pair = cp 0 "j"
          [ pay "x" "A" "1" Reject [ needs 1 ]
          , pay "y" "B" "50" Reject [ needs 0 ]
          ]
        result = resolve env [ pair ]
      fates (map _.consumed result) `shouldEqual` Right
        [ { c: 0, o: 0, fate: Lapsed, why: Just "other" }
        , { c: 0, o: 1, fate: Shorted, why: Nothing }
        ]
      map (Map.isEmpty <<< _.ledger) result `shouldEqual` Right true
    it "a chain of dependence terminates, however long" do
      let chain = cp 0 "c" [ pay "a" "A" "1" Reject [ needs 1 ], pay "b" "A" "1" Reject [ needs 2 ], pay "c" "A" "99" Reject [] ]
      map (map _.fate <<< _.consumed) (resolve env [ chain ]) `shouldEqual` Right [ Lapsed, Lapsed, Shorted ]
    it "does not depend on the order couplings arrive in" do
      let
        xs = [ cp 0 "a" [ pay "a" "A" "6" Cap [] ], cp 0 "b" [ pay "b" "A" "6" Cap [] ], cp 1 "c" [ pay "c" "B" "3" Cap [] ] ]
        flows = map (map (\c -> c.flow) <<< _.consumed)
      flows (resolve env (reverse xs)) `shouldEqual` flows (resolve env xs)
      map _.ledger (resolve env (reverse xs)) `shouldEqual` map _.ledger (resolve env xs)
    it "weighted consent passes only above its threshold" do
      let vote t = pay "v" "A" "1" Cap [ { condition: WeightedConsent { weights: Map.singleton (Id "B") (q "0.5"), residual: Q.zero, threshold: q t }, reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env [ (cp 0 "v" [ vote "0.5" ]) { consented = [ Id "B" ] } ]) `shouldEqual` Right [ Lapsed ]
      map (map _.fate <<< _.consumed) (resolve env [ (cp 0 "v" [ vote "0.49" ]) { consented = [ Id "B" ] } ]) `shouldEqual` Right [ Fulfilled ]
    it "a price limit reads the kernel's price" do
      let limited = pay "p" "A" "1" Cap [ { condition: PriceWithin (AtMost (q "2")), reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env { price = Just (q "3") } [ cp 0 "p" [ limited ] ]) `shouldEqual` Right [ Lapsed ]
    it "a stock condition reads what is held" do
      let gated x = pay "s" "A" "1" Cap [ { condition: StockAtLeast { at: K.HeldBy (Named (Id "A")), resource: m } (Exactly (q x)), reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "s" [ gated "11" ] ]) `shouldEqual` Right [ Lapsed ]
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "s" [ gated "10" ] ]) `shouldEqual` Right [ Fulfilled ]
    it "a proportional bond stands or falls like a whole one until a solver sets the fill" do
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "f" [ pay "a" "A" "1" Cap [ { condition: Performed 1 (Proportional (q "2")), reason: Nothing } ], pay "b" "A" "1" Cap [] ] ]) `shouldEqual` Right [ Fulfilled, Fulfilled ]
    it "minting and destroying cross the boundary" do
      let
        mint = { id: "m", obligor: Named (Id "A"), effect: Move { from: K.Created, to: K.HeldBy (Named (Id "A")), resource: m, owed: Exactly (q "2") }, when: [], onShort: Reject, next: noNext }
        burn = { id: "n", obligor: Named (Id "A"), effect: Move { from: K.HeldBy (Named (Id "A")), to: K.Destroyed, resource: m, owed: Exactly (q "1") }, when: [], onShort: Reject, next: noNext }
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "z" [ mint, burn ] ]) `shouldEqual` Right [ Fulfilled, Fulfilled ]

type C = Coupling Unit Unit Unit String Unit
type O = Obligation Unit Unit Unit String

env :: Kernel Unit Unit
env = { opening, holds: \_ _ -> true, read: \_ _ -> Q.zero, price: Nothing }

cp :: Int -> String -> Array O -> C
cp phase _ obligations = { obligations, holds: [], solver: Nothing, ration: { tier: 0, whole: false, scaleGroup: Nothing }, phase, cause: Nothing, consented: [] }

pay :: String -> String -> String -> Short -> Array (Guarded Unit Unit String) -> O
pay id who x onShort when = { id, obligor: Named (Id who), effect: Move { from: K.HeldBy (Named (Id who)), to: K.Destroyed, resource: m, owed: Exactly (q x) }, when, onShort, next: noNext }

needs :: Int -> Guarded Unit Unit String
needs i = { condition: Performed i Whole, reason: Just "other" }
