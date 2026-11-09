-- | Pins: the kernel's parts, directly. Expected values are literals.
module Test.Core.Kernel (kernel) where

import Prelude

import Core.Kernel.Post (Account(..), Outcome(..), available, post)
import Core.Kernel.Consume (ConsumeError(..), due, partiesOf, spawned, validateTemplates)
import Core.Kernel.Resolve (Fate(..), Kernel, resolve)
import Core.Kernel.Types (Account(HeldBy, Created, Destroyed)) as K
import Core.Kernel.Types (Amount(..), Bond(..), Condition(..), Transaction, Effect(..), Guarded, Limit(..), Obligation, Party(..), Short(..), noNext, Endpoint(..))
import Core.Num as Q
import Core.Types (Id(..), ResourceId)
import Data.Array (reverse)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
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
    it "an exchange reserve pays whole, since clearing bounds its batch" do
      (post opening Cap { from: InExchange (Id "X"), to: a, resource: m, owed: q "9" } Map.empty).flow.moved `shouldEqual` q "9"

  describe "Resolve" do
    let
      fates = map (map (\c -> { c: c.transaction, o: c.obligation, fate: c.fate, why: c.reason }))
    it "performs what stands, in phase order, on the settlement so far" do
      -- phase 0 leaves A with 4, so the capped payment of phase 1 moves 4
      let result = resolve env [ cp 1 "z" [ pay "z" "A" "9" Cap [] ], cp 0 "a" [ pay "a" "A" "6" Cap [] ] ]
      map (map (map _.moved <<< _.flow)) (map _.consumed result) `shouldEqual` Right [ Just (q "6"), Just (q "4") ]
    it "a condition's failure lapses the obligation and reports its reason" do
      let failing = pay "x" "A" "1" Cap [ { condition: Consent [ Id "B" ], reason: Just "no" } ]
      fates (map _.consumed (resolve env [ cp 0 "x" [ failing ] ])) `shouldEqual` Right [ { c: 0, o: 0, fate: Expired, why: Just "no" } ]
    it "a mutually dependent transaction with one failing party performs nothing" do
      -- B cannot pay 50: its obligation is short, A's then lapses in the next pass, and no settled Flow posts
      let
        pair = cp 0 "j"
          [ pay "x" "A" "1" Reject [ needs 1 ]
          , pay "y" "B" "50" Reject [ needs 0 ]
          ]
        result = resolve env [ pair ]
      fates (map _.consumed result) `shouldEqual` Right
        [ { c: 0, o: 0, fate: Expired, why: Just "other" }
        , { c: 0, o: 1, fate: Shorted, why: Nothing }
        ]
      map (Map.isEmpty <<< _.ledger) result `shouldEqual` Right true
    it "a chain of dependence terminates, however long" do
      let chain = cp 0 "c" [ pay "a" "A" "1" Reject [ needs 1 ], pay "b" "A" "1" Reject [ needs 2 ], pay "c" "A" "99" Reject [] ]
      map (map _.fate <<< _.consumed) (resolve env [ chain ]) `shouldEqual` Right [ Expired, Expired, Shorted ]
    it "does not depend on the order transactions arrive in" do
      let
        xs = [ cp 0 "a" [ pay "a" "A" "6" Cap [] ], cp 0 "b" [ pay "b" "A" "6" Cap [] ], cp 1 "c" [ pay "c" "B" "3" Cap [] ] ]
        flows = map (map (\c -> c.flow) <<< _.consumed)
      flows (resolve env (reverse xs)) `shouldEqual` flows (resolve env xs)
      map _.ledger (resolve env (reverse xs)) `shouldEqual` map _.ledger (resolve env xs)
    it "weighted consent passes only above its threshold" do
      let vote t = pay "v" "A" "1" Cap [ { condition: WeightedConsent { weights: Map.singleton (Id "B") (q "0.5"), residual: Q.zero, threshold: q t }, reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env [ (cp 0 "v" [ vote "0.5" ]) { consented = [ Id "B" ] } ]) `shouldEqual` Right [ Expired ]
      map (map _.fate <<< _.consumed) (resolve env [ (cp 0 "v" [ vote "0.49" ]) { consented = [ Id "B" ] } ]) `shouldEqual` Right [ Fulfilled ]
    it "a price limit reads the kernel's price" do
      let limited = pay "p" "A" "1" Cap [ { condition: PriceWithin (AtMost (q "2")), reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env { price = Just (q "3") } [ cp 0 "p" [ limited ] ]) `shouldEqual` Right [ Expired ]
    it "a stock condition reads what is held" do
      let gated x = pay "s" "A" "1" Cap [ { condition: StockAtLeast { stock: { at: K.HeldBy (Named (Id "A")), resource: m }, amount: Exactly (q x) }, reason: Nothing } ]
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "s" [ gated "11" ] ]) `shouldEqual` Right [ Expired ]
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "s" [ gated "10" ] ]) `shouldEqual` Right [ Fulfilled ]
    it "a proportional bond stands or falls like a whole one until a solver sets the fill" do
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "f" [ pay "a" "A" "1" Cap [ { condition: Performed { index: 1, bond: Proportional (q "2") }, reason: Nothing } ], pay "b" "A" "1" Cap [] ] ]) `shouldEqual` Right [ Fulfilled, Fulfilled ]
    it "minting and destroying cross the boundary" do
      let
        mint = { id: "m", obligor: Named (Id "A"), effect: Move { from: K.Created, to: K.HeldBy (Named (Id "A")), resource: m, owed: Exactly (q "2") }, when: [], onShort: Reject, next: noNext }
        burn = { id: "n", obligor: Named (Id "A"), effect: Move { from: K.HeldBy (Named (Id "A")), to: K.Destroyed, resource: m, owed: Exactly (q "1") }, when: [], onShort: Reject, next: noNext }
      map (map _.fate <<< _.consumed) (resolve env [ cp 0 "z" [ mint, burn ] ]) `shouldEqual` Right [ Fulfilled, Fulfilled ]
  describe "Consume" do
    let
      parent next = (cp 0 "l" [ (pay "pay" "A" "15" Cap []) { next = next } ]) { consented = [ Id "A", Id "B" ] }
      spawn t d = { template: t, due: d, args: Map.singleton "owed" Remainder, parties: Map.singleton "who" (Named (Id "A")) }
      debt =
        { params: [ "owed" ]
        , parties: [ "who" ]
        , transaction: (cp 2 "t" [ { id: "next", obligor: Bound "who", effect: Move { from: K.HeldBy (Bound "who"), to: K.Destroyed, resource: m, owed: Arg "owed" }, when: [], onShort: Cap, next: noNext { short = [ { template: "debt", due: 1, args: Map.singleton "owed" Remainder, parties: Map.singleton "who" (Bound "who") } ] } } ])
        }
      templates = Map.singleton "debt" debt
      run c = do
        r <- lmap show (resolve env [ c ])
        lmap show (spawned 1 templates [ c ] r.consumed)
      owedOf p = map
        ( \o -> case o.effect of
            Move l -> Just l.owed
            _ -> Nothing
        )
        p.transaction.obligations
    it "a short obligation spawns its short branch, with the remainder frozen and the party bound" do
      -- 15 owed, 10 held: 5 remains, due the round after
      let out = run (parent noNext { short = [ spawn "debt" 1 ] })
      map (map _.due) out `shouldEqual` Right [ 2 ]
      map (map owedOf) out `shouldEqual` Right [ [ Just (Exactly (q "5")) ] ]
      map (map (\p -> map _.obligor p.transaction.obligations)) out `shouldEqual` Right [ [ Named (Id "A") ] ]
    it "a performed obligation spawns only its performed branch" do
      let c = (cp 0 "l" [ (pay "pay" "A" "4" Cap []) { next = noNext { performed = [ spawn "debt" 3 ], short = [ spawn "debt" 1 ] } } ]) { consented = [ Id "A" ] }
      map (map _.due) (run c) `shouldEqual` Right [ 4 ]
    it "a lapsed obligation spawns its lapsed branch" do
      let c = (cp 0 "l" [ (pay "pay" "A" "4" Cap [ { condition: Consent [ Id "B" ], reason: Nothing } ]) { next = noNext { lapsed = [ spawn "debt" 2 ] } } ]) { consented = [ Id "A" ] }
      map (map _.due) (run c) `shouldEqual` Right [ 3 ]
    it "a self-spawning template carries its party and spawns itself again" do
      let
        first = run (parent noNext { short = [ spawn "debt" 1 ] })
        second = first >>= \ps -> case ps of
          [ p ] -> do
            -- 5 owed against 10 held would perform, so run from a holder of 2
            r <- lmap show (resolve env { opening = \_ _ -> q "2" } [ p.transaction ])
            lmap show (spawned 2 templates [ p.transaction ] r.consumed)
          _ -> Left "expected one"
      map (map owedOf) second `shouldEqual` Right [ [ Just (Exactly (q "3")) ] ]
      map (map (\p -> map _.obligor p.transaction.obligations)) second `shouldEqual` Right [ [ Named (Id "A") ] ]
    it "a successor carries its parent's consent and cause" do
      map (map _.transaction.consented) (run ((parent noNext { short = [ spawn "debt" 1 ] }) { cause = Just unit })) `shouldEqual` Right [ [ Id "A", Id "B" ] ]
      map (map _.transaction.cause) (run ((parent noNext { short = [ spawn "debt" 1 ] }) { cause = Just unit })) `shouldEqual` Right [ Just unit ]
    it "a spawn due in its own round is refused" do
      run (parent noNext { short = [ spawn "debt" 0 ] }) `shouldEqual` Left (show (DueTooSoon "debt" 0))
      let zero = debt { transaction { obligations = map (\o -> o { next = noNext { short = [ spawn "debt" 0 ] } }) debt.transaction.obligations } }
      validateTemplates (Map.singleton "debt" zero) `shouldEqual` Left (DueTooSoon "debt" 0)
      validateTemplates templates `shouldEqual` Right unit
    it "a successor may not name a party its parent did not bind" do
      let stranger = (spawn "debt" 1) { parties = Map.singleton "who" (Named (Id "Z")) }
      run (parent noNext { short = [ stranger ] }) `shouldEqual` Left (show (OutsideParent "debt" (Id "Z")))
    it "partiesOf reads obligors, legs and signers; due splits what falls due" do
      partiesOf (parent noNext) `shouldEqual` [ Id "A", Id "B" ]
      let { now, later } = due 3 [ { due: 3, transaction: parent noNext }, { due: 4, transaction: parent noNext } ]
      map _.due now `shouldEqual` [ 3 ]
      map _.due later `shouldEqual` [ 4 ]

type C = Transaction Unit Unit Unit String Unit
type O = Obligation Unit Unit Unit String

env :: Kernel Unit Unit
env = { opening, holds: \_ _ -> true, read: \_ _ -> Q.zero, price: Nothing, start: Map.empty }

cp :: Int -> String -> Array O -> C
cp phase _ obligations = { obligations, ration: { tier: 0, whole: false, scaleGroup: Nothing }, phase, cause: Nothing, consented: [] }

pay :: String -> String -> String -> Short -> Array (Guarded Unit Unit String) -> O
pay id who x onShort when = { id, obligor: Named (Id who), effect: Move { from: K.HeldBy (Named (Id who)), to: K.Destroyed, resource: m, owed: Exactly (q x) }, when, onShort, next: noNext }

needs :: Int -> Guarded Unit Unit String
needs i = { condition: Performed { index: i, bond: Whole }, reason: Just "other" }

q :: String -> Q.Q
q s = fromMaybe Q.zero (Q.parse s)
