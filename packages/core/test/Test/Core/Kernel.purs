-- | Pins: the kernel's parts, directly. Expected values are literals.
module Test.Core.Kernel (kernel) where

import Prelude

import Core.Kernel.Post (Account(..), Outcome(..), available, post)
import Core.Kernel.Types (Short(..))
import Core.Num as Q
import Core.Types (Id(..), ResourceId)
import Core.World (Endpoint(..))
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
