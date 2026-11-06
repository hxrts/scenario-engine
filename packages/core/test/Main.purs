module Test.Core.Main where

import Prelude

import Core.Draw (Seed(..), draw)
import Core.Num (Q)
import Core.Num as Q
import Core.World (Event(..))
import Core.Types (Capability(..), Controller(..), EntityKind(..), Id(..), Intent(..), Proposal, Rule(..), RuleChange(..), RuleKind(..), Transferability(..), proposalId)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (length)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.QuickCheck ((===), Result)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Any quantity with 4-dp precision in ±10,000.
genQ :: Gen Q
genQ = (\n -> Q.div (Q.fromInt n) (Q.fromInt 10000)) <$> chooseInt (-100000000) 100000000

genId :: forall t. Gen (Id t)
genId = Id <$> elements (cons' "US" [ "DE", "SA", "JP", "EU" ])

genCapability :: Gen Capability
genCapability = oneOf $ cons' (MayTrade <$> genId)
  [ MayTransfer <$> genId, MayTransform <$> genId, MayEnact <$> elements (cons' TariffRule [ AccessBanRule, WithholdingRule ]) ]

genRule :: Gen Rule
genRule = oneOf $ cons'
  ((\j o r -> Tariff { jurisdiction: j, origin: o, rate: r }) <$> genId <*> genId <*> genQ)
  [ (\j t -> AccessBan { jurisdiction: j, target: t }) <$> genId <*> genId
  , (\c u -> Withholding { capacity: c, units: u }) <$> genId <*> genQ
  ]

genIntent :: Gen Intent
genIntent = oneOf $ cons'
  ((\m q l -> Trade { market: m, quantity: q, limitPrice: l }) <$> genId <*> genQ <*> oneOf (cons' (pure Nothing) [ Just <$> genQ ]))
  [ (\f t r q -> Transfer { from: f, to: t, resource: r, quantity: q }) <$> genId <*> genId <*> genId <*> genQ
  , (\b t q -> Transform { by: b, transformation: t, quantity: q }) <$> genId <*> genId <*> genQ
  , ChangeRule <<< Enact <$> genRule
  , ChangeRule <<< Repeal <$> genId
  ]

genProposal :: Gen Proposal
genProposal = (\a is ss p -> { id: proposalId 1 a 0, author: a, intents: is, signers: ss, procedure: p })
  <$> genId
  <*> arrayOf genIntent
  <*> arrayOf genId
  <*> oneOf (cons' (pure Nothing) [ Just <$> genId ])

genKind :: Gen EntityKind
genKind = oneOf $ cons' (Actor <<< Seat <$> genId)
  [ Actor <<< NPC <$> genId
  , (\p ms w -> Aggregate { policy: p, members: ms, procedures: Map.singleton (Id "eu-trade") { weights: Map.singleton (Id "DE") w, residualWeight: w, threshold: w, authorizes: [ MayEnact TariffRule ] } })
      <$> genId
      <*> arrayOf genId
      <*> genQ
  ]

roundTrip :: forall a. Eq a => Show a => EncodeJson a => DecodeJson a => Gen a -> Gen Result
roundTrip g = g <#> \x -> decodeJson (encodeJson x) === Right x

q :: String -> Q
q s = case Q.parse s of
  Just x -> x
  Nothing -> Q.zero

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Core.Num" do
    it "round-trips through strings" $ quickCheck do
      x <- genQ
      pure (Q.parse (Q.format x) === Just x)
    it "rounds half to even" do
      Q.mul (q "0.0001") (q "0.5") `shouldEqual` q "0.0000"
      Q.mul (q "0.0003") (q "0.5") `shouldEqual` q "0.0002"
      Q.div (q "1") (q "3") `shouldEqual` q "0.3333"
      Q.div (q "-0.0001") (q "2") `shouldEqual` q "0.0000"
      Q.mul (q "2.05") (q "5") `shouldEqual` q "10.25"
    it "proRata conserves its total" $ quickCheck do
      total <- Q.fromInt <$> chooseInt 0 1000
      ws <- arrayOf (Q.fromInt <$> chooseInt 1 50)
      pure (if length ws == 0 then true === true else Q.sumQ (Q.proRata total ws) === total)
    it "proRataWhole allocates whole units by largest remainder" do
      Q.proRataWhole (Q.fromInt 30) [ Q.fromInt 15, Q.fromInt 8, Q.fromInt 9 ]
        `shouldEqual` [ Q.fromInt 14, Q.fromInt 8, Q.fromInt 8 ]
  describe "Core.Draw" do
    let key = { round: 3, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
    it "depends only on seed and key" do
      draw (Seed 26021) key `shouldEqual` draw (Seed 26021) key
    it "lies in [0, 1)" $ quickCheck do
      s <- chooseInt 0 100000
      i <- chooseInt 0 100
      let u = draw (Seed s) key { index = i }
      pure ((u >= Q.zero && u < Q.fromInt 1) === true)
    it "differs across seeds for equal keys" do
      draw (Seed 26021) key `shouldNotEqual` draw (Seed 1) key
  describe "Codecs" do
    it "Q" $ quickCheck (roundTrip genQ)
    it "Capability" $ quickCheck (roundTrip genCapability)
    it "Rule" $ quickCheck (roundTrip genRule)
    it "Intent" $ quickCheck (roundTrip genIntent)
    it "Proposal" $ quickCheck (roundTrip genProposal)
    it "EntityKind" $ quickCheck (roundTrip genKind)
    it "Event" $ quickCheck (roundTrip (genQ <#> \x -> Traded { round: 1, market: Id "energy-atlantic", entity: Id "US", quantity: x, price: x }))
    it "Transferability" $ quickCheck (roundTrip (elements (cons' Free [ Restricted, NonTransferable ])))
