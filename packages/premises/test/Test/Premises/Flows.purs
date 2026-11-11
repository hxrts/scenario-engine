-- | Primitive flows, instantiated purely through the premise DSL (docs/02 §5): a trade, a levy, a
-- | decay, and a wage, compiled over `Settled`, against hand-computed values.
module Test.Premises.Flows (spec) where

import Testkit.Meridian (orThrow)
import Prelude

import Core.Draw (Seed(..))
import Laws.Engine.Commit (resolveRound)
import Core.Num as Q
import Laws.Query (holding)
import Laws.Slice (event, levies, read, runSlice, select, shortfalls, total, transfers)
import Laws.Types (Capability(..), Direction(..), FlowEffect(..), FlowGovernance(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleF(..), RuleForm(..), candidateTransactionId)
import Laws.World (UnseededWorld, Cause(..), Endpoint(..), Event(..), Mechanism(..), settledEvent, seeded)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, (:=))
import Schema.Build.World (byKey, emptyWorld, actorsOf, noActor, noTransformation, noExchange, noResource, resourcesOf)
import Schema.Premise.World (rulesFromList)
import Schema.Recurring (Circulation, compileIn, kinds)
import Data.Map as Map
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Everything authored as the premise format's document, compiled by the DSL.
circulation :: Circulation
circulation = { production: [], flows: [ { from: Id "Emp", to: Id "Wrk", resource: Id "M", amount: Q.fromInt 10 } ], shares: [], requirements: [] }

decays :: Array { resource :: ResourceId, rate :: Q.Q }
decays = [ { resource: Id "E", rate: Q.div (Q.fromInt 1) (Q.fromInt 10) } ]

world :: UnseededWorld
world = emptyWorld
  { numeraire = Id "M"
  , resources = resourcesOf [ Tuple "M" noResource, Tuple "E" noResource ]
  , exchanges = byKey [ Tuple "m" (noExchange { base = Id "E", quote = Id "M", cost = { intercept: Q.fromInt 2, slope: Q.fromInt 0, domain: { lo: Q.fromInt (-100), hi: Q.fromInt 100 } }, reserves = keyed [ "E" := 50, "M" := 0 ], access = [ Id "B" ] }) ]
  , transformations = byKey [ Tuple "ship" noTransformation ]
  , flowGates = [ { resource: Id "E", exemptSharedExchange: true, bundled: Just (Id "ship") } ]
  , ruleKinds = Map.singleton (Id "toll") { form: LevyFlows GovernedByDestination, text: "" }
  , actors = actorsOf
      [ Tuple "A" (noActor { accounts = keyed [ "M" := 100, "E" := 20 ], capabilities = [ MayTransfer (Id "E"), MayTransform (Id "ship") ] })
      , Tuple "B" (noActor { accounts = keyed [ "M" := 100 ], capabilities = [ MayTrade (Id "m") ] })
      , Tuple "C" noActor
      , Tuple "Emp" (noActor { accounts = keyed [ "M" := 8 ] })
      , Tuple "Wrk" noActor
      ]
  , rules = rulesFromList [ FlowRule { kind: Id "toll", scope: { governor: Id "C", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: Q.div (Q.fromInt 1) (Q.fromInt 10), basis: PerUnit, in: Id "M", to: LevyToDestination } } ]
  , recurring = compileIn (Id "M") circulation decays
  }

spec :: Spec Unit
spec = describe "Primitive flows through the DSL" do
  let
    w = seeded (Seed 1) world
    tradeId = candidateTransactionId 1 (Id "B") 0
    shipId = candidateTransactionId 1 (Id "A") 0
    a = Id "A"
    b = Id "B"
    c = Id "C"
    emp = Id "Emp"
    wrk = Id "Wrk"
    e = Id "E"
    m = Id "M"
    submit author p intents = { candidateTransaction: { id: p, author, intents, signers: [], procedure: Nothing }, signatures: [] }
    trade = [ Trade { exchange: Id "m", quantity: Q.fromInt 2, limitPrice: Nothing } ] :: Array Intent
    ship =
      [ Transform { by: a, transformation: Id "ship", quantity: Q.fromInt 5 }
      , Transfer { from: a, to: c, resource: e, quantity: Q.fromInt 5 }
      ] :: Array Intent
    resolved = resolveRound w [ submit b tradeId trade, submit a shipId ship ]

    -- every Settled event as worked by hand from the premise's numbers
    expected :: Array Event
    expected =
      [ -- the trade: 2 of E at the flat price of 2
        settledEvent 1
          [ { from: InExchange (Id "m"), to: Held b, resource: e, owed: Q.fromInt 2, moved: Q.fromInt 2 }
          , { from: Held b, to: InExchange (Id "m"), resource: m, owed: Q.fromInt 4, moved: Q.fromInt 4 }
          ]
          (Just (Q.fromInt 2))
          (OfMatch { exchange: Id "m", buyer: Just { candidateTransaction: tradeId, intent: 0 }, seller: Nothing })
      , -- the order's run: no inputs, outputs, or cost, so no settled Flows
        settledEvent 1 [] Nothing (OfIntent { candidateTransaction: shipId, intent: 0, mechanism: Transformation { by: a, transformation: Id "ship", units: Q.fromInt 5 } })
      , -- the toll: 10% of 5 E, paid in the numeraire
        settledEvent 1 [ { from: Held a, to: Held c, resource: m, owed: Q.div (Q.fromInt 1) (Q.fromInt 2), moved: Q.div (Q.fromInt 1) (Q.fromInt 2) } ] Nothing (OfRule { kind: Id "toll" })
      , settledEvent 1 [ { from: Held a, to: Held c, resource: e, owed: Q.fromInt 5, moved: Q.fromInt 5 } ] Nothing (OfIntent { candidateTransaction: shipId, intent: 1, mechanism: Direct })
      , -- the wage: 10 owed, 8 held
        settledEvent 1 [ { from: Held emp, to: Held wrk, resource: m, owed: Q.fromInt 10, moved: Q.fromInt 8 } ] Nothing (OfRecurring { rule: Id "flow-0", kind: kinds.flow })
      , -- decay, 10% of what each holder holds after the round
        settledEvent 1 [ { from: Held a, to: Sink, resource: e, owed: Q.div (Q.fromInt 3) (Q.fromInt 2), moved: Q.div (Q.fromInt 3) (Q.fromInt 2) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      , settledEvent 1 [ { from: Held b, to: Sink, resource: e, owed: Q.div (Q.fromInt 1) (Q.fromInt 5), moved: Q.div (Q.fromInt 1) (Q.fromInt 5) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      , settledEvent 1 [ { from: Held c, to: Sink, resource: e, owed: Q.div (Q.fromInt 1) (Q.fromInt 2), moved: Q.div (Q.fromInt 1) (Q.fromInt 2) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      ]
  it "the engine's settlements are the premise's worked values, leg by leg" do
    r <- orThrow (resolved)
    map Settled (select (event @"Settled") r.record.events) `shouldEqual` expected
    -- each flow read through the slices the views use
    map _.owed (select levies r.record.events) `shouldEqual` [ Q.div (Q.fromInt 1) (Q.fromInt 2) ]
    map _.quantity (select transfers r.record.events) `shouldEqual` [ Q.fromInt 5, Q.fromInt 8 ]
    map _.quantity (select shortfalls r.record.events) `shouldEqual` [ Q.fromInt 2 ]
    runSlice (total (read @"amount" levies)) r.record.events `shouldEqual` Q.div (Q.fromInt 1) (Q.fromInt 2)
    for_
      [ { e: a, res: e, is: Q.div (Q.fromInt 27) (Q.fromInt 2) }
      , { e: a, res: m, is: Q.div (Q.fromInt 199) (Q.fromInt 2) }
      , { e: b, res: e, is: Q.div (Q.fromInt 9) (Q.fromInt 5) }
      , { e: b, res: m, is: Q.fromInt 96 }
      , { e: c, res: e, is: Q.div (Q.fromInt 9) (Q.fromInt 2) }
      , { e: c, res: m, is: Q.div (Q.fromInt 1) (Q.fromInt 2) }
      , { e: emp, res: m, is: Q.fromInt 0 }
      , { e: wrk, res: m, is: Q.fromInt 8 }
      ]
      \x -> holding r.world x.e x.res `shouldEqual` x.is
