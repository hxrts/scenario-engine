-- | Primitive flows, instantiated purely through the setting DSL (docs/02 §5): a trade, a levy, a
-- | decay, and a wage, compiled over `Coupled`, against hand-computed values.
module Test.Settings.Flows (spec) where

import Testkit.Meridian (orThrow)
import Prelude

import Core.Draw (Seed(..))
import Rules.Engine.Commit (resolveRound)
import Core.Num as Q
import Rules.Query (holding)
import Rules.Slice (event, levies, read, runSlice, select, shortfalls, total, transfers)
import Rules.Types (Capability(..), Direction(..), FlowEffect(..), FlowGovernance(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleF(..), RuleForm(..), proposalId)
import Rules.World (UnseededWorld, Cause(..), Endpoint(..), Event(..), Mechanism(..), coupled, seeded)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, (:=))
import Schema.Build.World (byKey, emptyWorld, entitiesOf, noEntity, noFacility, noMarket, noResource, resourcesOf)
import Schema.Setting.World (rulesFromList)
import Schema.Recurring (Circulation, compileIn, kinds)
import Data.Map as Map
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Everything authored as the setting format's document, compiled by the DSL.
circulation :: Circulation
circulation = { production: [], flows: [ { from: Id "Emp", to: Id "Wrk", resource: Id "M", amount: Q.fromInt 10 } ], shares: [], requirements: [] }

decays :: Array { resource :: ResourceId, rate :: Q.Q }
decays = [ { resource: Id "E", rate: Q.div (Q.fromInt 1) (Q.fromInt 10) } ]

world :: UnseededWorld
world = emptyWorld
  { numeraire = Id "M"
  , resources = resourcesOf [ Tuple "M" noResource, Tuple "E" noResource ]
  , markets = byKey [ Tuple "m" (noMarket { base = Id "E", quote = Id "M", cost = { intercept: Q.fromInt 2, slope: Q.fromInt 0, domain: { lo: Q.fromInt (-100), hi: Q.fromInt 100 } }, reserves = keyed [ "E" := 50, "M" := 0 ], access = [ Id "B" ] }) ]
  , transformations = byKey [ Tuple "ship" noFacility ]
  , flowGates = [ { resource: Id "E", exemptSharedMarket: true, bundled: Just (Id "ship") } ]
  , ruleKinds = Map.singleton (Id "toll") { form: LevyFlows GovernedByDestination, text: "" }
  , entities = entitiesOf
      [ Tuple "A" (noEntity { accounts = keyed [ "M" := 100, "E" := 20 ], capabilities = [ MayTransfer (Id "E"), MayTransform (Id "ship") ] })
      , Tuple "B" (noEntity { accounts = keyed [ "M" := 100 ], capabilities = [ MayTrade (Id "m") ] })
      , Tuple "C" noEntity
      , Tuple "Emp" (noEntity { accounts = keyed [ "M" := 8 ] })
      , Tuple "Wrk" noEntity
      ]
  , rules = rulesFromList [ FlowRule { kind: Id "toll", scope: { governor: Id "C", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: Q.div (Q.fromInt 1) (Q.fromInt 10), basis: PerUnit, in: Id "M", to: LevyToDestination } } ]
  , recurring = compileIn (Id "M") circulation decays
  }

spec :: Spec Unit
spec = describe "Primitive flows through the DSL" do
  let
    w = seeded (Seed 1) world
    tradeId = proposalId 1 (Id "B") 0
    shipId = proposalId 1 (Id "A") 0
    a = Id "A"
    b = Id "B"
    c = Id "C"
    emp = Id "Emp"
    wrk = Id "Wrk"
    e = Id "E"
    m = Id "M"
    submit author p intents = { proposal: { id: p, author, intents, signers: [], procedure: Nothing }, signatures: [] }
    trade = [ Trade { market: Id "m", quantity: Q.fromInt 2, limitPrice: Nothing } ] :: Array Intent
    ship =
      [ Transform { by: a, transformation: Id "ship", quantity: Q.fromInt 5 }
      , Transfer { from: a, to: c, resource: e, quantity: Q.fromInt 5 }
      ] :: Array Intent
    resolved = resolveRound w [ submit b tradeId trade, submit a shipId ship ]

    -- every Coupled event as worked by hand from the setting's numbers
    expected :: Array Event
    expected =
      [ -- the trade: 2 of E at the flat price of 2
        coupled 1
          [ { from: InMarket (Id "m"), to: Held b, resource: e, owed: Q.fromInt 2, moved: Q.fromInt 2 }
          , { from: Held b, to: InMarket (Id "m"), resource: m, owed: Q.fromInt 4, moved: Q.fromInt 4 }
          ]
          (Just (Q.fromInt 2))
          (OfIntent { proposal: tradeId, intent: 0, mechanism: Curve (Id "m") })
      , -- the order's run: no inputs, outputs, or cost, so no legs
        coupled 1 [] Nothing (OfIntent { proposal: shipId, intent: 0, mechanism: Conversion { by: a, transformation: Id "ship", units: Q.fromInt 5 } })
      , -- the toll: 10% of 5 E, paid in the numeraire
        coupled 1 [ { from: Held a, to: Held c, resource: m, owed: Q.div (Q.fromInt 1) (Q.fromInt 2), moved: Q.div (Q.fromInt 1) (Q.fromInt 2) } ] Nothing (OfRule { kind: Id "toll" })
      , coupled 1 [ { from: Held a, to: Held c, resource: e, owed: Q.fromInt 5, moved: Q.fromInt 5 } ] Nothing (OfIntent { proposal: shipId, intent: 1, mechanism: Direct })
      , -- the wage: 10 owed, 8 held
        coupled 1 [ { from: Held emp, to: Held wrk, resource: m, owed: Q.fromInt 10, moved: Q.fromInt 8 } ] Nothing (OfRecurring { rule: Id "flow-0", kind: kinds.flow })
      , -- decay, 10% of what each holder holds after the round
        coupled 1 [ { from: Held a, to: Sink, resource: e, owed: Q.div (Q.fromInt 3) (Q.fromInt 2), moved: Q.div (Q.fromInt 3) (Q.fromInt 2) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      , coupled 1 [ { from: Held b, to: Sink, resource: e, owed: Q.div (Q.fromInt 1) (Q.fromInt 5), moved: Q.div (Q.fromInt 1) (Q.fromInt 5) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      , coupled 1 [ { from: Held c, to: Sink, resource: e, owed: Q.div (Q.fromInt 1) (Q.fromInt 2), moved: Q.div (Q.fromInt 1) (Q.fromInt 2) } ] Nothing (OfRecurring { rule: Id "decay-0", kind: kinds.decay })
      ]
  it "the engine's settlements are the setting's worked values, leg by leg" do
    r <- orThrow (resolved)
    map Coupled (select (event @"Coupled") r.record.events) `shouldEqual` expected
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
