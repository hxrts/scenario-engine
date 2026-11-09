-- | Pins: settlement behaviour that any rewrite of the engine must keep. Every expected value is a
-- | literal, stated from docs/02 §3, §5, §6 and §8.2.
module Test.Laws.Pins (pins) where

import Prelude

import Laws.Engine.Commit (EngineError, resolveRound)

import Core.Num as Q
import Laws.Query (holding, reserveOf)
import Laws.Types (Capability(..), Direction(..), Actor, ActorKind(..), FlowEffect(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), Payment, CandidateTransactionId, ResourceId, RuleF(..), SignedCandidateTransaction, Transformation, candidateTransactionId)
import Laws.World (Cause(..), Endpoint(..), Event(..), Flow, Mechanism(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), RejectReason(..), RoundResult, ShortfallEffect(..), WorldState, settledEvent, lapse, orderRemoved, rejection)
import Data.Either (Either(..), either)
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Laws.Build (explicitActor, baseWorld, heldDraw, exchange, q, resource, submit, transformation, withAccount)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

a :: forall t. Id t
a = Id "A"

b :: forall t. Id t
b = Id "B"

m :: ResourceId
m = Id "M"

e :: ResourceId
e = Id "E"

g :: ResourceId
g = Id "G"

held :: String -> Endpoint
held = Held <<< Id

flow :: Endpoint -> Endpoint -> ResourceId -> String -> String -> Flow
flow from to r owed moved = { from, to, resource: r, owed: q owed, moved: q moved }

pid :: String -> Int -> CandidateTransactionId
pid who = candidateTransactionId 1 (Id who)

-- | The candidate Transaction's only Intent, or the given one, moved directly.
direct :: CandidateTransactionId -> Int -> Cause
direct candidateTransaction intent = OfIntent { candidateTransaction, intent, mechanism: Direct }

converted :: CandidateTransactionId -> String -> String -> Cause
converted candidateTransaction t units = OfIntent { candidateTransaction, intent: 0, mechanism: Transformation { by: a, transformation: Id t, units: q units } }

recurring :: String -> Cause
recurring name = OfRecurring { rule: Id name, kind: Id "k" }

events :: Either EngineError RoundResult -> Array Event
events = either (const []) _.record.events

send :: String -> String -> ResourceId -> String -> Intent
send from to r x = Transfer { from: Id from, to: Id to, resource: r, quantity: q x }

-- | A joint candidate Transaction of `author`, signed by `other`.
joint :: String -> String -> Int -> Array Intent -> SignedCandidateTransaction
joint author other index intents =
  { candidateTransaction: { id: pid author index, author: Id author, intents, signers: [ Id other ], procedure: Nothing }
  , signatures: [ Id other ]
  }

world :: Array Actor -> WorldState
world es = (baseWorld es) { resources = Map.insert g (resource "G") (baseWorld es).resources }

-- | A world holding the single Transformation `t`, which A and B may run.
withTransformation :: Transformation -> Array Actor -> WorldState
withTransformation t es = (world es) { transformations = Map.singleton t.id t }

may :: String -> Array Capability
may t = [ MayTransform (Id t) ]

flat :: String -> Payment
flat p = { curve: { intercept: q p, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }, resource: m, to: b }

order :: String -> String -> Intent
order t x = Transform { by: a, transformation: Id t, quantity: q x }

rule :: String -> RecurringSource -> RecurringDestination -> ResourceId -> RecurringAmount -> RecurringFlow
rule id source destination r amount =
  { id: Id id, kind: Id "k", source, destination, resource: r, amount, onShortfall: NoEffect, offsetable: false, delay: 0 }

-- | Resolve the round and the next `n` empty rounds, returning each round's events and the last world.
play :: Int -> WorldState -> Array SignedCandidateTransaction -> Either EngineError { log :: Array (Array Event), world :: WorldState }
play n w0 inputs = do
  r <- resolveRound w0 inputs
  foldM step { log: [ r.record.events ], world: r.world } (Map.keys (Map.fromFoldable (map (\i -> Tuple i unit) (ints n))))
  where
  step acc _ = resolveRound acc.world [] <#> \r -> { log: acc.log <> [ r.record.events ], world: r.world }
  ints k = if k <= 0 then [] else ints (k - 1) <> [ k ]

pins :: Spec Unit
pins = describe "Settlement pins" do
  it "a payment above what is held caps there, recording owed and moved" do
    -- a falling curve: escrow at the ceiling of 8 is 2 × 6 = 12, the settled price at 2 units is 9, so 18 is owed and 15 held
    let
      t = (transformation "t") { payment = Just (flat "10") { curve { slope = q "-1", domain = { lo: Q.zero, hi: Q.fromInt 8 } } } }
      w = withTransformation t [ (explicitActor "A" (may "t")) { accounts = Map.singleton m (q "15") }, explicitActor "B" [] ]
    events (resolveRound w [ submit "A" 0 [ order "t" "2" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") (held "B") m "18" "15" ] Nothing (converted (pid "A" 0) "t" "2") ]

  it "a recurring payment above what is held caps there" do
    let w = (world [ explicitActor "A" [], explicitActor "B" [] ]) { recurring = [ rule "r" (FromActor a) (ToActor b) m (FixedAmount (q "150")) ] }
    events (resolveRound w []) `shouldEqual` [ settledEvent 1 [ flow (held "A") (held "B") m "150" "100" ] Nothing (recurring "r") ]

  it "a Transform's legs are inputs, then outputs, then payment" do
    let
      t = (transformation "t") { inputs = Map.singleton e (q "2"), outputs = Map.singleton g (q "1"), payment = Just (flat "3") }
      w = withTransformation t [ withAccount e (q "10") (explicitActor "A" (may "t")), explicitActor "B" [] ]
    events (resolveRound w [ submit "A" 0 [ order "t" "2" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") Sink e "4" "4", flow Source (held "A") g "2" "2", flow (held "A") (held "B") m "6" "6" ] Nothing (converted (pid "A" 0) "t" "2") ]

  it "a delayed output and effect land on their rounds, and a limited effect reverses" do
    let
      t = (transformation "t") { outputs = Map.singleton g (q "1"), delay = 2, effects = [ { resource: e, holder: Nothing, perUnit: q "2", rounds: Just 2 } ] }
      w = withTransformation t [ explicitActor "A" (may "t") ]
      cause = converted (pid "A" 0) "t" "3"
      delivered round leg = Settled { round, legs: [ leg ], rate: Nothing, cause, delivered: true }
    map _.log (play 4 w [ submit "A" 0 [ order "t" "3" ] ]) `shouldEqual` Right
      [ [ settledEvent 1 [] Nothing cause ]
      , []
      , [ delivered 3 (flow Source (held "A") g "3" "3"), delivered 3 (flow Source (held "A") e "6" "6") ]
      , []
      , [ delivered 5 (flow (held "A") Sink e "6" "6") ]
      ]

  it "a Transfer of zero emits no event; a positive Transfer that moves nothing emits one" do
    let w = world [ (explicitActor "A" []) { accounts = Map.empty }, explicitActor "B" [] ]
    events (resolveRound w [ submit "A" 0 [ send "A" "B" m "0" ] ]) `shouldEqual` []
    -- funding scales an unfunded Transfer to zero, which is also no event
    events (resolveRound w [ submit "A" 0 [ send "A" "B" m "5" ] ]) `shouldEqual` []
    events (resolveRound w { recurring = [ rule "r" (FromActor a) (ToActor b) m (FixedAmount (q "5")) ] } []) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") (held "B") m "5" "0" ] Nothing (recurring "r") ]

  it "a Transform owing a price of zero has no payment leg" do
    let
      t = (transformation "t") { outputs = Map.singleton g (q "1"), payment = Just (flat "0") }
      w = withTransformation t [ explicitActor "A" (may "t"), explicitActor "B" [] ]
    events (resolveRound w [ submit "A" 0 [ order "t" "2" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow Source (held "A") g "2" "2" ] Nothing (converted (pid "A" 0) "t" "2") ]

  it "a levy to the sink posts before its Transfer, as its own event" do
    let
      levy = FlowRule { kind: Id "tariff", scope: { governor: b, counterparty: a, direction: Inbound, over: [ e ] }, effect: Levy { rate: q "0.5", basis: PerUnit, in: m, to: LevyToSink } }
      w = (world [ withAccount e (q "10") (explicitActor "A" []), explicitActor "B" [] ]) { rules = Map.singleton (Id "l") levy }
    events (resolveRound w [ submit "A" 0 [ send "A" "B" e "4" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") Sink m "2" "2" ] Nothing (OfRule { kind: Id "tariff" })
      , settledEvent 1 [ flow (held "A") (held "B") e "4" "4" ] Nothing (direct (pid "A" 0) 0)
      ]

  it "a consume's shortfall offsets an incoming flow" do
    -- 6 short at 2 per unit accrues 12, taken off the 20 owed to A
    let
      w = (world [ withAccount e (q "4") (explicitActor "A" []), explicitActor "B" [] ])
        { recurring =
            [ (rule "use" (FromActor a) ToSink e (FixedAmount (q "10"))) { onShortfall = OffsetIncoming (q "2") }
            , (rule "pay" (FromActor b) (ToActor a) m (FixedAmount (q "20"))) { offsetable = true }
            ]
        }
    events (resolveRound w []) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") Sink e "10" "4" ] Nothing (recurring "use")
      , settledEvent 1 [ flow (held "B") (held "A") m "8" "8" ] Nothing (recurring "pay")
      ]

  it "decay takes from every holder, in Actor order, and from no Exchange" do
    let
      w = (world [ withAccount e (q "10") (explicitActor "A" []), withAccount e (q "5") (explicitActor "B" []) ])
        { exchanges = Map.singleton (Id "EM") (exchange [])
        , recurring = [ rule "d" FromEachHolder ToSink e (FractionHeld (q "0.1")) ]
        }
    events (resolveRound w []) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") Sink e "1" "1" ] Nothing (recurring "d")
      , settledEvent 1 [ flow (held "B") Sink e "0.5" "0.5" ] Nothing (recurring "d")
      ]

  it "shortage law 1: one short Resource scales everything the Actor executes" do
    -- E: 5 held of 10 claimed, a ratio of 0.5, which also halves the Money Transfer
    let w = world [ withAccount e (q "5") (explicitActor "A" []), explicitActor "B" [] ]
    events (resolveRound w [ submit "A" 0 [ send "A" "B" e "10" ], submit "A" 1 [ send "A" "B" m "20" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") (held "B") e "5" "5" ] Nothing (direct (pid "A" 0) 0)
      , settledEvent 1 [ flow (held "A") (held "B") m "10" "10" ] Nothing (direct (pid "A" 1) 0)
      ]

  it "shortage law 2: joint CandidateTransactions fill first and are never scaled" do
    let w = world [ explicitActor "A" [], explicitActor "B" [] ]
    events (resolveRound w [ joint "A" "B" 0 [ send "A" "B" m "80" ], submit "A" 1 [ send "A" "B" m "40" ] ]) `shouldEqual`
      [ settledEvent 1 [ flow (held "A") (held "B") m "80" "80" ] Nothing (direct (pid "A" 0) 0)
      , settledEvent 1 [ flow (held "A") (held "B") m "20" "20" ] Nothing (direct (pid "A" 1) 0)
      ]

  it "shortage law 2: an overflowing party loses every joint CandidateTransaction claiming the Resource" do
    let w = world [ explicitActor "A" [], explicitActor "B" [] ]
    events (resolveRound w [ joint "A" "B" 0 [ send "A" "B" m "60" ], joint "A" "B" 1 [ send "A" "B" m "60" ], submit "A" 2 [ send "A" "B" m "10" ] ]) `shouldEqual`
      [ rejection 1 (pid "A" 0) PartyCannotFund
      , rejection 1 (pid "A" 1) PartyCannotFund
      , settledEvent 1 [ flow (held "A") (held "B") m "10" "10" ] Nothing (direct (pid "A" 2) 0)
      ]

  it "shortage law 3: a sale overdrawing quote reserves drops the sell side, and buys still clear" do
    let
      venue = (exchange [ "A", "B" ]) { position = Q.fromInt 10, reserves = Map.fromFoldable [ Tuple e (Q.fromInt 50), Tuple m (Q.fromInt 1) ] }
      trader name = (explicitActor name [ MayTrade (Id "EM") ])
      w = (world [ withAccount e (q "5") (trader "A"), trader "B" ]) { exchanges = Map.singleton venue.id venue }
      trade x = Trade { exchange: venue.id, quantity: q x, limitPrice: Nothing }
      curve candidateTransaction actor = OfIntent { candidateTransaction, intent: 0, mechanism: Curve venue.id } # \c -> { c, actor }
      result = resolveRound w [ submit "A" 0 [ trade "-5" ], submit "B" 0 [ trade "2" ] ]
    events result `shouldEqual`
      [ settledEvent 1 [ flow (InExchange venue.id) (held "B") e "2" "2", flow (held "B") (InExchange venue.id) m "2" "2" ] (Just (q "1")) (curve (pid "B" 0) "B").c
      , orderRemoved 1 (pid "A" 0) QuoteReservesShort
      ]
    map (\r -> { reserve: reserveOf r.world venue.id m, sold: holding r.world a e }) result `shouldEqual` Right { reserve: q "3", sold: q "5" }

  it "a joint CandidateTransaction in an overflowing pool is rejected, with single orders sharing the pool" do
    -- the pool can draw 5; joints claim 6, so both go and the single order of 4 fills whole
    let
      t = (transformation "t") { outputs = Map.singleton g (q "1"), draws = [ heldDraw e (q "1") ] }
      w = withTransformation t [ explicitActor "A" (may "t"), withAccount e (q "5") (explicitActor "B" []) ]
    events (resolveRound w [ joint "A" "B" 0 [ order "t" "3" ], joint "A" "B" 1 [ order "t" "3" ], submit "A" 2 [ order "t" "4" ] ]) `shouldEqual`
      [ rejection 1 (pid "A" 0) PoolExceeded
      , rejection 1 (pid "A" 1) PoolExceeded
      , settledEvent 1 [ flow Source (held "A") g "4" "4" ] Nothing (OfIntent { candidateTransaction: pid "A" 2, intent: 0, mechanism: Transformation { by: a, transformation: Id "t", units: q "4" } })
      ]

  it "a procedure passes only on strictly more than its threshold" do
    let
      proc = { weights: Map.singleton a (q "0.5"), residualWeight: Q.zero, threshold: q "0.5", authorizes: [] }
      bloc = (explicitActor "EU" []) { kind = Aggregate { strategy: Id "p", members: [ a ], procedures: Map.singleton (Id "vote") proc } }
      w = world [ bloc, explicitActor "A" [], explicitActor "B" [] ]
      id = pid "EU" 0
      signed = { candidateTransaction: { id, author: Id "EU", intents: [ send "EU" "B" m "1" ], signers: [ a ], procedure: Just (Id "vote") }, signatures: [ a ] }
    events (resolveRound w [ signed ]) `shouldEqual`
      [ ProcedureOutcome { round: 1, candidateTransaction: id, support: q "0.5", threshold: q "0.5", executed: false }
      , lapse 1 id
      ]

