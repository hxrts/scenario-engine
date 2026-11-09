-- | New designs written at the DSL layer only (work plan P16). Expected values are hand-computed in the comments.
module Test.Premises.Agreements (spec) where

import Prelude

import Laws.Slice (event, read, select)

import Core.Map (values)

import Core.Shape (Format(..), decodeJsonWith, encodeWith)
import Data.Either (Either(..), either)
import Laws.World (PendingTransaction(..), RejectReason(..), authoredLawShape, worldTemplateShape, LegQuery, RecurringAmount(..), RecurringDestination(..), RecurringSource(FromEachHolder), ShortfallEffect(..), WorldState, seeded)

import Core.Draw (Seed(..))
import Core.Kernel.Types (Account(..), Amount(..), Bond(..), Condition(..), Next, Party(..), Short(..), noNext)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Laws.Engine.Commit (resolveRound)
import Laws.Invariants (checkAfter)
import Laws.Query (holding)
import Laws.Types (Capability(..), Direction(..), ActorId, FlowEffect(..), FlowGovernance(..), RuleForm(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceTag, Rule, RuleF(..), RuleKindId, SignedCandidateTransaction, candidateTransactionId)
import Schema.Build.Agreement (Clause, Guard, agreement, onAction, clause, due, guard, held, repeal, spawn, step)
import Schema.Build.Base (keyed, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, actorsOf, noActor, noTransformation, noExchange, noResource, resourcesOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

money :: Id ResourceTag
money = Id "M"

goods :: Id ResourceTag
goods = Id "G"

plant :: Id ResourceTag
plant = Id "plant"

a :: ActorId
a = Id "A"

b :: ActorId
b = Id "B"

world :: WorldState
world = seeded (Seed 1) emptyWorld
  { round = 1
  , numeraire = money
  , ruleKinds = Map.fromFoldable [ Tuple (Id "tariff") { form: LevyFlows GovernedByDestination, text: "" }, Tuple kind { form: LevyFlows GovernedByDestination, text: "" } ]
  , resources = resourcesOf [ Tuple "M" noResource, Tuple "G" noResource, Tuple "plant" noResource ]
  , actors = actorsOf
      [ Tuple "A" noActor { accounts = keyed [ money := 100, goods := 10, plant := 20 ], capabilities = [ MayTransform (Id "run") ] }
      , Tuple "B" noActor { accounts = keyed [ money := 100 ] }
      ]
  }

-- | Resolve one round, failing the test on any invariant, including stock-flow reconciliation (INV-05).
play :: WorldState -> Array SignedCandidateTransaction -> WorldState
play w inputs = either (unsafeCrashWith <<< show) (\r -> check r.world (checkAfter w r)) (resolveRound w inputs)
  where
  check final = case _ of
    [] -> final
    failed -> unsafeCrashWith (show failed)

-- | The World after `n` empty rounds.
after :: Int -> WorldState -> WorldState
after n w = foldl (\x _ -> play x []) w (Map.keys (Map.fromFoldable (map (\i -> Tuple i unit) (upTo n))))
  where
  upTo k = if k <= 0 then [] else upTo (k - 1) <> [ k ]

submit :: String -> Int -> Array Intent -> SignedCandidateTransaction
submit who round intents = { candidateTransaction: { id: candidateTransactionId round (Id who) 0, author: Id who, intents, signers: [ Id who ], procedure: Nothing }, signatures: [] }

kind :: RuleKindId
kind = Id "agreement"

named :: ActorId -> Party
named = Named

move :: String -> ActorId -> ActorId -> Id ResourceTag -> String -> Short -> Array Guard -> Next LegQuery -> Clause
move id from to resource x = clause id (named from) { from: held (named from), to: held (named to), resource, owed: Exactly (q x) }

needs :: Int -> Guard
needs index = guard (Performed { index, bond: Whole })

has :: ActorId -> Id ResourceTag -> String -> Guard
has who resource x = guard (StockAtLeast { stock: { at: held (named who), resource }, amount: Exactly (q x) })

levyRule :: Rule
levyRule = FlowRule { kind: Id "tariff", scope: { governor: b, counterparty: a, direction: Inbound, over: [ goods ] }, effect: Levy { rate: q 1, basis: PerUnit, in: money, to: LevyToGovernor } }

spec :: Spec Unit
spec = describe "New designs, written with no core change" do
  it "depreciation is a time law on a stock: 20 at 10% is 18, then 16.2" do
    let w = world { recurring = [ { id: Id "dep", kind: Id "dep", source: FromEachHolder, destination: ToSink, resource: plant, amount: FractionHeld (q "0.1"), onShortfall: NoEffect, offsetable: false, delay: 0 } ] }
    holding (after 1 w) a plant `shouldEqual` q 18
    holding (after 2 w) a plant `shouldEqual` q "16.2"

  it "wear from use is an action law on a stock: 3 units run takes 3 of plant" do
    let
      w = world { transformations = byKey [ Tuple "run" noTransformation { outputs = keyed [ goods := 1 ] } ], laws = [ onAction kind "run" Destroyed plant (q 1) ] }
      ran = play w [ submit "A" 1 [ Transform { by: a, transformation: Id "run", quantity: q 3 } ] ]
    holding ran a plant `shouldEqual` q 17
    holding ran a goods `shouldEqual` q 13

  it "a tariff on a traded good is an action law on its Exchange: 4 bought at 1 pays 2 to the governor" do
    let
      venue = noExchange { base = goods, quote = money, cost = { intercept: q 1, slope: q 0, domain: { lo: q 0, hi: q 100 } }, reserves = keyed [ goods := 50, money := 50 ], access = [ a ] }
      w = world
        { exchanges = byKey [ Tuple "goods" venue ]
        , laws = [ onAction kind "goods" (held (named b)) money (q "0.5") ]
        , actors = Map.update (Just <<< \e -> e { capabilities = [ MayTrade (Id "goods") ] }) a world.actors
        }
      done = play w [ submit "A" 1 [ Trade { exchange: Id "goods", quantity: q 4, limitPrice: Just (q 1) } ] ]
    holding done a goods `shouldEqual` q 14
    holding done a money `shouldEqual` q 94
    holding done b money `shouldEqual` q 102

  describe "a due obligation in a settlement phase is rationed against the round's orders by its tier" do
    let
      debt tier whole' = (agreement kind [ b ] [ move "owe" b a money "80" Cap [] (noNext { lapsed = [ spawn "note" 1 [] [ Tuple "who" (named b) ] ] }) ])
        { phase = 1, ration = { tier, whole: whole', scaleGroup: Nothing } }
      note = step "note" [] [ "who" ] (agreement kind [] [ clause "note" (Bound "who") { from: Created, to: held (Bound "who"), resource: goods, owed: Exactly (q 1) } Cap [] noNext ])
      order = [ submit "B" 1 [ Transfer { from: b, to: a, resource: money, quantity: q 40 } ] ]
      run tier whole' = play (world { pending = [ due 1 (debt tier whole') ], templates = Map.fromFoldable [ note ] }) order
    it "a senior debt fills first and scales the order: 80 paid, then 20 of the 40" do
      holding (run 0 true) a money `shouldEqual` q 200
      holding (run 0 true) b money `shouldEqual` q 0
    it "a junior debt is scaled to what the order leaves: 40 paid, then 60 of the 80" do
      holding (run 2 false) a money `shouldEqual` q 200
      (run 2 false).pending `shouldEqual` []
    it "a junior whole debt that overflows lapses, and spawns its lapsed branch" do
      let lapsed = run 2 true
      holding lapsed a money `shouldEqual` q 140
      holding (play lapsed []) b goods `shouldEqual` q 1

  it "a tariff is a levy on the flow of a traded good, paid to the governor" do
    let done = play (world { rules = Map.singleton (Id "l") levyRule }) [ submit "A" 1 [ Transfer { from: a, to: b, resource: goods, quantity: q 4 } ] ]
    holding done b goods `shouldEqual` q 4
    holding done a money `shouldEqual` q 96
    holding done b money `shouldEqual` q 104

  describe "a bilateral agreement: B's payment is conditional on A's delivery and on B's stock" do
    let
      deal over goodsOwed = agreement kind [ a, b ]
        [ move "deliver" a b goods goodsOwed Reject [] noNext
        , move "pay" b a money "5" Cap [ needs 0, has b money over ] noNext
        ]
    it "performs when both hold" do
      let done = play (world { pending = [ due 1 (deal "50" "2") ] }) []
      holding done b goods `shouldEqual` q 2
      holding done a money `shouldEqual` q 105
    it "the delivery stands alone when the stock condition fails, and the payment lapses" do
      let done = play (world { pending = [ due 1 (deal "200" "2") ] }) []
      holding done b goods `shouldEqual` q 2
      holding done a money `shouldEqual` q 100
    it "nothing moves when the delivery cannot be made" do
      let done = play (world { pending = [ due 1 (deal "50" "50") ] }) []
      holding done b goods `shouldEqual` q 0
      holding done a money `shouldEqual` q 100

  describe "a treaty binds removing a levy to a payment" do
    let
      treaty payment = agreement kind [ a, b ]
        [ repeal "lift" (named b) (Id "l") [ needs 1 ]
        , move "pay" a b money payment Reject [ needs 0 ] noNext
        ]
      inForce = world { rules = Map.singleton (Id "l") levyRule }
      ship w = play w [ submit "A" 2 [ Transfer { from: a, to: b, resource: goods, quantity: q 2 } ] ]
    it "lifts the levy and takes the payment together" do
      let done = play (inForce { pending = [ due 1 (treaty "10") ] }) []
      holding done a money `shouldEqual` q 90
      Map.isEmpty done.rules `shouldEqual` true
      -- no levy on what ships next
      holding (ship done) a money `shouldEqual` q 90
    it "does neither if the payment cannot be made" do
      let done = play (inForce { pending = [ due 1 (treaty "200") ] }) []
      holding done a money `shouldEqual` q 100
      Map.size done.rules `shouldEqual` 1

  it "a loan is a self-spawning template, with interest on the remainder and a default that seizes collateral" do
    -- A lends 10, repayable at 50, the next round. B holds 110 less what it spends below.
    let
      lender = Bound "lender"
      borrower = Bound "borrower"
      pays = { from: held borrower, to: held lender, resource: money, owed: Arg "balance" }
      service = clause "service" borrower pays Cap [ guard (StockAtLeast { stock: { at: held borrower, resource: money }, amount: Exactly (q 1) }) ]
        noNext
          { short = [ spawn "loan" 1 [ Tuple "balance" (Times { ratio: q "1.1", amount: Remainder }) ] [ Tuple "borrower" borrower, Tuple "lender" lender ] ]
          , lapsed = [ spawn "default" 1 [] [ Tuple "borrower" borrower, Tuple "lender" lender ] ]
          }
      seize = clause "seize" borrower { from: held borrower, to: held lender, resource: plant, owed: Exactly (q 7) } Cap [] noNext
      origination = agreement kind [ a, b ]
        [ clause "advance" (named a) { from: held (named a), to: held (named b), resource: money, owed: Exactly (q 10) } Reject []
            noNext { performed = [ spawn "loan" 1 [ Tuple "balance" (Times { ratio: q 5, amount: Moved }) ] [ Tuple "borrower" (named b), Tuple "lender" (named a) ] ] }
        ]
      start = world
        { pending = [ due 1 origination ]
        , templates = Map.fromFoldable [ step "loan" [ "balance" ] [ "borrower", "lender" ] (agreement kind [] [ service ]), step "default" [] [ "borrower", "lender" ] (agreement kind [] [ seize ]) ]
        , actors = Map.update (Just <<< \e -> e { accounts = keyed [ money := 10, plant := 9 ] }) b world.actors
        }
      -- round 1: B holds 10 + 10 = 20, A 90
      r1 = play start []
      -- round 2: 50 owed, 20 held: 20 paid, 30 remains, carried at 33
      r2 = play r1 []
      -- round 3: B holds nothing, so the service condition fails and the loan lapses
      r3 = play r2 []
      -- round 4: the default seizes the collateral: 7 of B's 9
      r4 = play r3 []
    holding r1 b money `shouldEqual` q 20
    holding r1 a money `shouldEqual` q 90
    holding r2 b money `shouldEqual` q 0
    holding r2 a money `shouldEqual` q 110
    holding r3 a money `shouldEqual` q 110
    holding r4 a plant `shouldEqual` q 27
    holding r4 b plant `shouldEqual` q 2
    r4.pending `shouldEqual` []

  it "a vesting schedule is a finite chain of templates" do
    let
      grantee = Bound "grantee"
      grant name next = agreement kind [ a ]
        [ clause name grantee { from: Created, to: held grantee, resource: goods, owed: Exactly (q 1) } Cap [] (noNext { performed = next }) ]
      chain n = [ spawn n 2 [] [ Tuple "grantee" grantee ] ]
      root = agreement kind [ a, b ]
        [ clause "award" (named a) { from: Created, to: held (named b), resource: goods, owed: Exactly (q 1) } Cap [] (noNext { performed = [ spawn "second" 2 [] [ Tuple "grantee" (named b) ] ] }) ]
      w = world { pending = [ due 1 root ], templates = Map.fromFoldable [ step "second" [] [ "grantee" ] (grant "second" (chain "third")), step "third" [] [ "grantee" ] (grant "third" []) ] }
    -- vests in rounds 1, 3 and 5, and then stops
    holding (after 1 w) b goods `shouldEqual` q 1
    holding (after 2 w) b goods `shouldEqual` q 1
    holding (after 3 w) b goods `shouldEqual` q 2
    holding (after 5 w) b goods `shouldEqual` q 3
    holding (after 7 w) b goods `shouldEqual` q 3
    (after 7 w).pending `shouldEqual` []

  it "a successor naming a party its parent did not bind fails the round" do
    let
      parent = agreement kind [ a ]
        [ clause "open" (named a) { from: Created, to: held (named a), resource: goods, owed: Exactly (q 1) } Cap [] (noNext { performed = [ spawn "next" 1 [] [ Tuple "who" (named b) ] ] }) ]
      w = world { pending = [ due 1 parent ], templates = Map.fromFoldable [ step "next" [] [ "who" ] (agreement kind [] []) ] }
    either show (const "resolved") (resolveRound w []) `shouldEqual` "invalid successor: (OutsideParent \"next\" B)"

  describe "an agreement entered during play" do
    let
      lender = Bound "lender"
      borrower = Bound "borrower"
      service = clause "service" borrower { from: held borrower, to: held lender, resource: money, owed: Arg "balance" } Cap [ guard (StockAtLeast { stock: { at: held borrower, resource: money }, amount: Exactly (q 1) }) ]
        noNext
          { short = [ spawn "loan" 1 [ Tuple "balance" (Times { ratio: q "1.1", amount: Remainder }) ] both ]
          , lapsed = [ spawn "default" 1 [] both ]
          }
      both = [ Tuple "borrower" borrower, Tuple "lender" lender ]
      seize = clause "seize" borrower { from: held borrower, to: held lender, resource: plant, owed: Exactly (q 7) } Cap [] noNext
      origination = agreement kind []
        [ clause "advance" lender { from: held lender, to: held borrower, resource: money, owed: Arg "principal" } Reject []
            noNext { performed = [ spawn "loan" 1 [ Tuple "balance" (Times { ratio: q 5, amount: Moved }) ] both ] }
        ]
      templates = Map.fromFoldable
        [ step "origination" [ "principal" ] [ "borrower", "lender" ] origination
        , step "loan" [ "balance" ] [ "borrower", "lender" ] (agreement kind [] [ service ])
        , step "default" [] [ "borrower", "lender" ] (agreement kind [] [ seize ])
        ]
      lending = world { templates = templates, actors = Map.update (Just <<< \e -> e { accounts = keyed [ money := 10, plant := 9 ] }) b world.actors }
      entering args parties = Enter { template: "origination", args: map (\(Tuple name value) -> { name, value }) args, parties: map (\(Tuple role actor) -> { role, actor }) parties }
      fullLoan = entering [ Tuple "principal" (q 10) ] [ Tuple "lender" a, Tuple "borrower" b ]
      joint intents signatures = { candidateTransaction: { id: candidateTransactionId 1 a 0, author: a, intents, signers: [ b ], procedure: Nothing }, signatures }
      rejectedBy w input = select (read @"reason" (event @"Rejected")) (either (const []) _.record.events (resolveRound w [ input ]))
    it "a loan entered in round 1 settles from round 2 through to default" do
      let
        -- round 1: nothing moves; the first step is due next round, with both signatures carried
        r1 = play lending [ joint [ fullLoan ] [ b ] ]
        -- round 2: A advances 10, so B holds 20 and A 90; 50 falls due next round
        r2 = play r1 []
        -- round 3: 50 owed and 20 held: 20 paid, 30 remains, carried at 33
        r3 = play r2 []
        -- round 4: B holds nothing, so the service condition fails and the loan lapses
        r4 = play r3 []
        -- round 5: the default seizes 7 of B's 9
        r5 = play r4 []
      holding r1 a money `shouldEqual` q 100
      map (\(PendingTransaction p) -> p.transaction.consented) r1.pending `shouldEqual` [ [ a, b ] ]
      holding r2 b money `shouldEqual` q 20
      holding r3 a money `shouldEqual` q 110
      holding r3 b money `shouldEqual` q 0
      holding r4 a money `shouldEqual` q 110
      holding r5 a plant `shouldEqual` q 27
      holding r5 b plant `shouldEqual` q 2
      r5.pending `shouldEqual` []
    it "a treaty entered during play lifts the levy and takes the payment together" do
      let
        treaty = agreement kind []
          [ repeal "lift" (Bound "governor") (Id "l") [ needs 1 ]
          , clause "pay" (Bound "payer") { from: held (Bound "payer"), to: held (Bound "governor"), resource: money, owed: Arg "payment" } Reject [ needs 0 ] noNext
          ]
        w = world { rules = Map.singleton (Id "l") levyRule, templates = Map.fromFoldable [ step "treaty" [ "payment" ] [ "payer", "governor" ] treaty ] }
        enter' = Enter { template: "treaty", args: [ { name: "payment", value: q 10 } ], parties: [ { role: "payer", actor: a }, { role: "governor", actor: b } ] }
        r1 = play w [ joint [ enter' ] [ b ] ]
        r2 = play r1 []
      Map.size r1.rules `shouldEqual` 1
      holding r2 a money `shouldEqual` q 90
      holding r2 b money `shouldEqual` q 110
      Map.isEmpty r2.rules `shouldEqual` true
    it "an unknown template, a missing argument and a missing party are refused" do
      rejectedBy lending (joint [ Enter { template: "nothing", args: [], parties: [ { role: "lender", actor: a } ] } ] [ b ]) `shouldEqual` [ UnknownAgreement "nothing" ]
      rejectedBy lending (joint [ entering [] [ Tuple "lender" a, Tuple "borrower" b ] ] [ b ]) `shouldEqual` [ MissingArgument { template: "origination", name: "principal" } ]
      rejectedBy lending (joint [ entering [ Tuple "principal" (q 10) ] [ Tuple "lender" a ] ] [ b ]) `shouldEqual` [ MissingParty { template: "origination", name: "borrower" } ]
    it "a party who is not asked to sign refuses the agreement, and one who does not sign lapses it" do
      let alone = { candidateTransaction: { id: candidateTransactionId 1 a 0, author: a, intents: [ fullLoan ], signers: [], procedure: Nothing }, signatures: [] }
      rejectedBy lending alone `shouldEqual` [ MustSign b ]
      (play lending [ joint [ fullLoan ] [] ]).pending `shouldEqual` []
    it "templates and laws read and write through the Premise format" do
      let
        law = onAction kind "run" Destroyed plant (q "0.5")
      map (\t -> decodeJsonWith Authored worldTemplateShape (encodeWith Authored worldTemplateShape t) == Right t) (values templates) `shouldEqual` [ true, true, true ]
      decodeJsonWith Authored authoredLawShape (encodeWith Authored authoredLawShape law) `shouldEqual` Right law

  describe "a renegotiable step needs a fresh signature" do
    let
      renew = agreement kind []
        [ clause "renew" (Bound "payer") { from: held (Bound "payer"), to: held (named a), resource: money, owed: Exactly (q 5) } Cap [ guard (Consent [ b ]) ] noNext ]
      parent signers = agreement kind signers
        [ clause "open" (named a) { from: Created, to: held (named b), resource: goods, owed: Exactly (q 1) } Cap [] (noNext { performed = [ spawn "renew" 1 [] [ Tuple "payer" (named b) ] ] }) ]
      w signers = world { pending = [ due 1 (parent signers) ], templates = Map.fromFoldable [ step "renew" [] [ "payer" ] renew ] }
    it "lapses when the party has not signed" do
      holding (after 2 (w [ a ])) a money `shouldEqual` q 100
    it "performs when the party's consent is carried" do
      holding (after 2 (w [ a, b ])) a money `shouldEqual` q 105
