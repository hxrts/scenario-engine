module Test.Core.Main where

import Prelude

import JS.BigInt (BigInt)
import JS.BigInt as BI
import Core.Draw (Seed(..), draw)
import Core.Engine (EngineError, prepareRound, resolveRound)
import Core.Invariants (check)
import Core.Market (averagePrice, clear)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capacityHeld, holding)
import Core.Represent as Represent
import Core.World (Event(..), RoundResult, Target(..), WorldState)
import Core.Types (Capability(..), Controller(..), Entity, Market, EntityKind(..), Id(..), Intent(..), Proposal, Rule(..), RuleChange(..), RuleKind(..), SignedProposal, Transferability(..), proposalId)
import Data.Tuple (Tuple(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, concatMap, filter, length, zip)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
  ((\m n l -> Trade { market: m, quantity: n, limitPrice: l }) <$> genId <*> genQ <*> oneOf (cons' (pure Nothing) [ Just <$> genQ ]))
  [ (\f t r n -> Transfer { from: f, to: t, resource: r, quantity: n }) <$> genId <*> genId <*> genId <*> genQ
  , (\b t n -> Transform { by: b, transformation: t, quantity: n }) <$> genId <*> genId <*> genQ
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

-- | The 0.0001 scale, as a BigInt, for checks that work on raw units.
scale4 :: BigInt
scale4 = BI.fromInt 10000

bigAbs :: BigInt -> BigInt
bigAbs x = if x < BI.fromInt 0 then negate x else x

-- Engine fixtures -----------------------------------------------------------------

-- | An Actor holding 100 of the numeraire M and the given capabilities.
actor :: String -> Array Capability -> Entity
actor name caps =
  { id: Id name
  , accounts: Map.singleton (Id "M") (Q.fromInt 100)
  , capacities: Map.empty
  , capabilities: caps
  , kind: Actor (Seat (Id name))
  }

-- | A World State over the given Entities, with resources M (numeraire) and E, no markets, and
-- | no rules, in round 1.
baseWorld :: Array Entity -> WorldState
baseWorld es =
  { round: 1
  , seed: Seed 1
  , entities: Map.fromFoldable (map (\e -> Tuple e.id e) es)
  , resources: Map.fromFoldable (map (\r -> Tuple (Id r) { id: Id r, unit: r, transferability: Free, decay: Nothing, reservable: false }) [ "M", "E" ])
  , markets: Map.empty
  , transformations: Map.empty
  , representations: []
  , numeraire: Id "M"
  , transport: Map.empty
  , jurisdiction: Map.empty
  , rules: Map.empty
  , scheduled: Map.empty
  , deliveries: []
  , factors: Map.empty
  , circulation: { production: [], flows: [], shares: [], requirements: [] }
  , events: []
  }

-- | An ordinary, unsigned-by-others Proposal from `author`, numbered `index` in round 1.
submit :: String -> Int -> Array Intent -> SignedProposal
submit author index intents =
  { proposal: { id: proposalId 1 (Id author) index, author: Id author, intents, signers: [], procedure: Nothing }
  , signatures: []
  }

-- | The quantity of a Transformed event.
transformedQ :: Event -> Array Q
transformedQ = case _ of
  Transformed t -> [ t.quantity ]
  _ -> []

-- | The reasons of the Rejected events of a resolved round (or the engine error).
rejections :: Either EngineError RoundResult -> Array String
rejections = case _ of
  Right r -> concatMap reason r.record.events
  Left e -> [ show e ]
  where
  reason = case _ of
    Rejected x -> [ x.reason ]
    _ -> []

eventsOf :: Either EngineError RoundResult -> Array Event
eventsOf = case _ of
  Right r -> r.record.events
  Left _ -> []

-- | A Market for E against M, cost base 1 and slope 0 over [0, 100], with the given access.
market :: Array String -> Market
market access =
  { id: Id "EM"
  , base: Id "E"
  , quote: Id "M"
  , cost: { base: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }
  , position: Q.zero
  , reserves: Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 50), Tuple (Id "M") (Q.fromInt 50) ]
  , access: map Id access
  }

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
    it "proRataWhole floors a fractional total to whole units" do
      Q.proRataWhole (q "30.5") [ Q.fromInt 15, Q.fromInt 8, Q.fromInt 9 ]
        `shouldEqual` [ Q.fromInt 14, Q.fromInt 8, Q.fromInt 8 ]
    it "allocates nothing when the weights sum to zero" do
      Q.proRataWhole (Q.fromInt 30) [ Q.zero, Q.zero ] `shouldEqual` [ Q.zero, Q.zero ]
    it "parses only an optional sign, digits, and an optional fraction" do
      Q.parse "--1.5" `shouldEqual` Nothing
      Q.parse "1.-5" `shouldEqual` Nothing
      Q.parse "1..5" `shouldEqual` Nothing
      Q.parse "1." `shouldEqual` Nothing
      Q.parse " 1" `shouldEqual` Nothing
      Q.parse "-" `shouldEqual` Nothing
      Q.parse "+2.5" `shouldEqual` Just (q "2.5")
      Q.parse "-0.25" `shouldEqual` Just (q "-0.25")
    it "divMaybe and divFloor refuse a zero divisor; divFloor truncates to the grid" do
      Q.divMaybe (q "1") Q.zero `shouldEqual` Nothing
      Q.divFloor (q "1") Q.zero `shouldEqual` Nothing
      Q.divFloor (q "2") (q "3") `shouldEqual` Just (q "0.6666")
      Q.divFloor (q "-1") (q "3") `shouldEqual` Just (q "-0.3333")
      Q.divFloor (q "1") (q "-3") `shouldEqual` Just (q "-0.3333")
    it "a quotient that rounds up can overdraw its numerator, so a scale must use divFloor" do
      Q.mul (Q.div (q "2") (q "3")) (q "3") `shouldEqual` q "2.0001"
      Q.mul (fromMaybe Q.zero (Q.divFloor (q "2") (q "3"))) (q "3") `shouldEqual` q "1.9998"
    it "a negative total is allocated by its magnitude and then negated" do
      Q.proRata (q "-0.0002") [ Q.fromInt 1, Q.fromInt 1, Q.fromInt 1 ] `shouldEqual` [ q "-0.0001", q "-0.0001", Q.zero ]
      Q.divMaybe (q "2") (q "4") `shouldEqual` Just (q "0.5")
    it "format always writes exactly four decimals" do
      Q.format (Q.fromInt 3) `shouldEqual` "3.0000"
      Q.format (q "-0.5") `shouldEqual` "-0.5000"
      Q.format (q "0.0001") `shouldEqual` "0.0001"
    it "parse is strict: more than four decimals is an error, never a silent rounding" do
      Q.parse "0.00005" `shouldEqual` Nothing
      Q.parse "1.00000" `shouldEqual` Nothing
      Q.parse "1.0000" `shouldEqual` Just (Q.fromInt 1)
      Q.parse "" `shouldEqual` Nothing
      Q.parse "1e3" `shouldEqual` Nothing
    it "rounds the 0.00005 and 0.00015 ties half to even" do
      Q.div (q "0.0001") (q "2") `shouldEqual` q "0.0000"
      Q.div (q "0.0003") (q "2") `shouldEqual` q "0.0002"
      Q.div (q "0.0005") (q "2") `shouldEqual` q "0.0002"
      Q.div (q "0.0007") (q "2") `shouldEqual` q "0.0004"
      Q.div (q "-0.0003") (q "2") `shouldEqual` q "-0.0002"
      Q.mul (q "0.0005") (q "0.5") `shouldEqual` q "0.0002"
      Q.mul (q "0.0015") (q "0.1") `shouldEqual` q "0.0002"
    it "a product or quotient is symmetric under negation" $ quickCheck do
      a <- genQ
      b <- genQ
      pure ((Q.mul (Q.neg a) b == Q.neg (Q.mul a b)) && (Q.div (Q.neg a) b == Q.neg (Q.div a b)) === true)
    it "divMaybe is the nearest grid point: the remainder is at most half a step" $ quickCheck do
      a <- genQ
      b <- genQ
      pure $ case Q.divMaybe a b of
        Nothing -> Q.toUnits b === BI.fromInt 0
        Just r -> (bigAbs (BI.fromInt 2 * Q.toUnits r * Q.toUnits b - BI.fromInt 2 * Q.toUnits a * scale4) <= bigAbs (Q.toUnits b)) === true
    it "divFloor never grants more than its numerator for non-negative operands" $ quickCheck do
      a <- Q.abs <$> genQ
      b <- Q.abs <$> genQ
      pure $ case Q.divFloor a b of
        Nothing -> Q.toUnits b === BI.fromInt 0
        Just r -> ((Q.toUnits r * Q.toUnits b <= Q.toUnits a * scale4) === true)
    it "floorInt is a true floor, so negative quantities round down" do
      Q.floorInt (q "0.9999") `shouldEqual` 0
      Q.floorInt (q "-0.0001") `shouldEqual` (-1)
      Q.floorInt (q "-0.5") `shouldEqual` (-1)
      Q.floorInt (q "-2") `shouldEqual` (-2)
      Q.floorInt (q "2.5") `shouldEqual` 2
    it "proRata conserves the total exactly, for any sign and any non-negative weights" $ quickCheck do
      total <- genQ
      ws <- arrayOf (Q.abs <$> genQ)
      pure (if Q.sumQ ws == Q.zero then Q.sumQ (Q.proRata total ws) === Q.zero else Q.sumQ (Q.proRata total ws) === total)
    it "proRataWhole conserves the whole-unit part of a non-negative total exactly" $ quickCheck do
      total <- Q.abs <$> genQ
      ws <- arrayOf (Q.abs <$> genQ)
      let whole = Q.fromUnits (Q.toUnits total `div` scale4 * scale4)
      pure (if length ws == 0 || Q.sumQ ws == Q.zero then true === true else Q.sumQ (Q.proRataWhole total ws) === whole)
    it "proRata gives a zero-weight entry nothing, and shares never go negative for non-negative totals" $ quickCheck do
      total <- Q.abs <$> genQ
      ws <- arrayOf (Q.abs <$> genQ)
      pure ((all (\(Tuple w sh) -> w /= Q.zero || sh == Q.zero) (zip ws (Q.proRata total ws)) && all (_ >= Q.zero) (Q.proRata total ws)) === true)
    it "toDisplayNumber is the only float boundary and is exact for display" do
      Q.toDisplayNumber (q "1.5") `shouldEqual` 1.5
      Q.toDisplayNumber (q "-0.0001") `shouldEqual` (-0.0001)
  describe "Core.Market clearing" do
    let
      cost = { base: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.fromInt 0, hi: Q.fromInt 100 } }
      order qty = { quantity: Q.fromInt qty, limitPrice: Nothing, tag: 0 }
      sideSum sign fills = Q.sumQ (map _.quantity (filter (\f -> sign f.quantity) fills))
    it "a bound on net buying rations buys; sells fill in full" do
      -- buys 10, sells 4, net held at 3: buys are rationed to 7, sells all 4
      let r = clear cost Q.zero { minNet: Q.fromInt (-100), maxNet: Q.fromInt 3 } [ order 10, order (-4) ]
      sideSum (_ > Q.zero) r.fills `shouldEqual` Q.fromInt 7
      sideSum (_ < Q.zero) r.fills `shouldEqual` Q.fromInt (-4)
      Q.sumQ (map _.quantity r.fills) `shouldEqual` Q.fromInt 3
    it "a bound on net selling rations sells; buys fill in full" do
      -- buys 4, sells 10, net held at -3: buys all 4, sells rationed to 7
      let r = clear cost Q.zero { minNet: Q.fromInt (-3), maxNet: Q.fromInt 100 } [ order 4, order (-10) ]
      sideSum (_ > Q.zero) r.fills `shouldEqual` Q.fromInt 4
      sideSum (_ < Q.zero) r.fills `shouldEqual` Q.fromInt (-7)
      Q.sumQ (map _.quantity r.fills) `shouldEqual` Q.fromInt (-3)

  describe "Core.Market prices" do
    it "averagePrice is the marginal price at the midpoint of the move" do
      let c = { base: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.zero, hi: Q.fromInt 100 } }
      -- C(q) = q + q²/2, so C(3) - C(2) = 3.5
      averagePrice c (Q.fromInt 2) (Q.fromInt 1) `shouldEqual` q "3.5"
      averagePrice c (Q.fromInt 2) Q.zero `shouldEqual` q "3"
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
  describe "Core.Engine" do
    it "rejects a Trade on an unknown market" do
      let w = baseWorld [ actor "US" [ MayTrade (Id "XX") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Trade { market: Id "XX", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` [ "unknown market XX" ]
    it "rejects a Transform of an unknown transformation" do
      let w = baseWorld [ actor "US" [ MayTransform (Id "F") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "F", quantity: Q.fromInt 1 } ] ])
        `shouldEqual` [ "unknown transformation F" ]
    it "rejects a Transfer to an unknown entity" do
      let w = baseWorld [ actor "US" [ MayTransfer (Id "M") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Transfer { from: Id "US", to: Id "NOBODY", resource: Id "M", quantity: Q.fromInt 1 } ] ])
        `shouldEqual` [ "unknown entity NOBODY" ]
    it "rejects a Trade by an Entity outside the Market's access" do
      let w = (baseWorld [ actor "US" [ MayTrade (Id "EM") ] ]) { markets = Map.singleton (Id "EM") (market [ "EU" ]) }
      rejections (resolveRound w [ submit "US" 0 [ Trade { market: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` [ "US has no access to EM" ]
    it "accepts a Trade by an Entity with access" do
      let w = (baseWorld [ actor "US" [ MayTrade (Id "EM") ] ]) { markets = Map.singleton (Id "EM") (market [ "US" ]) }
      rejections (resolveRound w [ submit "US" 0 [ Trade { market: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` []
    it "rejects repealing an unknown rule" do
      let w = baseWorld [ actor "US" [ MayEnact TariffRule ] ]
      rejections (resolveRound w [ submit "US" 0 [ ChangeRule (Repeal (Id "nope")) ] ])
        `shouldEqual` [ "unknown rule nope" ]
    it "rejects every copy of a duplicated ProposalId" do
      let w = baseWorld [ actor "US" [] ]
      rejections (resolveRound w [ submit "US" 0 [], submit "US" 0 [] ])
        `shouldEqual` [ "duplicate proposal id", "duplicate proposal id" ]
    it "a procedure's enactment is limited to its own jurisdiction" do
      let
        proc = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: q "0.5", authorizes: [ MayEnact TariffRule ] }
        eu = (actor "EU" []) { kind = Aggregate { policy: Id "p", members: [], procedures: Map.singleton (Id "trade") proc } }
        tariff j = Tariff { jurisdiction: j, origin: Id "US", rate: q "0.1" }
        procedureSubmit j =
          { proposal: { id: proposalId 1 (Id "EU") 0, author: Id "EU", intents: [ ChangeRule (Enact (tariff j)) ], signers: [], procedure: Just (Id "trade") }
          , signatures: []
          }
        w = baseWorld [ eu ]
      rejections (resolveRound w [ procedureSubmit (Id "DE") ]) `shouldEqual` [ "a Tariff applies only to the enacting jurisdiction" ]
      rejections (resolveRound w [ procedureSubmit (Id "EU") ]) `shouldEqual` []
    it "a procedure's Intents face the same market access check" do
      let
        proc = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: q "0.5", authorizes: [ MayTrade (Id "EM") ] }
        eu = (actor "EU" []) { kind = Aggregate { policy: Id "p", members: [], procedures: Map.singleton (Id "trade") proc } }
        w = (baseWorld [ eu ]) { markets = Map.singleton (Id "EM") (market [ "US" ]) }
        p =
          { id: proposalId 1 (Id "EU") 0
          , author: Id "EU"
          , intents: [ Trade { market: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ]
          , signers: []
          , procedure: Just (Id "trade")
          }
      rejections (resolveRound w [ { proposal: p, signatures: [] } ]) `shouldEqual` [ "EU has no access to EM" ]
    it "an overdrawing Transfer records a shortfall and the round proceeds" do
      let w = baseWorld [ actor "US" [ MayTransfer (Id "M") ], actor "UK" [] ]
      case resolveRound w [ submit "US" 0 [ Transfer { from: Id "US", to: Id "UK", resource: Id "M", quantity: Q.fromInt 500 } ] ] of
        Right r -> do
          holding r.world (Id "US") (Id "M") `shouldEqual` Q.zero
          holding r.world (Id "UK") (Id "M") `shouldEqual` Q.fromInt 200
        Left e -> show e `shouldEqual` "a round"
    it "caps an uncapped facility's fills at its ceiling, so escrow bounds the settled price" do
      let
        fac = { id: Id "F", inputs: Map.empty, outputs: Map.empty, reserves: Map.empty, cost: Just { base: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 10 } }, capacity: Nothing, payee: Nothing, delay: 0, minimum: Q.zero, maximum: Nothing, capacityUse: Q.fromInt 1, ownerSigns: false, effects: [] }
        w = (baseWorld [ actor "US" [ MayTransform (Id "F") ], actor "UK" [ MayTransform (Id "F") ] ]) { transformations = Map.singleton (Id "F") fac }
        order e = submit e 0 [ Transform { by: Id e, transformation: Id "F", quantity: Q.fromInt 8 } ]
        filled = concatMap transformedQ (eventsOf (resolveRound w [ order "US", order "UK" ]))
      Q.sumQ filled `shouldEqual` Q.fromInt 10
    it "escrows a pooled facility at the pool ceiling, so a member with a larger capacityUse is still bounded" do
      let
        cost = { base: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.zero, hi: Q.fromInt 10 } }
        fac name use = { id: Id name, inputs: Map.empty, outputs: Map.empty, reserves: Map.empty, cost: Just cost, capacity: Just (Id "plant"), payee: Nothing, delay: 0, minimum: Q.zero, maximum: Nothing, capacityUse: Q.fromInt use, ownerSigns: false, effects: [] }
        us = (actor "US" [ MayTransform (Id "FA") ]) { capacities = Map.singleton (Id "plant") (Q.fromInt 10) }
        -- UK holds exactly the escrow of its order at the old per-facility ceiling (capacity / 2 = 5, price 1 + 5/2 = 3.5)
        uk = (actor "UK" [ MayTransform (Id "FB") ]) { accounts = Map.singleton (Id "M") (Q.div (Q.fromInt 7) (Q.fromInt 2)) }
        w = (baseWorld [ us, uk ]) { transformations = Map.fromFoldable [ Tuple (Id "FA") (fac "FA" 1), Tuple (Id "FB") (fac "FB" 2) ] }
        -- 8 units of FA (use 1) and 1 unit of FB (use 2) fill the whole capacity of 10: the pool total is 9
        r = resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "FA", quantity: Q.fromInt 8 } ], submit "UK" 0 [ Transform { by: Id "UK", transformation: Id "FB", quantity: Q.fromInt 1 } ] ]
      filter
        ( \e -> case e of
            Shortfall _ -> true
            _ -> false
        )
        (eventsOf r) `shouldEqual` []
    it "clamps a delivery at zero and records only the change applied" do
      let
        us = (actor "US" []) { capacities = Map.singleton (Id "plant") (Q.fromInt 5) }
        w = (baseWorld [ us ]) { deliveries = [ { round: 1, entity: Id "US", target: CapacityOf (Id "plant"), quantity: Q.fromInt (-10) } ] }
        prepared = prepareRound w
      capacityHeld prepared (Id "US") (Id "plant") `shouldEqual` Q.zero
      prepared.events `shouldEqual` [ Delivered { round: 1, entity: Id "US", target: CapacityOf (Id "plant"), quantity: Q.fromInt (-5) } ]
    it "a delivery to an unknown Entity applies nothing and records nothing" do
      let w = (baseWorld [ actor "US" [] ]) { deliveries = [ { round: 1, entity: Id "GHOST", target: Holding (Id "M"), quantity: Q.fromInt 5 } ] }
      (prepareRound w).events `shouldEqual` []
    it "prepareRound is idempotent, and resolveRound applies due deliveries itself" do
      let w = (baseWorld [ actor "US" [] ]) { deliveries = [ { round: 1, entity: Id "US", target: Holding (Id "M"), quantity: Q.fromInt 7 } ] }
      (prepareRound (prepareRound w)).events `shouldEqual` (prepareRound w).events
      map (\r -> holding r.world (Id "US") (Id "M")) (resolveRound w []) `shouldEqual` Right (Q.fromInt 107)
      map (\r -> holding r.world (Id "US") (Id "M")) (resolveRound (prepareRound w) []) `shouldEqual` Right (Q.fromInt 107)
    it "a Representation with a zero reference contributes no estimate" do
      let
        rep = { id: Id "r", actor: Id "US", aggregate: Id "EU", domain: Id "E", weight: Q.fromInt 1, estimator: { reference: Q.zero, shrinkage: Q.fromInt 1, deviationBound: Nothing, noiseSpread: Q.zero } }
        w = (baseWorld [ actor "US" [ MayTrade (Id "EM") ] ]) { markets = Map.singleton (Id "EM") (market [ "US" ]), representations = [ rep ] }
        p = { id: proposalId 1 (Id "US") 0, author: Id "US", intents: [ Trade { market: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ], signers: [], procedure: Nothing }
      (Represent.estimate w [ p ]).events `shouldEqual` []

  describe "Reservations (docs/02 §2.3, §8.2)" do
    it "a reservation escrows the holder's Mil, scales a competing Transfer, and consumes nothing" do
      let
        mil = { id: Id "Mil", unit: "Mil", transferability: Free, decay: Nothing, reservable: true }
        dep = { id: Id "dep", inputs: Map.empty, outputs: Map.empty, reserves: Map.singleton (Id "Mil") (Q.fromInt 3), cost: Nothing, capacity: Nothing, payee: Nothing, delay: 0, minimum: Q.zero, maximum: Nothing, capacityUse: Q.fromInt 1, ownerSigns: false, effects: [] }
        us = (actor "US" [ MayTransform (Id "dep"), MayTransfer (Id "Mil") ]) { accounts = Map.singleton (Id "Mil") (Q.fromInt 10) }
        w = (baseWorld [ us, actor "JP" [] ]) { resources = Map.insert (Id "Mil") mil (baseWorld []).resources, transformations = Map.singleton (Id "dep") dep }
        -- 2 units reserve 6 Mil and the Transfer asks 10: 16 escrowed against 10 held, so both scale by 5/8
        r = resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "dep", quantity: Q.fromInt 2 } ], submit "US" 1 [ Transfer { from: Id "US", to: Id "JP", resource: Id "Mil", quantity: Q.fromInt 10 } ] ]
      case r of
        Left e -> show e `shouldEqual` "a round"
        Right res -> do
          check w res `shouldEqual` []
          holding res.world (Id "US") (Id "Mil") `shouldEqual` Q.div (Q.fromInt 15) (Q.fromInt 4)
          holding res.world (Id "JP") (Id "Mil") `shouldEqual` Q.div (Q.fromInt 25) (Q.fromInt 4)
          concatMap transformedQ res.record.events `shouldEqual` [ Q.fromInt 5 `Q.div` Q.fromInt 4 ]
          filter
            ( \e -> case e of
                Consumed _ -> true
                _ -> false
            )
            res.record.events `shouldEqual` []
          -- the reservation is released at commit: the next round starts with nothing reserved
          case resolveRound res.world [ submit "US" 0 [ Transfer { from: Id "US", to: Id "JP", resource: Id "Mil", quantity: Q.div (Q.fromInt 15) (Q.fromInt 4) } ] ] of
            Left e -> show e `shouldEqual` "a round"
            Right next -> holding next.world (Id "US") (Id "Mil") `shouldEqual` Q.zero
    it "a reservation alongside inputs leaves the reserved Resource unchanged and still consumes the inputs" do
      let
        dep = { id: Id "dep", inputs: Map.singleton (Id "E") (Q.fromInt 1), outputs: Map.empty, reserves: Map.singleton (Id "Mil") (Q.fromInt 3), cost: Nothing, capacity: Nothing, payee: Nothing, delay: 0, minimum: Q.zero, maximum: Nothing, capacityUse: Q.fromInt 1, ownerSigns: false, effects: [] }
        us = (actor "US" [ MayTransform (Id "dep") ]) { accounts = Map.fromFoldable [ Tuple (Id "Mil") (Q.fromInt 10), Tuple (Id "E") (Q.fromInt 4) ] }
        mil = { id: Id "Mil", unit: "Mil", transferability: Free, decay: Nothing, reservable: true }
        w = (baseWorld [ us ]) { resources = Map.insert (Id "Mil") mil (baseWorld []).resources, transformations = Map.singleton (Id "dep") dep }
      case resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "dep", quantity: Q.fromInt 2 } ] ] of
        Left e -> show e `shouldEqual` "a round"
        Right res -> do
          check w res `shouldEqual` []
          holding res.world (Id "US") (Id "Mil") `shouldEqual` Q.fromInt 10
          holding res.world (Id "US") (Id "E") `shouldEqual` Q.fromInt 2

  describe "Codecs" do
    it "Q" $ quickCheck (roundTrip genQ)
    it "Capability" $ quickCheck (roundTrip genCapability)
    it "Rule" $ quickCheck (roundTrip genRule)
    it "Intent" $ quickCheck (roundTrip genIntent)
    it "Proposal" $ quickCheck (roundTrip genProposal)
    it "EntityKind" $ quickCheck (roundTrip genKind)
    it "Event" $ quickCheck (roundTrip (genQ <#> \x -> Traded { round: 1, market: Id "energy-atlantic", entity: Id "US", quantity: x, price: x }))
    it "Transferability" $ quickCheck (roundTrip (elements (cons' Free [ Restricted, NonTransferable ])))
