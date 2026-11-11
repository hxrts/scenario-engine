-- | Pins: expected engine values are the worked arithmetic of docs/02 §5 to §9 and the numerics of docs/02 §9.1, each case stating its calculation.
module Test.Laws.Main where

import Prelude

import JS.BigInt (BigInt)
import JS.BigInt as BI
import Core.Draw (Seed(..), draw)
import Laws.Engine.Commit (EngineError, resolveRound)
import Laws.Engine.Compile (delivery)
import Laws.Authority (authorized, viewFacts, worldFacts)
import Laws.Engine.Prepare (observe, prepareRound)
import Data.Foldable (for_)
import Laws.Invariants (check, orderIndependent, orderIndependentBy, checkAfter)
import Core.Exchange (averagePrice, clear)
import Core.Num (Q, qShape)
import Core.Num as Q
import Core.Shape (Format(..), Shape, encodeWith, fieldOf, mapOf, object, recordShape)
import Core.Shape as Shape
import Laws.Query (drawable, holderOf, holding)
import Laws.Represent as Represent
import Laws.Slice (runLegQuery, select, transformationFills, count, describeSlice, event, exchangeFills, items, legs, levies, matches, orKind, pickOf, read, runSlice, shortfalls, total, whereEq, whereNamed)
import Laws.World (PendingTransaction(..), Cause(..), CausePattern(..), Endpoint(..), Event(..), LegAggregate(..), Mechanism(..), RecurringAmount(..), RecurringDestination(..), RecurringFlow, RecurringSource(..), RejectReason(..), RoundResult, ShortfallEffect(..), Side(..), Window(..), costFunctionShape, settledEvent, renderReason)
import Laws.Types (Capability(..), Direction(..), ActorId, ActorKind(..), FlowEffect(..), FlowGovernance(..), Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), Exchange, ExchangeTag, CandidateTransaction, ResourceId, Rule, RuleChangeF(..), RuleF(..), RuleForm(..), Transferability(..), TransformationId, conforms, idShape, jointIntent, candidateTransactionId, ruleIdOf, submissionIndex)
import Data.Tuple (Tuple(..), snd)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, concatMap, filter, length, zip)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.String (Pattern(..)) as S
import Data.String as Str
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck ((===), Result)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf)
import Test.Laws.Build (heldDraw, ownDraw, withAccount, explicitActor, banR, baseWorld, holdR, exchange, onActor, priced, q, representation, resource, submit, tariffR, transformation)
import Test.Laws.Encoding (encoding)
import Test.Laws.Pins (pins)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldEqual, shouldNotEqual)
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
  [ MayTransfer <$> genId, MayTransform <$> genId, MayEnact <$> elements (cons' (Id "tariff") [ Id "accessBan", Id "withholding" ]) ]

genRule :: Gen Rule
genRule = oneOf $ cons'
  ((\j o r -> tariffR { jurisdiction: j, origin: o, rate: r }) <$> genId <*> genId <*> genQ)
  [ (\j t -> banR { jurisdiction: j, target: t }) <$> genId <*> genId
  , (\c u -> holdR { capacity: c, units: u }) <$> genId <*> genQ
  ]

genIntent :: Gen Intent
genIntent = oneOf $ cons'
  ((\m n l -> Trade { exchange: m, quantity: n, limitPrice: l }) <$> genId <*> genQ <*> oneOf (cons' (pure Nothing) [ Just <$> genQ ]))
  [ (\b c n l -> Route { pair: { base: b, quote: c }, quantity: n, limitPrice: l }) <$> genId <*> genId <*> genQ <*> oneOf (cons' (pure Nothing) [ Just <$> genQ ])
  , (\f t r n -> Transfer { from: f, to: t, resource: r, quantity: n }) <$> genId <*> genId <*> genId <*> genQ
  , (\b t n -> Transform { by: b, transformation: t, quantity: n }) <$> genId <*> genId <*> genQ
  , ChangeRule <<< Enact <$> genRule
  , ChangeRule <<< Repeal <$> genId
  ]

genCandidateTransaction :: Gen CandidateTransaction
genCandidateTransaction = (\a is ss p -> { id: candidateTransactionId 1 a 0, author: a, intents: is, signers: ss, procedure: p })
  <$> genId
  <*> arrayOf genIntent
  <*> arrayOf genId
  <*> oneOf (cons' (pure Nothing) [ Just <$> genId ])

genKind :: Gen ActorKind
genKind = oneOf $ cons' (PlayableActor <$> genId)
  [ NonPlayableActor <$> genId
  , (\p ms w -> Aggregate { strategy: p, members: ms, procedures: Map.singleton (Id "eu-trade") { weights: Map.singleton (Id "DE") w, residualWeight: w, threshold: w, authorizes: [ MayEnact (Id "tariff") ] } })
      <$> genId
      <*> arrayOf genId
      <*> genQ
  ]

roundTrip :: forall a. Eq a => Show a => EncodeJson a => DecodeJson a => Gen a -> Gen Result
roundTrip g = g <#> \x -> decodeJson (encodeJson x) === Right x

-- | A displayed value is within half a hundredth of the exact one and has at most two decimals: a
-- | fixed kind keeps exactly two, a trimmed kind keeps none that are trailing zeros. The "<0.01"
-- | marker stands only for a non-zero value under a hundredth.
displayWithin :: Boolean -> Q -> String -> Boolean
displayWithin fixed x s
  | Str.take 1 s == "<" || Str.take 2 s == "-<" = Q.abs x /= Q.zero && Q.abs x < q "0.01"
  | otherwise =
      case Q.parse s of
        Nothing -> false
        Just p -> bigAbs (Q.toUnits x - Q.toUnits p) <= BI.fromInt 50 && decimalsOk
      where
      decimalsOk = case fraction of
        Nothing -> not fixed
        Just f
          | fixed -> Str.length f == 2
          | otherwise -> Str.length f <= 2 && Str.take 1 (Str.drop (Str.length f - 1) f) /= "0"
      fraction = map (\i -> Str.drop (i + 1) s) (Str.indexOf (S.Pattern ".") s)

-- | The 0.0001 scale, as a BigInt, for checks that work on raw units.
scale4 :: BigInt
scale4 = BI.fromInt 10000

bigAbs :: BigInt -> BigInt
bigAbs x = if x < BI.fromInt 0 then negate x else x

-- Engine fixtures -----------------------------------------------------------------

-- | A trade's settlement, its base and quote settled Flows built from the signed quantity and price.
tradeE :: Id ExchangeTag -> ActorId -> Q -> Q -> Event
tradeE m e x price = settledEvent 1
  [ { from: InExchange m, to: Held e, resource: Id "E", owed: x, moved: x }
  , { from: Held e, to: InExchange m, resource: Id "M", owed: Q.mul price x, moved: Q.mul price x }
  ]
  (Just price)
  (OfMatch { exchange: m, buyer: Just { candidateTransaction: Id "none", intent: 0 }, seller: Nothing })

-- | A transformation's settlement of `units`, with no settled Flows.
convE :: ActorId -> TransformationId -> Q -> Event
convE by t units = settledEvent 1 [] Nothing (OfIntent { candidateTransaction: Id "none", intent: 0, mechanism: Transformation { by, transformation: t, units } })

testCause :: Cause
testCause = OfRecurring { rule: Id "r", kind: Id "k" }

-- | A recurring-flow rule of a local kind (core tests cannot use the DSL).
testRule :: String -> RecurringSource -> RecurringDestination -> ResourceId -> RecurringAmount -> RecurringFlow
testRule kind source destination resource amount = { id: Id ("test-" <> kind), kind: Id kind, source, destination, resource, amount, onShortfall: NoEffect, offsetable: false, delay: 0 }

-- | The quantity of a transformation's settlement.
transformedQ :: Event -> Array Q
transformedQ = (\ev_ -> maybe [] (\t -> [ t.quantity ]) (pickOf (transformationFills) ev_))

-- | The reasons of the Rejected events of a resolved round (or the engine error).
rejections :: Either EngineError RoundResult -> Array String
rejections = case _ of
  Right r -> concatMap reason r.record.events
  Left e -> [ show e ]
  where
  reason = (\ev_ -> maybe [] (\x -> [ renderReason x.reason ]) (pickOf (event @"Rejected") ev_))

isShortfall :: Event -> Boolean
isShortfall = (matches (shortfalls) (const true))

eventsOf :: Either EngineError RoundResult -> Array Event
eventsOf = case _ of
  Right r -> r.record.events
  Left _ -> []

-- | A routed buy (positive) or sell (negative) of `n` E for M, at `limit`.
routed :: String -> Int -> String -> Intent
routed base n limit = Route { pair: { base: Id base, quote: Id "M" }, quantity: Q.fromInt n, limitPrice: Just (q limit) }

-- | The (Exchange, quantity) of each fill of an Actor in a resolved round.
fillsOf :: String -> Either EngineError RoundResult -> Array (Tuple String Q)
fillsOf who r = concatMap
  (\ev_ -> maybe [] (\t -> if t.actor == Id who then [ Tuple (show t.exchange) t.quantity ] else []) (pickOf (exchangeFills) ev_))
  (eventsOf r)

-- | The quantity an Actor filled in one Exchange.
fillIn :: String -> String -> Either EngineError RoundResult -> Q
fillIn exchangeId who r = Q.sumQ (map snd (filter (\(Tuple m _) -> m == exchangeId) (fillsOf who r)))

-- | The Exchange record as hand-written fields, which `recordShape` must reproduce (format pin).
exchangeObject :: Shape Exchange
exchangeObject = object "Exchange"
  ( { id: _, base: _, quote: _, cost: _, position: _, reserves: _, access: _ }
      <$> fieldOf "id" "" idShape _.id
      <*> fieldOf "base" "" idShape _.base
      <*> fieldOf "quote" "" idShape _.quote
      <*> fieldOf "cost" "" costFunctionShape _.cost
      <*> fieldOf "position" "" qShape _.position
      <*> fieldOf "reserves" "" (mapOf idShape qShape) _.reserves
      <*> fieldOf "access" "" (Shape.arrayOf idShape) _.access
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  pins
  encoding
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
    it "display rounds half to even at two decimals" do
      Q.displayRatio (q "0.125") `shouldEqual` "0.12"
      Q.displayRatio (q "0.135") `shouldEqual` "0.14"
      Q.displayRatio (q "-0.125") `shouldEqual` "-0.12"
      Q.displayQuantity (q "2.675") `shouldEqual` "2.68"
      Q.displayQuantity (q "2.665") `shouldEqual` "2.66"
      Q.displayQuantity (q "12.125") `shouldEqual` "12.12"
      Q.displayQuantity (q "12.375") `shouldEqual` "12.38"
    it "display trims trailing zeros and shows the kind's precision" do
      Q.displayQuantity (Q.fromInt 12) `shouldEqual` "12"
      Q.displayQuantity (q "12.5") `shouldEqual` "12.5"
      Q.displayQuantity (q "12.25") `shouldEqual` "12.25"
      Q.displayQuantity (q "12.10") `shouldEqual` "12.1"
      Q.displayQuantity (q "12.001") `shouldEqual` "12"
      Q.displayQuantity (q "-12.5") `shouldEqual` "-12.5"
      Q.displayQuantity Q.zero `shouldEqual` "0"
      Q.displayQuantity (q "0.0001") `shouldEqual` "<0.01"
      Q.displayQuantity (q "0.004") `shouldEqual` "<0.01"
      Q.displayQuantity (q "-0.0001") `shouldEqual` "-<0.01"
      Q.displayQuantity (q "0.01") `shouldEqual` "0.01"
      Q.displayQuantity (q "3.10") `shouldEqual` "3.1"
      Q.displayQuantity (q "3.00") `shouldEqual` "3"
      Q.displayRatio (Q.fromInt 1) `shouldEqual` "1.00"
      Q.displayRatio (q "0.5") `shouldEqual` "0.50"
      Q.displayRatio (q "-0.25") `shouldEqual` "-0.25"
      Q.displayRatio (q "-0.004") `shouldEqual` "0.00"
      Q.display Q.Ratio (q "0.3333") `shouldEqual` "0.33"
      Q.displayTick Q.Quantity 2.5 `shouldEqual` "2.5"
      Q.displayTick Q.Ratio 0.333333 `shouldEqual` "0.33"
    it "a whole number shows no decimals" do
      quickCheck do
        n <- chooseInt (-100000) 100000
        pure (((Q.displayQuantity (Q.fromInt n) == show n) && (Q.displayQuantity (Q.fromInt n) == show n)) === true)
    it "display stays within half a hundredth, never drops digits, and trims" do
      quickCheck do
        x <- genQ
        pure ((displayWithin false x (Q.displayQuantity x) && displayWithin false x (Q.displayQuantity x) && displayWithin true x (Q.displayRatio x)) === true)
    it "display rounds every tie half to even" do
      quickCheck do
        -- k = 0 is 0.005, which rounds to zero but is not zero, so it shows as "<0.01" by design
        k <- chooseInt 1 5000
        let
          -- (2k+1) × 0.005 is exactly k + 1/2 hundredths, so the even neighbour is the answer
          x = Q.fromUnits (BI.fromInt (50 * (2 * k + 1)))
          expected = if k `mod` 2 == 0 then k else k + 1
          shown s = (Q.toUnits <$> Q.parse s) == Just (BI.fromInt (100 * expected))
        pure ((shown (Q.displayQuantity x) && shown (Q.displayRatio x) && shown (Q.displayQuantity x)) === true)
  describe "Core.Exchange clearing" do
    let
      cost = { intercept: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.fromInt 0, hi: Q.fromInt 100 } }
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

  describe "Core.Exchange prices" do
    it "averagePrice is the marginal price at the midpoint of the move" do
      let c = { intercept: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.zero, hi: Q.fromInt 100 } }
      -- C(q) = q + q²/2, so C(3) - C(2) = 3.5
      averagePrice c (Q.fromInt 2) (Q.fromInt 1) `shouldEqual` q "3.5"
      averagePrice c (Q.fromInt 2) Q.zero `shouldEqual` q "3"
  describe "Core.Draw" do
    let key = { round: 3, source: "sme-energy-sample", actor: "SME", category: "sampling", index: 0 }
    it "depends only on seed and key" do
      draw (Seed 26021) key `shouldEqual` draw (Seed 26021) key
    it "lies in [0, 1)" $ quickCheck do
      s <- chooseInt 0 100000
      i <- chooseInt 0 100
      let u = draw (Seed s) key { index = i }
      pure ((u >= Q.zero && u < Q.fromInt 1) === true)
    it "differs across seeds for equal keys" do
      draw (Seed 26021) key `shouldNotEqual` draw (Seed 1) key
  describe "Laws.Engine" do
    it "rejects a Trade on an unknown exchange" do
      let w = baseWorld [ explicitActor "US" [ MayTrade (Id "XX") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Trade { exchange: Id "XX", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` [ "unknown exchange XX" ]
    it "rejects a Transform of an unknown transformation" do
      let w = baseWorld [ explicitActor "US" [ MayTransform (Id "F") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "F", quantity: Q.fromInt 1 } ] ])
        `shouldEqual` [ "unknown transformation F" ]
    it "rejects a Transfer to an unknown actor" do
      let w = baseWorld [ explicitActor "US" [ MayTransfer (Id "M") ] ]
      rejections (resolveRound w [ submit "US" 0 [ Transfer { from: Id "US", to: Id "NOBODY", resource: Id "M", quantity: Q.fromInt 1 } ] ])
        `shouldEqual` [ "unknown actor NOBODY" ]
    it "rejects a Trade by an Actor outside the Exchange's access" do
      let w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM") ] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "EU" ]) }
      rejections (resolveRound w [ submit "US" 0 [ Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` [ "US has no access to EM" ]
    it "accepts a Trade by an Actor with access" do
      let w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM") ] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]) }
      rejections (resolveRound w [ submit "US" 0 [ Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ] ])
        `shouldEqual` []
    it "rejects repealing an unknown rule" do
      let w = baseWorld [ explicitActor "US" [ MayEnact (Id "tariff") ] ]
      rejections (resolveRound w [ submit "US" 0 [ ChangeRule (Repeal (Id "nope")) ] ])
        `shouldEqual` [ "unknown rule nope" ]
    it "rejects every copy of a duplicated CandidateTransactionId" do
      let w = baseWorld [ explicitActor "US" [] ]
      rejections (resolveRound w [ submit "US" 0 [], submit "US" 0 [] ])
        `shouldEqual` [ "duplicate candidateTransaction id", "duplicate candidateTransaction id" ]
    it "a procedure's enactment is limited to its own jurisdiction" do
      let
        proc = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: q "0.5", authorizes: [ MayEnact (Id "tariff") ] }
        eu = (explicitActor "EU" []) { kind = Aggregate { strategy: Id "p", members: [], procedures: Map.singleton (Id "trade") proc } }
        tariff j = tariffR { jurisdiction: j, origin: Id "US", rate: q "0.1" }
        procedureSubmit j =
          { candidateTransaction: { id: candidateTransactionId 1 (Id "EU") 0, author: Id "EU", intents: [ ChangeRule (Enact (tariff j)) ], signers: [], procedure: Just (Id "trade") }
          , signatures: []
          }
        w = baseWorld [ eu ]
      rejections (resolveRound w [ procedureSubmit (Id "DE") ]) `shouldEqual` [ "a Rule of kind tariff applies only to its governing Actor or owner, and only in its declared form" ]
      rejections (resolveRound w [ procedureSubmit (Id "EU") ]) `shouldEqual` []
    it "a joint CandidateTransaction bundles Transfers and Transforms, and only those" do
      let
        w = (baseWorld [ explicitActor "A" [ MayTrade (Id "EM"), MayTransform (Id "plant") ], explicitActor "B" [] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "A" ]) }
        joint intents = { candidateTransaction: { id: candidateTransactionId 1 (Id "A") 0, author: Id "A", intents, signers: [ Id "B" ], procedure: Nothing }, signatures: [ Id "B" ] }
        give = Transfer { from: Id "A", to: Id "B", resource: Id "M", quantity: Q.fromInt 1 }
        run = Transform { by: Id "A", transformation: Id "plant", quantity: Q.fromInt 1 }
        order = Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing }
        bundled = [ "joint CandidateTransactions bundle only Transfers and Transforms" ]
      rejections (resolveRound w [ joint [ give, order ] ]) `shouldEqual` bundled
      -- a Transform passes the joint candidate Transaction rule: any rejection is the unknown Transformation, not the bundle
      rejections (resolveRound w [ joint [ give, run ] ]) `shouldNotEqual` bundled
      map jointIntent [ give, run, order ] `shouldEqual` [ true, true, false ]

    it "a procedure's Intents face the same exchange access check" do
      let
        proc = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: q "0.5", authorizes: [ MayTrade (Id "EM") ] }
        eu = (explicitActor "EU" []) { kind = Aggregate { strategy: Id "p", members: [], procedures: Map.singleton (Id "trade") proc } }
        w = (baseWorld [ eu ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]) }
        p =
          { id: candidateTransactionId 1 (Id "EU") 0
          , author: Id "EU"
          , intents: [ Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ]
          , signers: []
          , procedure: Just (Id "trade")
          }
      rejections (resolveRound w [ { candidateTransaction: p, signatures: [] } ]) `shouldEqual` [ "EU has no access to EM" ]
    it "an overdrawing Transfer records a shortfall and the round proceeds" do
      let w = baseWorld [ explicitActor "US" [ MayTransfer (Id "M") ], explicitActor "UK" [] ]
      case resolveRound w [ submit "US" 0 [ Transfer { from: Id "US", to: Id "UK", resource: Id "M", quantity: Q.fromInt 500 } ] ] of
        Right r -> do
          holding r.world (Id "US") (Id "M") `shouldEqual` Q.zero
          holding r.world (Id "UK") (Id "M") `shouldEqual` Q.fromInt 200
        Left e -> show e `shouldEqual` "a round"
    it "caps an uncapped transformation's fills at its ceiling, so escrow bounds the settled price" do
      let
        fac = (transformation "F") { payment = Just { curve: { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 10 } }, resource: Id "M", to: Id "PAYEE" } }
        w = (baseWorld [ explicitActor "US" [ MayTransform (Id "F") ], explicitActor "UK" [ MayTransform (Id "F") ] ]) { transformations = Map.singleton (Id "F") fac }
        order e = submit e 0 [ Transform { by: Id e, transformation: Id "F", quantity: Q.fromInt 8 } ]
        filled = concatMap transformedQ (eventsOf (resolveRound w [ order "US", order "UK" ]))
      Q.sumQ filled `shouldEqual` Q.fromInt 10
    it "escrows a pooled transformation at the pool ceiling, so a member with a larger draw per unit is still bounded" do
      let
        cost = { intercept: Q.fromInt 1, slope: Q.fromInt 1, domain: { lo: Q.zero, hi: Q.fromInt 10 } }
        fac name use = (transformation name) { draws = [ heldDraw (Id "plant") (Q.fromInt use) ], payment = Just { curve: cost, resource: Id "M", to: Id "PAYEE" } }
        us = withAccount (Id "plant") (Q.fromInt 10) (explicitActor "US" [ MayTransform (Id "FA") ])
        -- UK holds exactly the escrow of its order at the old per-transformation ceiling (capacity / 2 = 5, price 1 + 5/2 = 3.5)
        uk = (explicitActor "UK" [ MayTransform (Id "FB") ]) { accounts = Map.singleton (Id "M") (Q.div (Q.fromInt 7) (Q.fromInt 2)) }
        w = (baseWorld [ us, uk ]) { transformations = Map.fromFoldable [ Tuple (Id "FA") (fac "FA" 1), Tuple (Id "FB") (fac "FB" 2) ] }
        -- 8 units of FA (use 1) and 1 unit of FB (use 2) fill the whole capacity of 10: the pool total is 9
        r = resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "FA", quantity: Q.fromInt 8 } ], submit "UK" 0 [ Transform { by: Id "UK", transformation: Id "FB", quantity: Q.fromInt 1 } ] ]
      filter
        (matches (shortfalls) (const true))
        (eventsOf r) `shouldEqual` []
    it "clamps a delivery at zero and records only the change applied" do
      let
        us = withAccount (Id "plant") (Q.fromInt 5) (explicitActor "US" [])
        w = (baseWorld [ us ]) { pending = [ delivery { round: 1, actor: Id "US", resource: Id "plant", quantity: Q.fromInt (-10), cause: testCause } ] }
        prepared = prepareRound w
      holding prepared (Id "US") (Id "plant") `shouldEqual` Q.zero
      prepared.events `shouldEqual` [ Settled { round: 1, legs: [ { from: Held (Id "US"), to: Sink, resource: Id "plant", owed: Q.fromInt 10, moved: Q.fromInt 5 } ], rate: Nothing, cause: testCause, delivered: true } ]
    it "a delivery to an unknown Actor applies nothing and records nothing" do
      let w = (baseWorld [ explicitActor "US" [] ]) { pending = [ delivery { round: 1, actor: Id "GHOST", resource: Id "M", quantity: Q.fromInt 5, cause: testCause } ] }
      (prepareRound w).events `shouldEqual` []
    it "prepareRound is idempotent, and resolveRound applies due deliveries itself" do
      let w = (baseWorld [ explicitActor "US" [] ]) { pending = [ delivery { round: 1, actor: Id "US", resource: Id "M", quantity: Q.fromInt 7, cause: testCause } ] }
      (prepareRound (prepareRound w)).events `shouldEqual` (prepareRound w).events
      map (\r -> holding r.world (Id "US") (Id "M")) (resolveRound w []) `shouldEqual` Right (Q.fromInt 107)
      map (\r -> holding r.world (Id "US") (Id "M")) (resolveRound (prepareRound w) []) `shouldEqual` Right (Q.fromInt 107)
    it "a Representation with a zero reference contributes no estimate" do
      let
        rep = representation { explicitActor: "US", aggregate: "EU", domain: "E", observes: "E", estimator: { reference: Q.zero, shrinkage: Q.fromInt 1, deviationBound: Nothing, noiseSpread: Q.zero } }
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM") ] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]), representations = [ rep ] }
        p = { explicitActor: Id "US", resource: Id "E", quantity: Q.fromInt 1 }
      (Represent.estimate w [ p ]).events `shouldEqual` []

  describe "Best execution (docs/02 §5.2)" do
    it "a routed buy fills the cheaper Exchange up to its price impact, then the next" do
      let
        -- A's price rises by 0.05 per unit (1 + 0.05·d), so it reaches B's price of 2 at about 20 units
        cheap = (priced "EM-A" "E" [ "US" ] "1") { cost = { intercept: Q.fromInt 1, slope: q "0.1", domain: { lo: Q.zero, hi: Q.fromInt 100 } } }
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A"), MayTrade (Id "EM-B") ] ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-A") cheap, Tuple (Id "EM-B") (priced "EM-B" "E" [ "US" ] "2") ] }
        r = resolveRound w [ submit "US" 0 [ routed "E" 30 "3" ] ]
        a = fillIn "EM-A" "US" r
        b = fillIn "EM-B" "US" r
      rejections r `shouldEqual` []
      (a > q "19.9" && a < q "20.1") `shouldEqual` true
      b `shouldEqual` Q.sub (Q.fromInt 30) a
      Q.sumQ (map snd (fillsOf "US" r)) `shouldEqual` Q.fromInt 30
    it "a routed buy takes the cheapest Exchange first when it can absorb the whole order" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A"), MayTrade (Id "EM-B") ] ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-A") (priced "EM-A" "E" [ "US" ] "1"), Tuple (Id "EM-B") (priced "EM-B" "E" [ "US" ] "2") ] }
        r = resolveRound w [ submit "US" 0 [ routed "E" 10 "3" ] ]
      fillsOf "US" r `shouldEqual` [ Tuple "EM-A" (Q.fromInt 10) ]
    it "a routed sell goes to the dearer Exchange first" do
      let
        -- a sell lowers an Exchange's position, so the Exchanges start at 50 (their domain allows 50 to be sold), and the seller holds E
        held m = m { position = Q.fromInt 50 }
        seller = (explicitActor "US" [ MayTrade (Id "EM-A"), MayTrade (Id "EM-B") ]) { accounts = Map.fromFoldable [ Tuple (Id "M") (Q.fromInt 100), Tuple (Id "E") (Q.fromInt 20) ] }
        w = (baseWorld [ seller ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-A") (held (priced "EM-A" "E" [ "US" ] "1")), Tuple (Id "EM-B") (held (priced "EM-B" "E" [ "US" ] "2")) ] }
        r = resolveRound w [ submit "US" 0 [ routed "E" (-10) "0.5" ] ]
      fillsOf "US" r `shouldEqual` [ Tuple "EM-B" (Q.fromInt (-10)) ]
    it "ties in merit break by ExchangeId" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-B"), MayTrade (Id "EM-A") ] ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-B") (priced "EM-B" "E" [ "US" ] "1"), Tuple (Id "EM-A") (priced "EM-A" "E" [ "US" ] "1") ] }
        r = resolveRound w [ submit "US" 0 [ routed "E" 10 "3" ] ]
      fillsOf "US" r `shouldEqual` [ Tuple "EM-A" (Q.fromInt 10) ]
    it "a routed order with no accessible Exchange is rejected" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A") ] ]) { exchanges = Map.singleton (Id "EM-A") (priced "EM-A" "E" [ "EU" ] "1") }
      rejections (resolveRound w [ submit "US" 0 [ routed "E" 10 "3" ] ])
        `shouldEqual` [ "US has no accessible exchange for E/M" ]
    it "a routed order without a limit price is rejected at screening" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A") ] ]) { exchanges = Map.singleton (Id "EM-A") (priced "EM-A" "E" [ "US" ] "1") }
      rejections (resolveRound w [ submit "US" 0 [ Route { pair: { base: Id "E", quote: Id "M" }, quantity: Q.fromInt 10, limitPrice: Nothing } ] ])
        `shouldEqual` [ "a routed order needs a limit price" ]
    it "a routed order's fills conserve quantity and holdings, and pass the invariants" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A"), MayTrade (Id "EM-B") ], explicitActor "EU" [ MayTrade (Id "EM-A") ] ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-A") (priced "EM-A" "E" [ "US", "EU" ] "1"), Tuple (Id "EM-B") (priced "EM-B" "E" [ "US" ] "2") ] }
        r = resolveRound w [ submit "US" 0 [ routed "E" 70 "3" ], submit "EU" 0 [ routed "E" 20 "3" ] ]
      case r of
        Left e -> show e `shouldEqual` "a round"
        Right res -> do
          rejections r `shouldEqual` []
          check (prepareRound w) res `shouldEqual` []
          let
            total = Q.sumQ (map snd (fillsOf "US" r))
            perExchange = Q.add (fillIn "EM-A" "US" r) (fillIn "EM-B" "US" r)
          total `shouldEqual` perExchange
          (total <= Q.fromInt 70) `shouldEqual` true
          holding res.world (Id "US") (Id "E") `shouldEqual` total
    it "the result does not depend on the order of submissions" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM-A"), MayTrade (Id "EM-B") ], explicitActor "EU" [ MayTrade (Id "EM-A") ] ]) { exchanges = Map.fromFoldable [ Tuple (Id "EM-A") (priced "EM-A" "E" [ "US", "EU" ] "1"), Tuple (Id "EM-B") (priced "EM-B" "E" [ "US" ] "2") ] }
        us = submit "US" 0 [ routed "E" 40 "3" ]
        eu = submit "EU" 0 [ routed "E" 25 "2.5" ]
      eventsOf (resolveRound w [ us, eu ]) `shouldEqual` eventsOf (resolveRound w [ eu, us ])
      fillsOf "EU" (resolveRound w [ eu, us ]) `shouldEqual` fillsOf "EU" (resolveRound w [ us, eu ])

  describe "Reservations (docs/02 §2.3, §8.2)" do
    it "a reservation escrows the holder's Mil, scales a competing Transfer, and consumes nothing" do
      let
        mil = resource "Mil"
        dep = (transformation "dep") { draws = [ ownDraw (Id "Mil") (Q.fromInt 3) ] }
        us = (explicitActor "US" [ MayTransform (Id "dep"), MayTransfer (Id "Mil") ]) { accounts = Map.singleton (Id "Mil") (Q.fromInt 10) }
        w = (baseWorld [ us, explicitActor "JP" [] ]) { resources = Map.insert (Id "Mil") mil (baseWorld []).resources, transformations = Map.singleton (Id "dep") dep }
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
            (matches legs (\l -> l.to == Sink))
            res.record.events `shouldEqual` []
          -- the reservation is released at commit: the next round starts with nothing reserved
          case resolveRound res.world [ submit "US" 0 [ Transfer { from: Id "US", to: Id "JP", resource: Id "Mil", quantity: Q.div (Q.fromInt 15) (Q.fromInt 4) } ] ] of
            Left e -> show e `shouldEqual` "a round"
            Right next -> holding next.world (Id "US") (Id "Mil") `shouldEqual` Q.zero
    it "a reservation alongside inputs leaves the reserved Resource unchanged and still consumes the inputs" do
      let
        dep = (transformation "dep") { inputs = Map.singleton (Id "E") (Q.fromInt 1), draws = [ ownDraw (Id "Mil") (Q.fromInt 3) ] }
        us = (explicitActor "US" [ MayTransform (Id "dep") ]) { accounts = Map.fromFoldable [ Tuple (Id "Mil") (Q.fromInt 10), Tuple (Id "E") (Q.fromInt 4) ] }
        mil = resource "Mil"
        w = (baseWorld [ us ]) { resources = Map.insert (Id "Mil") mil (baseWorld []).resources, transformations = Map.singleton (Id "dep") dep }
      case resolveRound w [ submit "US" 0 [ Transform { by: Id "US", transformation: Id "dep", quantity: Q.fromInt 2 } ] ] of
        Left e -> show e `shouldEqual` "a round"
        Right res -> do
          check w res `shouldEqual` []
          holding res.world (Id "US") (Id "Mil") `shouldEqual` Q.fromInt 10
          holding res.world (Id "US") (Id "E") `shouldEqual` Q.fromInt 2

  describe "Penalties, delayed effects, rules, and ownership" do
    let
      -- a flow of 5 M from US to UK, and UK requiring 10 E at a penalty of 0.25 per unit unmet, holding `held` E
      penalised held = (baseWorld [ explicitActor "US" [], (explicitActor "UK" []) { accounts = Map.fromFoldable [ Tuple (Id "M") (Q.fromInt 100), Tuple (Id "E") (Q.fromInt held) ] } ])
        { recurring =
            [ (testRule "requirement" (FromActor (Id "UK")) ToSink (Id "E") (FixedAmount (Q.fromInt 10))) { onShortfall = OffsetIncoming (q "0.25") }
            , (testRule "flow" (FromActor (Id "US")) (ToActor (Id "UK")) (Id "M") (FixedAmount (Q.fromInt 5))) { offsetable = true }
            ]
        }
      -- UK's Money after one round of no Intents
      moneyAfter w = either (const Q.zero) (\r -> holding r.world (Id "UK") (Id "M")) (resolveRound w [])
      -- the World State after one round of the given submissions; a failed round leaves it unchanged
      advance w inputs = either (const w) _.world (resolveRound w inputs)
    it "a shortfall cuts an incoming fixed flow by the penalty per unit unmet" do
      -- 4 E held of 10 required: 6 unmet, a penalty of 1.5 against the 5 owed
      moneyAfter (penalised 4) `shouldEqual` q "103.5"
      -- 8 E held: 2 unmet, a penalty of 0.5
      moneyAfter (penalised 8) `shouldEqual` q "104.5"
      -- nothing unmet: the flow arrives in full
      moneyAfter (penalised 10) `shouldEqual` Q.fromInt 105
    it "a CandidateTransaction id gives back its index" do
      submissionIndex (candidateTransactionId 3 (Id "US") 1) `shouldEqual` Just 1
    it "an enacted withholding reduces the owner's capacity from its scheduled round, and a repeal ends it a round later" do
      let
        uk = withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" [ MayEnact (Id "withholding") ])
        ruleId = ruleIdOf (candidateTransactionId 1 (Id "UK") 0) 0
        drawing = (baseWorld [ uk ]) { transformations = Map.singleton (Id "F") ((transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }) }
        round1 = advance drawing [ submit "UK" 0 [ ChangeRule (Enact (holdR { capacity: Id "plant", units: Q.fromInt 4 })) ] ]
        round2 = advance round1 [ submit "UK" 0 [ ChangeRule (Repeal ruleId) ] ]
      -- scheduled in round 1 and in force from round 2, so the round-1 world reads its scheduled Rule as not yet applied
      drawable round1 (Id "plant") `shouldEqual` Q.fromInt 10
      drawable (prepareRound round1) (Id "plant") `shouldEqual` Q.fromInt 6
      -- repealed in round 2: still in force in round 2, and gone from round 3
      drawable (prepareRound round2) (Id "plant") `shouldEqual` Q.fromInt 10
    it "a holding Rule limits only what can be drawn: fills stop at the reduced amount, holdings and flows are unchanged" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }
        holder = withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" [ MayTransform (Id "F") ])
        w = (baseWorld [ holder ]) { transformations = Map.singleton (Id "F") fac, rules = Map.singleton (Id "r") (holdR { capacity: Id "plant", units: Q.fromInt 4 }) }
        r = either (\e -> unsafeCrashWith (show e)) identity (resolveRound w [ submit "UK" 0 [ Transform { by: Id "UK", transformation: Id "F", quantity: Q.fromInt 9 } ] ])
      select transformationFills r.record.events `shouldEqual` [ { round: 1, by: Id "UK", transformation: Id "F", quantity: Q.fromInt 6, paid: Q.zero, candidateTransaction: candidateTransactionId 1 (Id "UK") 0 } ]
      holding r.world (Id "UK") (Id "plant") `shouldEqual` Q.fromInt 10
    it "an effect changes only its named holder's capacity, for its rounds" do
      let
        -- each unit ordered adds one unit of plant to JP in the round after the order, for two rounds
        fac = (transformation "F") { effects = [ { resource: Id "plant", holder: Just (Id "JP"), perUnit: Q.fromInt 1, rounds: Just 2 } ] }
        w = (baseWorld [ explicitActor "UK" [ MayTransform (Id "F") ], explicitActor "JP" [], explicitActor "US" [] ]) { transformations = Map.singleton (Id "F") fac }
        held e world = holding (prepareRound world) (Id e) (Id "plant")
        round2 = advance w [ submit "UK" 0 [ Transform { by: Id "UK", transformation: Id "F", quantity: Q.fromInt 3 } ] ]
        round3 = advance round2 []
        round4 = advance round3 []
      held "JP" round2 `shouldEqual` Q.fromInt 3
      held "JP" round3 `shouldEqual` Q.fromInt 3
      held "JP" round4 `shouldEqual` Q.zero
      held "UK" round2 `shouldEqual` Q.zero
      held "US" round2 `shouldEqual` Q.zero
    it "a transformation whose owner does not sign is rejected, and accepted once the owner signs" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ], ownerSigns = true }
        w = (baseWorld [ explicitActor "US" [ MayTransform (Id "F") ], withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" []) ]) { transformations = Map.singleton (Id "F") fac }
        order = Transform { by: Id "US", transformation: Id "F", quantity: Q.fromInt 1 }
        p signers =
          { candidateTransaction: { id: candidateTransactionId 1 (Id "US") 0, author: Id "US", intents: [ order ], signers: map Id signers, procedure: Nothing }
          , signatures: map Id signers
          }
      rejections (resolveRound w [ p [] ]) `shouldEqual` [ "the holder of what F draws must sign" ]
      rejections (resolveRound w [ p [ "UK" ] ]) `shouldEqual` []
      concatMap transformedQ (eventsOf (resolveRound w [ p [ "UK" ] ])) `shouldEqual` [ Q.fromInt 1 ]
    it "INV-03 reports a round that changes an Actor's capabilities" do
      let w = baseWorld [ explicitActor "US" [ MayTrade (Id "EM") ] ]
      case resolveRound w [] of
        Right res -> do
          map _.invariant (check w res) `shouldEqual` []
          map _.invariant (check w (res { world = res.world { actors = map (\e -> e { capabilities = [] }) res.world.actors } })) `shouldEqual` [ "INV-03" ]
        Left e -> show e `shouldEqual` "a round"

  describe "Funding, shipping, the record, and ownership" do
    let
      transport = transformation "ship"
      holder name = (explicitActor name [ MayTransform (Id "ship") ]) { accounts = Map.fromFoldable [ Tuple (Id "M") (Q.fromInt 100), Tuple (Id "E") (Q.fromInt 50) ] }
      shipWorld = (baseWorld [ holder "A", holder "B", holder "C" ]) { transformations = Map.singleton (Id "ship") transport, flowGates = [ { resource: Id "E", exemptSharedExchange: true, bundled: Just (Id "ship") } ] }
      send to n = Transfer { from: Id "A", to: Id to, resource: Id "E", quantity: Q.fromInt n }
      carry n = Transform { by: Id "A", transformation: Id "ship", quantity: Q.fromInt n }
      needsTransport = "this Transfer needs a matching bundled order by the sender"
      invariantsOf w r = map _.invariant (check w r)
      resolved w inputs = either (\e -> unsafeCrashWith (show e)) identity (resolveRound w inputs)
    it "a procedure CandidateTransaction whose agent is short is scaled to what the agent holds, with no Shortfall" do
      let
        proc = { weights: Map.empty, residualWeight: Q.fromInt 1, threshold: q "0.5", authorizes: [ MayTransfer (Id "E") ] }
        eu = (explicitActor "EU" []) { kind = Aggregate { strategy: Id "p", members: [ Id "POOL" ], procedures: Map.singleton (Id "aid") proc } }
        pool = (explicitActor "POOL" []) { accounts = Map.singleton (Id "E") (Q.fromInt 4) }
        w = baseWorld [ eu, pool, explicitActor "DE" [] ]
        p =
          { candidateTransaction: { id: candidateTransactionId 1 (Id "EU") 0, author: Id "EU", intents: [ Transfer { from: Id "POOL", to: Id "DE", resource: Id "E", quantity: Q.fromInt 10 } ], signers: [], procedure: Just (Id "aid") }
          , signatures: []
          }
        evs = eventsOf (resolveRound w [ p ])
      length (filter isShortfall evs) `shouldEqual` 0
      map (holding (resolved w [ p ]).world (Id "DE")) [ Id "E" ] `shouldEqual` [ Q.fromInt 4 ]
    it "one transport order cannot carry two shipments" do
      rejections (resolveRound shipWorld [ submit "A" 0 [ send "B" 5, send "C" 5, carry 5 ] ]) `shouldEqual` [ needsTransport ]
    it "one transport order covering the sender's shipments in total is accepted" do
      rejections (resolveRound shipWorld [ submit "A" 0 [ send "B" 5, send "C" 5, carry 10 ] ]) `shouldEqual` []
    it "a Rule applies to a flow with no gate and no carrier" do
      let
        embargo = FlowRule { kind: Id "embargo", scope: { governor: Id "B", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Block }
        declared = Map.singleton (Id "embargo") { form: BlockFlows GovernedByDestination, text: "" }
        w = shipWorld { flowGates = [], ruleKinds = declared, rules = Map.singleton (Id "r") embargo }
      rejections (resolveRound w [ submit "A" 0 [ send "B" 5 ] ]) `shouldEqual` [ "blocked by a Rule of kind embargo" ]
    it "a levy may be ad valorem, and may go to the sink" do
      let
        toll = FlowRule { kind: Id "toll", scope: { governor: Id "B", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: q "0.1", basis: AdValorem (Id "EM"), in: Id "M", to: LevyToSink } }
        declared = Map.singleton (Id "toll") { form: LevyFlows GovernedByDestination, text: "" }
        w = shipWorld { flowGates = [], ruleKinds = declared, rules = Map.singleton (Id "r") toll, exchanges = Map.singleton (Id "EM") (priced "EM" "E" [] "2") }
        r = resolved w [ submit "A" 0 [ send "B" 5 ] ]
      -- 0.1 x 5 units x price 2 is 1 Money, destroyed
      holding r.world (Id "A") (Id "M") `shouldEqual` Q.fromInt 99
      holding r.world (Id "B") (Id "M") `shouldEqual` Q.fromInt 100
    it "a Rule does not cover a Resource outside its over" do
      let
        embargo = FlowRule { kind: Id "embargo", scope: { governor: Id "B", counterparty: Id "A", direction: Inbound, over: [ Id "M" ] }, effect: Block }
        declared = Map.singleton (Id "embargo") { form: BlockFlows GovernedByDestination, text: "" }
        w = shipWorld { flowGates = [], ruleKinds = declared, rules = Map.singleton (Id "r") embargo }
      rejections (resolveRound w [ submit "A" 0 [ send "B" 5 ] ]) `shouldEqual` []
    it "a premise with no Rule kinds, conditions or groups runs: nothing is checked" do
      let plain = shipWorld { ruleKinds = Map.empty, flowGates = [], groups = Map.empty }
      rejections (resolveRound plain [ submit "A" 0 [ send "B" 5 ] ]) `shouldEqual` []
    it "with no declared kinds, no Rule can be enacted" do
      let w = (baseWorld [ explicitActor "US" [ MayEnact (Id "tariff") ] ]) { ruleKinds = Map.empty }
      rejections (resolveRound w [ submit "US" 0 [ ChangeRule (Enact (tariffR { jurisdiction: Id "US", origin: Id "EU", rate: q "0.1" })) ] ])
        `shouldEqual` [ "a Rule of kind tariff applies only to its governing Actor or owner, and only in its declared form" ]
    it "a new Rule kind, declared only as data, is enforced by the mechanism" do
      let
        embargo = FlowRule { kind: Id "embargo", scope: { governor: Id "B", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Block }
        toll = FlowRule { kind: Id "toll", scope: { governor: Id "B", counterparty: Id "A", direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: q "0.1", basis: PerUnit, in: Id "M", to: LevyToDestination } }
        declared = Map.fromFoldable [ Tuple (Id "embargo") { form: BlockFlows GovernedByDestination, text: "" }, Tuple (Id "toll") { form: LevyFlows GovernedByDestination, text: "" } ]
        blocked = shipWorld { ruleKinds = declared, rules = Map.singleton (Id "r") embargo }
        tolled = shipWorld { ruleKinds = declared, rules = Map.singleton (Id "r") toll }
      rejections (resolveRound blocked [ submit "A" 0 [ send "B" 5, carry 5 ] ]) `shouldEqual` [ "blocked by a Rule of kind embargo" ]
      length (filter (matches levies (\l -> l.kind == Id "toll")) (eventsOf (resolveRound tolled [ submit "A" 0 [ send "B" 5, carry 5 ] ]))) `shouldEqual` 1
    it "an Outbound Rule covers what leaves its governor, and only that" do
      let
        ban = FlowRule { kind: Id "exportBan", scope: { governor: Id "A", counterparty: Id "B", direction: Outbound, over: [ Id "E" ] }, effect: Block }
        declared = Map.singleton (Id "exportBan") { form: BlockFlows GovernedBySource, text: "" }
        w = shipWorld { ruleKinds = declared, rules = Map.singleton (Id "r") ban }
        back = Transfer { from: Id "B", to: Id "A", resource: Id "E", quantity: Q.fromInt 5 }
        carryBack = Transform { by: Id "B", transformation: Id "ship", quantity: Q.fromInt 5 }
      rejections (resolveRound w [ submit "A" 0 [ send "B" 5, carry 5 ] ]) `shouldEqual` [ "blocked by a Rule of kind exportBan" ]
      rejections (resolveRound w [ submit "B" 0 [ back, carryBack ] ]) `shouldEqual` []
    it "a Rule is checked against who its kind says governs" do
      let
        declared = Map.singleton (Id "exportBan") { form: BlockFlows GovernedBySource, text: "" }
        inbound = FlowRule { kind: Id "exportBan", scope: { governor: Id "A", counterparty: Id "B", direction: Inbound, over: [ Id "E" ] }, effect: Block }
      conforms (fromMaybe { form: ReduceHolding, text: "" } (Map.lookup (Id "exportBan") declared)) inbound `shouldEqual` false
    it "a Rule enacted in one round is in the next round's record" do
      let
        us = explicitActor "US" [ MayEnact (Id "tariff") ]
        w = baseWorld [ us ]
        tariff = tariffR { jurisdiction: Id "US", origin: Id "EU", rate: q "0.1" }
        next = either (const w) _.world (resolveRound w [ submit "US" 0 [ ChangeRule (Enact tariff) ] ])
        enacted = (matches (event @"Enacted") (const true))
      length (filter enacted (eventsOf (resolveRound next []))) `shouldEqual` 1
      -- a world prepared before resolution records the same events
      length (filter enacted (eventsOf (resolveRound (prepareRound next) []))) `shouldEqual` 1
    it "a limit that is not met removes that order only, and the CandidateTransaction is still checked" do
      let
        w = (baseWorld [ explicitActor "US" [ MayTrade (Id "EM") ] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]) }
        low = Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Just (q "0.5") }
        ok = Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing }
        r = resolved w [ submit "US" 0 [ low, ok ] ]
        removed = (matches (event @"OrderRemoved") (const true))
      length (filter removed r.record.events) `shouldEqual` 1
      rejections (Right r) `shouldEqual` []
      -- the candidate Transaction is not dropped from INV-02: a stripped capability is caught
      invariantsOf (w { actors = map (\e -> e { capabilities = [] }) w.actors }) r `shouldContain` "INV-02"
    it "a joint with one leg lacking inputs settles nothing" do
      let
        fac = transport { id = Id "F", inputs = Map.singleton (Id "E") (Q.fromInt 1) }
        a = (explicitActor "A" [ MayTransfer (Id "M"), MayTransform (Id "F") ]) { accounts = Map.fromFoldable [ Tuple (Id "M") (Q.fromInt 100), Tuple (Id "E") (Q.fromInt 1) ] }
        w = (baseWorld [ a, explicitActor "B" [] ]) { transformations = Map.singleton (Id "F") fac }
        joint =
          { candidateTransaction: { id: candidateTransactionId 1 (Id "A") 0, author: Id "A", intents: [ Transfer { from: Id "A", to: Id "B", resource: Id "M", quantity: Q.fromInt 5 }, Transform { by: Id "A", transformation: Id "F", quantity: Q.fromInt 5 } ], signers: [ Id "B" ], procedure: Nothing }
          , signatures: [ Id "B" ]
          }
        r = resolved w [ joint ]
      holding r.world (Id "B") (Id "M") `shouldEqual` Q.fromInt 100
      holding r.world (Id "A") (Id "E") `shouldEqual` Q.fromInt 1
      length (rejections (Right r)) `shouldEqual` 1
    it "a negative fixed flow is an engine error, not a crash" do
      let w = (baseWorld [ explicitActor "A" [], explicitActor "B" [] ]) { recurring = [ testRule "flow" (FromActor (Id "A")) (ToActor (Id "B")) (Id "M") (FixedAmount (Q.fromInt (-1))) ] }
      rejections (resolveRound w []) `shouldEqual` [ "negative fixed flow: A to B" ]
    it "the holder of a drawn Resource is its positive holder, whatever the Map order; an undrawn one has none" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }
        uk = withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" [])
        jp = withAccount (Id "plant") Q.zero (explicitActor "JP" [])
      holderOf ((baseWorld [ uk, jp ]) { transformations = Map.singleton (Id "F") fac }) (Id "plant") `shouldEqual` Just (Id "UK")
      holderOf (baseWorld [ uk, jp ]) (Id "plant") `shouldEqual` Nothing
    it "a Free Resource moves without a capability, a Restricted one needs it" do
      let
        a = (explicitActor "A" []) { accounts = Map.singleton (Id "M") (Q.fromInt 10) }
        w = baseWorld [ a, explicitActor "B" [] ]
        restricted = w { resources = map (_ { transferability = Restricted }) w.resources }
        give = submit "A" 0 [ Transfer { from: Id "A", to: Id "B", resource: Id "M", quantity: Q.fromInt 1 } ]
      rejections (resolveRound w [ give ]) `shouldEqual` []
      rejections (resolveRound restricted [ give ]) `shouldEqual` [ "A is not authorized" ]
    it "a decaying Resource acquired this round decays, with no prior account" do
      let
        giver = (explicitActor "G" [ MayTransfer (Id "E") ]) { accounts = Map.singleton (Id "E") (Q.fromInt 10) }
        base = baseWorld [ giver, explicitActor "U" [] ]
        w = base { recurring = [ testRule "decay" FromEachHolder ToSink (Id "E") (FractionHeld (q "0.5")) ] }
        r = resolved w [ submit "G" 0 [ Transfer { from: Id "G", to: Id "U", resource: Id "E", quantity: Q.fromInt 10 } ] ]
      holding r.world (Id "U") (Id "E") `shouldEqual` Q.fromInt 5
      invariantsOf w r `shouldEqual` []

  describe "One authority predicate" do
    it "a World State and an Actor's View of it answer every Intent the same" do
      let
        rule = banR { jurisdiction: Id "A", target: Id "B" }
        base = baseWorld [ explicitActor "A" [ MayEnact (Id "tariff"), MayTransfer (Id "E"), MayTrade (Id "EM") ], explicitActor "B" [] ]
        w = prepareRound (base { rules = Map.singleton (Id "ban") rule, resources = Map.update (\x -> Just x { transferability = Restricted }) (Id "E") base.resources })
        a = fromMaybe (explicitActor "A" []) (Map.lookup (Id "A") w.actors)
        move res = Transfer { from: Id "A", to: Id "B", resource: Id res, quantity: Q.fromInt 1 }

        intents :: Array Intent
        intents = [ move "M", move "E", move "unknown", ChangeRule (Repeal (Id "ban")), ChangeRule (Repeal (Id "gone")), ChangeRule (Enact rule), Trade { exchange: Id "EM", quantity: Q.fromInt 1, limitPrice: Nothing } ]
        ask facts caps = map (authorized facts caps (Id "A")) intents
      for_ [ a.capabilities, [], [ MayEnact (Id "accessBan") ], [ MayTransfer (Id "M") ] ] \caps ->
        ask (viewFacts (observe w a)) caps `shouldEqual` ask (worldFacts w) caps

  describe "Tampered results trip each invariant" do
    let
      invariantsOf w r = map _.invariant (check w r)
      empty w = either (\e -> unsafeCrashWith (show e)) identity (resolveRound w [])
      withEvent ev r = r { record = r.record { events = r.record.events <> [ ev ] } }
    it "INV-01: a trade with no Intent behind it" do
      let w = (baseWorld [ explicitActor "US" [] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]) }
      invariantsOf w (withEvent (tradeE (Id "EM") (Id "US") Q.zero Q.zero) (empty w)) `shouldContain` "INV-01"
    it "INV-11: a resolver that depends on the order of its inputs fails, and the engine does not" do
      let
        w = baseWorld [ explicitActor "A" [], explicitActor "B" [] ]
        r = empty w
        two = r { record = r.record { inputs = [ submit "A" 0 [], submit "B" 0 [] ] } }
        favoured = onActor (Id "A") (\e -> e { accounts = Map.singleton (Id "M") (Q.fromInt 1) }) r.world
        dependent inputs = Right (if inputs == two.record.inputs then r.world else favoured) :: Either String _
      map _.invariant (orderIndependentBy dependent two) `shouldContain` "INV-11"
      orderIndependent w (empty w) `shouldEqual` []
    it "INV-07: an Exchange's curve that creates a Resource" do
      let
        w = (baseWorld [ explicitActor "US" [] ]) { exchanges = Map.singleton (Id "EM") (exchange [ "US" ]) }
        created = settledEvent 1 [ { from: Source, to: Held (Id "US"), resource: Id "E", owed: Q.fromInt 1, moved: Q.fromInt 1 } ] Nothing (OfMatch { exchange: Id "EM", buyer: Just { candidateTransaction: Id "none", intent: 0 }, seller: Nothing })
      invariantsOf w (withEvent created (empty w)) `shouldContain` "INV-07"
      invariantsOf w (withEvent (tradeE (Id "EM") (Id "US") Q.zero Q.zero) (empty w)) `shouldNotContain` "INV-07"
    it "INV-07: a plain transfer that crosses the boundary" do
      let
        w = baseWorld [ explicitActor "US" [] ]
        minted = settledEvent 1 [ { from: Source, to: Held (Id "US"), resource: Id "E", owed: Q.fromInt 1, moved: Q.fromInt 1 } ] Nothing (OfIntent { candidateTransaction: Id "none", intent: 0, mechanism: Direct })
      invariantsOf w (withEvent minted (empty w)) `shouldContain` "INV-07"
    it "INV-07: a Rule or a recipe may cross the boundary" do
      let
        w = baseWorld [ explicitActor "US" [] ]
        sunk = settledEvent 1 [ { from: Held (Id "US"), to: Sink, resource: Id "E", owed: Q.fromInt 1, moved: Q.fromInt 1 } ] Nothing (OfRule { kind: Id "tariff" })
      invariantsOf w (withEvent sunk (empty w)) `shouldNotContain` "INV-07"
    it "INV-03: a successor naming a party its parent did not bind" do
      let
        w = baseWorld [ explicitActor "US" [], explicitActor "EU" [] ]
        parent = delivery { round: 1, actor: Id "US", resource: Id "M", quantity: Q.fromInt 1, cause: testCause }
        stranger = delivery { round: 2, actor: Id "ZZ", resource: Id "M", quantity: Q.fromInt 1, cause: testCause }
        signed = \(PendingTransaction p) -> PendingTransaction p { transaction { consented = [ Id "US" ] } }
        result = (empty w) { world { pending = [ signed stranger ] } }
      invariantsOf (w { pending = [ parent ] }) result `shouldContain` "INV-03"
      invariantsOf (w { pending = [ parent ] }) ((empty w) { world { pending = [ signed parent ] } }) `shouldNotContain` "INV-03"
    it "INV-37: an obligation that falls due and is not consumed" do
      let
        w = baseWorld [ explicitActor "US" [] ]
        kept = delivery { round: 1, actor: Id "US", resource: Id "M", quantity: Q.fromInt 1, cause: testCause }
      invariantsOf w ((empty w) { world { pending = [ kept ] } }) `shouldContain` "INV-37"
    it "INV-37: an obligation due later stands, and one consumed twice is reported" do
      let
        w = baseWorld [ explicitActor "US" [] ]
        later = delivery { round: 2, actor: Id "US", resource: Id "M", quantity: Q.fromInt 1, cause: testCause }
        deliveredEvent = Settled { round: 1, legs: [ { from: Source, to: Held (Id "US"), resource: Id "M", owed: Q.fromInt 1, moved: Q.fromInt 1 } ], rate: Nothing, cause: testCause, delivered: true }
        due = delivery { round: 1, actor: Id "US", resource: Id "M", quantity: Q.fromInt 1, cause: testCause }
        twice = (empty w) { record { events = [ deliveredEvent, deliveredEvent ] } }
      invariantsOf w ((empty w) { world { pending = [ later ] } }) `shouldNotContain` "INV-37"
      map _.invariant (checkAfter (w { pending = [ due ] }) twice) `shouldContain` "INV-37"
      map _.invariant (checkAfter (w { pending = [ due, due ] }) twice) `shouldNotContain` "INV-37"
    it "INV-04: holdings that move between Actors with no event, though the totals balance" do
      let
        w = baseWorld [ explicitActor "A" [], explicitActor "B" [] ]
        r = empty w
        moved = r { world = onActor (Id "A") (\e -> e { accounts = Map.singleton (Id "M") (Q.fromInt 99) }) (onActor (Id "B") (\e -> e { accounts = Map.singleton (Id "M") (Q.fromInt 101) }) r.world) }
      invariantsOf w moved `shouldEqual` [ "INV-04", "INV-04" ]
    it "INV-05: a Resource created with no leg from the source" do
      let
        w = baseWorld [ explicitActor "A" [] ]
        r = empty w
        grown = r { world = onActor (Id "A") (\e -> e { accounts = Map.singleton (Id "M") (Q.fromInt 101) }) r.world }
      invariantsOf w grown `shouldContain` "INV-05"
    it "INV-04 and INV-05: a drawn Resource changing with no leg behind it" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }
        holder = withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" [ MayTransform (Id "F") ])
        w = (baseWorld [ holder ]) { transformations = Map.singleton (Id "F") fac, resources = Map.insert (Id "plant") (resource "plant") (baseWorld [ holder ]).resources }
        r = either (\e -> unsafeCrashWith (show e)) identity (resolveRound w [ submit "UK" 0 [ Transform { by: Id "UK", transformation: Id "F", quantity: Q.fromInt 3 } ] ])
        drained = r { world = onActor (Id "UK") (\e -> e { accounts = Map.insert (Id "plant") (Q.fromInt 7) e.accounts }) r.world }
      invariantsOf w r `shouldEqual` []
      invariantsOf w drained `shouldContain` "INV-04"
      invariantsOf w drained `shouldContain` "INV-05"
    it "INV-06: draws on one Resource are read per pool, whichever Transformation made them" do
      let
        fa = (transformation "FA") { draws = [ heldDraw (Id "plant") Q.one ] }
        fb = (transformation "FB") { draws = [ heldDraw (Id "plant") (Q.fromInt 2) ] }
        w = (baseWorld [ withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" []) ]) { transformations = Map.fromFoldable [ Tuple (Id "FA") fa, Tuple (Id "FB") fb ] }
        -- each alone is within the 10 of plant: 6 of FA draws 6, and 3 of FB draws 6
        tampered = withEvent (convE (Id "UK") (Id "FB") (Q.fromInt 3)) (withEvent (convE (Id "UK") (Id "FA") (Q.fromInt 6)) (empty w))
      invariantsOf w tampered `shouldContain` "INV-06"
    it "INV-06: a fill above the pool's capacity" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }
        w = (baseWorld [ withAccount (Id "plant") (Q.fromInt 10) (explicitActor "UK" []) ]) { transformations = Map.singleton (Id "F") fac }
      invariantsOf w (withEvent (convE (Id "UK") (Id "F") (Q.fromInt 11)) (empty w)) `shouldContain` "INV-06"
    it "INV-08: two positive holders of a transformation's capacity" do
      let
        fac = (transformation "F") { draws = [ heldDraw (Id "plant") Q.one ] }
        holds n = withAccount (Id "plant") (Q.fromInt 10) (explicitActor n [])
        w = (baseWorld [ holds "UK", holds "JP" ]) { transformations = Map.singleton (Id "F") fac }
      invariantsOf w (empty w) `shouldContain` "INV-08"
    it "INV-08: a Resource drawn from its executor, or read as an amount held, may have many positive holders" do
      let
        own = (transformation "F") { draws = [ ownDraw (Id "plant") Q.one ] }
        holds n = withAccount (Id "plant") (Q.fromInt 10) (explicitActor n [])
        w = (baseWorld [ holds "UK", holds "JP" ]) { transformations = Map.singleton (Id "F") own }
        reading = (baseWorld [ holds "UK", holds "JP" ]) { recurring = [ testRule "flow" Created (ToActor (Id "UK")) (Id "M") (HeldAmount (Id "plant")) ] }
      invariantsOf w (empty w) `shouldEqual` []
      invariantsOf reading (empty reading) `shouldEqual` []
    it "INV-09: a factor outside its estimators' bounds" do
      let
        rep = representation { explicitActor: "A", aggregate: "G", domain: "energy-demand", observes: "E", estimator: { reference: Q.fromInt 1, shrinkage: q "0.5", deviationBound: Just { lo: q "-0.2", hi: q "0.2" }, noiseSpread: Q.zero } }
        w = (baseWorld [ explicitActor "A" [], explicitActor "G" [] ]) { representations = [ rep ] }
      invariantsOf w (withEvent (Estimated { round: 1, aggregate: Id "G", domain: Id "energy-demand", factor: q "1.2" }) (empty w)) `shouldContain` "INV-09"
      invariantsOf w (withEvent (Estimated { round: 1, aggregate: Id "G", domain: Id "energy-demand", factor: q "1.05" }) (empty w)) `shouldEqual` []
    it "INV-10: a procedure CandidateTransaction executed without enough support" do
      let
        proc = { weights: Map.empty, residualWeight: q "0.1", threshold: q "0.5", authorizes: [] }
        eu = (explicitActor "EU" []) { kind = Aggregate { strategy: Id "p", members: [], procedures: Map.singleton (Id "t") proc } }
        w = baseWorld [ eu ]
        r = empty w
        p = { candidateTransaction: { id: candidateTransactionId 1 (Id "EU") 0, author: Id "EU", intents: [], signers: [], procedure: Just (Id "t") }, signatures: [] }
      invariantsOf w (r { record = r.record { inputs = [ p ] } }) `shouldEqual` [ "INV-10" ]

  describe "Order bounds and payment legs (C10)" do
    let
      fac = (transformation "F") { inputs = Map.singleton (Id "E") (Q.fromInt 1) }
      runner e = (explicitActor "A" [ MayTransform (Id "F") ]) { accounts = Map.singleton (Id "E") (Q.fromInt e) }
      order n = Transform { by: Id "A", transformation: Id "F", quantity: Q.fromInt n }
      worldOf tr e = (baseWorld [ runner e ]) { transformations = Map.singleton (Id "F") tr }
      removed = select (read @"reason" (event @"OrderRemoved"))
    it "maximum holds per Actor per round, across its CandidateTransactions" do
      let w = worldOf (fac { maximum = Just (Q.fromInt 10) }) 100
      rejections (resolveRound w [ submit "A" 0 [ order 6 ], submit "A" 1 [ order 6 ] ]) `shouldEqual` [ "F order above its maximum" ]
    it "maximum holds across the Transforms of one CandidateTransaction" do
      let w = worldOf (fac { maximum = Just (Q.fromInt 10) }) 100
      rejections (resolveRound w [ submit "A" 0 [ order 6, order 6 ] ]) `shouldEqual` [ "F order above its maximum" ]
    it "orders within the maximum all stand" do
      let w = worldOf (fac { maximum = Just (Q.fromInt 12) }) 100
      rejections (resolveRound w [ submit "A" 0 [ order 6 ], submit "A" 1 [ order 6 ] ]) `shouldEqual` []
    it "funding that scales a Transform below its minimum removes it, with the reason" do
      let w = worldOf (fac { minimum = Q.fromInt 5 }) 4
      map renderReason (removed (eventsOf (resolveRound w [ submit "A" 0 [ order 10 ] ]))) `shouldEqual` [ "F order below its minimum" ]
      either (const Q.zero) (\r -> holding r.world (Id "A") (Id "E")) (resolveRound w [ submit "A" 0 [ order 10 ] ]) `shouldEqual` Q.fromInt 4
    it "a transformation's paid amount reads its payment leg and not its inputs" do
      let
        paying = fac { payment = Just { curve: { intercept: Q.fromInt 2, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }, resource: Id "M", to: Id "B" } }
        w = (baseWorld [ (runner 10) { accounts = Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 10), Tuple (Id "M") (Q.fromInt 100) ] }, explicitActor "B" [] ]) { transformations = Map.singleton (Id "F") paying }
      map _.paid (select transformationFills (eventsOf (resolveRound w [ submit "A" 0 [ order 3 ] ]))) `shouldEqual` [ Q.fromInt 6 ]

  describe "FlowOwed queries" do
    let
      fill round party = settledEvent round
        [ { from: InExchange (Id "M1"), to: Held (Id party), resource: Id "E", owed: Q.fromInt 3, moved: Q.fromInt 3 }
        , { from: Held (Id party), to: InExchange (Id "M1"), resource: Id "Cash", owed: Q.fromInt 6, moved: Q.fromInt 6 }
        ]
        (Just (Q.fromInt 2))
        (OfMatch { exchange: Id "M1", buyer: Just { candidateTransaction: Id "p", intent: 0 }, seller: Nothing })
      query window side aggregate = { window, party: Id "A", side, resource: Just (Id "E"), cause: CurveBase, aggregate }
      evs = [ fill 1 "A", fill 2 "A", fill 2 "B" ]
    it "reads this round so far, or the round before, and only its party" do
      runLegQuery 2 evs (query ThisRoundSoFar Incoming SumMoved) `shouldEqual` Just (Q.fromInt 3)
      runLegQuery 2 evs (query LastRound Incoming SumMoved) `shouldEqual` Just (Q.fromInt 3)
      runLegQuery 3 evs (query ThisRoundSoFar Incoming SumMoved) `shouldEqual` Just Q.zero
    it "reads value at the leg's rate, and nets outgoing at zero or above" do
      runLegQuery 2 evs (query ThisRoundSoFar Incoming SumValue) `shouldEqual` Just (Q.fromInt 6)
      runLegQuery 2 evs (query ThisRoundSoFar NetOutgoing SumValue) `shouldEqual` Just Q.zero
    it "a largest-units query that finds no transformation reads nothing" do
      runLegQuery 2 evs { window: LastRound, party: Id "A", side: Outgoing, resource: Nothing, cause: TransformationOf (Id "t"), aggregate: LargestUnits } `shouldEqual` Nothing

  describe "Derived shapes" do
    it "recordShape encodes an Exchange byte for byte as a hand-written object, in both formats, and reads it back" do
      let
        m = exchange [ "US" ]
        derived = recordShape "Exchange" { id: idShape, base: idShape, quote: idShape, cost: costFunctionShape, position: qShape, reserves: mapOf idShape qShape, access: Shape.arrayOf idShape }
        written = exchangeObject
      for_ [ Snapshot, Authored ] \fmt -> do
        stringify (encodeWith fmt derived m) `shouldEqual` stringify (encodeWith fmt written m)
        (derived.codec fmt).dec (encodeWith fmt written m) `shouldEqual` Right m
  describe "Event slices" do
    let
      us = Id "US"
      evs =
        [ tradeE (Id "m") us (Q.fromInt 2) (Q.fromInt 1)
        , Lapsed { round: 1, candidateTransaction: Id "p" }
        , tradeE (Id "m") (Id "CN") (Q.fromInt 5) (Q.fromInt 1)
        ]
      traded = whereEq @"actor" us exchangeFills
    it "runSlice counts, sums and reads in recorded order" do
      runSlice (count traded) evs `shouldEqual` 1
      runSlice (total (read @"quantity" traded)) evs `shouldEqual` Q.fromInt 2
      map _.actor (runSlice (items exchangeFills) evs) `shouldEqual` [ us, Id "CN" ]
      runSlice (total (read @"moved" legs)) evs `shouldEqual` Q.fromInt 14
      runSlice (count (orKind (read @"round" (event @"Lapsed")) (read @"round" exchangeFills))) evs `shouldEqual` 3
    it "selects on a leg's fields and on the cause" do
      let
        short = settledEvent 1 [ { from: Held us, to: Sink, resource: Id "E", owed: Q.fromInt 3, moved: Q.fromInt 1 } ] Nothing testCause
      runSlice (total (read @"quantity" shortfalls)) (evs <> [ short ]) `shouldEqual` Q.fromInt 2
      map _.actor (runSlice (items shortfalls) [ short ]) `shouldEqual` [ us ]
      runSlice
        ( count
            ( whereNamed "from an Exchange"
                ( \l -> case l.from of
                    InExchange _ -> true
                    _ -> false
                )
                legs
            )
        )
        evs `shouldEqual` 2
      -- a delivery is not a shortfall even when the moved quantity is clamped
      runSlice (count shortfalls) [ Settled { round: 1, legs: [ { from: Held us, to: Sink, resource: Id "E", owed: Q.fromInt 3, moved: Q.zero } ], rate: Nothing, cause: testCause, delivered: true } ] `shouldEqual` 0
    it "describeSlice renders the slice" do
      describeSlice (count traded) `shouldEqual` "count of Settled events where the cause is a pair an Exchange cleared and actor = US"
      describeSlice (total (read @"quantity" traded)) `shouldEqual` "sum of quantity of Settled events where the cause is a pair an Exchange cleared and actor = US"
      describeSlice (count (orKind (read @"round" (event @"Lapsed")) (read @"round" exchangeFills))) `shouldEqual` "count of round of Lapsed and Settled events where the cause is a pair an Exchange cleared"
  describe "Codecs" do
    it "Q" $ quickCheck (roundTrip genQ)
    it "Capability" $ quickCheck (roundTrip genCapability)
    it "Rule" $ quickCheck (roundTrip genRule)
    it "Intent" $ quickCheck (roundTrip genIntent)
    it "CandidateTransaction" $ quickCheck (roundTrip genCandidateTransaction)
    it "ActorKind" $ quickCheck (roundTrip genKind)
    it "Event" $ quickCheck (roundTrip (genQ <#> \x -> tradeE (Id "energy-atlantic") (Id "US") x x))
    it "Transferability" $ quickCheck (roundTrip (elements (cons' Free [ Restricted, NonTransferable ])))
    it "a rejection reason compares by its value, and a stored reason reads back equal" do
      let
        r1 = OutOfScope (tariffR { jurisdiction: Id "EU", origin: Id "US", rate: q "0.5" })
        r2 = OutOfScope (tariffR { jurisdiction: Id "EU", origin: Id "US", rate: q "0.25" })
      (r1 /= r2) `shouldEqual` true
      renderReason r1 `shouldEqual` renderReason r2
      for_ [ r1, PoolExceeded, NoRouteAccess (Id "A") (Id "E") (Id "M"), Blocked (Id "tariff") ] \r ->
        decodeJson (encodeJson r) `shouldEqual` Right r
    it "the snapshot form of an Intent is the stored record's form" do
      stringify (encodeJson (Trade { exchange: Id "energy-asia", quantity: q "12", limitPrice: Nothing } :: Intent))
        `shouldEqual` "{\"values\":[{\"quantity\":\"12.0000\",\"limitPrice\":null,\"exchange\":\"energy-asia\"}],\"tag\":\"Trade\"}"
    it "the snapshot form of a rule enactment nests the Rule the same way" do
      stringify (encodeJson (ChangeRule (Enact (tariffR { jurisdiction: Id "EU", origin: Id "US", rate: q "0.5" })) :: Intent))
        `shouldEqual` "{\"values\":[{\"values\":[{\"values\":[{\"scope\":{\"over\":[\"E\"],\"governor\":\"EU\",\"direction\":{\"values\":[],\"tag\":\"Inbound\"},\"counterparty\":\"US\"},\"kind\":\"tariff\",\"effect\":{\"values\":[{\"to\":{\"values\":[],\"tag\":\"LevyToDestination\"},\"rate\":\"0.5000\",\"in\":\"M\",\"basis\":{\"values\":[],\"tag\":\"PerUnit\"}}],\"tag\":\"Levy\"}}],\"tag\":\"FlowRule\"}],\"tag\":\"Enact\"}],\"tag\":\"ChangeRule\"}"
