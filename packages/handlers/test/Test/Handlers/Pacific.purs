-- | The Pacific composition's golden scenarios (docs/05_pacific.md §9): free trade, each ban, each investment,
-- | a ban met by substitution, no phantom trade, and the kernel invariants across them. Expected values are
-- | literals from docs/05, never computed.
module Test.Handlers.Pacific (spec, pacificParts) where

import Prelude

import Adapters.Filesystem (fileOps)
import Assembly.Assemble (assemble, liveBindings, presetBindings, withRounds)
import Assembly.Types (SeatBinding(..))
import Handlers.Seats (proposeAnswer, seatEnv)
import Laws.Engine.Prepare (observe)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..))
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Data.Array (filter, foldl, index, length, range, zip)
import Core.Map (values)
import Data.Foldable (for_, sum)
import Data.Traversable (for)
import Checks.Objectives (evaluateObjectives)
import Schema.Composition (Outcome(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Error.Class (throwError)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Premises.Pacific.Strategies (firmPolicy, presetId, statePolicy)
import Foreign.Object as Object
import Handlers.Replay (runReplay)
import Handlers.Simulate (SimRun)
import Laws.Invariants (orderIndependent)
import Laws.Query (lookupQ)
import Laws.Slice (event, exchangeFills, levies, select, shortfalls)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorId, IntentF(..), ResourceId)
import Laws.World (RejectReason(..), RoundResult)
import Premises.Load (loadFrom, parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Names (p, pacificId, pacificVersion)
import Schema.Build.Quantity (q)
import Schema.Build.Decision (asTemplate, eqQ, joint, lit, roundQ, rule, selfActor, signAll)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Meridian (orThrow, run)

pacificParts :: Aff CompositionParts
pacificParts = do
  loaded <- liftEffect (loadFrom fileOps (pacificId <> "/" <> pacificVersion))
  orThrow loaded

preset :: CompositionParts -> String -> Aff StrategyRules
preset parts name = case Object.lookup name parts.presets of
  Just s -> pure s
  Nothing -> throwError (error ("no preset " <> name))

-- | Play with all four seats on named presets, from a composition's parts; every round's invariants must hold.
playFour :: CompositionParts -> { us :: String, cn :: String, usFirm :: String, cnFirm :: String } -> Int -> Aff SimRun
playFour parts names rounds = do
  b <- orThrow (presetBindings parts.presets (\a -> if a == p.us then names.us else if a == p.cn then names.cn else if a == p.usFirm then names.usFirm else names.cnFirm) [ p.us, p.cn, p.usFirm, p.cnFirm ])
  g <- orThrow (assemble (FromComposition parts) b [])
  r <- run (Seed 1) (withRounds (Just rounds) g)
  checkRun r.manifest.initial r.records `shouldEqual` []
  pure r

roundOf :: SimRun -> Int -> Aff RoundResult
roundOf r k = case index r.records (k - 1) of
  Just x -> pure x
  Nothing -> throwError (error ("no round " <> show k))

holdingAt :: RoundResult -> ActorId -> ResourceId -> Q
holdingAt r a res = fromMaybe Q.zero (lookupQ res <<< _.accounts <$> Map.lookup a r.world.actors)

-- | What an Actor bought (positive) or sold (negative) of a Resource in a round's Exchange fills.
traded :: RoundResult -> ActorId -> ResourceId -> Q
traded r a res = foldl (\s f -> Q.add s f.quantity) Q.zero (filter (\f -> f.actor == a && f.resource == res) (select exchangeFills r.record.events))

shortOf :: RoundResult -> ActorId -> ResourceId -> Q
shortOf r a res = foldl (\s f -> Q.add s f.quantity) Q.zero (filter (\f -> f.actor == a && f.resource == res) (select shortfalls r.record.events))

blocked :: RoundResult -> Int
blocked r = length (filter isBlocked (select (event @"OrderRemoved") r.record.events) <> filter isBlocked (select (event @"Rejected") r.record.events))
  where
  isBlocked x = case x.reason of
    Blocked _ -> true
    _ -> false

-- | All the Money in the game: the states', the firms' and the private sectors'.
allMoney :: RoundResult -> Q
allMoney r = Q.sumQ (map (\ac -> lookupQ p.money ac.accounts) (values r.world.actors))

levied :: RoundResult -> Q
levied r = Q.sumQ (map _.amount (select levies r.record.events))

-- | Play with every seat on one preset name each (the states', then the firms').
playAll :: CompositionParts -> String -> String -> String -> String -> Int -> Aff SimRun
playAll parts us cn usFirm cnFirm = playFour parts { us, cn, usFirm, cnFirm }

spec :: Spec Unit
spec = describe "Pacific (docs/05 §9)" do
  describe "Trade, barriers and substitution (docs/05 §9.1)" do
    it "A Free trade: the US firm exports 8 T; China imports 3 and its firm 1; the US imports 5 I and its firm 1; the tax is 2 M a round" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r1 <- roundOf r 1
      traded r1 p.usFirm p.technology `shouldEqual` Q.fromInt (-8)
      traded r1 p.cn p.technology `shouldEqual` Q.fromInt 3
      traded r1 p.us p.industry `shouldEqual` Q.fromInt 5
      traded r1 p.cn p.industry `shouldEqual` Q.fromInt (-4)
      traded r1 p.cnFirm p.industry `shouldEqual` Q.fromInt (-2)
      map (\a -> holdingAt r1 a p.money) [ p.us, p.cn, p.usFirm, p.cnFirm ] `shouldEqual` map Q.fromInt [ 108, 85, 54, 53 ]
      for_ r.records \x -> do
        levied x `shouldEqual` Q.fromInt 2
        map (\(Tuple a g) -> shortOf x a g) [ Tuple p.us p.industry, Tuple p.cn p.technology, Tuple p.usFirm p.industry, Tuple p.cnFirm p.technology ] `shouldEqual` map Q.fromInt [ 0, 0, 0, 0 ]
        allMoney x `shouldEqual` Q.fromInt 300
      r6 <- roundOf r 6
      holdingAt r6 p.us p.money `shouldEqual` Q.fromInt 48
    it "B US import ban: from round 2 the US is short 5 I and its firm 1, and China's state holds 4 more unsold I each round" do
      parts <- pacificParts
      r <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      shortOf r2 p.us p.industry `shouldEqual` Q.fromInt 5
      shortOf r2 p.usFirm p.industry `shouldEqual` Q.fromInt 1
      traded r2 p.cn p.industry `shouldEqual` Q.zero
      traded r2 p.cn p.technology `shouldEqual` Q.fromInt 3
      blocked r2 `shouldEqual` 4
      r6 <- roundOf r 6
      map (\a -> holdingAt r6 a p.industry) [ p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 20, 10 ]
    it "C China's import ban: from round 2 China is short 3 T and its firm 1, and the US firm, selling only to its own state, holds 20 unsold T by round 6" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 3
      shortOf r2 p.cnFirm p.technology `shouldEqual` Q.fromInt 1
      traded r2 p.usFirm p.technology `shouldEqual` Q.fromInt (-4)
      traded r2 p.us p.industry `shouldEqual` Q.fromInt 5
      r6 <- roundOf r 6
      holdingAt r6 p.usFirm p.technology `shouldEqual` Q.fromInt 20
    it "D US substitution: investing costs the US state 2 T of its own need in round 1; it holds 4 K_I and imports 3 I in round 3, and 6 and 1 in round 4" do
      parts <- pacificParts
      r <- playAll parts "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r1 <- roundOf r 1
      shortOf r1 p.us p.technology `shouldEqual` Q.fromInt 2
      r3 <- roundOf r 3
      holdingAt r3 p.us p.industryCapacity `shouldEqual` Q.fromInt 4
      traded r3 p.us p.industry `shouldEqual` Q.fromInt 3
      r4 <- roundOf r 4
      holdingAt r4 p.us p.industryCapacity `shouldEqual` Q.fromInt 6
      traded r4 p.us p.industry `shouldEqual` Q.fromInt 1
    it "E The importer pays the tariff: under the US tariff the US firm pays 1 M a round for its input, and trade goes on" do
      parts <- pacificParts
      r <- playAll parts "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      traded r2 p.cn p.industry `shouldEqual` Q.fromInt (-4)
      holdingAt r2 p.usFirm p.money `shouldEqual` Q.fromInt 57
      holdingAt r2 p.cn p.money `shouldEqual` Q.fromInt 90
    it "F Adaptation: under a barrier against it a state builds its substitute: China's technology capacity 0, 0, 0, 2, 4, 4; reversed, the US's industrial capacity 0, 0, 2, 4, 6, 6" do
      parts <- pacificParts
      forward <- playAll parts "Restrict" "Adaptive" "FreeTrade" "FreeTrade" 6
      map (\x -> holdingAt x p.cn p.technologyCapacity) forward.records `shouldEqual` map Q.fromInt [ 0, 0, 0, 2, 4, 4 ]
      reverse <- playAll parts "Adaptive" "Restrict" "FreeTrade" "FreeTrade" 6
      map (\x -> holdingAt x p.us p.industryCapacity) reverse.records `shouldEqual` map Q.fromInt [ 0, 0, 2, 4, 6, 6 ]
    it "H Replay and submission order reproduce a run; a ban enacted twice is one Rule beside the tax" do
      parts <- pacificParts
      b <- orThrow (presetBindings parts.presets (\a -> if a == p.us then presetId.hawk else if a == p.cn then presetId.reformer else presetId.exporter) [ p.us, p.cn, p.usFirm, p.cnFirm ])
      g <- orThrow (assemble (FromComposition parts) b [])
      a <- run (Seed 1) g
      checkRun a.manifest.initial a.records `shouldEqual` []
      out <- orThrow (runReplay (seatEnv (Seed 1) g) 6 (map _.record a.records) g)
      map _.world out.records `shouldEqual` map _.world a.records
      let befores = [ a.manifest.initial ] <> map _.world a.records
      for_ (zip befores a.records) \(Tuple before x) -> orderIndependent before x `shouldEqual` []
      for_ a.records \x -> allMoney x `shouldEqual` Q.fromInt 300
      restricted <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 3
      r3 <- roundOf restricted 3
      Map.size r3.world.rules `shouldEqual` 2
    it "H A ban cannot be bypassed by a gift Transfer, a joint candidate Transaction, or a routed purchase" do
      parts <- pacificParts
      restrict <- preset parts presetId.restrict
      let
        gift = rule [ eqQ roundQ (lit 2) ] [ asTemplate (Transfer { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 3 }) ]
        jointGift = rule [ eqQ roundQ (lit 2) ] [ joint { signers: [ p.cn ], transfers: [ { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 1 } ], transforms: [] } ]
        route = rule [] [ asTemplate (Route { pair: { base: p.technology, quote: p.money }, quantity: lit 4, limitPrice: Just (lit 1) }) ]
      g <- orThrow (assemble (FromComposition parts) (strategyRulesBindings [ Tuple p.us { rules: [ gift, jointGift ] <> restrict.rules, sign: [] }, Tuple p.cn { rules: [ route ] <> restrict.rules, sign: [ signAll ] }, Tuple p.usFirm restrict, Tuple p.cnFirm restrict ]) [])
      r <- run (Seed 1) (withRounds (Just 2) g)
      checkRun r.manifest.initial r.records `shouldEqual` []
      r2 <- roundOf r 2
      holdingAt r2 p.cn p.technology `shouldEqual` Q.zero
      shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 3
      (blocked r2 >= 3) `shouldEqual` true
    it "A human China seat plays its routine after its speech: commerce sells only what the spoken investment leaves" do
      parts <- pacificParts
      g <- orThrow (assemble (FromComposition parts) (liveBindings [ p.cn ] parts.config) [])
      case Map.lookup p.cn g.seats, Map.lookup p.cn g.world.actors of
        Just b@(Composed _), Just cn -> do
          let
            sells v = foldl
              ( \s c -> foldl
                  ( \t i -> case i of
                      Trade x | x.quantity < Q.zero -> Q.add t x.quantity
                      _ -> t
                  )
                  s
                  c.intents
              )
              Q.zero
              (proposeAnswer (seatEnv (Seed 1) g) p.cn v b)
            invests n = [ Transform { by: p.cn, transformation: p.cnTechnologyInvestment, quantity: Q.fromInt n } ]
            view = observe g.world cn
          sells view `shouldEqual` Q.fromInt (-4)
          sells (view { proposed = invests 1 }) `shouldEqual` Q.zero
          -- an order above the maximum of 1 holds back only what 1 unit uses
          sells (view { proposed = invests 4 }) `shouldEqual` Q.zero
        _, _ -> throwError (error "the China seat is not composed with its routine")

  describe "The firms, credit and the private sectors (docs/05 §9.2)" do
    it "I Credit: China's firm invests at any rate, and pays its state 2 M a unit at the high rate" do
      parts <- pacificParts
      high <- playAll parts "FreeTrade" "Planner" "FreeTrade" "Exporter" 2
      free <- playAll parts "FreeTrade" "Reformer" "FreeTrade" "Exporter" 2
      h1 <- roundOf high 1
      f1 <- roundOf free 1
      map (\x -> holdingAt x p.cnFirm p.money) [ h1, f1 ] `shouldEqual` map Q.fromInt [ 48, 50 ]
      h2 <- roundOf high 2
      f2 <- roundOf free 2
      map (\x -> holdingAt x p.cnFirm p.industryCapacity) [ h2, f2 ] `shouldEqual` map Q.fromInt [ 3, 3 ]
    it "P Subsidy: the US pays its firm 2 M for a unit it invested the round before" do
      parts <- pacificParts
      subsidised <- playAll parts "Hawk" "FreeTrade" "Exporter" "FreeTrade" 2
      plain <- playAll parts "Restrict" "FreeTrade" "Exporter" "FreeTrade" 2
      s2 <- roundOf subsidised 2
      p2 <- roundOf plain 2
      Q.sub (holdingAt s2 p.usFirm p.money) (holdingAt p2 p.usFirm p.money) `shouldEqual` Q.fromInt 2
    it "J Paying households: firms paying a high share put 7.5 M in each private sector in round 1, and the US firm sells 3 T at home in round 2" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Patron" "Patron" 6
      r1 <- roundOf r 1
      map (\a -> holdingAt r1 a p.money) [ p.usPrivate, p.cnPrivate ] `shouldEqual` [ q "7.5", q "7.5" ]
      r2 <- roundOf r 2
      traded r2 p.usFirm p.technology `shouldEqual` Q.fromInt (-3)
    it "K Accumulating against a weak home market: China's firm reaches 8 capacity by round 6, and the US firm holds over 40 unsold T" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Builder" "Builder" 6
      r6 <- roundOf r 6
      holdingAt r6 p.cnFirm p.industryCapacity `shouldEqual` Q.fromInt 8
      (holdingAt r6 p.usFirm p.technology > Q.fromInt 40) `shouldEqual` true
    it "L Levies: the export tax alone is 2 M a round; with the US tariff, 8 M" do
      parts <- pacificParts
      r <- playAll parts "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      map levied r.records `shouldEqual` map Q.fromInt [ 2, 8, 8, 8, 8, 8 ]
    it "M Asymmetry: released alike, China's private sector saves what America's spends" do
      parts <- pacificParts
      r <- playAll parts "Rebalance" "Rebalance" "FreeTrade" "FreeTrade" 6
      r6 <- roundOf r 6
      holdingAt r6 p.cnPrivate p.money `shouldEqual` q "49.5238"
      holdingAt r6 p.usPrivate p.money `shouldEqual` q "31.0646"
    it "N Who a ban hurts: a US ban leaves China's state with 20 unsold I and its firm 10; a Chinese ban leaves the US firm with 20 unsold T" do
      parts <- pacificParts
      us <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      cn <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      u6 <- roundOf us 6
      c6 <- roundOf cn 6
      map (\a -> holdingAt u6 a p.industry) [ p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 20, 10 ]
      map (\a -> holdingAt c6 a p.technology) [ p.us, p.usFirm ] `shouldEqual` map Q.fromInt [ 0, 20 ]
    it "O Stockpiling: a firm's stock of its input covers the first rounds of a ban against it" do
      parts <- pacificParts
      us <- playAll parts "Restrict" "FreeTrade" "Hedger" "Hedger" 6
      map (\x -> shortOf x p.usFirm p.industry) us.records `shouldEqual` [ Q.zero, Q.zero, q "0.75", Q.one, Q.one, Q.one ]
      cn <- playAll parts "FreeTrade" "Restrict" "Hedger" "Hedger" 6
      map (\x -> shortOf x p.cnFirm p.technology) cn.records `shouldEqual` [ Q.zero, Q.zero, q "0.6", Q.one, Q.one, Q.one ]
      bare <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      map (\x -> shortOf x p.usFirm p.industry) bare.records `shouldEqual` [ Q.zero, Q.one, Q.one, Q.one, Q.one, Q.one ]

  describe "Balance (docs/05 §9.4)" do
    it "over 400 sampled settings of every seat's four tools, each seat meets a like number of its four criteria" do
      parts <- pacificParts
      let lcg x = (x * 75 + 74) `mod` 65537
      let lvOf x = { one: x `mod` 3, two: (x / 3) `mod` 3, three: (x / 9) `mod` 3, four: (x / 27) `mod` 3 }
      rows <- for (range 1 400) \i -> do
        let x1 = lcg (lcg (i * 131))
        let x2 = lcg x1
        let x3 = lcg x2
        let x4 = lcg x3
        g <- orThrow (assemble (FromComposition parts) (strategyRulesBindings [ Tuple p.us (statePolicy p.us (lvOf x1)), Tuple p.cn (statePolicy p.cn (lvOf x2)), Tuple p.usFirm (firmPolicy p.usFirm (lvOf x3)), Tuple p.cnFirm (firmPolicy p.cnFirm (lvOf x4)) ]) [])
        r <- run (Seed 1) (withRounds (Just 6) g)
        let os = evaluateObjectives parts.composition r.records
        let met a = sum (map (\o -> length (filter (_ == Met) (map _.outcome o.criteria))) (filter (\o -> o.explicitActor == a) os))
        pure [ met p.us, met p.cn, met p.usFirm, met p.cnFirm ]
      map (\i -> sum (map (\x -> fromMaybe 0 (index x i)) rows)) [ 0, 1, 2, 3 ] `shouldEqual` [ 980, 1011, 1002, 1018 ]

  describe "Sensitivity (docs/05 §10)" do
    it "over the spend shares, the capacity split, the tax rate, credit and the home premium, each structural difference keeps its effect" do
      let
        d = Pacific.defaultDials
        variants =
          [ d
          , d { private = d.private { spendShare = { us: q "0.6", cn: q "0.3" } } }
          , d { technologyCapacity = { us: Q.fromInt 4, usFirm: Q.fromInt 6 } }
          , d { industryCapacity = { cn: Q.fromInt 6, cnFirm: Q.fromInt 4 } }
          , d { taxRate = q "0.25" }
          , d { taxRate = Q.one }
          , d { credit = Q.fromInt 4 }
          , d { homePremium = q "1.5" }
          ]
      for_ variants \dials -> do
        source <- orThrow (parts (Pacific.pacific dials))
        -- a US ban leaves China holding industrial inputs it could not export
        banned <- playAll source "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 4
        b4 <- roundOf banned 4
        (holdingAt b4 p.cn p.industry > Q.zero) `shouldEqual` true
        -- a stockpile shortens the firm's shortfall under a ban against it
        hedged <- playAll source "Restrict" "FreeTrade" "Hedger" "FreeTrade" 4
        h4 <- roundOf hedged 4
        b2 <- roundOf banned 2
        h2 <- roundOf hedged 2
        (shortOf h2 p.usFirm p.industry < shortOf b2 p.usFirm p.industry) `shouldEqual` true
        (holdingAt h4 p.usFirm p.money > Q.zero) `shouldEqual` true
        -- released alike, China's private sector saves more than America's
        released <- playAll source "Rebalance" "Rebalance" "FreeTrade" "FreeTrade" 6
        r6 <- roundOf released 6
        (holdingAt r6 p.cnPrivate p.money > holdingAt r6 p.usPrivate p.money) `shouldEqual` true
        -- the US state's income from its firm's exports is positive
        free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 2
        f2 <- roundOf free 2
        (levied f2 > Q.zero) `shouldEqual` true
