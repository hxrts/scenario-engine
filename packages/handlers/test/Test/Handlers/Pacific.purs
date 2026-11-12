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
import Data.Array (filter, foldl, index, length, zip)
import Core.Map (values)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Error.Class (throwError)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Class (liftEffect)
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
import Premises.Pacific.Strategies (presetId)
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

-- | What an Actor received in Money for its sales of a Resource in a round.
receipts :: RoundResult -> ActorId -> ResourceId -> Q
receipts r a res = foldl (\s f -> Q.add s (Q.neg (Q.mul f.price f.quantity))) Q.zero (filter (\f -> f.actor == a && f.resource == res && f.quantity < Q.zero) (select exchangeFills r.record.events))

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
  describe "Trade, bans and substitution (docs/05 §9.1)" do
    it "A Free trade: the US firm exports 8 T, half to its own state; China exports 6 I; the tax is 2 M a round" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r1 <- roundOf r 1
      traded r1 p.usFirm p.technology `shouldEqual` Q.fromInt (-8)
      traded r1 p.us p.technology `shouldEqual` Q.fromInt 4
      traded r1 p.cn p.technology `shouldEqual` Q.fromInt 4
      traded r1 p.cn p.industry `shouldEqual` Q.fromInt (-4)
      traded r1 p.cnFirm p.industry `shouldEqual` Q.fromInt (-2)
      map (\a -> holdingAt r1 a p.money) [ p.us, p.usFirm, p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 86, 56, 104, 54 ]
      for_ r.records \x -> do
        levied x `shouldEqual` Q.fromInt 2
        shortOf x p.us p.industry `shouldEqual` Q.zero
        shortOf x p.cn p.technology `shouldEqual` Q.zero
        allMoney x `shouldEqual` Q.fromInt 300
      r6 <- roundOf r 6
      holdingAt r6 p.us p.money `shouldEqual` Q.fromInt 16
    it "B US technology ban: from round 2 China is short 4 T, and the US firm, still selling to its state, holds 4 unsold" do
      parts <- pacificParts
      r <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 4
      traded r2 p.usFirm p.technology `shouldEqual` Q.fromInt (-4)
      holdingAt r2 p.usFirm p.technology `shouldEqual` Q.fromInt 4
      traded r2 p.cn p.industry `shouldEqual` Q.fromInt (-4)
      (blocked r2 > 0) `shouldEqual` true
      r6 <- roundOf r 6
      holdingAt r6 p.usFirm p.technology `shouldEqual` Q.fromInt 20
    it "C China's industrial ban binds its firm too: the US is short 6 I, and China holds 20 unsold I and its firm 10" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      shortOf r2 p.us p.industry `shouldEqual` Q.fromInt 6
      traded r2 p.cnFirm p.industry `shouldEqual` Q.zero
      traded r2 p.cn p.technology `shouldEqual` Q.fromInt 4
      r6 <- roundOf r 6
      map (\a -> holdingAt r6 a p.industry) [ p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 20, 10 ]
    it "D US substitution: investing costs the US state 2 T of its own need in round 1, and by round 6 it imports no I" do
      parts <- pacificParts
      r <- playAll parts "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r1 <- roundOf r 1
      shortOf r1 p.us p.technology `shouldEqual` Q.fromInt 2
      r3 <- roundOf r 3
      holdingAt r3 p.us p.industryCapacity `shouldEqual` Q.fromInt 4
      traded r3 p.us p.industry `shouldEqual` Q.fromInt 4
      r6 <- roundOf r 6
      holdingAt r6 p.us p.industryCapacity `shouldEqual` Q.fromInt 6
      traded r6 p.us p.industry `shouldEqual` Q.zero
    it "F Ban and adaptation: China's technology shortfall goes 0, 4, 4, 4, 2, 0; reversed, the US industrial shortfall 0, 6, 6, 4, 2, 0" do
      parts <- pacificParts
      forward <- playAll parts "Restrict" "Adaptive" "FreeTrade" "FreeTrade" 6
      map (\x -> shortOf x p.cn p.technology) forward.records `shouldEqual` map Q.fromInt [ 0, 4, 4, 4, 2, 0 ]
      reverse <- playAll parts "Adaptive" "Restrict" "FreeTrade" "FreeTrade" 6
      map (\x -> shortOf x p.us p.industry) reverse.records `shouldEqual` map Q.fromInt [ 0, 6, 6, 4, 2, 0 ]
    it "G No phantom trade: once the US needs no I, China and its firm hold what nobody buys and are paid nothing for it" do
      parts <- pacificParts
      r <- playAll parts "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r6 <- roundOf r 6
      map (\a -> holdingAt r6 a p.industry) [ p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 12, 6 ]
      receipts r6 p.cn p.industry `shouldEqual` Q.zero
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
      g <- orThrow (assemble (FromComposition parts) (strategyRulesBindings [ Tuple p.us { rules: [ gift, jointGift ] <> restrict.rules, sign: [] }, Tuple p.cn { rules: [ route ], sign: [ signAll ] }, Tuple p.usFirm restrict, Tuple p.cnFirm restrict ]) [])
      r <- run (Seed 1) (withRounds (Just 2) g)
      checkRun r.manifest.initial r.records `shouldEqual` []
      r2 <- roundOf r 2
      holdingAt r2 p.cn p.technology `shouldEqual` Q.zero
      shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 4
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

  describe "The firms and the private sectors (docs/05 §9.2)" do
    it "I Credit: a releasing China signs its firm's investment, which grows to 3 capacity; a directing China refuses it" do
      parts <- pacificParts
      signed <- playAll parts "FreeTrade" "Reformer" "FreeTrade" "Exporter" 2
      s2 <- roundOf signed 2
      holdingAt s2 p.cnFirm p.industryCapacity `shouldEqual` Q.fromInt 3
      refused <- playAll parts "FreeTrade" "Planner" "FreeTrade" "Exporter" 2
      f2 <- roundOf refused 2
      holdingAt f2 p.cnFirm p.industryCapacity `shouldEqual` Q.fromInt 2
    it "J Spending grows the home market: a firm paying half its Money lets its private sector buy at home the next round" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Patron" "Patron" 6
      r1 <- roundOf r 1
      holdingAt r1 p.usPrivate p.money `shouldEqual` Q.fromInt 25
      traded r1 p.usPrivate p.technology `shouldEqual` Q.zero
      r2 <- roundOf r 2
      traded r2 p.usPrivate p.technology `shouldEqual` Q.fromInt 10
      receipts r2 p.usFirm p.technology `shouldEqual` q "12.5"
      -- selling at home leaves the US state no technology to buy
      shortOf r2 p.us p.technology `shouldEqual` Q.fromInt 4
    it "K Accumulating against a weak home market overshoots: the US firm builds 13 capacity and holds 46 unsold T" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Builder" "Builder" 6
      r6 <- roundOf r 6
      holdingAt r6 p.usFirm p.technologyCapacity `shouldEqual` Q.fromInt 13
      holdingAt r6 p.usFirm p.technology `shouldEqual` Q.fromInt 46
    it "L The tax and the tariff fund the US state: 2 M a round from its firm, 8 M with the tariff on China" do
      parts <- pacificParts
      r <- playAll parts "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      map levied r.records `shouldEqual` map Q.fromInt [ 2, 8, 8, 8, 8, 8 ]
    it "M Asymmetry: released alike, China's private sector saves what America's spends" do
      parts <- pacificParts
      r <- playAll parts "Rebalance" "Rebalance" "FreeTrade" "FreeTrade" 6
      r6 <- roundOf r 6
      holdingAt r6 p.cnPrivate p.money `shouldEqual` q "43.525"
      holdingAt r6 p.usPrivate p.money `shouldEqual` q "13.2591"
    it "N Asymmetry: a US ban costs the US firm its sales, a Chinese ban costs mostly the Chinese state" do
      parts <- pacificParts
      us <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      cn <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      u6 <- roundOf us 6
      c6 <- roundOf cn 6
      map (\a -> holdingAt u6 a p.technology) [ p.us, p.usFirm ] `shouldEqual` map Q.fromInt [ 0, 20 ]
      map (\a -> holdingAt c6 a p.industry) [ p.cn, p.cnFirm ] `shouldEqual` map Q.fromInt [ 20, 10 ]

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
          , d { credit = Q.fromInt 2 }
          , d { homePremium = q "1.5" }
          ]
      for_ variants \dials -> do
        source <- orThrow (parts (Pacific.pacific dials))
        -- a US ban leaves the US firm holding technology it could not export
        banned <- playAll source "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 4
        b4 <- roundOf banned 4
        (holdingAt b4 p.usFirm p.technology > Q.zero) `shouldEqual` true
        -- credit is the Chinese state's to give: its firm grows only if it signs
        signed <- playAll source "FreeTrade" "Reformer" "FreeTrade" "Exporter" 3
        refused <- playAll source "FreeTrade" "Planner" "FreeTrade" "Exporter" 3
        s3 <- roundOf signed 3
        f3 <- roundOf refused 3
        (holdingAt s3 p.cnFirm p.industryCapacity > holdingAt f3 p.cnFirm p.industryCapacity) `shouldEqual` true
        -- released alike, China's private sector saves more than America's
        released <- playAll source "Rebalance" "Rebalance" "FreeTrade" "FreeTrade" 6
        r6 <- roundOf released 6
        (holdingAt r6 p.cnPrivate p.money > holdingAt r6 p.usPrivate p.money) `shouldEqual` true
        -- the US state's income from its firm's exports is positive
        free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 2
        f2 <- roundOf free 2
        (levied f2 > Q.zero) `shouldEqual` true
