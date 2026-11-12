-- | The Pacific composition's golden scenarios (docs/05_pacific.md §9): free trade, each ban, each investment,
-- | a ban met by substitution, no phantom trade, and the kernel invariants across them. Expected values are
-- | literals from docs/05, never computed.
module Test.Handlers.Pacific (spec, pacificParts, playAll, playFour, roundOf, holdingAt, traded, shortOf, output, taxes, totalOf, fx, ran, boughtBy, moved) where

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
import Laws.Slice (event, exchangeFills, levies, select, shortfalls, transfers, transformationFills)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorId, Id(..), IntentF(..), ResourceId, TransformationId)
import Laws.World (RejectReason(..), RoundResult)
import Premises.Load (loadFrom, parts)
import Schema.Run (RunStatus(..), emptySpec)
import Views.Common (storedRun)
import Views.Finance (externalAccounts)
import Premises.Pacific as Pacific
import Premises.Pacific.Names (p, pacificId, pacificVersion)
import Schema.Build.Quantity (q)
import Schema.Build.Decision (asTemplate, eqQ, joint, lit, roundQ, rule, selfActor, signAll)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (orThrow, run)

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

-- | What a firm produced in a round: its production's fills.
output :: RoundResult -> ActorId -> Q
output r a = foldl (\s f -> Q.add s f.quantity) Q.zero (filter (\f -> f.by == a && (f.transformation == p.usFirmProduction || f.transformation == p.cnFirmProduction || f.transformation == p.usFirmBaseProduction || f.transformation == p.cnFirmBaseProduction)) (select transformationFills r.record.events))

-- | The levies a round charged, apart from wages: the tariffs and the export tax.
taxes :: RoundResult -> Q
taxes r = Q.sumQ (map _.amount (filter (\l -> l.kind /= Id "wage") (select levies r.record.events)))

-- | All of a currency in the game: every Actor's, and the Exchanges' reserves.
totalOf :: ResourceId -> RoundResult -> Q
totalOf res r = Q.add (Q.sumQ (map (\ac -> lookupQ res ac.accounts) (values r.world.actors))) (Q.sumQ (map (\x -> lookupQ res x.reserves) (values r.world.exchanges)))

-- | The currency Exchange's price, RMB per USD, after a round.
fx :: RoundResult -> Q
fx r = fromMaybe Q.zero ((\x -> Q.add x.cost.intercept (Q.mul x.cost.slope x.position)) <$> Map.lookup p.currency r.world.exchanges)

-- | What a Transformation delivered of its good in a round, by whoever ran it.
ran :: RoundResult -> TransformationId -> Q
ran r t = Q.sumQ (map _.quantity (filter (\f -> f.transformation == t) (select transformationFills r.record.events)))

-- | The goods an Actor bought on the Exchanges in a round.
boughtBy :: RoundResult -> ActorId -> Q
boughtBy r a = Q.sumQ (map _.quantity (filter (\f -> f.actor == a && f.quantity > Q.zero && (f.resource == p.technology || f.resource == p.industry)) (select exchangeFills r.record.events)))

-- | What one Actor Transferred another of a Resource in a round: a loan's principal, a transfer, dividends.
moved :: RoundResult -> ActorId -> ActorId -> ResourceId -> Q
moved r from to res = Q.sumQ (map _.quantity (filter (\t -> t.from == from && t.to == to && t.resource == res) (select transfers r.record.events)))

levied :: RoundResult -> Q
levied r = Q.sumQ (map _.amount (select levies r.record.events))

-- | Play with every seat on one preset name each (the states', then the firms').
playAll :: CompositionParts -> String -> String -> String -> String -> Int -> Aff SimRun
playAll parts us cn usFirm cnFirm = playFour parts { us, cn, usFirm, cnFirm }

spec :: Spec Unit
spec = describe "Pacific (docs/05 §9)" do
  describe "Trade, barriers and substitution (docs/05 §9.1)" do
    it "A Free trade: the US firm exports 8 T, China imports 3 and its firm 1, the US imports 5 I for RMB it bought; the tax is 2 USD in round 1; each currency's total never changes" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r1 <- roundOf r 1
      [ traded r1 p.usFirm p.technology, traded r1 p.cn p.technology, traded r1 p.cnFirm p.technology, traded r1 p.us p.industry, traded r1 p.cn p.industry, traded r1 p.cnFirm p.industry, taxes r1 ] `shouldEqual` [ q (-8), q 3, q 1, q 5, q (-4), q (-2), q 2 ]
      (map (\a -> holdingAt r1 a p.usd) [ p.us, p.usFirm ] <> map (\a -> holdingAt r1 a p.rmb) [ p.cn, p.cnFirm ]) `shouldEqual` [ q "112.5", q 52, q "93.6", q "57.9" ]
      for_ r.records \x -> [ totalOf p.usd x, totalOf p.rmb x ] `shouldEqual` [ q 352, q 374 ]
    it "B US export control: China is short 3 T from round 2, and its firm, without the input half its capacity runs on, produces 2, 2, then 1 a round" do
      parts <- pacificParts
      r <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      (map (\x -> output x p.cnFirm) r.records <> map (\x -> shortOf x p.cn p.technology) r.records) `shouldEqual` [ q 2, q 2, q 1, q 1, q 1, q 1, q 0, q 3, q 3, q 3, q 3, q 3 ]
      r2 <- roundOf r 2
      [ Q.fromInt (blocked r2) ] `shouldEqual` [ q 4 ]
    it "C China's export control: the US is short 5 I from round 2; its firm produces 8, 8, then 4 a round, so China goes short of T; China keeps the industrial inputs it cannot sell" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      (map (\x -> output x p.usFirm) r.records <> map (\x -> shortOf x p.us p.industry) r.records <> map (\x -> shortOf x p.cn p.technology) r.records) `shouldEqual` [ q 8, q 8, q 4, q 4, q 4, q 4, q 0, q 5, q 5, q 5, q 5, q 5, q 0, q "0.331", q "0.447", q "1.7489", q "1.8962", q "1.9444" ]
      r6 <- roundOf r 6
      [ holdingAt r6 p.cn p.industry ] `shouldEqual` [ q 20 ]
    it "D US substitution: investing costs the US state 2 T in round 1; the capacity it builds runs only as far as the competence to run it, which training and production build" do
      parts <- pacificParts
      r <- playAll parts "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      (map (\x -> holdingAt x p.us p.industryCapacity) r.records <> map (\x -> holdingAt x p.us p.industryCompetence) r.records <> map (\x -> ran x p.usIndustryProduction) r.records) `shouldEqual` [ q 0, q 2, q "3.996", q "5.992", q "5.992", q "5.992", q "0.225", q "1.1025", q "2.0555", q "3.0565", q "3.2093", q "3.3698", q 0, q "1.2226", q "2.2839", q "3.3961", q "3.5659", q "3.7442" ]
    it "E The importer pays the tariff: under the US tariff China still sells its industrial inputs in round 2, and the US firm pays the tariff on its input" do
      parts <- pacificParts
      r <- playAll parts "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      r2 <- roundOf r 2
      [ traded r2 p.cn p.industry, holdingAt r2 p.usFirm p.usd ] `shouldEqual` [ q (-4), q "53.0093" ]
    it "F Adaptation: under a barrier against it a state builds its substitute: China's technology capacity under the US control, and the US's industrial capacity under China's" do
      parts <- pacificParts
      forward <- playAll parts "Restrict" "Adaptive" "FreeTrade" "FreeTrade" 6
      reverse <- playAll parts "Adaptive" "Restrict" "FreeTrade" "FreeTrade" 6
      (map (\x -> holdingAt x p.cn p.technologyCapacity) forward.records <> map (\x -> holdingAt x p.us p.industryCapacity) reverse.records) `shouldEqual` [ q 0, q 0, q 0, q 2, q 4, q 4, q 0, q 0, q "1.996", q "3.992", q "5.992", q "5.992" ]
    it "H Replay and submission order reproduce a run; a control enacted twice is one Rule beside the tax" do
      parts <- pacificParts
      b <- orThrow (presetBindings parts.presets (\a -> if a == p.us then presetId.hawk else if a == p.cn then presetId.reformer else presetId.exporter) [ p.us, p.cn, p.usFirm, p.cnFirm ])
      g <- orThrow (assemble (FromComposition parts) b [])
      a <- run (Seed 1) g
      checkRun a.manifest.initial a.records `shouldEqual` []
      out <- orThrow (runReplay (seatEnv (Seed 1) g) 6 (map _.record a.records) g)
      map _.world out.records `shouldEqual` map _.world a.records
      let befores = [ a.manifest.initial ] <> map _.world a.records
      for_ (zip befores a.records) \(Tuple before x) -> orderIndependent before x `shouldEqual` []
      for_ a.records \x -> [ totalOf p.usd x, totalOf p.rmb x ] `shouldEqual` [ q 352, q 374 ]
      restricted <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 3
      r3 <- roundOf restricted 3
      Map.size r3.world.rules `shouldEqual` 2
    it "H An export control cannot be bypassed by a gift Transfer, a joint candidate Transaction, or a routed purchase" do
      parts <- pacificParts
      restrict <- preset parts presetId.restrict
      let
        gift = rule [ eqQ roundQ (lit 2) ] [ asTemplate (Transfer { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 3 }) ]
        jointGift = rule [ eqQ roundQ (lit 2) ] [ joint { signers: [ p.cn ], transfers: [ { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 1 } ], transforms: [] } ]
        route = rule [] [ asTemplate (Route { pair: { base: p.technology, quote: p.usd }, quantity: lit 4, limitPrice: Just (lit 1) }) ]
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

  describe "The firms, loans and the private sectors (docs/05 §9.2)" do
    it "I Loans: repressing, China lends its firm a fifth of its RMB, free; rebalanced, a twentieth at 5%, repaid the next round" do
      parts <- pacificParts
      high <- playAll parts "FreeTrade" "StatusQuo" "FreeTrade" "Exporter" 3
      off <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "Exporter" 3
      h1 <- roundOf high 1
      o1 <- roundOf off 1
      h2 <- roundOf high 2
      o2 <- roundOf off 2
      [ moved h1 p.cn p.cnFirm p.rmb, moved o1 p.cn p.cnFirm p.rmb, moved h2 p.cnPrivate p.cn p.rmb, moved o2 p.cnPrivate p.cn p.rmb, holdingAt h2 p.cnFirm p.industryCapacity, holdingAt o2 p.cnFirm p.industryCapacity ] `shouldEqual` [ q "17.6", q "4.4", q "6.195", q "6.281", q 3, q 3 ]
    it "J Wages and dividends: on top of the wage, firms paying a high share put 7.5 in each private sector in round 1, and the US firm sells at home in round 2" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Patron" "Patron" 6
      r1 <- roundOf r 1
      r2 <- roundOf r 2
      [ moved r1 p.usFirm p.usPrivate p.usd, moved r1 p.cnFirm p.cnPrivate p.rmb, traded r2 p.usFirm p.technology ] `shouldEqual` [ q "7.5", q "7.5", q "-3.856" ]
    it "K Selling only at home: with neither firm on the world Exchange, China's firm loses its US input from round 2, and runs only the half of its capacity that needs none, which its investment grows" do
      parts <- pacificParts
      r <- playAll parts "FreeTrade" "FreeTrade" "Builder" "Builder" 6
      map (\x -> output x p.cnFirm) r.records `shouldEqual` [ q 2, q "1.5", q 2, q "2.375", q "2.875", q "3.4687" ]
    it "L Levies: the export tax alone is 2 USD in round 1; with the US tariff on the US's imports, more a round after" do
      parts <- pacificParts
      r <- playAll parts "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      map taxes r.records `shouldEqual` [ q 2, q "8.2241", q "8.2963", q "7.9305", q "8.3026", q "7.9833" ]
    it "M Asymmetry: China's households lend their state most of what they do not spend, and buy far fewer goods than America's" do
      parts <- pacificParts
      r <- playAll parts "Populist" "Populist" "FreeTrade" "FreeTrade" 6
      r6 <- roundOf r 6
      [ holdingAt r6 p.cnPrivate p.rmb, holdingAt r6 p.usPrivate p.usd, Q.sumQ (map (\x -> boughtBy x p.cnPrivate) r.records), Q.sumQ (map (\x -> boughtBy x p.usPrivate) r.records) ] `shouldEqual` [ q "11.1816", q "18.6689", q "3.5849", q "12.8052" ]
    it "N Who an export control hurts: China's control leaves it with industrial inputs it cannot sell; America's leaves the US firm with technology it cannot sell" do
      parts <- pacificParts
      cn <- playAll parts "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 6
      us <- playAll parts "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      c6 <- roundOf cn 6
      u4 <- roundOf us 4
      [ holdingAt c6 p.cn p.industry, holdingAt u4 p.us p.technology, holdingAt u4 p.usFirm p.technology ] `shouldEqual` [ q 20, q 0, q "11.45" ]
    it "O Input inventory: a firm's inventory keeps its capacity running a round further into an export control on its input" do
      parts <- pacificParts
      us <- playAll parts "FreeTrade" "Restrict" "Hedger" "Hedger" 6
      cn <- playAll parts "Restrict" "FreeTrade" "Hedger" "Hedger" 6
      (map (\x -> output x p.usFirm) us.records <> map (\x -> output x p.cnFirm) cn.records) `shouldEqual` [ q 8, q 8, q "6.8572", q 4, q 4, q 4, q 2, q 2, q 2, q "1.4", q 1, q 1 ]

  describe "Money between the countries (docs/05 §9.3)" do
    it "Q The currency: under free trade America's imports sell USD and the dollar falls; with China's reserves it holds up" do
      parts <- pacificParts
      free <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      dep <- playAll parts "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
      (map fx free.records <> map fx dep.records) `shouldEqual` [ q "0.995", q "0.9867", q "0.9889", q "0.9863", q "0.9768", q "0.9626", q "1.025", q "1.0323", q "1.0514", q "1.0666", q "1.0773", q "1.084" ]
    it "R The asset fee: foreign savings lend less and sell USD, the dollar falls, and the US state loses its foreign financing" do
      parts <- pacificParts
      fee <- playAll parts "Rebalance" "FreeTrade" "FreeTrade" "FreeTrade" 6
      free <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      f2 <- roundOf fee 2
      n2 <- roundOf free 2
      (map fx fee.records <> [ moved f2 p.row p.us p.usd, moved n2 p.row p.us p.usd ]) `shouldEqual` [ q "0.995", q "0.8897", q "0.8113", q "0.7423", q "0.6747", q "0.6051", q "6.5", q "16.25" ]

  describe "Balance (docs/05 §9.4)" do

    it "over 400 sampled settings of every seat's four tools, each seat meets a like number of its four criteria, and every external account balances" do
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
        -- and every flow across a border is accounted (docs/05 §4.3): no external account has a residual
        let unaccounted = length (filter (\l -> l.residual /= Q.zero) (externalAccounts (storedRun { id: "balance", name: "balance", status: Completed, manifest: r.manifest, playerSeed: "1", spec: emptySpec, sourceDeleted: false } r.records (map (const []) r.records))))
        pure [ met p.us, met p.cn, met p.usFirm, met p.cnFirm, unaccounted ]
      map (\i -> sum (map (\x -> fromMaybe 0 (index x i)) rows)) [ 0, 1, 2, 3, 4 ] `shouldEqual` [ 993, 997, 991, 1006, 0 ]

  describe "Sensitivity (docs/05 §10)" do
    it "over the spend shares, the capacity split, the tax rate, China's lending and the home premium, each structural difference keeps its effect" do
      let
        d = Pacific.defaultDials
        variants =
          [ d
          , d { private = d.private { spendShare = { us: q "0.6", cn: q "0.3" } } }
          , d { technologyCapacity = { us: Q.fromInt 4, usFirm: Q.fromInt 6 } }
          , d { industryCapacity = { cn: Q.fromInt 6, cnFirm: Q.fromInt 4 } }
          , d { taxRate = q "0.25" }
          , d { taxRate = Q.one }
          , d { world = d.world { loanShare = d.world.loanShare { off = q "0.1" } } }
          , d { homePremium = q "1.5" }
          ]
      for_ (zip (range 0 (length variants - 1)) variants) \(Tuple k dials) -> do
        source <- orThrow (parts (Pacific.pacific dials))
        -- China's export control leaves it holding industrial inputs it could not export
        controlled <- playAll source "FreeTrade" "Restrict" "FreeTrade" "FreeTrade" 4
        c4 <- roundOf controlled 4
        Tuple (Tuple k 1) (holdingAt c4 p.cn p.industry > Q.zero) `shouldEqual` Tuple (Tuple k 1) true
        -- an input inventory keeps a firm producing longer under a control on its input
        hedged <- playAll source "FreeTrade" "Restrict" "Hedger" "FreeTrade" 4
        c3 <- roundOf controlled 3
        h3 <- roundOf hedged 3
        Tuple (Tuple k 2) (output h3 p.usFirm > output c3 p.usFirm) `shouldEqual` Tuple (Tuple k 2) true
        -- China's households lend their state more of their savings than America's lend theirs
        free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
        Tuple (Tuple k 3) (Q.sumQ (map (\x -> moved x p.cnPrivate p.cn p.rmb) free.records) > Q.sumQ (map (\x -> moved x p.usPrivate p.us p.usd) free.records)) `shouldEqual` Tuple (Tuple k 3) true
        -- the US state's income from its firm's exports is positive
        f2 <- roundOf free 2
        Tuple (Tuple k 4) (levied f2 > Q.zero) `shouldEqual` Tuple (Tuple k 4) true
