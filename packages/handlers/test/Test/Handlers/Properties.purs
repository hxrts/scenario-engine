-- | Properties of Pacific over many runs rather than one (docs/07_simulator.md §14): generated whole runs keep every
-- | invariant; each sector's round-end money reconciles with the round's recorded flows and every currency's net
-- | financial assets sum to what crossed the boundary; and each policy dial moves its outcome in the direction the
-- | model says. Samples come from a fixed generator, so a failure names a sample that reproduces.
module Test.Handlers.Properties (spec, Sample, sampleOf, playSample, describeSample, playUnder, nonDecreasing, nonIncreasing) where

import Prelude

import Assembly.Assemble (assemble, withRounds)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..), enactable)
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Core.Kernel.Types (Endpoint(..))
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap, drop, elem, filter, foldl, index, last, length, range, zip, zipWith)
import Data.Foldable (all, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Handlers.Simulate (SimRun)
import Laws.External (bondsOf)
import Laws.Query (lookupQ)
import Laws.Slice (legs, select)
import Laws.StrategyRules (Expr(..), StrategyRules)
import Laws.Types (ActorId, ResourceId, Rule, RuleF(..), RuleKindId, idText)
import Laws.World (RoundResult, WorldState)
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Dials (Dials)
import Premises.Pacific.Names (p)
import Premises.Pacific.Strategies (tariffing)
import Strategy.InWorld (inWorldOf)
import Schema.Build.Decision (asTemplate, eqQ, lit, roundQ, rule, selfEnact)
import Schema.Build.Quantity (q)
import Test.Handlers.Pacific (boughtBy, fx, moved, playAll, roundOf, traded)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Run (orThrow, run)

-- | The fixed generator: each sample's numbers follow from its index.
lcg :: Int -> Int
lcg x = (x * 75 + 74) `mod` 65537

pick :: forall a. a -> Array a -> Int -> a
pick d xs n = fromMaybe d (index xs (n `mod` max 1 (length xs)))

seats :: Array ActorId
seats = [ p.us, p.cn, p.usFirm, p.cnFirm ]

-- | One generated run: a preset per seat, the dials, the player seed, the rounds, and a Rule its governor enacts.
type Sample = { presets :: Array String, dials :: Dials, seed :: Int, rounds :: Int, enact :: Maybe { round :: Int, rule :: Rule } }

describeSample :: Int -> Sample -> String
describeSample i s = "sample " <> show i <> ": presets " <> joinWith "/" s.presets <> ", seed " <> show s.seed <> ", " <> show s.rounds <> " rounds"
  <> ", spend "
  <> Q.format s.dials.private.spendShare.us
  <> "/"
  <> Q.format s.dials.private.spendShare.cn
  <> ", tax "
  <> Q.format s.dials.taxRate
  <> ", tariff "
  <> Q.format s.dials.tariffRate
  <> ", premium "
  <> Q.format s.dials.homePremium
  <> ", loan share "
  <> Q.format s.dials.world.loanShare.off
  <> maybe "" (\e -> ", enacts " <> idText (kindOf e.rule) <> " in round " <> show e.round) s.enact

kindOf :: Rule -> RuleKindId
kindOf = case _ of
  FlowRule r -> r.kind
  HoldingRule r -> r.kind

governorOf :: Rule -> Maybe ActorId
governorOf = case _ of
  FlowRule r -> Just r.scope.governor
  HoldingRule _ -> Nothing

-- | The sample of an index: every number drawn within the range docs/05 §9 sweeps, or around the reference.
sampleOf :: Array String -> Array Rule -> Int -> Sample
sampleOf presetNames rules i =
  { presets: map (\x -> pick "FreeTrade" presetNames x) [ x1, x2, x3, x4 ]
  , dials: d
      { private = d.private { spendShare = { us: pick d.private.spendShare.us (map q [ "0.5", "0.6", "0.7", "0.8", "0.9" ]) x5, cn: pick d.private.spendShare.cn (map q [ "0.1", "0.2", "0.3", "0.4" ]) x6 } }
      , taxRate = pick d.taxRate (map q [ "0", "0.1", "0.25", "0.5", "1" ]) x7
      , tariffRate = pick d.tariffRate (map q [ "0.5", "1", "1.5", "2" ]) x8
      , homePremium = pick d.homePremium (map q [ "1", "1.1", "1.25", "1.5" ]) x9
      , world = d.world { loanShare = d.world.loanShare { off = pick d.world.loanShare.off (map q [ "0.02", "0.05", "0.1" ]) x10 } }
      }
  , seed: 1 + x11 `mod` 1000
  , rounds: 1 + x12 `mod` 12
  , enact: if x13 `mod` 3 == 0 then Nothing else Just { round: 1 + x14 `mod` 6, rule: pick dummy flowRules x15 }
  }
  where
  d = Pacific.defaultDials
  xs = foldl (\acc _ -> acc <> [ lcg (fromMaybe (i * 131) (index acc (length acc - 1))) ]) [] (range 1 15)
  at k = fromMaybe 0 (index xs (k - 1))
  x1 = at 1
  x2 = at 2
  x3 = at 3
  x4 = at 4
  x5 = at 5
  x6 = at 6
  x7 = at 7
  x8 = at 8
  x9 = at 9
  x10 = at 10
  x11 = at 11
  x12 = at 12
  x13 = at 13
  x14 = at 14
  x15 = at 15
  flowRules = filter (\r -> maybe false (\g -> elem g seats) (governorOf r)) rules
  dummy = fromMaybe (HoldingRule { kind: p.usAssetFee, resource: p.usd, reduce: Q.zero }) (index flowRules 0)

-- | Play a sample: each seat on its preset, the enacting seat with one more clause enacting its Rule in its round.
playSample :: Sample -> Aff { source :: CompositionParts, run :: SimRun }
playSample s = do
  source <- orThrow (parts (Pacific.pacific s.dials))
  bound <- for (zip seats s.presets) \(Tuple seat name) -> do
    base <- presetOf source name
    pure (Tuple seat (withEnactment seat base))
  g <- orThrow (assemble (FromComposition source) (strategyRulesBindings bound) [])
  r <- run (Seed s.seed) (withRounds (Just s.rounds) g)
  pure { source, run: r }
  where
  withEnactment seat base = case s.enact of
    Just e | governorOf e.rule == Just seat -> base { rules = base.rules <> [ rule [ eqQ roundQ (lit e.round) ] [ asTemplate (selfEnact (map Lit e.rule)) ] ] }
    _ -> base

presetOf :: CompositionParts -> String -> Aff StrategyRules
presetOf source name = case Object.lookup name source.presets of
  Just s -> pure s
  Nothing -> fail ("no preset " <> name) *> pure { rules: [], sign: [] }

-- | What a generated run must keep: every invariant, no negative holding or reserve, and every Exchange's position in
-- | its domain at a positive price.
soundness :: SimRun -> Array String
soundness r = map (\f -> "invariant: " <> show f) (checkRun r.manifest.initial r.records) <> concatMap perRound r.records
  where
  perRound x =
    concatMap (\a -> map (\(Tuple res v) -> "round " <> show x.record.round <> ": " <> idText a.id <> " holds " <> Q.format v <> " " <> idText res) (filter (\(Tuple _ v) -> v < Q.zero) (Map.toUnfoldable a.accounts))) (values x.world.actors)
      <> concatMap (\e -> map (\(Tuple res v) -> "round " <> show x.record.round <> ": " <> idText e.id <> " reserves " <> Q.format v <> " " <> idText res) (filter (\(Tuple _ v) -> v < Q.zero) (Map.toUnfoldable e.reserves))) (values x.world.exchanges)
      <> concatMap (\e -> if e.position < e.cost.domain.lo || e.position > e.cost.domain.hi then [ "round " <> show x.record.round <> ": " <> idText e.id <> " position " <> Q.format e.position <> " outside its domain" ] else if price e <= Q.zero then [ "round " <> show x.record.round <> ": " <> idText e.id <> " price " <> Q.format (price e) ] else []) (values x.world.exchanges)
  price e = Q.add e.cost.intercept (Q.mul e.cost.slope e.position)

-- | Every Exchange is a bonding curve: each round its reserve of the base falls by exactly what its position rises,
-- | and its reserve of the quote changes by the area under its curve over that move, within a grid step of rounding.
-- | A flat Exchange with no reserve has no position to move, and passes trivially.
curves :: SimRun -> Array String
curves r = concat' (zipWith perRound (zip worlds (drop 1 worlds)) r.records)
  where
  concat' = concatMap identity
  worlds = [ r.manifest.initial ] <> map _.world r.records
  perRound (Tuple w0 w1) x = concatMap (\e1 -> maybe [] (check x e1) (Map.lookup e1.id w0.exchanges)) (values w1.exchanges)
  check x e1 e0 =
    let
      dPos = Q.sub e1.position e0.position
      dBase = Q.sub (lookupQ e1.base e1.reserves) (lookupQ e1.base e0.reserves)
      dQuote = Q.sub (lookupQ e1.quote e1.reserves) (lookupQ e1.quote e0.reserves)
      area = Q.mul (Q.add e0.cost.intercept (Q.div (Q.mul e0.cost.slope (Q.add e0.position e1.position)) (Q.fromInt 2))) dPos
    in
      if Q.add dBase dPos == Q.zero && Q.abs (Q.sub dQuote area) <= q "0.001" then []
      else [ "round " <> show x.record.round <> ", " <> idText e1.id <> ": position moved " <> Q.format dPos <> ", its base reserve " <> Q.format dBase <> ", its quote reserve " <> Q.format dQuote <> " against the curve's " <> Q.format area ]

-- | The sector an endpoint belongs to: each country's state, firm and private sector; the rest of the world; the
-- | Exchanges; and the boundary.
sectorOf :: Endpoint -> String
sectorOf = case _ of
  Held a
    | elem a [ p.us, p.usFirm, p.usPrivate ] -> "US"
    | elem a [ p.cn, p.cnFirm, p.cnPrivate ] -> "CN"
    | otherwise -> "ROW"
  InExchange _ -> "Exchanges"
  Source -> "boundary"
  Sink -> "boundary"

sectors :: Array String
sectors = [ "US", "CN", "ROW", "Exchanges" ]

currencies :: Array ResourceId
currencies = [ p.usd, p.rmb ]

-- | A sector's money in a currency in a World.
moneyOf :: WorldState -> String -> ResourceId -> Q
moneyOf w s c =
  Q.add (Q.sumQ (map (\a -> lookupQ c a.accounts) (filter (\a -> sectorOf (Held a.id) == s) (values w.actors))))
    (if s == "Exchanges" then Q.sumQ (map (\e -> lookupQ c e.reserves) (values w.exchanges)) else Q.zero)

-- | A sector's net financial assets in a currency: its money, plus what its bonds lend, less what they borrow.
netFinancial :: WorldState -> String -> ResourceId -> Q
netFinancial w s c = Q.add (moneyOf w s c) (Q.sub (side _.lender) (side _.borrower))
  where
  side f = Q.sumQ (map _.principal (filter (\b -> b.currency == c && sectorOf (Held (f b)) == s) (bondsOf w)))

-- | The sector accounts of one round against the World it was played from. Each sector's change in money is what the
-- | round's recorded legs moved into it less what they moved out; and in each currency the sectors' change in net
-- | financial assets sums to what crossed the boundary (every lender's claim is a borrower's liability).
sectorBalances :: WorldState -> RoundResult -> Array String
sectorBalances before x = concatMap perCurrency currencies
  where
  ls = select legs x.record.events
  inflow s c = Q.sub (Q.sumQ (map _.moved (filter (\l -> l.resource == c && sectorOf l.to == s && sectorOf l.from /= s) ls))) (Q.sumQ (map _.moved (filter (\l -> l.resource == c && sectorOf l.from == s && sectorOf l.to /= s) ls)))
  perCurrency c =
    let
      reconcile s =
        let
          stock = Q.sub (moneyOf x.world s c) (moneyOf before s c)
          flow = inflow s c
        in
          if stock == flow then [] else [ "round " <> show x.record.round <> ", " <> idText c <> ": " <> s <> "'s money changed by " <> Q.format stock <> " but its recorded flows net " <> Q.format flow ]
      total = Q.sumQ (map (\s -> Q.sub (netFinancial x.world s c) (netFinancial before s c)) sectors)
      boundary = Q.neg (inflow "boundary" c)
    in
      concatMap reconcile sectors
        <> (if total == boundary then [] else [ "round " <> show x.record.round <> ", " <> idText c <> ": net financial assets changed by " <> Q.format total <> " in sum, but " <> Q.format boundary <> " crossed the boundary" ])

-- | Every round's sector accounts in a run.
runBalances :: SimRun -> Array String
runBalances r = concat' (zipWith sectorBalances ([ r.manifest.initial ] <> map _.world r.records) r.records)
  where
  concat' = concatMap identity

nonDecreasing :: Array Q -> Boolean
nonDecreasing xs = all identity (zipWith (<=) xs (drop 1 xs))

nonIncreasing :: Array Q -> Boolean
nonIncreasing xs = all identity (zipWith (>=) xs (drop 1 xs))

-- | Play all four seats on presets under some dials; the dials' own composition.
playUnder :: Dials -> Array String -> Int -> Aff SimRun
playUnder dials names rounds = do
  source <- orThrow (parts (Pacific.pacific dials))
  case names of
    [ us, cn, usFirm, cnFirm ] -> playAll source us cn usFirm cnFirm rounds
    _ -> fail "four presets" *> playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" rounds

-- | The industrial inputs America's seats and households bought over a run's rounds from the second on.
usImports :: SimRun -> Q
usImports r = Q.sumQ (concatMap (\x -> map (\a -> max Q.zero (traded x a p.industry)) [ p.us, p.usFirm, p.usPrivate ]) (drop 1 r.records))

spec :: Spec Unit
spec = describe "Pacific over many runs (docs/07 §14)" do
  it "generated whole runs: 60 samples of presets, dials, seeds, round counts up to 12 and enacted Rules keep every invariant, hold nothing negative, and keep each Exchange in its domain" do
    reference <- orThrow (parts (Pacific.pacific Pacific.defaultDials))
    let
      presetNames = Object.keys reference.presets
      rules = enactable reference (inWorldOf reference.composition)
    enacted <- for (range 1 60) \i -> do
      let s = sampleOf presetNames rules i
      played <- playSample s
      Tuple (describeSample i s) (soundness played.run <> runBalances played.run <> curves played.run) `shouldEqual` Tuple (describeSample i s) []
      -- whether the run ends with more Rules in force than it opened with
      pure (maybe 0 (\x -> Map.size x.world.rules) (last played.run.records) > Map.size played.run.manifest.initial.rules)
    -- the samples exercise what they claim: a good share enact a Rule that takes effect
    (length (filter identity enacted) >= 20) `shouldEqual` true

  it "sector balances and curves: under every preset, each sector's money reconciles with the round's recorded flows, each currency's net financial assets sum to what crossed the boundary, and every Exchange moves along its curve" do
    source <- orThrow (parts (Pacific.pacific Pacific.defaultDials))
    for_ (Object.keys source.presets) \name -> do
      r <- playAll source name name name name 8
      Tuple name (runBalances r <> curves r) `shouldEqual` Tuple name []

  describe "Each dial moves its outcome the way the model says" do
    let d = Pacific.defaultDials
    it "a higher tariff rate never raises the industrial inputs America imports" do
      imports <- for (map q [ "0.5", "1", "1.5", "2", "3" ]) \rate -> do
        source <- orThrow (parts (Pacific.pacific d { tariffRate = rate }))
        free <- presetOf source "FreeTrade"
        g <- orThrow (assemble (FromComposition source) (strategyRulesBindings [ Tuple p.us (free { rules = free.rules <> (tariffing p.us).rules }), Tuple p.cn free, Tuple p.usFirm free, Tuple p.cnFirm free ]) [])
        r <- run (Seed 1) (withRounds (Just 6) g)
        pure (usImports r)
      Tuple (map Q.format imports) (nonIncreasing imports) `shouldEqual` Tuple (map Q.format imports) true
    it "China's export control never raises the industrial inputs America imports, at any spend share or tax rate" do
      for_ [ d, d { private = d.private { spendShare = { us: q "0.6", cn: q "0.3" } } }, d { taxRate = q "0.25" }, d { taxRate = Q.one } ] \dials -> do
        free <- playUnder dials [ "FreeTrade", "FreeTrade", "FreeTrade", "FreeTrade" ] 6
        controlled <- playUnder dials [ "FreeTrade", "Restrict", "FreeTrade", "FreeTrade" ] 6
        (usImports controlled <= usImports free) `shouldEqual` true
    it "a higher loan rate never lowers what China's firm pays its state" do
      paid <- for (map q [ "0", "0.02", "0.05", "0.1" ]) \rate -> do
        r <- playUnder d { world = d.world { loanRate = d.world.loanRate { off = rate } } } [ "FreeTrade", "FreeTrade", "FreeTrade", "FreeTrade" ] 6
        pure (Q.sumQ (map (\x -> moved x p.cnFirm p.cn p.rmb) r.records))
      Tuple (map Q.format paid) (nonDecreasing paid) `shouldEqual` Tuple (map Q.format paid) true
    it "a higher spend share never lowers what America's households buy" do
      bought <- for (map q [ "0.5", "0.6", "0.7", "0.8", "0.9" ]) \share -> do
        r <- playUnder d { private = d.private { spendShare = d.private.spendShare { us = share } } } [ "FreeTrade", "FreeTrade", "FreeTrade", "FreeTrade" ] 6
        pure (Q.sumQ (map (\x -> boughtBy x p.usPrivate) r.records))
      Tuple (map Q.format bought) (nonDecreasing bought) `shouldEqual` Tuple (map Q.format bought) true
    it "more Chinese reserves never weaken the dollar" do
      rates <- for (map q [ "0", "3", "6", "9" ]) \level -> do
        r <- playUnder d { world = d.world { reserves = { low: level, high: level } } } [ "StatusQuo", "StatusQuo", "FreeTrade", "FreeTrade" ] 6
        x <- roundOf r 6
        pure (fx x)
      Tuple (map Q.format rates) (nonDecreasing rates) `shouldEqual` Tuple (map Q.format rates) true

