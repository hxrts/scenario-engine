-- | The economic model checked against itself (docs/07_simulator.md §14): properties that hold in every run because
-- | of how Pacific is authored, each of which a past error broke. No Actor claims more than it holds, so nothing it
-- | does is scaled down; every law and Rule kind is charged somewhere; wages and upkeep are what the dials say and
-- | are paid; a steady economy repeats; prices answer excess demand and supply; and a substitute runs exactly when
-- | the Strategy's condition says.
module Test.Handlers.Model (spec) where

import Prelude

import Assembly.Assemble (assemble, withRounds)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..), enactable)
import Core.Draw (Seed(..))
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (all, any, concatMap, drop, elem, filter, last, length, mapMaybe, nub, range, zip)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Either (note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Handlers.Simulate (SimRun)
import Laws.Engine.Prepare (prepareRound)
import Laws.Query (lookupQ)
import Laws.Slice (levies, select, transformationFills)
import Laws.Types (ActorId, ExchangeId, IntentF(..), ResourceId, RuleF(..), RuleKindId, idText)
import Laws.World (RoundResult, WorldState)
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Country (Country, countries, countryOf)
import Premises.Pacific.Names (p)
import Premises.Pacific.Strategies (banRule, tariffing, wageParam)
import Schema.Build.Decision (acting, asTemplate, ge, lit, roundQ, rule, selfEnact)
import Schema.Build.Quantity (q)
import Strategy.InWorld (inWorldOf)
import Test.Handlers.Pacific (blockedKinds, holdingAt, output, playAll, ran, shortOf, traded)
import Test.Handlers.Properties (describeSample, nonDecreasing, nonIncreasing, playSample, sampleOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (orThrow, run)

seats :: Array ActorId
seats = [ p.us, p.cn, p.usFirm, p.cnFirm ]

type Played = { name :: String, source :: CompositionParts, run :: SimRun }

reference :: Aff CompositionParts
reference = orThrow (parts (Pacific.pacific Pacific.defaultDials))

-- | Every preset over 8 rounds.
presetRuns :: Aff (Array Played)
presetRuns = do
  source <- reference
  for (Object.keys source.presets) \name -> do
    r <- playAll source name name name name 8
    pure { name, source, run: r }

-- | Every preset, and 20 generated runs of presets, dials, seeds, round counts and enacted Rules.
everyRun :: Aff (Array Played)
everyRun = do
  source <- reference
  byPreset <- presetRuns
  let rules = enactable source (inWorldOf source.composition)
  sampled <- for (range 1 20) \i -> do
    let s = sampleOf (Object.keys source.presets) rules i
    played <- playSample s
    pure { name: describeSample i s, source: played.source, run: played.run }
  pure (byPreset <> sampled)

-- | Each round with the World its Strategies saw: the one it was played from, prepared, so what fell due at the
-- | boundary has settled.
rounds :: SimRun -> Array (Tuple WorldState RoundResult)
rounds r = zip (map prepareRound ([ r.manifest.initial ] <> map _.world r.records)) r.records

held :: WorldState -> ActorId -> ResourceId -> Q
held w a res = maybe Q.zero (\x -> lookupQ res x.accounts) (Map.lookup a w.actors)

at :: RoundResult -> String
at x = "round " <> show x.record.round

-- | 1. Every Transformation an Actor orders runs in full: funding sets its wages aside before the Actor's other
-- | orders, and its routines order no more production than its capacity runs.
unscaled :: SimRun -> Array String
unscaled r = concatMap perRound r.records
  where
  perRound x = concatMap (\s -> concatMap (check x s.candidateTransaction) s.candidateTransaction.intents) x.record.inputs
  check x c = case _ of
    Transform t | t.quantity > Q.zero ->
      let
        done = Q.sumQ (map _.quantity (filter (\f -> f.candidateTransaction == c.id && f.transformation == t.transformation) (select transformationFills x.record.events)))
      in
        if done == t.quantity then [] else [ at x <> ": " <> idText t.by <> " ordered " <> Q.format t.quantity <> " of " <> idText t.transformation <> " and ran " <> Q.format done ]
    _ -> []

-- | 4. No participant claims more of a currency than it holds: its orders, Transfers and loans, each unlimited buy at
-- | the highest price its Exchange reaches, with the wages and upkeep funding set aside first, fit what it holds,
-- | since every participant shares out what it holds in shares that sum to no more than all of it (docs/05 §7).
claims :: SimRun -> Array String
claims r = concatMap perRound (rounds r)
  where
  perRound (Tuple w x) = concatMap (\a -> concatMap (check w x a) [ p.usd, p.rmb ]) (map _.id (values w.actors))
  check w x a c =
    let
      claimed = Q.sumQ (concatMap (\s -> if signed s then mapMaybe (claim w a c s.candidateTransaction.author) s.candidateTransaction.intents else []) x.record.inputs)
      has = held w a c
      -- the wages it owed and the upkeep it paid this round in this currency, which funding set aside first from what
      -- it held: upkeep paid beyond that came from the round's own sales
      setAside = Q.sumQ (map (\l -> if l.kind == p.wage then l.owed else l.amount) (filter (\l -> l.from == a && (l.kind == p.wage || l.kind == p.upkeep) && elem a seats && (countryOf a).money == c) (select levies x.record.events)))
    in
      if Q.add claimed (min setAside has) <= Q.add has (q "0.001") then []
      else [ at x <> ": " <> idText a <> " claims " <> Q.format claimed <> " " <> idText c <> " with " <> Q.format setAside <> " set aside, holding " <> Q.format has ]
  signed s = all (\x -> x == s.candidateTransaction.author || elem x s.signatures) s.candidateTransaction.signers
  claim w a c author = case _ of
    Trade o | author == a -> Map.lookup o.exchange w.exchanges >>= \m ->
      if o.quantity > Q.zero && m.quote == c then Just (Q.mul (fromMaybe (Q.add m.cost.intercept (Q.mul m.cost.slope m.cost.domain.hi)) o.limitPrice) o.quantity)
      else if o.quantity < Q.zero && m.base == c then Just (Q.neg o.quantity)
      else Nothing
    Transfer t | t.from == a && t.resource == c -> Just t.quantity
    _ -> Nothing

charged :: RoundResult -> ActorId -> Array RuleKindId -> { owed :: Q, paid :: Q }
charged x a kinds =
  let
    ls = filter (\l -> l.from == a && elem l.kind kinds) (select levies x.record.events)
  in
    { owed: Q.sumQ (map _.owed ls), paid: Q.sumQ (map _.amount ls) }

-- | 3. Every seat pays its wages in full, and its upkeep whenever it opened the round holding as much: funding sets
-- | both aside before anything else it orders.
wagesPaid :: SimRun -> Array String
wagesPaid r = concatMap (\(Tuple w x) -> concatMap (check w x) seats) (rounds r)
  where
  check w x a =
    let
      wages = charged x a [ p.wage ]
      upkeep = charged x a [ p.upkeep ]
      opened = held w a (countryOf a).money
    in
      (if wages.paid == wages.owed then [] else [ at x <> ": " <> idText a <> " owed " <> Q.format wages.owed <> " in wages and paid " <> Q.format wages.paid ])
        <> (if upkeep.paid == upkeep.owed || opened < upkeep.owed then [] else [ at x <> ": " <> idText a <> " owed " <> Q.format upkeep.owed <> " in upkeep, paid " <> Q.format upkeep.paid <> ", and opened with " <> Q.format opened ])

-- | 8. The wages a seat owes are its dials' wage on every unit it ran, and its upkeep the dials' rate on the capacity
-- | it holds: income to its households that no other flow stands in for.
namedFlows :: CompositionParts -> SimRun -> Array String
namedFlows source r = concatMap (\x -> concatMap (check x) seats) r.records
  where
  rate name = fromMaybe Q.zero (Object.lookup name source.composition.parameters)
  check x a =
    let
      wages = Q.sumQ (map (\f -> Q.mul (rate (wageParam f.transformation)) f.quantity) (filter (\f -> f.by == a) (select transformationFills x.record.events)))
      upkeep = Q.sumQ (map (\cap -> Q.mul (rate "upkeep-per-unit") (holdingAt x a cap)) [ p.technologyCapacity, p.industryCapacity ])
      owedWages = (charged x a [ p.wage ]).owed
      owedUpkeep = (charged x a [ p.upkeep ]).owed
    in
      (if owedWages == wages then [] else [ at x <> ": " <> idText a <> " owes " <> Q.format owedWages <> " in wages, against " <> Q.format wages <> " on what it ran" ])
        <> (if owedUpkeep == upkeep then [] else [ at x <> ": " <> idText a <> " owes " <> Q.format owedUpkeep <> " in upkeep, against " <> Q.format upkeep <> " on its capacity" ])

priceOn :: ExchangeId -> WorldState -> Q
priceOn m w = maybe Q.zero (\x -> Q.add x.cost.intercept (Q.mul x.cost.slope x.position)) (Map.lookup m w.exchanges)

-- | 7. A state orders its substitute only when its Strategy's condition holds at the round's opening prices: the
-- | good cannot come in, or making it costs no more than importing it at the exchange rate.
substituteRuns :: CompositionParts -> SimRun -> { wrong :: Array String, ran :: Int, idle :: Int }
substituteRuns source r = { wrong: concatMap _.wrong each, ran: length (filter _.ran each), idle: length (filter _.idle each) }
  where
  each = concatMap (\(Tuple w x) -> map (check w x) countries) (rounds r)
  rate name = fromMaybe Q.zero (Object.lookup name source.composition.parameters)

  check :: WorldState -> RoundResult -> Country -> { wrong :: Array String, ran :: Boolean, idle :: Boolean }
  check w x c =
    let
      ordered = Q.sumQ (concatMap (\s -> mapMaybe (substitute c) s.candidateTransaction.intents) x.record.inputs)
      inForce kind governor = any
        ( \law -> case law of
            FlowRule f -> f.kind == kind && f.scope.governor == governor
            _ -> false
        )
        (values w.rules)
      shut = inForce c.otherControl c.other || inForce c.ban c.state
      rate' = priceOn p.currency w
      importCost = if c.money == p.usd then fromMaybe Q.zero (Q.divFloor (priceOn c.world w) rate') else Q.mul (priceOn c.world w) rate'
      cheaper = rate (wageParam c.substitute) <= importCost
      able = held w c.state c.foreignCapacity > Q.zero && held w c.state c.foreignSkill > Q.zero && held w c.state c.money > q 10
    in
      { wrong:
          (if ordered > Q.zero && not (shut || cheaper) then [ at x <> ": " <> idText c.state <> " ordered " <> Q.format ordered <> " of its substitute though importing costs " <> Q.format importCost ] else [])
            <> (if ordered == Q.zero && able && (shut || cheaper) then [ at x <> ": " <> idText c.state <> " left its substitute idle though importing costs " <> Q.format importCost ] else [])
      , ran: ordered > Q.zero
      , idle: able && ordered == Q.zero
      }
  substitute c = case _ of
    Transform t | t.transformation == c.substitute && t.by == c.state -> Just t.quantity
    _ -> Nothing

spec :: Spec Unit
spec = describe "Pacific's model checked against itself (docs/07 §14)" do
  -- every preset and 20 generated runs, played once for the four properties that read them; a failure names its
  -- property and its run
  it "over every preset and 20 generated runs: nothing is scaled (every Transformation ordered runs in full); every seat pays its wages in full, and its upkeep when it can; no participant claims more than it holds; and wages and upkeep are what the dials say" do
    runs <- everyRun
    for_ runs \x -> Tuple x.name (map ("scaled: " <> _) (unscaled x.run) <> map ("unpaid: " <> _) (wagesPaid x.run) <> map ("over budget: " <> _) (claims x.run) <> map ("dials: " <> _) (namedFlows x.source x.run)) `shouldEqual` Tuple x.name []
  it "every law and Rule kind is charged somewhere: across the presets and each state's tariff, each kind levies or blocks at least once" do
    source <- reference
    byPreset <- presetRuns
    free <- orThrow (note "no FreeTrade" (Object.lookup "FreeTrade" source.presets))
    tariffed <- for [ p.us, p.cn ] \state -> do
      g <- orThrow (assemble (FromComposition source) (strategyRulesBindings (map (\a -> Tuple a (if a == state then free { rules = free.rules <> (tariffing state).rules } else free)) seats)) [])
      run (Seed 1) (withRounds (Just 4) g)
    let
      records = concatMap _.records (map _.run byPreset <> tariffed)
      seen = nub (concatMap (\x -> map _.kind (select levies x.record.events) <> blockedKinds x) records)
      kinds = [ p.wage, p.upkeep, p.usExportControl, p.cnExportControl, p.usImportBan, p.cnImportBan, p.usImportTariff, p.cnImportTariff, p.usExportTax, p.usAssetFee ]
    map idText (filter (\k -> not (elem k seen)) kinds) `shouldEqual` []
  it "a steady economy repeats: under free trade with ample stock at a flat price and a flat exchange rate, production and competence are the same every round from the second and no requirement goes short" do
    source <- orThrow (parts (Pacific.pacific (Pacific.defaultDials { stock = { technology: q 1000, industry: q 1000 }, stockSlope = { technology: q 0, industry: q 0 }, homeStock = { technology: q 1000, industry: q 1000 }, homeStockSlope = { technology: q 0, industry: q 0 }, currency = Pacific.defaultDials.currency { slope = q 0 } })))
    r <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
    let
      state x =
        [ ran x p.usTechnologyProduction, output x p.usFirm, ran x p.cnIndustryProduction, output x p.cnFirm ]
          <> [ holdingAt x p.us p.technologyCompetence, holdingAt x p.usFirm p.technologyCompetence, holdingAt x p.cn p.industryCompetence, holdingAt x p.cnFirm p.industryCompetence ]
    for_ (drop 1 r.records) \x -> state x `shouldEqual` [ q 2, q 8, q 8, q 2, q "2.7", q "10.8", q "10.8", q "2.7" ]
    for_ (drop 1 r.records) \x -> [ shortOf x p.us p.technology, shortOf x p.us p.industry, shortOf x p.cn p.technology, shortOf x p.cn p.industry ] `shouldEqual` [ q 0, q 0, q 0, q 0 ]
  it "households budget their tariff: under the US tariff with ample stock and the dollar moved off par, every order America's households place for industrial inputs fills in full" do
    source <- orThrow (parts (Pacific.pacific (Pacific.defaultDials { stock = { technology: q 1000, industry: q 1000 }, stockSlope = { technology: q 0, industry: q 0 } })))
    free <- orThrow (note "no FreeTrade" (Object.lookup "FreeTrade" source.presets))
    quo <- orThrow (note "no StatusQuo" (Object.lookup "StatusQuo" source.presets))
    g <- orThrow (assemble (FromComposition source) (strategyRulesBindings [ Tuple p.us (free { rules = free.rules <> (tariffing p.us).rules }), Tuple p.cn quo, Tuple p.usFirm free, Tuple p.cnFirm free ]) [])
    r <- run (Seed 1) (withRounds (Just 8) g)
    let
      ordered x = Q.sumQ
        ( concatMap
            ( \s ->
                if s.candidateTransaction.author /= p.usPrivate then []
                else mapMaybe
                  ( \i -> case i of
                      Trade o | o.exchange == p.industryWorld -> Just o.quantity
                      _ -> Nothing
                  )
                  s.candidateTransaction.intents
            )
            x.record.inputs
        )
      rows = map (\x -> Tuple (ordered x) (traded x p.usPrivate p.industry)) r.records
    -- the tariff is in force from round 2 and they hold RMB from round 2, so the orders are not all empty
    (Q.sumQ (map (\(Tuple o _) -> o) (drop 2 rows)) > Q.one) `shouldEqual` true
    map (\(Tuple o t) -> Q.format (Q.sub o t)) rows `shouldEqual` map (const "0.0000") rows
  it "prices answer the market: under free trade the industrial inputs' price never falls and ends above where it began, within its curve; once a US import ban removes their buyers it never rises and returns to where it began" do
    source <- reference
    free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 8
    let
      d = Pacific.defaultDials
      price x = priceOn p.industryWorld x.world
      rising = map price free.records
    Tuple (map Q.format rising) (nonDecreasing rising && maybe false (_ > d.industryPrice) (last rising) && all (_ <= Q.add d.industryPrice (Q.mul d.stockSlope.industry d.stock.industry)) rising) `shouldEqual` Tuple (map Q.format rising) true
    policy <- orThrow (note "no FreeTrade" (Object.lookup "FreeTrade" source.presets))
    let banning = policy { rules = policy.rules <> [ rule [ acting p.us, ge roundQ (lit 4) ] [ asTemplate (selfEnact (banRule p.us)) ] ] }
    g <- orThrow (assemble (FromComposition source) (strategyRulesBindings (map (\a -> Tuple a (if a == p.us then banning else policy)) seats)) [])
    banned <- run (Seed 1) (withRounds (Just 8) g)
    let falling = map price (drop 4 banned.records)
    Tuple (map Q.format falling) (nonIncreasing falling && last falling == Just d.industryPrice) `shouldEqual` Tuple (map Q.format falling) true
  it "a substitute runs exactly when its condition holds: where each state has the capacity, it orders its substitute when the good cannot come in or costs no less to import at the round's prices, and never otherwise" do
    source <- reference
    dear <- orThrow (parts (Pacific.pacific (Pacific.defaultDials { wage = Pacific.defaultDials.wage { usIndustry = q 4, cnTechnology = q 2 } })))
    results <- for (concatMap (\s -> map (Tuple s) [ "Substitute", "Planner", "Reformer", "Adaptive", "Restrict" ]) [ source, dear ]) \(Tuple s name) -> do
      r <- playAll s name name name name 8
      pure (substituteRuns s r)
    concatMap _.wrong results `shouldEqual` []
    -- both branches are exercised: a substitute that ran, and one with capacity left idle
    [ any (\x -> x.ran > 0) results, any (\x -> x.idle > 0) results ] `shouldEqual` [ true, true ]
