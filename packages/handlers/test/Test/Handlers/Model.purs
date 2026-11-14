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
import Core.Kernel.Types (Account(..), Amount(..), Effect(..), Party(..)) as K
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (all, any, concatMap, drop, elem, filter, last, length, mapMaybe, nub, range, zip)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Either (note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Handlers.Simulate (SimRun)
import Laws.Engine.Prepare (prepareRound)
import Laws.Query (lookupQ)
import Laws.Slice (levies, select, transformationFills)
import Laws.Types (ActorId, ExchangeId, IntentF(..), ResourceId, RuleF(..), idText)
import Laws.World (RoundResult, WorldState)
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Country (Country, countries, countryOf)
import Premises.Pacific.Names (p)
import Premises.Pacific.Strategies (banRule, tariffing, wageParam)
import Schema.Build.Decision (acting, asTemplate, ge, lit, roundQ, rule, selfEnact)
import Schema.Build.Quantity (q)
import Strategy.InWorld (inWorldOf)
import Test.Handlers.Pacific (blockedKinds, holdingAt, output, playAll, ran, shortOf)
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

-- | What the transactions pending against an Actor and due by the round take of a Resource from it.
owing :: WorldState -> ActorId -> ResourceId -> Q
owing w a res = Q.sumQ (concatMap (\pending -> let x = unwrap pending in if x.due <= w.round then mapMaybe takes x.transaction.obligations else []) w.pending)
  where
  takes o = case o.effect of
    K.Move { from: K.HeldBy (K.Named e), resource, owed } | e == a && resource == res -> Just (fixed owed)
    _ -> Nothing
  fixed = case _ of
    K.Exactly x -> x
    K.Times t -> Q.mul t.ratio (fixed t.amount)
    _ -> Q.zero

at :: RoundResult -> String
at x = "round " <> show x.record.round

-- | 1. Every Transformation an Actor orders runs in full: its routines size each order within what it holds, so
-- | funding scales nothing down.
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

-- | 4. No Actor's signed orders, Transfers and loans claim more of a currency than it holds after what falls due
-- | from it, each unlimited buy at the highest price its Exchange reaches.
claims :: SimRun -> Array String
claims r = concatMap perRound (rounds r)
  where
  perRound (Tuple w x) = concatMap (\a -> concatMap (check w x a) [ p.usd, p.rmb ]) (map _.id (values w.actors))
  check w x a c =
    let
      claimed = Q.sumQ (concatMap (\s -> if signed s then mapMaybe (claim w a c s.candidateTransaction.author) s.candidateTransaction.intents else []) x.record.inputs)
      due = owing w a c
      has = held w a c
    in
      if due > has || Q.add claimed due <= Q.add has (q "0.001") then []
      else [ at x <> ": " <> idText a <> " claims " <> Q.format claimed <> " " <> idText c <> " and owes " <> Q.format due <> ", holding " <> Q.format has ]
  signed s = all (\x -> x == s.candidateTransaction.author || elem x s.signatures) s.candidateTransaction.signers
  claim w a c author = case _ of
    Trade o | author == a -> Map.lookup o.exchange w.exchanges >>= \m ->
      if o.quantity > Q.zero && m.quote == c then Just (Q.mul (fromMaybe (Q.add m.cost.intercept (Q.mul m.cost.slope m.cost.domain.hi)) o.limitPrice) o.quantity)
      else if o.quantity < Q.zero && m.base == c then Just (Q.neg o.quantity)
      else Nothing
    Transfer t | t.from == a && t.resource == c -> Just t.quantity
    _ -> Nothing

charged :: RoundResult -> ActorId -> Array String -> { owed :: Q, paid :: Q }
charged x a kinds =
  let
    ls = filter (\l -> l.from == a && elem (idText l.kind) kinds) (select levies x.record.events)
  in
    { owed: Q.sumQ (map _.owed ls), paid: Q.sumQ (map _.amount ls) }

-- | 3. A seat that opened the round with the money for its wages and upkeep, after what fell due from it, paid them
-- | in full.
wagesPaid :: SimRun -> Array String
wagesPaid r = concatMap (\(Tuple w x) -> concatMap (check w x) seats) (rounds r)
  where
  check w x a =
    let
      money = (countryOf a).money
      bill = charged x a [ "wage", "upkeep" ]
      spendable = Q.sub (held w a money) (owing w a money)
    in
      if bill.paid < bill.owed && spendable >= bill.owed then [ at x <> ": " <> idText a <> " owed " <> Q.format bill.owed <> " in wages and upkeep, paid " <> Q.format bill.paid <> ", and opened with " <> Q.format spendable <> " to spend" ]
      else []

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
      owedWages = (charged x a [ "wage" ]).owed
      owedUpkeep = (charged x a [ "upkeep" ]).owed
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
      able = held w c.state c.foreignCapacity > Q.zero && held w c.state c.foreignSkill > Q.zero && Q.sub (held w c.state c.money) (owing w c.state c.money) > q 10
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
  it "nothing is scaled: in every preset and 20 generated runs, every Transformation an Actor orders runs in full" do
    runs <- everyRun
    for_ runs \x -> Tuple x.name (unscaled x.run) `shouldEqual` Tuple x.name []
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
  it "wages are paid: in every preset and 20 generated runs, a seat that opened with the money for its wages and upkeep paid them in full" do
    runs <- everyRun
    for_ runs \x -> Tuple x.name (wagesPaid x.run) `shouldEqual` Tuple x.name []
  it "nobody claims more than it holds: in every preset and 20 generated runs, each Actor's orders, Transfers and loans fit each currency it holds after what falls due" do
    runs <- everyRun
    for_ runs \x -> Tuple x.name (claims x.run) `shouldEqual` Tuple x.name []
  it "a steady economy repeats: under free trade with ample stock at a flat price, production and competence are the same every round from the second and no requirement goes short" do
    source <- orThrow (parts (Pacific.pacific (Pacific.defaultDials { stock = { technology: q 1000, industry: q 1000 }, stockSlope = { technology: q 0, industry: q 0 }, homeStock = { technology: q 1000, industry: q 1000 }, homeStockSlope = { technology: q 0, industry: q 0 } })))
    r <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
    let
      state x =
        [ ran x p.usTechnologyProduction, output x p.usFirm, ran x p.cnIndustryProduction, output x p.cnFirm ]
          <> [ holdingAt x p.us p.technologyCompetence, holdingAt x p.usFirm p.technologyCompetence, holdingAt x p.cn p.industryCompetence, holdingAt x p.cnFirm p.industryCompetence ]
    for_ (drop 1 r.records) \x -> state x `shouldEqual` [ q 2, q 8, q 8, q 2, q "2.7", q "10.8", q "10.8", q "2.7" ]
    -- what each state goes short of is no more than the exchange rate's move within a round costs it: it buys the
    -- currency its imports need at the round's opening rate, and with a flat price it holds no margin over it
    for_ (drop 1 r.records) \x -> let short = [ shortOf x p.us p.technology, shortOf x p.us p.industry, shortOf x p.cn p.technology, shortOf x p.cn p.industry ] in Tuple (map Q.format short) (all (_ <= q "0.05") short) `shouldEqual` Tuple (map Q.format short) true
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
  it "wages and upkeep are what the dials say: in every preset and 20 generated runs, each seat owes its wage on every unit it ran and its upkeep on the capacity it holds" do
    runs <- everyRun
    for_ runs \x -> Tuple x.name (namedFlows x.source x.run) `shouldEqual` Tuple x.name []
