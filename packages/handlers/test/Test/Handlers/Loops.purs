-- | Pacific's circuits and feedback loops (docs/05_pacific.md §4.5, §7): every leg of each circular flow carries a
-- | flow; the bonds and the external accounts balance; and each feedback loop shows its sign under the conditions
-- | docs/05 states, with the counter-case where a countervailing force undoes it. Expected values are literals from
-- | docs/05.
module Test.Handlers.Loops (spec, experiments, indicatorsSpec) where

import Prelude

import Assembly.Assemble (assemble, withRounds)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..))
import Checks.Run (checkRun)
import Checks.Objectives (evaluateObjectives)
import Schema.Composition (Outcome(..))
import Core.Draw (Seed(..))
import Core.Kernel.Types (Endpoint(..))
import Core.Num (Q)
import Core.Num as Q
import Data.Array (all, any, concatMap, drop, filter, length, nub, sort, take)
import Data.Traversable (traverse)
import Data.Foldable (for_, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Handlers.Simulate (SimRun)
import Laws.Slice (FlowOwed, exchangeFills, legs, select)
import Laws.Types (ActorId, ExchangeId, ResourceId)
import Laws.World (Cause(..))
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Names (p)
import Schema.Run (RunStatus(..), emptySpec)
import Views.Common (storedRun)
import Views.Indicators (countryIndicators, seatIndicators)
import Premises.Pacific.Private (deposit, firmBond, loan, usBond)
import Premises.Pacific.Strategies (Levels, controlRule, firmPolicy, statePolicy)
import Schema.Build.Decision (acting, asTemplate, ge, lit, roundQ, rule, selfEnact)
import Schema.Build.Quantity (q)

import Test.Handlers.Pacific (fx, holdingAt, moved, pacificParts, playAll, playTariffed, ran, roundOf, shortOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (orThrow, run)

import Laws.External (bondsOf)

lv :: Int -> Int -> Int -> Int -> Levels
lv one two three four = { one, two, three, four }

-- | Play every seat on its own levels for some rounds; every round's invariants must hold.
playLevels :: CompositionParts -> { us :: Levels, cn :: Levels, usFirm :: Levels, cnFirm :: Levels } -> Int -> Aff SimRun
playLevels source l rounds = do
  let bindings = strategyRulesBindings [ Tuple p.us (statePolicy p.us l.us), Tuple p.cn (statePolicy p.cn l.cn), Tuple p.usFirm (firmPolicy p.usFirm l.usFirm), Tuple p.cnFirm (firmPolicy p.cnFirm l.cnFirm) ]
  g <- orThrow (assemble (FromComposition source) bindings [])
  r <- run (Seed 1) (withRounds (Just rounds) g)
  checkRun r.manifest.initial r.records `shouldEqual` []
  pure r

-- | What moved over a run in the settled Flows a predicate picks.
flowed :: SimRun -> (FlowOwed -> Boolean) -> Q
flowed r keep = Q.sumQ (concatMap (\x -> map _.moved (filter keep (select legs x.record.events))) r.records)

held :: ActorId -> Endpoint -> Boolean
held a = case _ of
  Held b -> a == b
  _ -> false

-- | A settled Flow of a currency from one Actor to another, by its cause.
wageTo :: ActorId -> FlowOwed -> Boolean
wageTo a l = held a l.to && case l.cause of
  OfRule _ -> true
  _ -> false

flowOf :: ActorId -> ActorId -> ResourceId -> FlowOwed -> Boolean
flowOf from to res l = held from l.from && held to l.to && l.resource == res

-- | What an Actor bought on one Exchange over a run.
boughtOn :: SimRun -> ActorId -> ExchangeId -> Q
boughtOn r a x = Q.sumQ (concatMap (\y -> map _.quantity (filter (\f -> f.actor == a && f.exchange == x && f.quantity > Q.zero) (select exchangeFills y.record.events))) r.records)

-- | What an Actor sold on one Exchange over a run.
soldOn :: SimRun -> ActorId -> ExchangeId -> Q
soldOn r a x = Q.neg (Q.sumQ (concatMap (\y -> map _.quantity (filter (\f -> f.actor == a && f.exchange == x && f.quantity < Q.zero) (select exchangeFills y.record.events))) r.records))

-- | Every preset's run of 8 rounds.
presetNames :: Array String
presetNames = [ "FreeTrade", "Ban", "Restrict", "Substitute", "Adaptive", "Retaliate", "StatusQuo", "Rebalance", "Cautious", "Hawk", "Populist", "Planner", "Reformer", "Exporter", "Merchant", "Builder", "Patron", "Hedger" ]

-- | A dial variant's parts.
variant :: (Pacific.Dials -> Pacific.Dials) -> Aff CompositionParts
variant f = orThrow (parts (Pacific.pacific (f Pacific.defaultDials)))

spec :: Spec Unit
spec = describe "Pacific's circuits and loops (docs/05 §5, §9)" do
  arrearsSpec
  describe "Circular flows" do
    it "every leg of the domestic, savings, external and world-savings circuits carries a flow" do
      source <- pacificParts
      r <- playLevels source { us: lv 0 0 1 0, cn: lv 0 0 1 1, usFirm: lv 1 0 1 1, cnFirm: lv 1 0 1 1 } 6
      let
        positive x = x > Q.zero
        legsOf =
          -- 1 domestic: wages, transfers and dividends to households, and their purchases at home
          [ flowed r (wageTo p.usPrivate)
          , flowed r (wageTo p.cnPrivate)
          , flowed r (flowOf p.us p.usPrivate p.usd)
          , flowed r (flowOf p.usFirm p.usPrivate p.usd)
          , flowed r (flowOf p.cnFirm p.cnPrivate p.rmb)
          , boughtOn r p.usPrivate p.technologyUs
          , boughtOn r p.cnPrivate p.industryCn
          -- 2 savings: deposits, China's loan to its firm, and repayment with interest
          , flowed r (flowOf p.usPrivate p.us p.usd)
          , flowed r (flowOf p.cnPrivate p.cn p.rmb)
          , flowed r (flowOf p.cn p.cnFirm p.rmb)
          , flowed r (flowOf p.us p.usPrivate p.usd)
          , flowed r (flowOf p.cn p.cnPrivate p.rmb)
          , flowed r (flowOf p.cnFirm p.cn p.rmb)
          -- 3 external: US importers buy RMB, China buys USD, China lends the US, US households import
          , soldOn r p.usPrivate p.currency
          , boughtOn r p.cn p.currency
          , flowed r (flowOf p.cn p.us p.usd)
          , boughtOn r p.usPrivate p.industryWorld
          , boughtOn r p.cnPrivate p.technologyWorld
          -- 4 world savings: it buys USD, lends the US, and is repaid
          , boughtOn r p.row p.currency
          , flowed r (flowOf p.row p.us p.usd)
          , flowed r (flowOf p.us p.row p.usd)
          ]
      map positive legsOf `shouldEqual` map (const true) legsOf
      -- nothing leaves the economy: every currency Flow has an Actor or a reserve at both ends
      flowed r (\l -> (l.resource == p.usd || l.resource == p.rmb) && not (isAccount l.from && isAccount l.to)) `shouldEqual` Q.zero

  describe "Accounting" do
    it "every open bond is one lender's claim and one borrower's liability, in one currency; under the status quo China's households deposit free, its firm borrows free, and America's households lend their state and their firm" do
      source <- pacificParts
      r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 3
      r2 <- roundOf r 2
      let bonds = bondsOf r2.world
      all (\b -> b.lender /= b.borrower && b.principal >= Q.zero) bonds `shouldEqual` true
      sort (nub (map (\b -> b.template) bonds)) `shouldEqual` sort [ usBond, firmBond, deposit 2, loan 2 ]
      nub (map (\b -> Tuple b.template b.rate) (filter (\b -> b.template /= usBond && b.template /= firmBond) bonds)) `shouldEqual` [ Tuple (loan 2) Q.zero, Tuple (deposit 2) Q.zero ]
    it "financial repression moves income from households to the firm: under the status quo China's households give up interest against the 5% rate, and its firm saves it on its loans" do
      source <- pacificParts
      r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
      let
        given template = Q.sumQ (concatMap (\x -> map (\b -> Q.mul b.principal (Q.sub (q "0.05") b.rate)) (filter (\b -> b.template == template) (bondsOf x.world))) r.records)
      [ given (deposit 2), given (loan 2) ] `shouldEqual` [ q "2.1376", q "8.419" ]

  describe "Feedback loops, under their conditions" do
    it "F1 Learning by doing: the US's industrial competence grows while it produces; F2 Industrial decay: idle, it wears away" do
      source <- pacificParts
      building <- playAll source "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      idle <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      b2 <- roundOf building 2
      b6 <- roundOf building 6
      i1 <- roundOf idle 1
      i6 <- roundOf idle 6
      [ holdingAt b2 p.us p.industryCompetence, holdingAt b6 p.us p.industryCompetence, holdingAt i1 p.us p.industryCompetence, holdingAt i6 p.us p.industryCompetence ] `shouldEqual` [ q "1.1025", q "2.46", q "0.225", q "0.1329" ]
    it "F3 Repression grows industry: China's firm, financing its investment by its state's loans, builds more capacity repressed than rebalanced, the more so the less money of its own it opens with" do
      poor <- variant (\d -> d { private = d.private { firmMoney = d.private.firmMoney { cn = q 0 } } })
      rich <- pacificParts
      let l n = { us: lv 0 0 0 0, cn: lv 0 0 n 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 2 0 }
      pr <- playLevels poor (l 2) 6
      po <- playLevels poor (l 0) 6
      rr <- playLevels rich (l 2) 6
      ro <- playLevels rich (l 0) 6
      ends <- traverse (\x -> map (\y -> holdingAt y p.cnFirm p.industryCapacity) (roundOf x 6)) [ pr, po, rr, ro ]
      ends `shouldEqual` [ q "4.9089", q 3, q "5.5357", q 4 ]
    it "F4 Debt burden: at 5% China's firm pays its state interest every round it rolls its loans; free, it pays none" do
      source <- pacificParts
      off <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      high <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 2 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      -- what it repaid its state, less the principal of the loans made in rounds 1 to 4, which fell due by round 6
      let interest r = Q.sub (flowed r (flowOf p.cnFirm p.cn p.rmb)) (Q.sumQ (map (\x -> moved x p.cn p.cnFirm p.rmb) (take 4 r.records)))
      [ interest off, interest high ] `shouldEqual` [ q "0.9271", q 0 ]
    it "Debt-driven postponement: repressing, China's state lends its firm a fifth of its growing RMB each round, so the firm repays every loan from the next and never falls behind, while what it owes grows from 17.6 to about 43 RMB over the run, whether it sells at home or abroad" do
      poor <- variant (\d -> d { private = d.private { firmMoney = d.private.firmMoney { cn = q 0 } } })
      thin <- playLevels poor { us: lv 0 0 0 0, cn: lv 0 0 2 0, usFirm: lv 2 0 0 0, cnFirm: lv 0 0 2 0 } 8
      abroad <- playLevels poor { us: lv 0 0 0 0, cn: lv 0 0 2 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 2 0 } 8
      let owed r = map (\x -> Q.sumQ (map _.principal (filter (\b -> b.borrower == p.cnFirm) (bondsOf x.world)))) r.records
      let unpaid r = map (\x -> Q.sumQ (map (\l -> Q.sub l.owed l.moved) (filter (\l -> held p.cnFirm l.from && isAgreement l.cause) (select legs x.record.events)))) r.records
      (owed thin <> owed abroad <> unpaid thin <> unpaid abroad) `shouldEqual` [ q "17.6", q "33.385", q "32.2795", q "34.0278", q "35.6796", q "36.9637", q "38.1319", q "39.3719", q "17.6", q "33.361", q "32.1875", q "33.6158", q "35.2107", q "36.7532", q "38.1389", q "39.3611", q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0, q 0 ]
    it "F5 and F6: America's deficit sells USD and the dollar falls; China's reserves buy it back and hold it up" do
      source <- pacificParts
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      held' <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
      f6 <- roundOf free 6
      h6 <- roundOf held' 6
      [ fx f6, fx h6 ] `shouldEqual` [ q "0.9131", q "1.0227" ]
    it "F7 Strong-dollar trap: US industry costs a little more than imports do at par, so a strong dollar leaves the capacity the US built idle and its competence lags; a weak one runs it" do
      near <- pacificParts
      strong <- playLevels near { us: lv 0 1 0 0, cn: lv 0 0 0 2, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      weak <- playLevels near { us: lv 0 1 0 2, cn: lv 0 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      let made r = Q.sumQ (map (\x -> ran x p.usIndustryProduction) r.records)
      s6 <- roundOf strong 6
      w6 <- roundOf weak 6
      [ made strong, made weak, fx s6, fx w6, holdingAt s6 p.us p.industryCompetence, holdingAt w6 p.us p.industryCompetence ] `shouldEqual` [ q 0, q "12.2148", q "0.9984", q "0.5665", q "2.1085", q "3.1829" ]
    it "F8 Home market: dividends become household purchases and the firm's sales at home" do
      source <- pacificParts
      paying <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 1 0 0 2, cnFirm: lv 1 0 0 2 } 6
      keeping <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 1 0 0 0, cnFirm: lv 1 0 0 0 } 6
      [ boughtOn paying p.usPrivate p.technologyUs, boughtOn keeping p.usPrivate p.technologyUs, soldOn paying p.usFirm p.technologyUs, soldOn keeping p.usFirm p.technologyUs ] `shouldEqual` [ q "11.7521", q "7.8738", q "11.7521", q "7.8738" ]
    it "F9 Tariff erosion: a tariff cuts the imports it is levied on, and with them its base" do
      source <- pacificParts
      tariffed <- playTariffed source 6
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      [ boughtOn tariffed p.usPrivate p.industryWorld, boughtOn free p.usPrivate p.industryWorld ] `shouldEqual` [ q "4.2894", q "5.6237" ]
    it "F10 Restriction leverage: under China's export control, a US that kept its industrial competence goes short of far less than one that lost it" do
      skilled <- variant (\d -> d { competence = d.competence { us = d.competence.us { industry = q 3 } } })
      unskilled <- pacificParts
      let l = { us: lv 0 2 0 0, cn: lv 2 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
      a <- playLevels skilled l 6
      b <- playLevels unskilled l 6
      let short r = Q.sumQ (map (\x -> shortOf x p.us p.industry) r.records)
      [ short a, short b ] `shouldEqual` [ q 9, q "14.5175" ]
    it "F11 Political constraint, through behaviour: a cautious China rebalances while its firm keeps producing, and represses again when a US export control cuts its firm's output" do
      source <- pacificParts
      calm <- playAll source "Cautious" "Cautious" "FreeTrade" "FreeTrade" 6
      shocked <- playAll source "Restrict" "Cautious" "FreeTrade" "FreeTrade" 6
      let repressing r = length (filter (\x -> any (\b -> b.template == deposit 2) (bondsOf x.world)) r.records)
      [ repressing calm, repressing shocked ] `shouldEqual` [ 2, 5 ]
    it "F12 Overcapacity cost: output it cannot sell still pays its wages, so under its own export control the US firm ends with less USD" do
      source <- pacificParts
      controlled <- playAll source "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      c6 <- roundOf controlled 6
      f6 <- roundOf free 6
      [ holdingAt c6 p.usFirm p.usd, holdingAt f6 p.usFirm p.usd, holdingAt c6 p.usFirm p.technology, holdingAt f6 p.usFirm p.technology ] `shouldEqual` [ q "22.7354", q "27.8181", q 8, q 0 ]

  describe "No dead ends" do
    it "over 8 rounds of every preset, no Actor's currency runs out" do
      source <- pacificParts
      for_ presetNames \name -> do
        r <- playAll source name name name name 8
        r8 <- roundOf r 8
        let dry = filter (\(Tuple a m) -> holdingAt r8 a m <= q "0.01") [ Tuple p.us p.usd, Tuple p.cn p.rmb, Tuple p.usFirm p.usd, Tuple p.cnFirm p.rmb, Tuple p.usPrivate p.usd, Tuple p.cnPrivate p.rmb, Tuple p.row p.usd ]
        Tuple name dry `shouldEqual` Tuple name []
  where
  isAgreement c = case c of
    OfAgreement _ -> true
    _ -> false
  isAccount e = case e of
    Held _ -> true
    InExchange _ -> true
    _ -> false

-- | The derived indicators on Pacific runs (docs/05 §6).
indicatorsSpec :: Spec Unit
indicatorsSpec = describe "Derived indicators (docs/05 §6)" do
  it "each indicator on a Pacific run: the status quo's net external positions, domestic absorption, the firms' debt service, the states' viability margins and each country's substitute gap" do
    source <- pacificParts
    r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
    let
      s = storedRun { id: "indicators", name: "indicators", status: Completed, manifest: r.manifest, playerSeed: "1", spec: emptySpec } r.records (map (const []) r.records)
      cs = filter (\x -> x.round == 6) (countryIndicators s)
      ss = filter (\x -> x.round == 6) (seatIndicators source.composition s)
    map (\c -> Tuple c.country c.netExternal) cs `shouldEqual` [ Tuple p.cn (q "27.9314"), Tuple p.row (q "81.0464"), Tuple p.us (q "-38.2266") ]
    -- every firm offers all it makes abroad, so neither country's households buy any of its output
    map (\c -> Tuple c.country (map (\a -> Tuple a.good a.share) c.absorption)) cs `shouldEqual` [ Tuple p.cn [ Tuple p.industry (Just (q "0.0418")) ], Tuple p.row [], Tuple p.us [ Tuple p.technology (Just Q.zero) ] ]
    -- China's firm owes 4.6 times what it sells; the US state, selling nothing, has no ratio, and its viability failed
    map (\x -> Tuple x.actor (Tuple x.debtService x.margins)) ss `shouldEqual` [ Tuple p.cn (Tuple (Just (q "0.4731")) [ q 2 ]), Tuple p.cnFirm (Tuple (Just (q "3.5846")) []), Tuple p.us (Tuple Nothing [ q "-0.2356" ]), Tuple p.usFirm (Tuple (Just (q "0.0574")) []) ]
    -- the strong dollar leaves America's substitute dearer than importing, so it stands idle; China's runs
    map (\c -> Tuple c.country (map (\g -> Tuple g.good g.gap) c.gaps)) cs `shouldEqual` [ Tuple p.cn [ Tuple p.technology (q "-0.2283") ], Tuple p.row [], Tuple p.us [ Tuple p.industry (q "0.1364") ] ]

-- | The experiments (docs/05 §9).
experiments :: Spec Unit
experiments = describe "Experiments (docs/05 §9)" do
  it "Political trap: rebalancing costs China no industrial output; a US export control cuts it to 9 a round, still above its floor of 8; its households gain the interest repression took" do
    source <- pacificParts
    let arm cn us = { us, cn, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
    repressed <- playLevels source (arm (lv 0 0 2 2) (lv 0 0 0 0)) 6
    rebalanced <- playLevels source (arm (lv 0 0 0 2) (lv 0 0 0 0)) 6
    shocked <- playLevels source (arm (lv 0 0 0 2) (lv 2 0 0 0)) 6
    let
      industry r = map (\x -> Q.add (ran x p.cnIndustryProduction) (Q.add (ran x p.cnFirmProduction) (ran x p.cnFirmBaseProduction))) r.records
      purchases r = Q.sumQ (map (\x -> boughtBy' x p.cnPrivate) r.records)
      wagesOf r = flowed r (wageTo p.cnPrivate)
    (industry repressed <> industry rebalanced <> industry shocked <> [ purchases repressed, purchases rebalanced, wagesOf repressed, wagesOf rebalanced ]) `shouldEqual` [ q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 10, q 9, q 9, q 9, q 9, q "6.3344", q "6.4368", q 18, q 18 ]
  it "Dollar trap: the asset fee weakens the dollar; with US industry costing a little more than imports do at par, the strong dollar holds it back and the weaker one runs it, and US households, paid its wages, buy more; where US industry is cheap, the dollar does not matter" do
    source <- pacificParts
    cheap <- variant (\d -> d { wage = d.wage { usIndustry = q "1.5" } })
    let arm us = { us, cn: lv 0 0 2 2, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
    noFee <- playLevels source (arm (lv 0 1 0 0)) 6
    fee <- playLevels source (arm (lv 0 1 0 2)) 6
    cheapNoFee <- playLevels cheap (arm (lv 0 1 0 0)) 6
    cheapFee <- playLevels cheap (arm (lv 0 1 0 2)) 6
    let
      made r = Q.sumQ (map (\x -> ran x p.usIndustryProduction) r.records)
      purchases r = Q.sumQ (map (\x -> boughtBy' x p.usPrivate) r.records)
    e1 <- roundOf noFee 6
    e2 <- roundOf fee 6
    [ fx e1, fx e2, purchases noFee, purchases fee, made noFee, made fee, made cheapNoFee, made cheapFee ] `shouldEqual` [ q "0.9978", q "0.6666", q "11.5127", q "11.9179", q 0, q "9.1179", q "14.2316", q "14.2316" ]
  it "Lost knowledge: with the same 4 units of industrial capacity under China's export control, the US that kept its workforce runs them all from round 2, when the control takes effect; the one that lost it runs a quarter of a unit, learning a twentieth more a round" do
    skilled <- variant (\d -> d { industryCapacity = d.industryCapacity { us = q 4 }, competence = d.competence { us = d.competence.us { industry = q 6 } } })
    unskilled <- variant (\d -> d { industryCapacity = d.industryCapacity { us = q 4 } })
    let l = { us: lv 0 0 0 0, cn: lv 2 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
    a <- playLevels skilled l 6
    b <- playLevels unskilled l 6
    (map (\x -> ran x p.usIndustryProduction) a.records <> map (\x -> ran x p.usIndustryProduction) b.records <> map (\x -> shortOf x p.us p.industry) a.records <> map (\x -> shortOf x p.us p.industry) b.records) `shouldEqual` [ q 0, q 4, q 4, q 4, q 4, q 4, q 0, q "0.225", q "0.2363", q "0.2481", q "0.2605", q "0.2736", q 0, q 5, q 1, q 1, q 1, q 1, q 0, q 5, q "4.775", q "4.7637", q "4.7519", q "4.7395" ]
  it "International leverage: China's export control costs America 5 I a round until it substitutes, building down to 1.06 by round 8; China's import ban nearly halves the US firm's sales abroad and trims its money by a sixth, while its debt barely moves" do
    source <- pacificParts
    let arm us cn = { us, cn, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
    supplyIdle <- playLevels source (arm (lv 0 0 0 0) (lv 2 0 0 0)) 8
    supplyBuilding <- playLevels source (arm (lv 0 2 0 0) (lv 2 0 0 0)) 8
    demand <- playLevels source (arm (lv 0 0 0 0) (lv 1 0 0 0)) 8
    free <- playLevels source (arm (lv 0 0 0 0) (lv 0 0 0 0)) 8
    let
      short r = map (\x -> shortOf x p.us p.industry) r.records
      soldAbroad r = Q.neg (Q.sumQ (map (\x -> Q.sumQ (map _.quantity (filter (\fl -> fl.actor == p.usFirm && fl.exchange == p.technologyWorld && fl.quantity < Q.zero) (select exchangeFills x.record.events)))) r.records))
      firmDebt r = map (\x -> Q.sumQ (map _.principal (filter (\b -> b.borrower == p.usFirm) (bondsOf x.world)))) (drop 7 r.records)
    d8 <- roundOf demand 8
    f8 <- roundOf free 8
    (short supplyIdle <> short supplyBuilding <> [ soldAbroad demand, soldAbroad free, holdingAt d8 p.usFirm p.usd, holdingAt f8 p.usFirm p.usd ] <> firmDebt demand <> firmDebt free) `shouldEqual` [ q 0, q 5, q 5, q 5, q 5, q 5, q 5, q 5, q 0, q 5, q "3.775", q "2.7137", q "1.5994", q "1.4294", q "1.2509", q "1.0634", q 36, q 64, q "27.748", q "33.0181", q "0.9872", q "1.4047" ]
  it "Mutually reinforcing dependence: under the status quo China lends America more dollars over the run, from 4.2 to about 9.7 a round, while America keeps buying about 7 units of China's industrial inputs a round and China's industry runs at about 10" do
    source <- pacificParts
    r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
    let
      lent = map (\x -> moved x p.cn p.us p.usd) r.records
      americaBuys = map (\x -> Q.sumQ (map _.quantity (filter (\fl -> (fl.actor == p.us || fl.actor == p.usFirm || fl.actor == p.usPrivate) && fl.resource == p.industry && fl.quantity > Q.zero) (select exchangeFills x.record.events)))) r.records
      chinaMakes = map (\x -> Q.add (ran x p.cnIndustryProduction) (Q.add (ran x p.cnFirmProduction) (ran x p.cnFirmBaseProduction))) r.records
    (lent <> americaBuys <> chinaMakes) `shouldEqual` [ q 0, q "4.2", q "2.6874", q "7.0224", q "5.2632", q "9.6901", q 6, q "7.855", q "6.7802", q "7.0244", q "7.0311", q "7.2007", q 10, q 10, q 10, q 10, q 10, q 10 ]
  it "Status quo against rebalancing: each country's arrangement, kept or undone, alone or together, over 8 rounds; then the other's export control from round 5" do
    source <- pacificParts
    let
      -- each builds a little of its substitute throughout
      keepUs = lv 0 1 0 0
      undoUs = lv 0 1 0 2
      keepCn = lv 0 1 2 2
      undoCn = lv 0 1 0 0
      arms = [ Tuple keepUs keepCn, Tuple undoUs keepCn, Tuple keepUs undoCn, Tuple undoUs undoCn ]
      -- each firm sells half at home, so households can buy from the first round
      firms = { usFirm: lv 1 0 0 0, cnFirm: lv 1 0 0 0 }
      margins r =
        [ Q.sub (minOf (map (\x -> boughtBy' x p.usPrivate) r.records)) (q "1.25")
        , Q.sub (minOf (map (\x -> Q.add (ran x p.cnIndustryProduction) (Q.add (ran x p.cnFirmProduction) (ran x p.cnFirmBaseProduction))) r.records)) (q "9.2")
        ]
    rows <- traverse
      ( \(Tuple us cn) -> do
          r <- playLevels source { us, cn, usFirm: firms.usFirm, cnFirm: firms.cnFirm } 8
          -- the same arm, with the other country's export control from round 5: what the arrangement left it exposed to
          hitUs <- playShocked source { us, cn, usFirm: firms.usFirm, cnFirm: firms.cnFirm } p.cn
          hitCn <- playShocked source { us, cn, usFirm: firms.usFirm, cnFirm: firms.cnFirm } p.us
          r4 <- roundOf r 4
          r8 <- roundOf r 8
          let
            owed = Q.sumQ (map _.principal (filter (\b -> b.borrower == p.us && b.lender /= p.usPrivate) (bondsOf r8.world)))
            short x who res = Q.sumQ (map (\y -> shortOf y who res) (drop 4 x.records))
          pure (margins r <> [ fx r8, owed, holdingAt r4 p.us p.industryCompetence, holdingAt r4 p.cn p.technologyCompetence, short hitUs p.us p.industry, short hitCn p.cn p.technology ])
      )
      arms
    -- by arm: the US's and China's viability margins, the dollar after round 8, what the US owes abroad, US industrial
    -- and Chinese technology competence after round 4, and each one's shortfall under the other's export control
    rows `shouldEqual`
      [ [ q "0.7976", q "0.1669", q "1.1372", q "81.3056", q "2.6031", q "1.845", q "12.0934", q "2.5884" ]
      , [ q "0.8082", q "0.1669", q "0.7414", q "35.2817", q "2.8869", q "1.845", q "4.382", q "2.6754" ]
      , [ q "0.802", q "0.1669", q "0.904", q "47.995", q "2.8869", q "1.845", q "7.7872", q "2.671" ]
      , [ q "0.802", q "0.1669", q "0.5403", q "2.4262", q "2.8869", q "1.845", q "4.382", q "5.5014" ]
      ]
  where
  minOf xs = fromMaybe Q.zero (minimum xs)
  playShocked source l by = do
    let
      shock = [ rule [ acting by, ge roundQ (lit 5) ] [ asTemplate (selfEnact (controlRule by)) ] ]
      policy a levels = let x = statePolicy a levels in if a == by then x { rules = x.rules <> shock } else x
      bindings = strategyRulesBindings [ Tuple p.us (policy p.us l.us), Tuple p.cn (policy p.cn l.cn), Tuple p.usFirm (firmPolicy p.usFirm l.usFirm), Tuple p.cnFirm (firmPolicy p.cnFirm l.cnFirm) ]
    g <- orThrow (assemble (FromComposition source) bindings [])
    run (Seed 1) (withRounds (Just 8) g)
  boughtBy' x a = Q.sumQ (map _.quantity (filter (\f -> f.actor == a && f.quantity > Q.zero && (f.resource == p.technology || f.resource == p.industry)) (select exchangeFills x.record.events)))

-- | China's firm in arrears (docs/05 §6, §9): opening with no money, investing high and paying high dividends
-- | under China's own export control, it cannot repay what falls due from round 4. China's viability reads its output,
-- | which clears its floor of 8 every round, and the further condition that its firm owes nothing in arrears, which
-- | does not: the criterion fails on the arrears alone.
arrearsSpec :: Spec Unit
arrearsSpec = it "Arrears: a Chinese firm with no money of its own, borrowing at 30%, falls behind on its loans from round 3 and fails China's viability, though its output clears the floor every round" do
  poor <- variant (\d -> d { private = d.private { firmMoney = { us: q 0, cn: q 0 } }, world = d.world { loanRate = { off: q "0.3", low: q "0.3", high: q "0.3" } } })
  b <- playLevels poor { us: lv 0 0 0 0, cn: lv 2 0 0 0, usFirm: lv 2 0 2 2, cnFirm: lv 0 0 2 2 } 8
  let
    isAgreement c = case c of
      OfAgreement _ -> true
      _ -> false
  let unpaid who r = map (\x -> Q.sumQ (map (\l -> Q.sub l.owed l.moved) (filter (\l -> l.from == Held who && isAgreement l.cause) (select legs x.record.events)))) r.records
  let viability = concatMap (\o -> take 1 (drop 2 o.criteria)) (filter (\o -> o.explicitActor == p.cn) (evaluateObjectives poor.composition b.records))
  unpaid p.cnFirm b `shouldEqual` [ q 0, q 0, q "0.699", q "2.59", q 0, q "0.7828", q "2.4743", q 0 ]
  map _.values viability `shouldEqual` [ [ q 8, q 10, q 8, q 8, q "9.9083", q 8, q 8, q "9.0257" ] ]
  map _.bound viability `shouldEqual` [ [ q 8, q 8, q 8, q 8, q 8, q 8, q 8, q 8 ] ]
  map (\c -> c.outcome == Met) viability `shouldEqual` [ false ]
