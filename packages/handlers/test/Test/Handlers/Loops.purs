-- | Pacific's circuits and feedback loops (docs/05_pacific.md §4.3, §9.5): every leg of each circular flow carries a
-- | flow; the bonds and the external accounts balance; and each feedback loop shows its sign under the conditions
-- | docs/05 states, with the counter-case where a countervailing force undoes it. Expected values are literals from
-- | docs/05.
module Test.Handlers.Loops (spec) where

import Prelude

import Assembly.Assemble (assemble, withRounds)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..))
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Core.Kernel.Types (Endpoint(..))
import Core.Num (Q)
import Core.Num as Q
import Data.Array (all, any, concatMap, filter, length, nub, sort, take)
import Data.Traversable (traverse)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Handlers.Simulate (SimRun)
import Laws.Slice (FlowOwed, exchangeFills, legs, select)
import Laws.Types (ActorId, ExchangeId, ResourceId)
import Laws.World (Cause(..))
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Names (p)
import Premises.Pacific.Private (deposit, loan, usBond)
import Premises.Pacific.Strategies (Levels, firmPolicy, statePolicy)
import Schema.Build.Quantity (q)
import Schema.Run (RunStatus(..), emptySpec)
import Test.Handlers.Pacific (fx, holdingAt, moved, pacificParts, playAll, ran, roundOf, shortOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (orThrow, run)
import Views.Common (StoredRun, storedRun)
import Views.Finance (bondsOf, externalAccounts)

-- | A run as the Run Store reads it back.
stored :: SimRun -> StoredRun
stored r = storedRun { id: "loops", name: "loops", status: Completed, manifest: r.manifest, playerSeed: "1", spec: emptySpec, sourceDeleted: false } r.records (map (const []) r.records)

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
presetNames = [ "FreeTrade", "Tariff", "Restrict", "Substitute", "Adaptive", "Retaliate", "StatusQuo", "Rebalance", "Cautious", "Hawk", "Populist", "Planner", "Reformer", "Exporter", "Merchant", "Builder", "Patron", "Hedger" ]

-- | A dial variant's parts.
variant :: (Pacific.Dials -> Pacific.Dials) -> Aff CompositionParts
variant f = orThrow (parts (Pacific.pacific (f Pacific.defaultDials)))

spec :: Spec Unit
spec = describe "Pacific's circuits and loops (docs/05 §4.3, §9.5)" do
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
    it "every open bond is one lender's claim and one borrower's liability, in one currency; under the status quo China's households deposit free and its firm borrows free" do
      source <- pacificParts
      r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 3
      r2 <- roundOf r 2
      let bonds = bondsOf r2.world
      all (\b -> b.lender /= b.borrower && b.principal > Q.zero) bonds `shouldEqual` true
      sort (nub (map (\b -> b.template) bonds)) `shouldEqual` sort [ usBond, deposit 2, loan 2 ]
      map (\b -> Tuple b.template b.rate) (filter (\b -> b.template /= usBond) bonds) `shouldEqual` [ Tuple (loan 2) Q.zero, Tuple (deposit 2) Q.zero ]
    it "in every country and currency, every flow across the border is goods, currency, levies or interest: the residual is zero, in every preset over 8 rounds" do
      source <- pacificParts
      for_ presetNames \name -> do
        r <- playAll source name name name name 8
        let lines = externalAccounts (stored r)
        filter (\l -> l.residual /= Q.zero) lines `shouldEqual` []
    it "financial repression moves income from households to the firm: under the status quo China's households give up interest against the 5% rate, and its firm saves it on its loans" do
      source <- pacificParts
      r <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
      let
        given template = Q.sumQ (concatMap (\x -> map (\b -> Q.mul b.principal (Q.sub (q "0.05") b.rate)) (filter (\b -> b.template == template) (bondsOf x.world))) r.records)
      [ given (deposit 2), given (loan 2) ] `shouldEqual` [ q "1.5888", q "5.2504" ]

  describe "Feedback loops, under their conditions" do
    it "F1 Learning by doing: the US's industrial competence grows while it produces; F2 Industrial decay: idle, it wears away" do
      source <- pacificParts
      building <- playAll source "Substitute" "FreeTrade" "FreeTrade" "FreeTrade" 6
      idle <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      b2 <- roundOf building 2
      b6 <- roundOf building 6
      i1 <- roundOf idle 1
      i6 <- roundOf idle 6
      [ holdingAt b2 p.us p.industryCompetence, holdingAt b6 p.us p.industryCompetence, holdingAt i1 p.us p.industryCompetence, holdingAt i6 p.us p.industryCompetence ] `shouldEqual` [ q "1.1025", q "3.3698", q "0.225", q "0.1329" ]
    it "F3 Repression grows industry, when the firm's investment is short of money: with little RMB, China's firm builds more capacity repressed than rebalanced; with plenty, the same" do
      poor <- variant (\d -> d { private = d.private { firmMoney = d.private.firmMoney { cn = q 0 } } })
      rich <- pacificParts
      let l n = { us: lv 0 0 0 0, cn: lv 0 0 n 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 2 0 }
      pr <- playLevels poor (l 2) 6
      po <- playLevels poor (l 0) 6
      rr <- playLevels rich (l 2) 6
      ro <- playLevels rich (l 0) 6
      ends <- traverse (\x -> map (\y -> holdingAt y p.cnFirm p.industryCapacity) (roundOf x 6)) [ pr, po, rr, ro ]
      ends `shouldEqual` [ q "2.875", q "2.7652", q "9.05", q "9.05" ]
    it "F4 Debt burden: at 5% China's firm pays its state interest every round it rolls its loans; free, it pays none" do
      source <- pacificParts
      off <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      high <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 2 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      -- what it repaid its state, less the principal of the loans made in rounds 1 to 5, which fell due by round 6
      let interest r = Q.sub (flowed r (flowOf p.cnFirm p.cn p.rmb)) (Q.sumQ (map (\x -> moved x p.cn p.cnFirm p.rmb) (take 5 r.records)))
      [ interest off, interest high ] `shouldEqual` [ q "1.2082", q 0 ]
    it "F5 and F6: America's deficit sells USD and the dollar falls; China's reserves buy it back and hold it up" do
      source <- pacificParts
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      held' <- playAll source "StatusQuo" "StatusQuo" "FreeTrade" "FreeTrade" 6
      f6 <- roundOf free 6
      h6 <- roundOf held' 6
      [ fx f6, fx h6 ] `shouldEqual` [ q "0.9626", q "1.084" ]
    it "F7 Strong-dollar trap, when US industry costs nearly what imports do: a strong dollar idles the capacity the US built, a weak one runs it" do
      near <- variant (\d -> d { wage = d.wage { usIndustry = q "1.95" } })
      strong <- playLevels near { us: lv 0 1 0 0, cn: lv 0 0 0 2, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      weak <- playLevels near { us: lv 0 1 0 2, cn: lv 0 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 } 6
      let made r = Q.sumQ (map (\x -> ran x p.usIndustryProduction) r.records)
      s6 <- roundOf strong 6
      w6 <- roundOf weak 6
      [ made strong, made weak, fx s6, fx w6, holdingAt s6 p.us p.industryCompetence, holdingAt w6 p.us p.industryCompetence ] `shouldEqual` [ q "1.225", q "14.2127", q "1.0645", q "0.5617", q "2.229", q "3.3698" ]
    it "F8 Home market: dividends become household purchases and the firm's sales at home" do
      source <- pacificParts
      paying <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 1 0 0 2, cnFirm: lv 1 0 0 2 } 6
      keeping <- playLevels source { us: lv 0 0 0 0, cn: lv 0 0 0 0, usFirm: lv 1 0 0 0, cnFirm: lv 1 0 0 0 } 6
      [ boughtOn paying p.usPrivate p.technologyUs, boughtOn keeping p.usPrivate p.technologyUs, soldOn paying p.usFirm p.technologyUs, soldOn keeping p.usFirm p.technologyUs ] `shouldEqual` [ q "20.9296", q "8.5109", q "20.9296", q "8.5109" ]
    it "F9 Tariff erosion: a tariff cuts the imports it is levied on, and with them its base" do
      source <- pacificParts
      tariffed <- playAll source "Tariff" "FreeTrade" "FreeTrade" "FreeTrade" 6
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      [ boughtOn tariffed p.usPrivate p.industryWorld, boughtOn free p.usPrivate p.industryWorld ] `shouldEqual` [ q "5.5769", q "7.352" ]
    it "F10 Restriction leverage: under China's export control, a US that kept its industrial competence goes short of far less than one that lost it" do
      skilled <- variant (\d -> d { competence = d.competence { us = d.competence.us { industry = q 3 } } })
      unskilled <- pacificParts
      let l = { us: lv 0 2 0 0, cn: lv 2 0 0 0, usFirm: lv 2 0 0 0, cnFirm: lv 2 0 0 0 }
      a <- playLevels skilled l 6
      b <- playLevels unskilled l 6
      let short r = Q.sumQ (map (\x -> shortOf x p.us p.industry) r.records)
      [ short a, short b ] `shouldEqual` [ q "9.008", q "14.5315" ]
    it "F11 Political constraint, through behaviour: a cautious China rebalances while its firm keeps producing, and represses again when a US export control cuts its firm's output" do
      source <- pacificParts
      calm <- playAll source "Cautious" "Cautious" "FreeTrade" "FreeTrade" 6
      shocked <- playAll source "Restrict" "Cautious" "FreeTrade" "FreeTrade" 6
      let repressing r = length (filter (\x -> any (\b -> b.template == deposit 2) (bondsOf x.world)) r.records)
      [ repressing calm, repressing shocked ] `shouldEqual` [ 1, 4 ]
    it "F12 Overcapacity cost: output it cannot sell still pays its wages, so under its own export control the US firm ends with less USD" do
      source <- pacificParts
      controlled <- playAll source "Restrict" "FreeTrade" "FreeTrade" "FreeTrade" 6
      free <- playAll source "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
      c6 <- roundOf controlled 6
      f6 <- roundOf free 6
      [ holdingAt c6 p.usFirm p.usd, holdingAt f6 p.usFirm p.usd, holdingAt c6 p.usFirm p.technology, holdingAt f6 p.usFirm p.technology ] `shouldEqual` [ q "53.9454", q "60.6411", q "17.6984", q 0 ]

  describe "No dead ends" do
    it "over 8 rounds of every preset, no Actor's currency runs out but the US firm's under Builder, which invests two units a round into a home market too thin to pay for them" do
      source <- pacificParts
      for_ presetNames \name -> do
        r <- playAll source name name name name 8
        r8 <- roundOf r 8
        let dry = filter (\(Tuple a m) -> holdingAt r8 a m <= q "0.01") [ Tuple p.us p.usd, Tuple p.cn p.rmb, Tuple p.usFirm p.usd, Tuple p.cnFirm p.rmb, Tuple p.usPrivate p.usd, Tuple p.cnPrivate p.rmb, Tuple p.row p.usd ]
        Tuple name dry `shouldEqual` Tuple name (if name == "Builder" then [ Tuple p.usFirm p.usd ] else [])
  where
  isAccount e = case e of
    Held _ -> true
    InExchange _ -> true
    _ -> false
