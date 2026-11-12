-- | The view models and the spec's error paths: unknown presets and overrides, out-of-range
-- | rounds, the last round's "n/a" cells, run compatibility, unreadable runs in the list, and
-- | objectives over no rounds.
-- | Pins: reply and column texts of docs/07 §9 and §10.
module Test.Views.Views (spec) where

import Prelude

import Laws.Types (Id(..), ResourceTag)
import Laws.World (Cause(..), Endpoint(..), Event(..))
import Schema.Build.Quantity (q)

import Testkit.Edit (onActor)

import Test.Views.Support (toStored)

import Schema.Run (RunClass(..), RunStatus(..), emptySpec, specPairs)
import Checks.Objectives (evaluateObjectives)
import Schema.Composition (Outcome(..))

import Core.Draw (Seed(..), tokenFor)
import Core.Num as Q
import Laws.Query (holding)
import Data.Array (all, filter, find, index, length, mapWithIndex, last, nub)
import Data.Map as Map
import Data.Either (Either(..), isLeft)
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains, split)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Handlers.Simulate (SimRun)
import Assembly.Assemble (assemblePreset)
import Assembly.Override (Override(..), resolve)
import Assembly.Composition (CompositionParts)
import Assembly.Recipe (encodeSpec)
import Foreign.Object as Object
import Testkit.Run (run, orThrow)
import Views.Balances (balances, events)
import Views.Common (Table, completed, exact, final, quantity)
import Views.Balance (measures, measuresOf)
import Views.Compare (balanceBars, baselineDiff, compatible, keyValues, netWorthCharts, standingsBars)
import Views.Report (roundReport)
import Views.BalanceSheet (balanceSheetAt, balanceSheetLinks)
import Data.Int as Int
import Views.Actor (actorHistory)
import Views.Standings (standings, standingsOf)
import Views.Glossary (glossary)
import Views.Overview (overview, runList)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

-- | A setup: every seat plays one preset for a number of rounds, under overrides and a player seed.
type Setup = { preset :: String, rounds :: Int, overrides :: Array Override, seed :: Int }

everySeat :: String -> Int -> Setup
everySeat preset rounds = { preset, rounds, overrides: [], seed: 1 }

conservative :: Int -> Setup
conservative = everySeat "FreeTrade"

-- | The verdict column of a compatibility table.
verdicts :: Table -> Array String
verdicts t = map (\row -> cell row 7) t.rows

-- | A table cell by column, or "" when the row is short.
cell :: Array String -> Int -> String
cell row i = fromMaybe "" (index row i)

play :: CompositionParts -> Setup -> Aff SimRun
play ctx s = do
  g <- orThrow (assemblePreset ctx s.preset s.overrides (Just s.rounds))
  run (Seed s.seed) g

spec :: Aff CompositionParts -> Spec Unit
spec context = describe "Views and error paths" do
  it "the stored specification is typed: what Assembly.Recipe writes is what the views read" do
    let written = encodeSpec { gameId: "g", composition: "c@v1", premises: "p@v1,q@v2", playerSeed: Seed 7, preset: Just "P", rounds: Just 3, overrides: [], cast: Map.empty, model: Nothing, humans: Nothing, runClass: Simulated, cacheKey: Nothing, tag: Nothing, metadata: Nothing }
    written.gameId `shouldEqual` Just "g"
    written.preset `shouldEqual` Just "P"
    written.playerSeed `shouldEqual` Just 7
    written.rounds `shouldEqual` Just 3
    specPairs written `shouldEqual` [ Tuple "gameId" "g", Tuple "composition" "c@v1", Tuple "premises" "p@v1,q@v2", Tuple "preset" "P", Tuple "playerSeed" "7", Tuple "overrides" "[]", Tuple "rounds" "3", Tuple "cast" "{}", Tuple "class" "simulated" ]
  it "an unknown preset or override is an error, never a silent no-op (§3, §4.2)" do
    ctx <- context
    let base = conservative 2
    case assemblePreset ctx "NoSuchPreset" [] (Just base.rounds) of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown preset NoSuchPreset")
      Right _ -> fail "an unknown preset ran"
    case resolve { composition: ctx.composition, config: ctx.config } [ Parameter "no-such-parameter" (Q.fromInt 1) ] of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown parameter no-such-parameter")
      Right _ -> fail "an unknown parameter was applied"
    case resolve { composition: ctx.composition, config: ctx.config } [ Shrinkage "no-such-scheme" (Q.fromInt 0) ] of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown scheme")
      Right _ -> fail "an unknown scheme was applied"

  it "the cross-run key values have one column per run, and their values are the views' own (§10.2)" do
    ctx <- context
    a <- play ctx (conservative 3)
    b <- play ctx (everySeat "Hawk" 3)
    let
      r1 = toStored "run-1" a
      r2 = toStored "run-2" b
      runs = [ r1, r2 ]
      kv = keyValues (const (Just ctx.composition)) runs
      rowOf label = fromMaybe [] (find (\row -> cell row 0 == label) kv.rows)
      valueIn label r = fromMaybe "" (find (\row -> cell row 0 == label) (measures r).rows >>= \row -> index row 1)
      standingOf p r = fromMaybe "" (find (\row -> cell row 0 == p) (standings r).rows >>= \row -> index row 1)
    kv.columns `shouldEqual` [ "Measure", "Unit", "run-1", "run-2" ]
    cell (rowOf "Spread of final net worth") 2 `shouldEqual` valueIn "Spread" r1
    cell (rowOf "Spread of final net worth") 3 `shouldEqual` valueIn "Spread" r2
    cell (rowOf "Final net worth, US") 2 `shouldEqual` standingOf "US" r1
    cell (rowOf "Final net worth, US") 3 `shouldEqual` standingOf "US" r2
    cell (rowOf "Final net worth, US") 1 `shouldEqual` "USD"
    -- one row of net worth per Player, whatever the number of runs
    length (filter (\row -> cell row 0 == "Final net worth, US") kv.rows) `shouldEqual` 1
    -- one net-worth chart per Player, with one line per run
    let charts = netWorthCharts runs
    length charts `shouldEqual` length (standings r1).rows
    for_ charts \c -> map _.name c.chart.series `shouldEqual` [ "run-1", "run-2" ]
    -- the standings bars: one group per Player, one bar per run
    let bars = standingsBars runs
    length bars.categories `shouldEqual` length (standings r1).rows
    map _.name bars.series `shouldEqual` [ "run-1", "run-2" ]
    (balanceBars runs).gini.categories `shouldEqual` [ "Gini" ]
    -- bars are sent exact: the chart keeps full precision and a value under 0.01 keeps its height
    map _.values bars.series `shouldEqual` map (\r -> map (\p -> maybe "" (exact <<< _.netWorth) (find (\s -> s.player == Id p) (standingsOf r))) bars.categories) runs
    map _.values (balanceBars runs).gini.series `shouldEqual` map (\r -> [ maybe "n/a" exact (measuresOf r).gini ]) runs
    map _.values (balanceBars runs).spread.series `shouldEqual` map (\r -> [ maybe "n/a" exact (measuresOf r).spread ]) runs
    (balanceBars runs).gini.unit `shouldEqual` "index, 0 to 1 − 1/n"

  it "the Gini's upper end is stated as 1 − 1/n wherever the glossary names it" do
    Object.lookup "Gini" (glossary pacificFocus).definitions `shouldEqual` Just "The Gini coefficient of final net worth, from 0 (even) to 1 − 1/n (one player holds everything)."

  it "a resource the baseline holds and the other run lacks is reported by the baseline diff (§10.2)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      base = toStored "base" r
      e = Id "US"
      money = base.manifest.initial.numeraire
      held = holding (final base) e money
      -- the other run's every round has no Money account for US, so it lacks what the baseline holds
      dropped w = onActor e (\en -> en { accounts = Map.delete money en.accounts }) w
      other = base { id = "other", rounds = map (\x -> x { world = dropped x.world }) base.rounds }
      rows = (baselineDiff base [ other ]).rows
    (held == Q.zero) `shouldEqual` false
    any (\row -> row == [ "other", show e, show money, quantity (Q.neg held) ]) rows `shouldEqual` true

  it "a round outside the run is rejected, never shown as the initial state (§9.3)" do
    ctx <- context
    r <- play ctx (conservative 5)
    let s = toStored "run-1" r
    isLeft (balances s 6) `shouldEqual` true
    isLeft (balances s (-1)) `shouldEqual` true
    isLeft (events s 0) `shouldEqual` true
    isLeft (events s 6) `shouldEqual` true
    isLeft (balances s 5) `shouldEqual` false
    isLeft (events s 5) `shouldEqual` false

  it "runs differing in engine, units, Exchanges, Actors, or round count are flagged, not combined (§10.1)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      a = toStored "a" r
      b = toStored "b" r
      engine = b { manifest = b.manifest { engine = "engine-other" } }
      exchanges = b { manifest = b.manifest { initial = b.manifest.initial { exchanges = map (\m -> m { position = Q.fromInt 999 }) b.manifest.initial.exchanges } } }
      explicitActors = b { spec = emptySpec { preset = Just "Restrict" } }
      fewer = toStored "c" (r { records = [] })
    verdicts (compatible [ a, a ]) `shouldEqual` [ "yes", "yes" ]
    verdicts (compatible [ a, engine ]) `shouldEqual` [ "yes", "no: flagged, not combined (engine)" ]
    verdicts (compatible [ a, exchanges ]) `shouldEqual` [ "yes", "no: flagged, not combined (Exchange definitions)" ]
    verdicts (compatible [ a, explicitActors ]) `shouldEqual` [ "yes", "no: flagged, not combined (Actor selection)" ]
    verdicts (compatible [ a, fewer ]) `shouldEqual` [ "yes", "no: flagged, not combined (round count)" ]

  it "a run with no rounds: every objective is not evaluated, never met (§10.5)" do
    ctx <- context
    let os = evaluateObjectives ctx.composition []
    length os `shouldEqual` 4
    all (\o -> o.outcome == NotEvaluated) os `shouldEqual` true

  it "the run list shows an unreadable run with its reason, beside the readable ones (§9.1)" do
    ctx <- context
    r <- play ctx (conservative 2)
    let
      t = runList [ Right (summary (toStored "run-1" r)), Left { id: "run-2", name: "broken", reason: "manifest: bad json" } ]
    map (\row -> cell row 6) t.rows `shouldEqual` [ show Completed, "unreadable" ]
    map (\row -> cell row 8) t.rows `shouldEqual` [ "", "manifest: bad json" ]

  it "an Actor's history shows each round's holding, and its net worth matches the standings (§9.3)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      s = toStored "run-1" r
      e = Id "US"
      money = s.manifest.initial.numeraire
      -- the round-2 snapshot (index 1) holds a known amount of Money for US; the others are unchanged
      setMoney w = onActor e (\en -> en { accounts = Map.insert money (Q.fromInt 777) en.accounts }) w
      known = s { rounds = mapWithIndex (\i x -> if i == 1 then x { world = setMoney x.world } else x) s.rounds }
      moneyRow h = filter (\row -> index row 0 == Just (show money)) h.table.rows
    case actorHistory known e of
      Left err -> fail err
      Right h -> do
        h.table.columns `shouldEqual` [ "Resource", "Round 0", "Round 1", "Round 2", "Round 3" ]
        for_ (moneyRow h) \row -> do
          cell row 3 `shouldEqual` "777"
          cell row 1 `shouldEqual` quantity (holding s.manifest.initial e money)
        h.chart.series `shouldSatisfy` \ss -> length (filter (\x -> x.name == show money) ss) == 1
    case actorHistory s e of
      Left err -> fail err
      Right h -> do
        -- the net-worth row's final column is the Actor's net worth in the standings
        let finalNw = fromMaybe "" (map (\row -> cell row 4) (index (filter (\row -> index row 0 == Just "Net worth") h.table.rows) 0))
        let standing = filter (\row -> index row 0 == Just "US") (standings s).rows
        map (\row -> cell row 1) standing `shouldEqual` [ finalNw ]
    isLeft (actorHistory s (Id "NoSuchActor")) `shouldEqual` true

  it "the round report: what one round moved, with literal changes" do
    ctx <- context
    r <- play ctx (conservative 2)
    let
      s = toStored "run-1" r
      before = s.manifest.initial
      trade = Settled { round: 1, legs: [ { from: InExchange (Id "technology-world"), to: Held (Id "US"), resource: Id "T", owed: q "10", moved: q "10" } ], rate: Nothing, cause: OfRule { kind: Id "tariff" }, delivered: false }
      after = before { actors = Map.update (Just <<< \a -> a { accounts = Map.insert (Id "T") (q "80") a.accounts }) (Id "US") before.actors }
      one = s { manifest = s.manifest { initial = before { actors = Map.update (Just <<< \a -> a { accounts = Map.insert (Id "T") (q "70") a.accounts }) (Id "US") before.actors } }, rounds = [ { record: { round: 1, inputs: [], events: [ trade ] }, world: after } ] }
      report = roundReport one 1
    map _.round report `shouldEqual` Just 1
    map (filter (\b -> b.actor == "US" && b.resource == "T") <<< _.balances) report `shouldEqual` Just [ { actor: "US", resource: "T", holding: "80", change: "10" } ]
    map (_.events) report `shouldEqual` Just [ trade ]
    map _.round (roundReport one 2) `shouldEqual` Nothing

  it "a balanceSheet opens by its token, and shows one Actor" do
    ctx <- context
    r <- play ctx (conservative 2)
    let
      stored = toStored "run-1" r
      links = balanceSheetLinks stored
      us = find (\l -> l.actor == "US") links
      none = { title: "Objectives", columns: [], rows: [] }
    map _.path us `shouldEqual` Just ("/balancesheet/run-1/" <> tokenFor (fromMaybe 0 (Int.fromString stored.playerSeed)) "US")
    length (nub (map _.path links)) `shouldEqual` length links
    map _.actor (balanceSheetAt stored none (fromMaybe "" (map (\l -> fromMaybe "" (last (split (Pattern "/") l.path))) us))) `shouldEqual` Just "US"
    map _.actor (balanceSheetAt stored none "nope") `shouldEqual` Nothing
    -- nothing in the sheet is another Actor's holdings: its one holdings table is this Actor's
    map (_.title <<< _.holdings) (balanceSheetAt stored none (fromMaybe "" (map (\l -> fromMaybe "" (last (split (Pattern "/") l.path))) us))) `shouldEqual` Just "Balance sheet of US by round"

  it "the overview returns the header's facts, in order" do
    ctx <- context
    r <- play ctx (conservative 2)
    map _.label (overview (toStored "run-1" r)).facts `shouldEqual` [ "Name", "Preset", "Composition", "Premises", "Status", "Rounds completed", "Game ID" ]
  where
  summary s =
    { id: s.id
    , name: s.name
    , status: s.status
    , reason: Nothing
    , composition: s.manifest.composition
    , gameSeed: show s.manifest.initial.seed
    , playerSeed: s.playerSeed
    , rounds: completed s
    , assertionsFailing: 0
    , runClass: Simulated
    , tag: Nothing
    }

pacificFocus :: Array { resource :: Id ResourceTag, label :: String }
pacificFocus = [ { resource: Id "T", label: "Technology" } ]
