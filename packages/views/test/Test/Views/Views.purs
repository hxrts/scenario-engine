-- | The view models and the spec's error paths: unknown presets and overrides, out-of-range
-- | rounds, the last round's "n/a" cells, run compatibility, unreadable runs in the list, and
-- | objectives over no rounds.
module Test.Views.Views (spec) where

import Prelude

import Core.Run (DeriveKind(..), RunStatus(..), runStatusName)
import Checks.Objectives (evaluateObjectives)
import Schema.Gameboard (Outcome(..))

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Id(..))
import Data.Array (all, filter, find, index, length, mapWithIndex)
import Data.Map as Map
import Data.Either (Either(..), isLeft)
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Simulator.Simulate (SimRun)
import Game.Assemble (assemblePreset)
import Game.Override (Override(..), resolve)
import Game.Premise (GameboardParts)
import Game.Recipe (encodeSpec)
import Foreign.Object as Object
import Testkit.Capstone (runOf)
import Testkit.Meridian (orThrow)
import Views.Balances (balances, events)
import Views.Common (StoredRun, Table, completed, defaultMetricSpec, final, quantity, specKeys, specValue, storedRun)
import Views.Balance (measures)
import Views.Compare (balanceBars, baselineDiff, compatible, keyValues, netWorthCharts, standingsBars)
import Views.Entity (entityHistory)
import Views.Standings (standings)
import Views.Overview (overview, runList)
import Views.Representation (representation)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

-- | A setup: every seat plays one preset for a number of rounds, under overrides and a player seed.
type Setup = { preset :: String, rounds :: Int, overrides :: Array Override, seed :: Int }

everySeat :: String -> Int -> Setup
everySeat preset rounds = { preset, rounds, overrides: [], seed: 1 }

conservative :: Int -> Setup
conservative = everySeat "Conservative"

-- | A simulated run as the Run Store reads it back: the same records and snapshots.
toStored :: String -> SimRun -> StoredRun
toStored id r = storedRun
  { id
  , name: id
  , status: Completed
  , manifest: r.manifest
  , playerSeed: case r.playerSeed of
      Seed n -> show n
  , spec: []
  , sourceDeleted: false
  }
  r.records
  (map (const []) r.records)

-- | The verdict column of a compatibility table.
verdicts :: Table -> Array String
verdicts t = map (\row -> cell row 7) t.rows

-- | A table cell by column, or "" when the row is short.
cell :: Array String -> Int -> String
cell row i = fromMaybe "" (index row i)

play :: GameboardParts -> Setup -> Aff SimRun
play ctx s = do
  g <- orThrow (assemblePreset ctx s.preset s.overrides (Just s.rounds))
  runOf (Seed s.seed) g

spec :: Aff GameboardParts -> Spec Unit
spec context = describe "Views and error paths" do
  it "the views read the specification keys Game.Recipe writes" do
    let
      written = Object.toUnfoldable (encodeSpec { gameId: "g", playerSeed: Seed 7, preset: "P", rounds: Just 3, overrides: [], derivation: Just { source: "run-1", kind: Variant }, model: Nothing, humans: Nothing }) :: Array (Tuple String String)
    specValue written specKeys.gameId `shouldEqual` Just "g"
    specValue written specKeys.preset `shouldEqual` Just "P"
    specValue written specKeys.playerSeed `shouldEqual` Just "7"
    specValue written specKeys.rounds `shouldEqual` Just "3"
    specValue written specKeys.source `shouldEqual` Just "run-1"
    specValue written specKeys.kind `shouldEqual` Just "variant"
  it "an unknown preset or override is an error, never a silent no-op (§3, §4.2)" do
    ctx <- context
    let base = conservative 2
    case assemblePreset ctx "NoSuchPreset" [] (Just base.rounds) of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown preset NoSuchPreset")
      Right _ -> fail "an unknown preset ran"
    case resolve { gameboard: ctx.gameboard, config: ctx.config } [ Parameter "no-such-parameter" (Q.fromInt 1) ] of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown parameter no-such-parameter")
      Right _ -> fail "an unknown parameter was applied"
    case resolve { gameboard: ctx.gameboard, config: ctx.config } [ Shrinkage "no-such-scheme" (Q.fromInt 0) ] of
      Left e -> show e `shouldSatisfy` contains (Pattern "unknown scheme")
      Right _ -> fail "an unknown scheme was applied"

  it "the cross-run key values have one column per run, and their values are the views' own (§10.2)" do
    ctx <- context
    a <- play ctx (conservative 3)
    b <- play ctx (everySeat "Aggressive" 3)
    let
      r1 = toStored "run-1" a
      r2 = toStored "run-2" b
      runs = [ r1, r2 ]
      kv = keyValues ctx.gameboard runs
      rowOf label = fromMaybe [] (find (\row -> cell row 0 == label) kv.rows)
      valueIn label r = fromMaybe "" (find (\row -> cell row 0 == label) (measures r).rows >>= \row -> index row 1)
      standingOf p r = fromMaybe "" (find (\row -> cell row 0 == p) (standings r).rows >>= \row -> index row 1)
    kv.columns `shouldEqual` [ "Measure", "Unit", "run-1", "run-2" ]
    cell (rowOf "Spread of final net worth") 2 `shouldEqual` valueIn "Spread (M)" r1
    cell (rowOf "Spread of final net worth") 3 `shouldEqual` valueIn "Spread (M)" r2
    cell (rowOf "Final net worth, US") 2 `shouldEqual` standingOf "US" r1
    cell (rowOf "Final net worth, US") 3 `shouldEqual` standingOf "US" r2
    cell (rowOf "Final net worth, US") 1 `shouldEqual` "M"
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

  it "a resource the baseline holds and the other run lacks is reported by the baseline diff (§10.2)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      base = toStored "base" r
      e = Id "US"
      money = base.manifest.initial.numeraire
      held = holding (final base) e money
      -- the other run's every round has no Money account for US, so it lacks what the baseline holds
      dropped w = w { entities = Map.update (\en -> Just en { accounts = Map.delete money en.accounts }) e w.entities }
      other = base { id = "other", rounds = map (\x -> x { world = dropped x.world }) base.rounds }
      rows = (baselineDiff base [ other ]).rows
    (held == Q.zero) `shouldEqual` false
    any (\row -> row == [ "other", show e, show money, quantity (Q.neg held) ]) rows `shouldEqual` true

  it "a round outside the run is rejected, never shown as the initial state (§9.2)" do
    ctx <- context
    r <- play ctx (conservative 5)
    let s = toStored "run-1" r
    isLeft (balances s 6) `shouldEqual` true
    isLeft (balances s (-1)) `shouldEqual` true
    isLeft (events s 0) `shouldEqual` true
    isLeft (events s 6) `shouldEqual` true
    isLeft (balances s 5) `shouldEqual` false
    isLeft (events s 5) `shouldEqual` false

  it "the last round shows n/a for the next round's purchase and the influence ratio (§9.2)" do
    ctx <- context
    -- a three-round run whose final round has representative observations; the last round is
    -- the run's own final round, so the test does not depend on the random choices of later rounds
    r <- play ctx (everySeat "RandomValid" 3)
    let
      t :: Table
      t = representation (toStored "run-1" r)
      lastRound = filter (\row -> cell row 0 == "3") t.rows
    (length t.rows > 0) `shouldEqual` true
    (length lastRound > 0) `shouldEqual` true
    for_ lastRound \row -> do
      cell row 11 `shouldEqual` "n/a"
      cell row 12 `shouldEqual` "n/a"

  it "runs differing in engine, units, Markets, Actors, or round count are flagged, not combined (§10.1)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      a = toStored "a" r
      b = toStored "b" r
      engine = b { manifest = b.manifest { engine = "engine-other" } }
      markets = b { manifest = b.manifest { initial = b.manifest.initial { markets = map (\m -> m { position = Q.fromInt 999 }) b.manifest.initial.markets } } }
      actors = b { spec = [ Tuple "preset" "Passive" ] }
      fewer = toStored "c" (r { records = [] })
    verdicts (compatible [ a, a ]) `shouldEqual` [ "yes", "yes" ]
    verdicts (compatible [ a, engine ]) `shouldEqual` [ "yes", "no: flagged, not combined (engine)" ]
    verdicts (compatible [ a, markets ]) `shouldEqual` [ "yes", "no: flagged, not combined (Market definitions)" ]
    verdicts (compatible [ a, actors ]) `shouldEqual` [ "yes", "no: flagged, not combined (Actor selection)" ]
    verdicts (compatible [ a, fewer ]) `shouldEqual` [ "yes", "no: flagged, not combined (round count)" ]

  it "a run with no rounds: every objective is not evaluated, never met (§10.5)" do
    ctx <- context
    let os = evaluateObjectives ctx.gameboard []
    length os `shouldEqual` 16
    all (\o -> o.outcome == NotEvaluated) os `shouldEqual` true

  it "the run list shows an unreadable run with its reason, beside the readable ones (§9.1)" do
    ctx <- context
    r <- play ctx (conservative 2)
    let
      t = runList [ Right (summary (toStored "run-1" r)), Left { id: "run-2", name: "broken", reason: "manifest: bad json" } ]
    map (\row -> cell row 6) t.rows `shouldEqual` [ runStatusName Completed, "unreadable" ]
    map (\row -> cell row 8) t.rows `shouldEqual` [ "", "manifest: bad json" ]

  it "an Entity's history shows each round's holding, and its net worth matches the standings (§9.2)" do
    ctx <- context
    r <- play ctx (conservative 3)
    let
      s = toStored "run-1" r
      e = Id "US"
      money = s.manifest.initial.numeraire
      -- the round-2 snapshot (index 1) holds a known amount of Money for US; the others are unchanged
      setMoney w = w { entities = Map.update (\en -> Just en { accounts = Map.insert money (Q.fromInt 777) en.accounts }) e w.entities }
      known = s { rounds = mapWithIndex (\i x -> if i == 1 then x { world = setMoney x.world } else x) s.rounds }
      moneyRow h = filter (\row -> index row 0 == Just (show money)) h.table.rows
    case entityHistory known e of
      Left err -> fail err
      Right h -> do
        h.table.columns `shouldEqual` [ "Resource", "Round 0", "Round 1", "Round 2", "Round 3" ]
        for_ (moneyRow h) \row -> do
          cell row 3 `shouldEqual` "777"
          cell row 1 `shouldEqual` quantity (holding s.manifest.initial e money)
        h.chart.series `shouldSatisfy` \ss -> length (filter (\x -> x.name == show money) ss) == 1
    case entityHistory s e of
      Left err -> fail err
      Right h -> do
        -- the net-worth row's final column is the Entity's net worth in the standings
        let finalNw = fromMaybe "" (map (\row -> cell row 4) (index (filter (\row -> index row 0 == Just "Net worth (M)") h.table.rows) 0))
        let standing = filter (\row -> index row 0 == Just "US") (standings s).rows
        map (\row -> cell row 1) standing `shouldEqual` [ finalNw ]
    isLeft (entityHistory s (Id "NoSuchEntity")) `shouldEqual` true

  it "the overview labels procurement from the metric spec" do
    ctx <- context
    r <- play ctx (conservative 2)
    let t = overview defaultMetricSpec (toStored "run-1" r)
    map (\row -> cell row 0) t.rows `shouldSatisfy` \labels -> length (filter (_ == "Military procured") labels) == 1
  where
  summary s =
    { id: s.id
    , name: s.name
    , status: s.status
    , reason: Nothing
    , gameboard: s.manifest.gameboard
    , gameSeed: show s.manifest.initial.seed
    , playerSeed: s.playerSeed
    , rounds: completed s
    , assertionsFailing: 0
    , archived: false
    , source: Nothing
    , sourceDeleted: false
    }
