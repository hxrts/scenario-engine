-- | The view models and the spec's error paths: unknown presets and overrides, out-of-range
-- | rounds, the last round's "n/a" cells, run compatibility, unreadable runs in the list, and
-- | objectives over no rounds.
module Test.Harness.Views (spec) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (Id(..))
import Data.Array (all, filter, index, length)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Harness.Analysis (Outcome(..), evaluateObjectives)
import Harness.Simulate (Context, Override(..), Run, SimulationSpec, presetSeats, resolve, simulate)
import Harness.Views.Balances (balances, events)
import Harness.Views.Common (StoredRun, Table, defaultMetricSpec)
import Harness.Views.Compare (compatible)
import Harness.Views.Overview (overview, runList)
import Harness.Views.Representation (representation)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

seatIds :: Array String
seatIds = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

everySeat :: String -> Int -> SimulationSpec
everySeat preset rounds = { seats: presetSeats (const preset) (map Id seatIds), playerSeed: Seed 1, overrides: [], rounds: Just rounds }

conservative :: Int -> SimulationSpec
conservative = everySeat "Conservative"

-- | A simulated run as the Run Store reads it back: the same records and snapshots.
toStored :: String -> Run -> StoredRun
toStored id r =
  { id
  , name: id
  , status: "completed"
  , manifest: r.manifest
  , playerSeed: case r.playerSeed of
      Seed n -> show n
  , spec: []
  , records: map _.record r.records
  , snapshots: map _.world r.records
  , assertions: map (const []) r.records
  , sourceDeleted: false
  }

-- | The verdict column of a compatibility table.
verdicts :: Table -> Array String
verdicts t = map (\row -> cell row 7) t.rows

-- | A table cell by column, or "" when the row is short.
cell :: Array String -> Int -> String
cell row i = fromMaybe "" (index row i)

play :: Context -> SimulationSpec -> Aff Run
play ctx s = case simulate ctx s of
  Left e -> throwError (error e)
  Right r -> pure r

spec :: Aff Context -> Spec Unit
spec context = describe "Views and error paths" do
  it "an unknown preset or override is an error, never a silent no-op (§3, §4.2)" do
    ctx <- context
    let base = conservative 2
    case simulate ctx base { seats = presetSeats (const "NoSuchPreset") (map Id seatIds) } of
      Left e -> e `shouldSatisfy` contains (Pattern "unknown preset NoSuchPreset")
      Right _ -> fail "an unknown preset ran"
    case resolve ctx base { overrides = [ Parameter "no-such-parameter" (Q.fromInt 1) ] } of
      Left e -> e `shouldSatisfy` contains (Pattern "unknown parameter no-such-parameter")
      Right _ -> fail "an unknown parameter was applied"
    case resolve ctx base { overrides = [ Shrinkage "no-such-scheme" (Q.fromInt 0) ] } of
      Left e -> e `shouldSatisfy` contains (Pattern "unknown scheme")
      Right _ -> fail "an unknown scheme was applied"

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
    map (\row -> cell row 6) t.rows `shouldEqual` [ "completed", "unreadable" ]
    map (\row -> cell row 8) t.rows `shouldEqual` [ "", "manifest: bad json" ]

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
    , rounds: length s.records
    , assertionsFailing: 0
    , archived: false
    , source: Nothing
    , sourceDeleted: false
    }
