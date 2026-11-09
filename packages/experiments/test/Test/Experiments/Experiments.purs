-- | The variant experiments of docs/06_simulator.md §11 on Meridian,
-- | reported with the §12 indicators, plus the performance targets. Every variant must keep
-- | every invariant; the printed table is the balancing evidence for docs/05 §28–§29.
module Test.Experiments.Experiments (spec) where

import Schema.Build.Quantity (q)
import Prelude

import Core.Map (values)

import Settings.Meridian.Entities.Seats (seatActors)
import Testkit.Meridian (orThrow, run)
import Checks.Objectives (evaluateObjectives)
import Schema.Gameboard (Outcome(..))

import Core.Draw (Seed(..))
import Checks.Run (checkRun)
import Laws.Query (priceOf)
import Core.Num as Q
import Laws.Types (Id(..))
import Laws.Slice (event, matches, exchanges, conversions)
import Data.Array (concatMap, elem, filter, last, length, range)
import Data.Foldable (for_, maximum)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Experiments.Agency (agency)
import Handlers.Simulate (SimRun)
import Game.Assemble (assemble, presetBindings)
import Game.Override (Override(..))
import Game.Setting (GameboardParts, Setting(..))
import Game.Types (Game)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | A variant: each seat's preset by name, the overrides, and the player seed.
type Variant = { preset :: String -> String, overrides :: Array Override, seed :: Int }

spec' :: (String -> String) -> Array Override -> Int -> Variant
spec' preset overrides seed = { preset, overrides, seed }

-- | The game a variant plays: every seat bound to its preset, under the variant's overrides.
gameOf :: GameboardParts -> Variant -> Aff Game
gameOf p v = do
  bindings <- orThrow (presetBindings p.presets (\e -> v.preset (show e)) seatActors)
  orThrow (assemble (GameboardSetting p) bindings v.overrides)

play :: GameboardParts -> Variant -> Aff SimRun
play ctx v = do
  g <- gameOf ctx v
  run (Seed v.seed) g

-- | The §12 indicators of one run, as a printed row.
indicators :: GameboardParts -> SimRun -> { row :: String, failures :: Int }
indicators ctx r = { row, failures }
  where
  events = concatMap _.record.events r.records
  count f = length (filter f events)
  activity = count \e -> matches (exchanges) (const true) e || matches (conversions) (const true) e
  rejected = count (matches (event @"Rejected") (const true))
  lapsed = count (matches (event @"Lapsed") (const true))
  proposals = length (concatMap _.record.inputs r.records)
  final = fromMaybe r.manifest.initial (map _.world (last r.records))
  price w m = maybe "—" Q.displayQuantity (priceOf w (Id m))
  money w = map (\e -> fromMaybe Q.zero (Map.lookup (Id "M") e.accounts)) (values w.entities)
  -- circulation health: the largest holder's share of Money, at the start and the end
  share w = let ms = money w in maybe "—" Q.displayRatio (Q.divMaybe (fromMaybe Q.zero (maximum ms)) (Q.sumQ ms))
  met = length (filter (\o -> o.outcome == Met) (evaluateObjectives ctx.gameboard r.records))
  failures = length (checkRun r.manifest.initial r.records)
  row = joinWith " | "
    [ show activity
    , show (proposals - rejected - lapsed) <> "/" <> show proposals
    , show rejected
    , show lapsed
    , price final "energy-atlantic"
    , price final "energy-asia"
    , share r.manifest.initial <> "→" <> share final
    , show met <> "/16"
    , show failures
    ]

header :: String
header = "variant | activity | executed/proposed | rejected | lapsed | P atlantic | P asia | top Money share | objectives met | invariant failures"

millis :: Aff Number
millis = liftEffect (unwrap <<< unInstant <$> now)

spec :: Aff GameboardParts -> Spec Unit
spec context = describe "Experiments (§11) and performance" do
  it "variant experiments keep every invariant (report printed)" do
    ctx <- context
    let
      conservative = const "Conservative"
      variants =
        [ Tuple "baseline, all Conservative" (spec' conservative [] 1)
        , Tuple "all Aggressive" (spec' (const "Aggressive") [] 1)
        , Tuple "governments Aggressive, corporations Investor" (spec' (\s -> if s `elem` [ "ENCO", "CHIP", "SHIP", "DEF" ] then "Investor" else if s `elem` [ "LABOR", "SME", "AID" ] then "Conservative" else "Aggressive") [] 1)
        , Tuple "one Opportunistic deviator among Cooperative" (spec' (\s -> if s == "US" then "Opportunistic" else "Cooperative") [] 1)
        , Tuple "all RandomValid" (spec' (const "RandomValid") [] 1)
        ] <> map (\x -> Tuple ("SME shrinkage " <> x) (spec' conservative [ Shrinkage "sme-energy-sample" (q x) ] 1)) [ "0", "0.1", "0.25", "0.5" ]
          <> map (\x -> Tuple ("major-action threshold " <> x) (spec' (const "Aggressive") [ Parameter "major-action-threshold" (q x) ] 1)) [ "6", "12", "24" ]
          <> map (\x -> Tuple ("disruption cut atlantic " <> x) (spec' conservative [ Parameter "disruption-cut-atlantic" (q x) ] 1)) [ "0", "12", "24" ]
          <> map (\x -> Tuple ("supplier offer atlantic " <> x) (spec' conservative [ Parameter "supplier-offer-atlantic" (q x) ] 1)) [ "12", "24", "36" ]
          <> map (\x -> Tuple ("EU matching tariff " <> x) (spec' (const "Aggressive") [ Parameter "eu-matching-tariff" (q x) ] 1)) [ "0", "0.5", "1" ]
          <> map (\x -> Tuple ("game seed " <> show x) (spec' (const "RandomValid") [ GameSeed x ] 1)) [ 1, 2, 3 ]
    log header
    for_ variants \(Tuple name s) -> do
      r <- play ctx s
      let i = indicators ctx r
      log (name <> " | " <> i.row)
      i.failures `shouldEqual` 0
  it "agency: every seat has a measurable effect, or the gap is a balancing finding" do
    ctx <- context
    for_ [ "Conservative", "RandomValid" ] \baseline -> do
      g <- gameOf ctx (spec' (const baseline) [] 1)
      ps <- orThrow (agency ctx.gameboard ctx.presets (Seed 1) g "Aggressive" seatActors)
      log ("baseline " <> baseline <> ": measurable " <> show (length (filter _.measurable ps)) <> "/15; not measurable: " <> joinWith ", " (map (show <<< _.actor) (filter (not <<< _.measurable) ps)))
      length ps `shouldEqual` 15
  it "a 5-round game and a 100-player-seed batch finish in generous wall-clock bounds" do
    ctx <- context
    t0 <- millis
    _ <- play ctx (spec' (const "RandomValid") [] 1)
    t1 <- millis
    for_ (range 1 100) \seed -> void (play ctx (spec' (const "RandomValid") [] seed))
    t2 <- millis
    log ("one game " <> show (t1 - t0) <> " ms; 100 seeds " <> show (t2 - t1) <> " ms")
    -- the targets are 1 s and 1 min (docs/06); a loaded CI machine must not fail the gate, so the
    -- bound is ten times the target: a regression of an order of magnitude is what this catches
    (t1 - t0) `shouldSatisfy` (_ < 10000.0)
    (t2 - t1) `shouldSatisfy` (_ < 600000.0)

