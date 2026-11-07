-- | Phase 9: the variant experiments of docs/06_simulator.md §11 on Meridian,
-- | reported with the §12 indicators, plus the performance targets. Every variant must keep
-- | every invariant; the printed table is the balancing evidence for docs/05 §28–§29.
module Test.Experiments.Experiments (spec) where

import Prelude

import Testkit.Meridian (orThrow)
import Checks.Objectives (evaluateObjectives)

import Core.Draw (Seed(..))
import Checks.Run (checkRun)
import Core.Query (priceOf)
import Core.Num as Q
import Core.Types (Id(..))
import Core.World (Event(..))
import Data.Array (concatMap, elem, filter, fromFoldable, last, length, range)
import Data.Either (Either(..))
import Data.Foldable (for_, maximum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Experiments.Agency (agency)
import Testkit.Meridian (seats)
import Checks.Objectives (Outcome(..))
import Simulator.Simulate (SimRun)
import Game.Assemble (assemble, presetBindings)
import Game.Override (Override(..))
import Game.Premise (GameboardParts, Premise(..))
import Game.Types (Game)
import Testkit.Capstone (runOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

-- | A variant: each seat's preset by name, the overrides, and the player seed.
type Variant = { preset :: String -> String, overrides :: Array Override, seed :: Int }

spec' :: (String -> String) -> Array Override -> Int -> Variant
spec' preset overrides seed = { preset, overrides, seed }

-- | The game a variant plays: every seat bound to its preset, under the variant's overrides.
gameOf :: GameboardParts -> Variant -> Aff Game
gameOf p v = do
  bindings <- orThrow (presetBindings p.presets (\e -> v.preset (show e)) (map Id seats))
  orThrow (assemble (GameboardPremise p) bindings v.overrides)

play :: GameboardParts -> Variant -> Aff SimRun
play ctx v = do
  g <- gameOf ctx v
  runOf (Seed v.seed) g

-- | The §12 indicators of one run, as a printed row.
indicators :: GameboardParts -> SimRun -> { row :: String, failures :: Int }
indicators ctx r = { row, failures }
  where
  events = concatMap _.record.events r.records
  count f = length (filter f events)
  activity = count case _ of
    Traded _ -> true
    Transformed _ -> true
    _ -> false
  rejected = count case _ of
    Rejected _ -> true
    _ -> false
  lapsed = count case _ of
    Lapsed _ -> true
    _ -> false
  proposals = length (concatMap _.record.inputs r.records)
  final = fromMaybe r.manifest.initial (map _.world (last r.records))
  price w m = maybe "—" Q.displayPrice (priceOf w (Id m))
  money w = map (\e -> fromMaybe Q.zero (Map.lookup (Id "M") e.accounts)) (fromFoldable (Map.values w.entities))
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
      case agency ctx.gameboard ctx.presets (Seed 1) g "Aggressive" (map Id seats) of
        Left e -> fail (show e)
        Right ps -> do
          log ("baseline " <> baseline <> ": measurable " <> show (length (filter _.measurable ps)) <> "/15; not measurable: " <> joinWith ", " (map (show <<< _.actor) (filter (not <<< _.measurable) ps)))
          length ps `shouldEqual` 15
  it "a 5-round game resolves in under 1 s; a 100-player-seed batch in under 1 min" do
    ctx <- context
    t0 <- millis
    _ <- play ctx (spec' (const "RandomValid") [] 1)
    t1 <- millis
    for_ (range 1 100) \seed -> void (play ctx (spec' (const "RandomValid") [] seed))
    t2 <- millis
    log ("one game " <> show (t1 - t0) <> " ms; 100 seeds " <> show (t2 - t1) <> " ms")
    (t1 - t0) `shouldSatisfy` (_ < 1000.0)
    (t2 - t1) `shouldSatisfy` (_ < 60000.0)
  where
  q s = fromMaybe Q.zero (Q.parse s)

