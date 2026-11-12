-- | The variant experiments of docs/07_simulator.md §11 on Pacific,
-- | reported with the §12 indicators, plus the performance targets. Every variant must keep
-- | every invariant; the printed table is balancing evidence (docs/05 §10.4).
module Test.Experiments.Experiments (spec) where

import Schema.Build.Quantity (q)
import Prelude

import Core.Map (values)

import Testkit.Run (orThrow, run)
import Checks.Objectives (evaluateObjectives)
import Schema.Composition (Outcome(..))

import Core.Draw (Seed(..))
import Checks.Run (checkRun)
import Laws.Query (priceOf)
import Core.Num as Q
import Laws.Types (ActorId, Id(..))
import Laws.Slice (event, matches, exchangeFills, transformationFills)
import Data.Array (concatMap, filter, last, length, range)
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
import Assembly.Assemble (assemble, presetBindings)
import Assembly.Override (Override(..))
import Assembly.Composition (CompositionParts, Origin(..))
import Assembly.Types (PreparedRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Pacific's four seats.
playableActors :: Array ActorId
playableActors = map Id [ "US", "CN", "US-firm", "CN-firm" ]

-- | A variant: each seat's preset by name, the overrides, and the player seed.
type Variant = { preset :: String -> String, overrides :: Array Override, seed :: Int }

spec' :: (String -> String) -> Array Override -> Int -> Variant
spec' preset overrides seed = { preset, overrides, seed }

-- | The game a variant plays: every seat bound to its preset, under the variant's overrides.
gameOf :: CompositionParts -> Variant -> Aff PreparedRun
gameOf p v = do
  bindings <- orThrow (presetBindings p.presets (\e -> v.preset (show e)) playableActors)
  orThrow (assemble (FromComposition p) bindings v.overrides)

play :: CompositionParts -> Variant -> Aff SimRun
play ctx v = do
  g <- gameOf ctx v
  run (Seed v.seed) g

-- | The §12 indicators of one run, as a printed row.
indicators :: CompositionParts -> SimRun -> { row :: String, failures :: Int }
indicators ctx r = { row, failures }
  where
  events = concatMap _.record.events r.records
  count f = length (filter f events)
  activity = count \e -> matches (exchangeFills) (const true) e || matches (transformationFills) (const true) e
  rejected = count (matches (event @"Rejected") (const true))
  lapsed = count (matches (event @"Lapsed") (const true))
  candidateTransactions = length (concatMap _.record.inputs r.records)
  final = fromMaybe r.manifest.initial (map _.world (last r.records))
  price w m = maybe "—" Q.displayQuantity (priceOf w (Id m))
  money w = map (\e -> fromMaybe Q.zero (Map.lookup (Id "M") e.accounts)) (values w.actors)
  -- circulation health: the largest holder's share of Money, at the start and the end
  share w = let ms = money w in maybe "—" Q.displayRatio (Q.divMaybe (fromMaybe Q.zero (maximum ms)) (Q.sumQ ms))
  met = length (filter (\o -> o.outcome == Met) (evaluateObjectives ctx.composition r.records))
  failures = length (checkRun r.manifest.initial r.records)
  row = joinWith " | "
    [ show activity
    , show (candidateTransactions - rejected - lapsed) <> "/" <> show candidateTransactions
    , show rejected
    , show lapsed
    , price final "technology-world"
    , price final "industry-world"
    , share r.manifest.initial <> "→" <> share final
    , show met <> "/4"
    , show failures
    ]

header :: String
header = "variant | activity | executed/proposed | rejected | lapsed | P technology | P industry | top Money share | objectives met | invariant failures"

millis :: Aff Number
millis = liftEffect (unwrap <<< unInstant <$> now)

spec :: Aff CompositionParts -> Spec Unit
spec context = describe "Experiments (§11) and performance" do
  it "variant experiments keep every invariant (report printed)" do
    ctx <- context
    let
      free = const "FreeTrade"
      firms f s = if s == "US-firm" || s == "CN-firm" then f else "FreeTrade"
      variants =
        [ Tuple "baseline, all FreeTrade" (spec' free [] 1)
        , Tuple "all Hawk" (spec' (const "Hawk") [] 1)
        , Tuple "US Hawk, China Reformer" (spec' (\s -> if s == "US" then "Hawk" else if s == "CN" then "Reformer" else "FreeTrade") [] 1)
        , Tuple "firms Hedger" (spec' (firms "Hedger") [] 1)
        , Tuple "firms Builder" (spec' (firms "Builder") [] 1)
        ] <> map (\x -> Tuple ("asset fee " <> x) (spec' (const "Rebalance") [ Parameter "asset-fee-high" (q x) ] 1)) [ "0", "0.05", "0.2" ]
          <> map (\x -> Tuple ("input share " <> x) (spec' (const "Restrict") [ Parameter "input-share" (q x) ] 1)) [ "0", "0.5", "1" ]
          <> map (\x -> Tuple ("game seed " <> show x) (spec' (const "Hawk") [ GameSeed x ] 1)) [ 1, 2, 3 ]
    log header
    for_ variants \(Tuple name s) -> do
      r <- play ctx s
      let i = indicators ctx r
      log (name <> " | " <> i.row)
      i.failures `shouldEqual` 0
  it "agency: every seat has a measurable effect, or the gap is a balancing finding" do
    ctx <- context
    for_ [ "FreeTrade", "Hawk" ] \baseline -> do
      ps <- orThrow (agency ctx (Seed 1) baseline ctx.config.alternatives)
      log ("baseline " <> baseline <> ": measurable " <> show (length (filter _.measurable ps)) <> "/4; not measurable: " <> joinWith ", " (map (show <<< _.explicitActor) (filter (not <<< _.measurable) ps)))
      length ps `shouldEqual` 4
  it "a 6-round game and a 100-player-seed batch finish in generous wall-clock bounds" do
    ctx <- context
    t0 <- millis
    _ <- play ctx (spec' (const "Hawk") [] 1)
    t1 <- millis
    for_ (range 1 100) \seed -> void (play ctx (spec' (const "Hawk") [] seed))
    t2 <- millis
    log ("one game " <> show (t1 - t0) <> " ms; 100 seeds " <> show (t2 - t1) <> " ms")
    -- the targets are 1 s and 1 min (docs/07); a loaded CI machine must not fail the gate, so the
    -- bound is ten times the target: a regression of an order of magnitude is what this catches
    (t1 - t0) `shouldSatisfy` (_ < 10000.0)
    (t2 - t1) `shouldSatisfy` (_ < 600000.0)

