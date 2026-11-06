-- | Phase 9: the variant experiments of docs/06_simulator.md §11 on The Fractured World,
-- | reported with the §12 indicators, plus the performance targets. Every variant must keep
-- | every invariant; the printed table is the balancing evidence for docs/05 §28–§29.
module Test.Harness.Experiments (spec, seats) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Invariants (checkRun)
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
import Effect.Exception (error)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Harness.Analysis (Outcome(..), agency, evaluateObjectives)
import Harness.Simulate (Context, Override(..), Run, SimulationSpec, presetSeats, simulate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

spec' :: (String -> String) -> Array Override -> Int -> SimulationSpec
spec' preset overrides seed = { seats: presetSeats (preset <<< show) (map Id seats), playerSeed: Seed seed, overrides, rounds: Nothing }

play :: Context -> SimulationSpec -> Aff Run
play ctx s = case simulate ctx s of
  Left e -> throwError (error e)
  Right r -> pure r

-- | The §12 indicators of one run, as a printed row.
indicators :: Context -> Run -> { row :: String, failures :: Int }
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
  price w m = maybe "—" Q.format (priceOf w (Id m))
  money w = map (\e -> fromMaybe Q.zero (Map.lookup (Id "M") e.accounts)) (fromFoldable (Map.values w.entities))
  -- circulation health: the largest holder's share of Money, at the start and the end
  share w = let ms = money w in maybe "—" Q.format (Q.divMaybe (fromMaybe Q.zero (maximum ms)) (Q.sumQ ms))
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

spec :: Aff Context -> Spec Unit
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
    for_ [ "Conservative", "RandomValid" ] \baseline -> case agency ctx (spec' (const baseline) [] 1) "Aggressive" (map Id seats) of
      Left e -> fail e
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

