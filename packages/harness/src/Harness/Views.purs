-- | View models for the dashboard (docs/06_simulator.md §9–§10). Every view is a generic
-- | Table or Chart computed here, so the frontend only renders. Pure.
module Harness.Views
  ( Table
  , Chart
  , StoredRun
  , MetricSpec
  , defaultMetricSpec
  , RunSummary
  , Unreadable
  , Pressure
  , MarketPrice
  , roundPressure
  , marketPrices
  , runList
  , overview
  , balances
  , markets
  , facilities
  , economy
  , representation
  , procedures
  , objectives
  , events
  , trajectories
  , finalState
  , baselineDiff
  , configDiff
  , agencyTable
  , compatible
  ) where

import Prelude

import Board.Gameboard (Gameboard)
import Core.Invariants (Failure)
import Core.Market (marginal)
import Core.Query (holding, lookupQ, priceOf, totalHolding)
import Core.Num (Q)
import Core.Num as Q
import Core.World (Event(..), Manifest, RoundRecord, WorldState)
import Core.Types (EntityId, EntityKind(..), Id, Intent(..), MarketTag, ResourceId)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concat, concatMap, filter, find, foldl, fromFoldable, index, length, mapWithIndex, nub, sortWith, tail)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Either (Either(..))
import Data.Foldable (any, for_)
import Data.Tuple (Tuple(..))
import Data.String (joinWith)
import Foreign.Object as Object
import Harness.Analysis (AgencyProfile, evaluateObjectives)

type Table = { title :: String, columns :: Array String, rows :: Array (Array String) }

-- | A metric per round; values are decimal strings in their own units.
type Chart = { title :: String, unit :: String, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }

-- | A run as read back from the Run Store, with its records and snapshots (single-run views).
type StoredRun =
  { id :: String
  , name :: String
  , status :: String
  , manifest :: Manifest
  , playerSeed :: String
  , spec :: Array (Tuple String String) -- flattened specification, for configuration diffs
  , records :: Array RoundRecord
  , snapshots :: Array WorldState
  , assertions :: Array (Array Failure)
  }

-- | The scenario-specific names the metric views use. The default is the reference scenario
-- | (docs/05_test_instance.md); a caller passes its own to view another scenario.
type MetricSpec =
  { procurement :: Array String -- transformations counted as procurement of the military
  , procurementLabel :: String -- the overview's label for that procurement
  , market :: String -- the Market a trajectory comparison shows by default
  }

defaultMetricSpec :: MetricSpec
defaultMetricSpec =
  { procurement: [ "defense-procurement", "defense-procurement-cn" ]
  , procurementLabel: "Military"
  , market: "energy-atlantic"
  }

-- | A run as the list shows it: its summary, read without its records or snapshots.
type RunSummary =
  { id :: String
  , name :: String
  , status :: String
  , reason :: Maybe String
  , gameboard :: String
  , gameSeed :: String
  , playerSeed :: String
  , rounds :: Int
  , assertionsFailing :: Int
  }

-- | A stored run that cannot be read: the list shows it with the reason, and keeps the others.
type Unreadable = { id :: String, name :: String, reason :: String }

q :: Q -> String
q = Q.format

-- | The World State after round `n` (0 = initial); Nothing for a round the run does not have.
at :: StoredRun -> Int -> Maybe WorldState
at r n
  | n < 0 = Nothing
  | n == 0 = Just r.manifest.initial
  | otherwise = index r.snapshots (n - 1)

final :: StoredRun -> WorldState
final r = fromMaybe r.manifest.initial (at r (length r.snapshots))

outOfRange :: StoredRun -> Int -> Int -> String
outOfRange r lo n = "round " <> show n <> " is out of range for run " <> r.id <> " (" <> show lo <> ".." <> show (length r.records) <> ")"

kindOf :: WorldState -> EntityId -> String
kindOf w e = case _.kind <$> Map.lookup e w.entities of
  Just (Aggregate _) -> "Aggregate (residual)"
  _ -> "Actor"

runList :: Array (Either Unreadable RunSummary) -> Table
runList rows =
  { title: "Simulation runs"
  , columns: [ "Run", "Name", "Gameboard", "Game seed", "Player seed", "Rounds", "Status", "Assertions failing", "Reason" ]
  , rows: map row rows
  }
  where
  row = case _ of
    Left u -> [ u.id, u.name, "—", "—", "—", "—", "unreadable", "—", u.reason ]
    Right s -> [ s.id, s.name, s.gameboard, s.gameSeed, s.playerSeed, show s.rounds, s.status, show s.assertionsFailing, fromMaybe "" s.reason ]

-- | A Market's price after a World State, as the views show it.
type MarketPrice = { market :: String, price :: String }

marketPrices :: WorldState -> Array MarketPrice
marketPrices w = map (\(Tuple id m) -> { market: show id, price: q (marginal m.cost m.position) }) (Map.toUnfoldable w.markets)

overview :: MetricSpec -> StoredRun -> Table
overview spec r =
  { title: "Overview"
  , columns: [ "Field", "Value" ]
  , rows:
      [ [ "Run", r.id ]
      , [ "Name", r.name ]
      , [ "Gameboard", r.manifest.gameboard ]
      , [ "Engine", r.manifest.engine ]
      , [ "Draws", r.manifest.draw ]
      , [ "Game seed", show r.manifest.initial.seed ]
      , [ "Player seed", r.playerSeed ]
      , [ "Rounds", show (length r.records) ]
      , [ "Assertions failing", show (length (concat r.assertions)) ]
      , [ "Rejected Proposals", show (count isRejected) ]
      , [ spec.procurementLabel <> " procured", q (Q.sumQ (concatMap procured (concatMap _.events r.records))) ]
      ] <> map (\p -> [ "Final price " <> p.market, p.price ]) (marketPrices (final r))
  }
  where
  count f = length (filter f (concatMap _.events r.records))
  isRejected = case _ of
    Rejected _ -> true
    _ -> false
  procured = case _ of
    Transformed t | any (\name -> unwrap t.transformation == name) spec.procurement -> [ t.quantity ]
    _ -> []

-- | Every Actor's and Aggregate's balance sheet after round `n` (0 = initial).
balances :: StoredRun -> Int -> Either String Table
balances r n = case at r n of
  Nothing -> Left (outOfRange r 0 n)
  Just w -> Right
    { title: "Balance sheets after round " <> show n
    , columns: [ "Entity", "Kind", "Resource", "Initial", "Current", "Change" ]
    , rows: concatMap (sheet w) (fromFoldable (Map.keys w.entities))
    }
  where
  w0 = r.manifest.initial
  sheet w e = map (\res -> [ show e, kindOf w e, show res, q (holding w0 e res), q (holding w e res), q (Q.sub (holding w e res) (holding w0 e res)) ])
    (resources w e)

  resources :: WorldState -> EntityId -> Array ResourceId
  resources w e = nub (concatMap (\world -> fromFoldable (Map.keys (fromMaybe Map.empty (_.accounts <$> Map.lookup e world.entities)))) [ w0, w ])

series :: forall a. StoredRun -> String -> (WorldState -> Array (Tuple String a)) -> (a -> String) -> Array { name :: String, points :: Array { round :: Int, value :: String } }
series r _ f g =
  let
    states = mapWithIndex Tuple ([ r.manifest.initial ] <> r.snapshots)
    names = nub (concatMap (\(Tuple _ w) -> map (\(Tuple n _) -> n) (f w)) states)
  in
    map (\name -> { name, points: concatMap (\(Tuple i w) -> map (\(Tuple _ v) -> { round: i, value: g v }) (filter (\(Tuple n _) -> n == name) (f w))) states }) names

markets :: StoredRun -> Array Chart
markets r =
  [ { title: "Marginal price", unit: "M/E", series: series r "price" (\w -> map (\(Tuple id m) -> Tuple (show id) (marginal m.cost m.position)) (Map.toUnfoldable w.markets)) q }
  , { title: "Energy reserves", unit: "E", series: series r "reserves" (\w -> map (\(Tuple id m) -> Tuple (show id) (lookupQ m.base m.reserves)) (Map.toUnfoldable w.markets)) q }
  , { title: "Traded volume", unit: "E", series: volume }
  ]
  where
  volume = map
    ( \(Tuple id _) ->
        { name: show id
        , points: mapWithIndex
            ( \i rec ->
                { round: i + 1
                , value: q
                    ( Q.sumQ
                        ( concatMap
                            ( case _ of
                                Traded t | t.market == id && t.quantity > Q.zero -> [ t.quantity ]
                                _ -> []
                            )
                            rec.events
                        )
                    )
                }
            )
            r.records
        }
    )
    (Map.toUnfoldable r.manifest.initial.markets :: Array _)

facilities :: StoredRun -> Table
facilities r =
  { title: "Facilities"
  , columns: [ "Round", "Facility", "Ordered by", "Kind", "Quantity", "Paid" ]
  , rows: concatMap
      ( \rec -> concatMap
          ( case _ of
              Transformed t -> [ [ show rec.round, show t.transformation, show t.by, kindOf r.manifest.initial t.by, q t.quantity, q t.paid ] ]
              _ -> []
          )
          rec.events
      )
      r.records
  }

economy :: StoredRun -> { totals :: Chart, flows :: Table, pressure :: Table }
economy r = { totals: resourceTotals r, flows: moneyFlows r, pressure: pressure r }

resourceTotals :: StoredRun -> Chart
resourceTotals r =
  { title: "Resource totals across all accounts"
  , unit: ""
  , series: series r "totals" (\w -> map (\res -> Tuple (show res) (totalHolding w res)) (fromFoldable (Map.keys w.resources))) q
  }

-- | Money moved between categories of Entity (Actor or Aggregate) over the run.
moneyFlows :: StoredRun -> Table
moneyFlows r =
  { title: "Money circulation (from → to category)"
  , columns: [ "From", "To", "Money" ]
  , rows: map (\(Tuple (Tuple a b) m) -> [ a, b, q m ]) (Map.toUnfoldable (foldl add Map.empty (concatMap _.events r.records)))
  }
  where
  w0 = r.manifest.initial
  add acc = case _ of
    Transferred t | t.resource == w0.numeraire -> Map.insertWith Q.add (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc
    _ -> acc

-- | One round's pressure: shortfalls, consumption and decay, and the Proposals rejected or lapsed.
type Pressure = { shortfall :: Q, consumed :: Q, decayed :: Q, rejected :: Int, lapsed :: Int }

roundPressure :: RoundRecord -> Pressure
roundPressure rec =
  { shortfall: sum' shortfallOf
  , consumed: sum' consumedOf
  , decayed: sum' decayedOf
  , rejected: count isRejected
  , lapsed: count isLapsed
  }
  where
  sum' f = Q.sumQ (concatMap f rec.events)
  count f = length (filter f rec.events)
  shortfallOf = case _ of
    Shortfall s -> [ s.quantity ]
    _ -> []
  consumedOf = case _ of
    Consumed c -> [ c.quantity ]
    _ -> []
  decayedOf = case _ of
    Decayed d -> [ d.quantity ]
    _ -> []
  isRejected = case _ of
    Rejected _ -> true
    _ -> false
  isLapsed = case _ of
    Lapsed _ -> true
    _ -> false

-- | Per round: shortfalls, rejected and lapsed Proposals, consumption, and decay.
pressure :: StoredRun -> Table
pressure r =
  { title: "Pressure"
  , columns: [ "Round", "Shortfalls (E)", "Rejected", "Lapsed", "Consumed (E)", "Decayed" ]
  , rows: map (\rec -> let p = roundPressure rec in [ show rec.round, q p.shortfall, show p.rejected, show p.lapsed, q p.consumed, q p.decayed ]) r.records
  }

-- | The four-stage record: observation and estimate in round r, the Aggregate's adjusted
-- | purchase and its execution in round r+1, and the influence ratio. The last round has no
-- | r+1, so its next purchase and influence ratio are "n/a".
representation :: StoredRun -> Table
representation r =
  { title: "Representation"
  , columns: [ "Round", "Representative", "Aggregate", "Domain", "Observation (proposed)", "Executed (own)", "Weight", "Reference", "Shrinkage", "Noise spread", "Stored factor", "Aggregate purchase (r+1)", "Influence ratio" ]
  , rows: concatMap rows (mapWithIndex Tuple r.records)
  }
  where
  rows (Tuple i rec) = concatMap
    ( case _ of
        Estimated est -> map (stages i rec est.factor) (filter (\rep -> rep.aggregate == est.aggregate && rep.domain == est.domain) r.manifest.initial.representations)
        _ -> []
    )
    rec.events
  stages i rec factor rep =
    let
      observed = proposedPurchases rep.actor rec
      own = bought rep.actor rec
      next = bought rep.aggregate <$> index r.records (i + 1)
      induced nx = if factor == Q.zero then Q.zero else Q.sub nx (Q.div nx factor)
      ratio = case next of
        Just nx | own /= Q.zero -> q (Q.div (induced nx) own)
        _ -> "n/a"
    in
      [ show rec.round, show rep.actor, show rep.aggregate, show rep.domain, q observed, q own, q rep.weight, q rep.estimator.reference, q rep.estimator.shrinkage, q rep.estimator.noiseSpread, q factor, maybe "n/a" q next, ratio ]

-- | Quantities an Entity offered to buy in a round's Proposals.
proposedPurchases :: EntityId -> RoundRecord -> Q
proposedPurchases e rec = Q.sumQ (concatMap (\s -> if s.proposal.author == e then concatMap buy s.proposal.intents else []) rec.inputs)
  where
  buy = case _ of
    Trade o | o.quantity > Q.zero -> [ o.quantity ]
    _ -> []

-- | Quantities an Entity actually bought in a round.
bought :: EntityId -> RoundRecord -> Q
bought e rec = Q.sumQ
  ( concatMap
      ( case _ of
          Traded t | t.entity == e && t.quantity > Q.zero -> [ t.quantity ]
          _ -> []
      )
      rec.events
  )

procedures :: StoredRun -> Table
procedures r =
  { title: "Collective procedures"
  , columns: [ "Round", "Proposal", "Members who signed", "Support", "Threshold", "Outcome" ]
  , rows: concatMap
      ( \rec -> concatMap
          ( case _ of
              ProcedureOutcome o -> [ [ show rec.round, show o.proposal, signedBy rec o.proposal, q o.support, q o.threshold, if o.executed then "executed" else "lapsed" ] ]
              _ -> []
          )
          rec.events
      )
      r.records
  }
  where
  signedBy rec pid = joinComma (concatMap (\s -> if s.proposal.id == pid then map show s.signatures else []) rec.inputs)

objectives :: Gameboard -> StoredRun -> Table
objectives gb r =
  { title: "Objectives"
  , columns: [ "Actor", "Objective", "Criterion", "Outcome", "Values", "Bound" ]
  , rows: concatMap (\o -> mapWithIndex (\i c -> [ show o.actor, o.description, show (i + 1), show c.outcome, joinComma (map q c.values), joinComma (map q c.bound) ]) o.criteria)
      (evaluateObjectives gb (mapWithIndex (\i rec -> { world: fromMaybe r.manifest.initial (at r (i + 1)), record: rec }) r.records))
  }

-- | A round's events in canonical phase order (the order the engine records them).
events :: StoredRun -> Int -> Either String Table
events r n = case index r.records (n - 1) of
  Just rec | n >= 1 -> Right { title: "Events, round " <> show n, columns: [ "Event" ], rows: map (\e -> [ show e ]) rec.events }
  _ -> Left (outOfRange r 1 n)

-- Comparison --------------------------------------------------------------------

-- | Runs are directly comparable only when they agree on everything the manifest fixes: the
-- | Gameboard version, the engine, the round count, the units, the Market definitions, and the
-- | Actor selection (the preset every seat plays). Each difference is named, and the run is flagged.
compatible :: Array StoredRun -> Table
compatible runs =
  { title: "Compatibility"
  , columns: [ "Run", "Gameboard", "Engine", "Rounds", "Units", "Market definitions", "Actor selection", "Comparable with the first" ]
  , rows: map row runs
  }
  where
  row r = [ r.id, r.manifest.gameboard, r.manifest.engine, show (length r.records), sameAs units r, sameAs markets' r, actors r, verdict r ]
  sameAs f r = maybe "—" (\first -> if f first == f r then "same" else "differs") (index runs 0)
  verdict r = case index runs 0 of
    Nothing -> "yes"
    Just first -> case mismatches first r of
      [] -> "yes"
      ms -> "no: flagged, not combined (" <> joinComma ms <> ")"
  mismatches f r = concat
    [ if f.manifest.gameboard == r.manifest.gameboard then [] else [ "Gameboard" ]
    , if f.manifest.engine == r.manifest.engine then [] else [ "engine" ]
    , if length f.records == length r.records then [] else [ "round count" ]
    , if units f == units r then [] else [ "units" ]
    , if markets' f == markets' r then [] else [ "Market definitions" ]
    , if actors f == actors r then [] else [ "Actor selection" ]
    ]

  units :: StoredRun -> String
  units r = stringify (encodeJson (Object.fromFoldable (map (\(Tuple k d) -> Tuple (show k) d.unit) (Map.toUnfoldable r.manifest.initial.resources :: Array _))))

  markets' :: StoredRun -> String
  markets' r = stringify (encodeJson (Object.fromFoldable (map (\(Tuple k m) -> Tuple (show k) m) (Map.toUnfoldable r.manifest.initial.markets :: Array _))))

  actors :: StoredRun -> String
  actors r = fromMaybe "—" (map (\(Tuple _ v) -> v) (find (\(Tuple k _) -> k == "preset") r.spec))

-- | One market's price across runs, aligned by round. The unit is the Market's quote per base unit,
-- | read from the Resources' units in the run's manifest. A run without the Market is an error.
trajectories :: Array StoredRun -> String -> Either String Chart
trajectories runs market = do
  for_ runs \r -> maybe (Left ("market " <> market <> " is not in run " <> r.id)) Right (Map.lookup mid r.manifest.initial.markets)
  unit <- maybe (Right "") marketUnit (index runs 0)
  pure
    { title: "Price, " <> market
    , unit
    , series: map (\r -> { name: r.id, points: mapWithIndex (\i w -> { round: i, value: q (price w) }) ([ r.manifest.initial ] <> r.snapshots) }) runs
    }
  where
  mid :: Id MarketTag
  mid = wrap market

  price w = fromMaybe Q.zero (priceOf w mid)

  marketUnit r = do
    m <- maybe (Left ("market " <> market <> " is not in run " <> r.id)) Right (Map.lookup mid r.manifest.initial.markets)
    base <- resourceUnit r m.base
    quoteUnit <- resourceUnit r m.quote
    pure (quoteUnit <> "/" <> base)

  resourceUnit r res = maybe (Left ("resource " <> show res <> " is not in run " <> r.id)) (Right <<< _.unit) (Map.lookup res r.manifest.initial.resources)

finalState :: Array StoredRun -> Table
finalState runs =
  { title: "Final state"
  , columns: [ "Entity", "Resource" ] <> map _.id runs
  , rows: map (\(Tuple e res) -> [ show e, show res ] <> map (\r -> q (holding (final r) e res)) runs) keys
  }
  where
  keys = case index runs 0 of
    Just r -> sortWith (\(Tuple e res) -> show e <> show res) (concatMap (\(Tuple e x) -> map (Tuple e) (fromFoldable (Map.keys x.accounts))) (Map.toUnfoldable (final r).entities))
    Nothing -> []

-- | Each run's final holdings minus the baseline's, where they differ.
baselineDiff :: StoredRun -> Array StoredRun -> Table
baselineDiff base others =
  { title: "Difference from baseline " <> base.id
  , columns: [ "Run", "Entity", "Resource", "Δ" ]
  , rows: concatMap
      ( \r -> concatMap
          ( \(Tuple e x) -> concatMap
              ( \(Tuple res v) ->
                  let
                    d = Q.sub v (holding (final base) e res)
                  in
                    if d == Q.zero then [] else [ [ r.id, show e, show res, q d ] ]
              )
              (Map.toUnfoldable x.accounts)
          )
          (Map.toUnfoldable (final r).entities)
      )
      others
  }

-- | Exact specification differences between runs.
configDiff :: StoredRun -> Array StoredRun -> Table
configDiff base others =
  { title: "Configuration differences"
  , columns: [ "Parameter", base.id ] <> map _.id others
  , rows: filter (\row -> nub (fromMaybe [] (tail row)) /= fromMaybe [] (map pure (index row 1))) (map (\k -> [ k, value base k ] <> map (\r -> value r k) others) keys)
  }
  where
  keys = nub (concatMap (\r -> map (\(Tuple k _) -> k) r.spec) ([ base ] <> others) <> [ "game seed", "player seed" ])
  value r k
    | k == "game seed" = show r.manifest.initial.seed
    | k == "player seed" = r.playerSeed
    | otherwise = fromMaybe "—" (map (\(Tuple _ v) -> v) (find (\(Tuple k' _) -> k' == k) r.spec))

agencyTable :: Array AgencyProfile -> Table
agencyTable ps =
  { title: "Agency"
  , columns: [ "Actor", "Alternative", "Price change", "Others' holdings", "Aggregate holdings", "Institutional decisions", "Own criteria changed", "Measurable" ]
  , rows: map (\p -> [ show p.actor, p.alternative, q p.prices, q p.others, q p.aggregates, show p.institutions, show p.objectives, if p.measurable then "yes" else "no: balancing finding" ]) ps
  }

joinComma :: Array String -> String
joinComma = joinWith ", "
