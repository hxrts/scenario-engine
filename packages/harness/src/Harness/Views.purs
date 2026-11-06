-- | View models for the dashboard (docs/06_simulator.md §9–§10). Every view is a generic
-- | Table or Chart computed here, so the frontend only renders. Pure.
module Harness.Views
  ( Table
  , Chart
  , StoredRun
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
import Core.Num (Q)
import Core.Num as Q
import Core.World (Event(..), Manifest, RoundRecord, WorldState)
import Core.Types (EntityId, EntityKind(..), Id, Intent(..), ResourceId)
import Data.String (joinWith)
import Data.Array (concatMap, filter, find, foldl, fromFoldable, index, length, mapWithIndex, nub, sortWith, tail)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Harness.Analysis (AgencyProfile, evaluateObjectives)

type Table = { title :: String, columns :: Array String, rows :: Array (Array String) }

-- | A metric per round; values are decimal strings in their own units.
type Chart = { title :: String, unit :: String, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }

-- | A run as read back from the Run Store.
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

q :: Q -> String
q = Q.format

-- | The World State after round `n` (0 = initial).
at :: StoredRun -> Int -> WorldState
at r n = if n <= 0 then r.manifest.initial else fromMaybe r.manifest.initial (index r.snapshots (n - 1))

final :: StoredRun -> WorldState
final r = at r (length r.snapshots)

kindOf :: WorldState -> EntityId -> String
kindOf w e = case _.kind <$> Map.lookup e w.entities of
  Just (Aggregate _) -> "Aggregate (residual)"
  _ -> "Actor"

runList :: Array StoredRun -> Table
runList runs =
  { title: "Simulation runs"
  , columns: [ "Run", "Name", "Gameboard", "Game seed", "Player seed", "Rounds", "Status", "Assertions failing" ]
  , rows: map (\r -> [ r.id, r.name, r.manifest.gameboard, show r.manifest.initial.seed, r.playerSeed, show (length r.records), r.status, show (length (concatMap identity r.assertions)) ]) runs
  }

overview :: StoredRun -> Table
overview r =
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
      , [ "Assertions failing", show (length (concatMap identity r.assertions)) ]
      , [ "Rejected Proposals", show (count isRejected) ]
      , [ "Military procured", q (Q.sumQ (concatMap (procured "Mil") (concatMap _.events r.records))) ]
      ] <> map (\(Tuple id m) -> [ "Final price " <> show id, q (marginal m.cost m.position) ]) (Map.toUnfoldable (final r).markets)
  }
  where
  count f = length (filter f (concatMap _.events r.records))
  isRejected = case _ of
    Rejected _ -> true
    _ -> false
  procured _ = case _ of
    Transformed t | unwrap t.transformation == "defense-procurement" || unwrap t.transformation == "defense-procurement-cn" -> [ t.quantity ]
    _ -> []

-- | Every Actor's and Aggregate's balance sheet after round `n`.
balances :: StoredRun -> Int -> Table
balances r n =
  { title: "Balance sheets after round " <> show n
  , columns: [ "Entity", "Kind", "Resource", "Initial", "Current", "Reserved", "Change" ]
  , rows: concatMap sheet (fromFoldable (Map.keys w.entities))
  }
  where
  w = at r n
  w0 = r.manifest.initial
  amount world e res = fromMaybe Q.zero (Map.lookup e world.entities >>= \x -> Map.lookup res x.accounts)
  sheet e = map (\res -> [ show e, kindOf w e, show res, q (amount w0 e res), q (amount w e res), q Q.zero, q (Q.sub (amount w e res) (amount w0 e res)) ])
    (resources e)

  resources :: EntityId -> Array ResourceId
  resources e = nub (concatMap (\world -> fromFoldable (Map.keys (fromMaybe Map.empty (_.accounts <$> Map.lookup e world.entities)))) [ w0, w ])

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
  , { title: "Energy reserves", unit: "E", series: series r "reserves" (\w -> map (\(Tuple id m) -> Tuple (show id) (fromMaybe Q.zero (Map.lookup m.base m.reserves))) (Map.toUnfoldable w.markets)) q }
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
economy r =
  { totals: { title: "Resource totals across all accounts", unit: "", series: series r "totals" (\w -> map (\res -> Tuple (show res) (total w res)) (fromFoldable (Map.keys w.resources))) q }
  , flows:
      { title: "Money circulation (from → to category)"
      , columns: [ "From", "To", "Money" ]
      , rows: map (\(Tuple (Tuple a b) m) -> [ a, b, q m ])
          ( Map.toUnfoldable
              ( foldl
                  ( \acc ev -> case ev of
                      Transferred t | t.resource == r.manifest.initial.numeraire -> Map.insertWith Q.add (Tuple (kindOf w0 t.from) (kindOf w0 t.to)) t.quantity acc
                      _ -> acc
                  )
                  Map.empty
                  (concatMap _.events r.records)
              )
          )
      }
  , pressure:
      { title: "Pressure"
      , columns: [ "Round", "Shortfalls (E)", "Rejected", "Lapsed", "Consumed (E)", "Decayed" ]
      , rows: map (\rec -> [ show rec.round, sum' shortfall rec, show (length (filter rejected rec.events)), show (length (filter lapsed rec.events)), sum' consumed rec, sum' decayed rec ]) r.records
      }
  }
  where
  w0 = r.manifest.initial
  total w res = Q.add (Q.sumQ (map (\e -> fromMaybe Q.zero (Map.lookup res e.accounts)) (fromFoldable (Map.values w.entities))))
    (Q.sumQ (map (\m -> fromMaybe Q.zero (Map.lookup res m.reserves)) (fromFoldable (Map.values w.markets))))
  sum' f rec = q (Q.sumQ (concatMap f rec.events))
  shortfall = case _ of
    Shortfall s -> [ s.quantity ]
    _ -> []
  consumed = case _ of
    Consumed c -> [ c.quantity ]
    _ -> []
  decayed = case _ of
    Decayed d -> [ d.quantity ]
    _ -> []
  rejected = case _ of
    Rejected _ -> true
    _ -> false
  lapsed = case _ of
    Lapsed _ -> true
    _ -> false

-- | The four-stage record: observation and estimate in round r, the Aggregate's adjusted
-- | purchase and its execution in round r+1, and the influence ratio.
representation :: StoredRun -> Table
representation r =
  { title: "Representation"
  , columns: [ "Round", "Representative", "Aggregate", "Domain", "Observation (proposed)", "Executed (own)", "Weight", "Reference", "Shrinkage", "Noise spread", "Stored factor", "Aggregate purchase (r+1)", "Influence ratio" ]
  , rows: concatMap rows (mapWithIndex Tuple r.records)
  }
  where
  rows (Tuple i rec) = concatMap
    ( case _ of
        Estimated est -> concatMap
          ( \rep ->
              let
                observed = Q.sumQ
                  ( concatMap
                      ( \s ->
                          if s.proposal.author == rep.actor then concatMap
                            ( case _ of
                                Trade o -> if o.quantity > Q.zero then [ o.quantity ] else []
                                _ -> []
                            )
                            s.proposal.intents
                          else []
                      )
                      rec.inputs
                  )
                own = bought rep.actor rec
                next = maybe' Q.zero (bought rep.aggregate) (index r.records (i + 1))
                induced = if est.factor == Q.zero then Q.zero else Q.sub next (Q.div next est.factor)
                ratio = if own == Q.zero then "n/a" else q (Q.div induced own)
              in
                [ [ show rec.round, show rep.actor, show rep.aggregate, show rep.domain, q observed, q own, q rep.weight, q rep.estimator.reference, q rep.estimator.shrinkage, q rep.estimator.noiseSpread, q est.factor, q next, ratio ] ]
          )
          (filter (\rep -> rep.aggregate == est.aggregate && rep.domain == est.domain) r.manifest.initial.representations)
        _ -> []
    )
    rec.events
  bought e rec = Q.sumQ
    ( concatMap
        ( case _ of
            Traded t | t.entity == e && t.quantity > Q.zero -> [ t.quantity ]
            _ -> []
        )
        rec.events
    )
  maybe' d f = case _ of
    Just x -> f x
    Nothing -> d

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
  , columns: [ "Actor", "Objective", "Criterion", "Met", "Values", "Bound" ]
  , rows: concatMap (\o -> mapWithIndex (\i c -> [ show o.actor, o.description, show (i + 1), if c.met then "met" else "not met", joinComma (map q c.values), joinComma (map q c.bound) ]) o.criteria)
      (evaluateObjectives gb (mapWithIndex (\i rec -> { world: at r (i + 1), record: rec }) r.records))
  }

-- | A round's events in canonical phase order (the order the engine records them).
events :: StoredRun -> Int -> Table
events r n =
  { title: "Events, round " <> show n
  , columns: [ "Event" ]
  , rows: maybe' [] (map (\e -> [ show e ]) <<< _.events) (index r.records (n - 1))
  }
  where
  maybe' d f = case _ of
    Just x -> f x
    Nothing -> d

-- Comparison --------------------------------------------------------------------

-- | Runs are directly comparable only on the same Gameboard version and round count.
compatible :: Array StoredRun -> Table
compatible runs =
  { title: "Compatibility"
  , columns: [ "Run", "Gameboard", "Rounds", "Comparable with the first" ]
  , rows: map (\r -> [ r.id, r.manifest.gameboard, show (length r.records), if same r then "yes" else "no: flagged, not combined" ]) runs
  }
  where
  first = index runs 0
  same r = maybe' true (\f -> f.manifest.gameboard == r.manifest.gameboard && length f.records == length r.records) first
  maybe' d f = case _ of
    Just x -> f x
    Nothing -> d

-- | One market's price across runs, aligned by round.
trajectories :: Array StoredRun -> String -> Chart
trajectories runs market =
  { title: "Price, " <> market
  , unit: "M/E"
  , series: map (\r -> { name: r.id, points: mapWithIndex (\i w -> { round: i, value: q (price w) }) ([ r.manifest.initial ] <> r.snapshots) }) runs
  }
  where
  price w = fromMaybe Q.zero (map (\m -> marginal m.cost m.position) (Map.lookup (wrapId market) w.markets))

  wrapId :: forall t. String -> Id t
  wrapId = wrap

finalState :: Array StoredRun -> Table
finalState runs =
  { title: "Final state"
  , columns: [ "Entity", "Resource" ] <> map _.id runs
  , rows: map (\(Tuple e res) -> [ show e, show res ] <> map (\r -> q (amount (final r) e res)) runs) keys
  }
  where
  keys = case index runs 0 of
    Just r -> sortWith (\(Tuple e res) -> show e <> show res) (concatMap (\(Tuple e x) -> map (Tuple e) (fromFoldable (Map.keys x.accounts))) (Map.toUnfoldable (final r).entities))
    Nothing -> []
  amount w e res = fromMaybe Q.zero (Map.lookup e w.entities >>= \x -> Map.lookup res x.accounts)

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
                    d = Q.sub v (fromMaybe Q.zero (Map.lookup e (final base).entities >>= \b -> Map.lookup res b.accounts))
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
