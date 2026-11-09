-- | Shared shapes and helpers for the dashboard view models (docs/06_simulator.md §9–§10): the
-- | Table and Chart the frontend renders, the stored run the single-run views read, the metric
-- | names, round scoping, and cell formatting. Pure.
module Views.Common
  ( Table
  , TableRow
  , Chart
  , Point
  , Bars
  , Statistic
  , Reason
  , Column
  , column
  , emptied
  , names
  , Field
  , cellOf
  , tableOf
  , columnsOf
  , col
  , sharedColumns
  , StoredRun
  , focusOf
  , RunCore
  , RunHeader
  , RunHeaderRow
  , storedRun
  , records
  , snapshots
  , worlds
  , completed
  , target
  , pointsOf
  , recordPoints
  , quantity
  , price
  , ratio
  , unitOf
  , listedHolding
  , ofCount
  , supportThreshold
  , exact
  , at
  , final
  , outOfRange
  , kindOf
  , series
  , outcomeLabel
  , Procedure
  , asProcedure
  , proceduresOf
  , players
  ) where

import Prelude

import Core.Map (entries)

import Record as Record
import Laws.World (Focus, Manifest, RoundRecord, RoundResult, WorldState)
import Laws.Types (idText, ActorId, CandidateTransactionId, ResourceDefinition, ResourceId)
import Schema.Composition (capacityCategory)

import Laws.Invariants (Failure)
import Core.Num (Kind, Q)
import Core.Num as Q
import Laws.Query (isAggregate)
import Laws.Slice (event, select)
import Schema.Run (RunStatus, Spec)
import Data.Array (concatMap, filter, index, length, mapMaybe, mapWithIndex, nub)
import Data.Foldable (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)

-- | The fields of a table, extended by the view models that are a table with more beside it.
type TableRow r = (title :: String, columns :: Array String, rows :: Array (Array String) | r)

type Table = Record (TableRow ())

-- | What an empty value of a column shows: a short label, and the reason on hover.
type Reason = { label :: String, reason :: String }

-- | A column or header field: its name, what it means, and what an empty value of it means. Tables and
-- | facts are built from these, and the glossary is generated from them, so a column cannot go undefined.
type Column = { name :: String, definition :: String, emptyReason :: Maybe Reason }

-- | A column with its plain-language definition.
column :: String -> String -> Column
column name definition = { name, definition, emptyReason: Nothing }

-- | A column whose empty value has a reason of its own.
emptied :: Reason -> Column -> Column
emptied emptyReason c = c { emptyReason = Just emptyReason }

-- | The headings of a table's columns.
names :: Array Column -> Array String
names = map _.name

-- | A table column: its heading and definition, and the cell it shows for a row. A table is built from these,
-- | so a heading and its cells cannot drift apart, and the glossary reads the same array.
type Field a = { column :: Column, cell :: a -> String }

-- | A field from its column and its cell.
cellOf :: forall a. Column -> (a -> String) -> Field a
cellOf column' cell = { column: column', cell }

-- | A table with one row per item.
tableOf :: forall a. String -> Array (Field a) -> Array a -> Table
tableOf title fields items = { title, columns: names (columnsOf fields), rows: map (\x -> map (\f -> f.cell x) fields) items }

-- | The columns of a table's fields, for the glossary.
columnsOf :: forall a. Array (Field a) -> Array Column
columnsOf = map _.column

-- | The columns several views share. A view's own columns are defined where it builds its table.
col
  :: { run :: Column
     , name :: Column
     , composition :: Column
     , gameSeed :: Column
     , playerSeed :: Column
     , rounds :: Column
     , status :: Column
     , explicitActor :: Column
     , objective :: Column
     , outcome :: Column
     , round :: Column
     , actor :: Column
     , kind :: Column
     , resource :: Column
     , player :: Column
     , measure :: Column
     , spread :: Column
     }
col =
  { run: column "Run" "The run's id in the Run Store. Selecting a row shows that run's results."
  , name: column "Name" "The name the run was given at launch, or by the derivation that created it." # emptied { label: "not recorded", reason: "The run was stored without a name." }
  , composition: column "Composition" "The PremiseComposition version the run was played on."
  , gameSeed: column "Game seed" "The seed fixed by the premise. It drives the in-world draws, so the same game seed gives the same draws." # emptied { label: "not recorded", reason: "The run's stored manifest has no game seed." }
  , playerSeed: column "Player seed" "The seed for seat-strategy choices. It is separate from the game seed, so the two streams never share draws." # emptied { label: "not recorded", reason: "The run's stored specification has no player seed (runs made before seeds were recorded)." }
  , rounds: column "Rounds" "Rounds completed so far. A run stopped short of its target can be resumed."
  , status: column "Status" "running, completed or failed. A failed run gives its reason in the Reason column."
  , explicitActor: column "Explicit Actor" "The Actor (or Aggregate) this row describes."
  , objective: column "Objective" "A goal from the composition for one Actor. It is measured from the run and never changes the economy."
  , outcome: column "Outcome" "For objectives: met, not met or not evaluated. A criterion is not evaluated when the run has no rounds or no data, and is never reported as met. For procedures: executed if the support reached the threshold, lapsed otherwise."
  , round: column "Round" "The round this row belongs to. Round 1 is the first round after the initial state."
  , actor: column "Actor" "The Actor whose balance sheet this is: an Actor or an Aggregate."
  , kind: column "Kind" "Actor, or Aggregate (residual): a residual representation of many holders, not one Actor."
  , resource: column "Resource" "The Resource held."
  , player: column "Player" "The Actor this row describes, by its seat."
  , measure: column "Measure" "What this row measures."
  , spread: column "Spread" "The highest final net worth minus the lowest, in the numeraire. Larger means less even."
  }

-- | The shared columns, for the glossary.
sharedColumns :: Array Column
sharedColumns = [ col.run, col.name, col.composition, col.gameSeed, col.playerSeed, col.rounds, col.status, col.explicitActor, col.objective, col.outcome, col.round, col.actor, col.kind, col.resource, col.player, col.measure, col.spread ]

-- | A metric per round in its own unit. The points are exact decimal strings (`exact`), so the plot
-- | keeps full precision; the chart's axis and tooltips show the value under its display `kind`.
type Chart = { title :: String, unit :: String, kind :: Kind, series :: Array { name :: String, points :: Array Point } }

-- | A chart point: a round and its exact value.
type Point = { round :: Int, value :: String }

-- | Grouped bars: one group per category, one bar per series (a run). Each series' values line up with
-- | `categories`; a missing value is the empty string. The bars' values are shown under their display `kind`.
type Bars = { title :: String, unit :: String, kind :: Kind, categories :: Array String, series :: Array { name :: String, values :: Array String } }

-- | A statistic the views show: its name, the question it answers, its one-line definition, and its
-- | formula. Each statistic is defined once, in the module that computes it; the dashboard renders it.
type Statistic = { name :: String, question :: String, definition :: String, formula :: String }

-- | What every read of a run shares: its id, name, and status.
type RunCore r = (id :: String, name :: String, status :: RunStatus | r)

-- | A run's identity and specification, as read back from the Run Store.
type RunHeaderRow r =
  RunCore
    ( manifest :: Manifest
    , playerSeed :: String
    , spec :: Spec
    , sourceDeleted :: Boolean -- the run it was derived from is no longer in the store
    | r
    )

type RunHeader = Record (RunHeaderRow ())

-- | A run as read back from the Run Store, with each round's record and the World State it left
-- | (single-run views). `records`, `snapshots` and `worlds` read the rounds.
type StoredRun = Record (RunHeaderRow (rounds :: Array RoundResult, assertions :: Array (Array Failure)))

-- | The Resource a run's exchange views centre on: the one its premise declares, else its numeraire.
focusOf :: StoredRun -> Focus
focusOf r = fromMaybe { resource: r.manifest.initial.numeraire, label: idText r.manifest.initial.numeraire } r.manifest.initial.focus

-- | A stored run from its header, its rounds, and each round's assertions.
storedRun :: RunHeader -> Array RoundResult -> Array (Array Failure) -> StoredRun
storedRun h rounds assertions = Record.disjointUnion h { rounds, assertions }

-- | The run's round records, in round order.
records :: StoredRun -> Array RoundRecord
records r = map _.record r.rounds

-- | The World State after each completed round, in round order.
snapshots :: StoredRun -> Array WorldState
snapshots r = map _.world r.rounds

-- | The World States of the run, indexed by round: the initial one, then one per completed round.
worlds :: StoredRun -> Array WorldState
worlds r = [ r.manifest.initial ] <> snapshots r

-- | The rounds the run completed.
completed :: StoredRun -> Int
completed r = length r.rounds

-- | The rounds the run was launched for; a run whose spec does not say is at its completed rounds.
target :: StoredRun -> Int
target r = fromMaybe (completed r) r.spec.rounds

-- | Display (docs/02 §9.1): a count of units (Resource holdings and flows) as a cell.
quantity :: Q -> String
quantity = Q.displayQuantity

-- | A price (quote per base unit) as a cell.
price :: Q -> String
price = Q.displayQuantity

-- | A ratio or share (Gini, influence, shrinkage, factors, support) as a cell.
ratio :: Q -> String
ratio = Q.displayRatio

-- | Whether a Resource shows in a listing of holdings: a capacity is not listed among them (the premise's category).
listedHolding :: forall r. { resources :: Map.Map ResourceId ResourceDefinition | r } -> ResourceId -> Boolean
listedHolding w res = (Map.lookup res w.resources >>= _.category) /= Just capacityCategory

-- | A Resource's unit, from the run's manifest; empty for a Resource the manifest does not name.
unitOf :: StoredRun -> ResourceId -> String
unitOf r res = maybe "" _.unit (Map.lookup res r.manifest.initial.resources)

-- | "N of M", the prose for a count of the total it is out of.
ofCount :: Int -> Int -> String
ofCount n total = show n <> " of " <> show total

-- | "support S, threshold T", the prose for a collective procedure's support against its threshold.
supportThreshold :: Q -> Q -> String
supportThreshold support threshold = "support " <> ratio support <> ", threshold " <> ratio threshold

-- | The exact value, at four decimals, for chart points: the plot keeps full precision.
exact :: Q -> String
exact = Q.format

-- | The World State after round `n` (0 = initial); Nothing for a round the run does not have.
at :: StoredRun -> Int -> Maybe WorldState
at r n
  | n < 0 = Nothing
  | n == 0 = Just r.manifest.initial
  | otherwise = _.world <$> index r.rounds (n - 1)

-- | The World State after the run's last completed round.
final :: StoredRun -> WorldState
final r = fromMaybe r.manifest.initial (at r (completed r))

-- | The error for a round outside `lo..` the run's completed rounds.
outOfRange :: StoredRun -> Int -> Int -> String
outOfRange r lo n = "round " <> show n <> " is out of range for run " <> r.id <> " (" <> show lo <> ".." <> show (completed r) <> ")"

-- | An Actor's kind as the views label it: an Aggregate is residual, every other Actor an Actor.
kindOf :: WorldState -> ActorId -> String
kindOf w e
  | maybe false isAggregate (Map.lookup e w.actors) = "Aggregate (residual)"
  | otherwise = "Actor"

-- | One named series per key of `f` over the initial state and every snapshot, each point the
-- | value of that key at a round (0 = initial).
series :: StoredRun -> (WorldState -> Array (Tuple String Q)) -> Array { name :: String, points :: Array Point }
series r f = map (\name -> { name, points: pointsOf r (\w -> exact <$> lookup name (f w)) }) seriesNames
  where
  seriesNames = nub (concatMap (map fst <<< f) (worlds r))

-- | A point per World State of the run (the index is the round) where `f` gives a value.
pointsOf :: StoredRun -> (WorldState -> Maybe String) -> Array Point
pointsOf r f = mapMaybe (\(Tuple i w) -> { round: i, value: _ } <$> f w) (mapWithIndex Tuple (worlds r))

-- | A point per round record of the run (the first round is 1).
recordPoints :: StoredRun -> (RoundRecord -> String) -> Array Point
recordPoints r f = mapWithIndex (\i rec -> { round: i + 1, value: f rec }) (records r)

-- | A collective procedure's outcome, as the views and the glossary word it.
outcomeLabel :: Boolean -> String
outcomeLabel executed = if executed then "executed" else "lapsed"

-- | The players of a World State: its Actors, every Actor except the Aggregates (which are residual
-- | representations, not players), in Actor order.
players :: WorldState -> Array ActorId
players w = map fst (filter (\(Tuple _ e) -> not (isAggregate e)) (entries w.actors))

-- | A collective procedure the run recorded: its candidate Transaction, the members who signed it, and whether it
-- | executed, with its support against its threshold. Drivers and Representation both render their
-- | rows from this one read.
type Procedure = { round :: Int, candidateTransaction :: CandidateTransactionId, executed :: Boolean, support :: Q, threshold :: Q, signers :: Array ActorId }

-- | The procedure a ProcedureOutcome records, within its round.
asProcedure :: RoundRecord -> { round :: Int, candidateTransaction :: CandidateTransactionId, support :: Q, threshold :: Q, executed :: Boolean } -> Procedure
asProcedure rec o =
  { round: rec.round
  , candidateTransaction: o.candidateTransaction
  , executed: o.executed
  , support: o.support
  , threshold: o.threshold
  , signers: concatMap (\s -> if s.candidateTransaction.id == o.candidateTransaction then s.signatures else []) rec.inputs
  }

-- | Every procedure outcome in the run, in round order.
proceduresOf :: StoredRun -> Array Procedure
proceduresOf r = concatMap (\rec -> map (asProcedure rec) (select (event @"ProcedureOutcome") rec.events)) (records r)
