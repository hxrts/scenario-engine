-- | Player actions (docs/07_simulator.md §9.3): per Actor and round, the trades, transforms,
-- | transfers paid, joint candidate Transactions signed and candidate Transactions rejected, counted from the recorded events. This is
-- | the one definition of those counts; the run chart, the totals and the Overview's rejection count
-- | read it. Pure.
module Views.Actions
  ( actionsStat
  , rejectionCount
  , rejections
  , Counts
  , PlayerCounts
  , actionTotals
  , actions
  , ActionsChart
  , ActionsView
  , columns
  ) where

import Prelude

import Laws.Query (droppedCandidateTransactions)
import Laws.Slice (Select, Slice, transformationFills, count, describeSlice, event, exchangeFills, orKind, runSlice, transfers, whereEq, whereNamed)
import Laws.Types (ActorId, Id(..), IntentF(..), CandidateTransaction, CandidateTransactionId)
import Laws.World (RejectReason, RoundRecord)
import Data.Array (any, concatMap, elem, filter, find, foldl, length, sortBy)
import Data.Maybe (Maybe, maybe)
import Views.Common (Field, cellOf, columnsOf, tableOf, Column, Statistic, StoredRun, Table, col, column, players, records)

actionsStat :: Statistic
actionsStat =
  { name: "Actions"
  , question: "What do players do over the run?"
  , definition: "Per player, over the whole run: exchange trades (buys and sells), production and procurement, payments to other players, joint candidate Transactions signed, and proposals rejected."
  , formula: "trades = " <> describeSlice (tradesBy explicitActor) <> "; transforms = " <> describeSlice (transformsBy explicitActor) <> "; transfers = " <> describeSlice (transfersBy explicitActor) <> "; joint candidate Transactions = multi-signer candidate Transactions with a Transfer or Transform that the Actor signed and that were neither rejected nor lapsed; rejected = " <> describeSlice (rejectedOwn (const false)) <> "."
  }

-- | The Actor a formula names: the slices below render their filter's value.
explicitActor :: ActorId
explicitActor = Id "the Actor"

tradesBy :: ActorId -> Slice Int
tradesBy e = count (whereEq @"actor" e exchangeFills)

transformsBy :: ActorId -> Slice Int
transformsBy e = count (whereEq @"by" e transformationFills)

transfersBy :: ActorId -> Slice Int
transfersBy e = count (whereEq @"from" e transfers)

-- | Every Rejected or OrderRemoved event: the rejections the Overview reports.
rejections :: Slice Int
rejections = count rejectedEvents

rejectedOwn :: (CandidateTransactionId -> Boolean) -> Slice Int
rejectedOwn own = count (whereNamed "the candidate Transaction is the Actor's own" (own <<< _.candidateTransaction) rejectedEvents)

-- | One Actor's counts of each kind of action, over a round or a whole run.
type Counts = { trades :: Int, transforms :: Int, transfers :: Int, joints :: Int, rejected :: Int }

-- | The kinds of action counted, in stacking order, with the plain-language label each is shown
-- | under, the short column heading of the per-round table, and the count it takes from a player's
-- | counts. The tables are built from this list, so their headings and cells cannot drift.
actionKinds :: Array { key :: String, label :: String, short :: String, definition :: String, count :: Counts -> Int }
actionKinds =
  [ { key: "trades", label: "Exchange trades (buys and sells)", short: "Trades", definition: "Exchange trades the player made: buys and sells on an Exchange.", count: _.trades }
  , { key: "transforms", label: "Production and procurement", short: "Transforms", definition: "Productions and procurements the player made through a Transformation.", count: _.transforms }
  , { key: "transfers", label: "Payments to other players", short: "Transfers", definition: "Payments the player made to other players, including those inside a joint candidate Transaction.", count: _.transfers }
  , { key: "joint candidate Transactions", label: "joint candidate Transactions signed", short: "joint candidate Transactions signed", definition: "joint candidate Transactions the player signed.", count: _.joints }
  , { key: "rejected", label: "candidate Transactions rejected", short: "candidate Transactions rejected", definition: ("candidate Transactions of the player the engine rejected. Computed as: " <> describeSlice rejections <> "."), count: _.rejected }
  ]

-- | The run chart: one bar per player, ordered by total actions (highest first), each stacked by
-- | kind. `kinds` names the segments and their legend labels; a bar's counts follow that order.
type ActionsChart =
  { title :: String
  , purpose :: String
  , kinds :: Array { key :: String, label :: String }
  , bars :: Array { player :: String, total :: Int, counts :: Array { key :: String, count :: Int } }
  }

-- | The actions view model (`/api/runs/:id/actions`): the chart, the run totals per player, and the
-- | per-round counts.
type ActionsView = { chart :: ActionsChart, totals :: Table, byRound :: Table }

-- | A player's counts over the whole run, and their total.
type PlayerCounts = { player :: ActorId, counts :: Counts, total :: Int }

-- | Every Rejected or OrderRemoved event in a round: the number the Overview reports.
rejectionCount :: RoundRecord -> Int
rejectionCount rec = runSlice rejections rec.events

-- | The candidate Transaction a round's inputs carry, by id.
candidateTransactionOf :: RoundRecord -> CandidateTransactionId -> Maybe CandidateTransaction
candidateTransactionOf rec pid = map _.candidateTransaction (find (\s -> s.candidateTransaction.id == pid) rec.inputs)

-- | A joint candidate Transaction: a candidate Transaction with several signers that moves Resources or goods.
isJoint :: CandidateTransaction -> Boolean
isJoint p = length p.signers >= 2 && any movesStuff p.intents
  where
  movesStuff = case _ of
    Transfer _ -> true
    Transform _ -> true
    _ -> false

countsOf :: RoundRecord -> ActorId -> Counts
countsOf rec e =
  { trades: runSlice (tradesBy e) rec.events
  , transforms: runSlice (transformsBy e) rec.events
  , transfers: runSlice (transfersBy e) rec.events
  , joints: length (filter signedJoint rec.inputs)
  , rejected: runSlice (rejectedOwn ownCandidateTransaction) rec.events
  }
  where
  signedJoint s = elem e s.signatures && isJoint s.candidateTransaction && not (elem s.candidateTransaction.id dropped)
  dropped = droppedCandidateTransactions rec.events
  ownCandidateTransaction pid = maybe false (\p -> p.author == e) (candidateTransactionOf rec pid)

rejectedEvents :: Select { round :: Int, candidateTransaction :: CandidateTransactionId, reason :: RejectReason }
rejectedEvents = orKind (event @"Rejected") (event @"OrderRemoved")

zeroCounts :: Counts
zeroCounts = { trades: 0, transforms: 0, transfers: 0, joints: 0, rejected: 0 }

addCounts :: Counts -> Counts -> Counts
addCounts a b =
  { trades: a.trades + b.trades
  , transforms: a.transforms + b.transforms
  , transfers: a.transfers + b.transfers
  , joints: a.joints + b.joints
  , rejected: a.rejected + b.rejected
  }

-- | The sum of the five kinds: the bar's height and the Total column.
totalOf :: Counts -> Int
totalOf c = foldl (\acc k -> acc + k.count c) 0 actionKinds

-- | Every Actor's counts over the whole run, in Actor order: one read of the run, with no table built.
actionTotals :: StoredRun -> Array PlayerCounts
actionTotals r = map totalsOf (players r.manifest.initial)
  where
  totalsOf e =
    let
      counts = foldl addCounts zeroCounts (map (\rec -> countsOf rec e) (records r))
    in
      { player: e, counts, total: totalOf counts }

labelColumn :: forall r. { label :: String, definition :: String | r } -> Column
labelColumn k = column k.label k.definition

shortColumn :: forall r. { short :: String, definition :: String | r } -> Column
shortColumn k = column k.short k.definition

totalColumn :: Column
totalColumn = column "Total" "The sum of the five kinds. A joint candidate Transaction that is also a payment is counted in both."

-- | A player's counts in one round.
type RoundCounts = { round :: Int, player :: ActorId, counts :: Counts }

-- | The per-round table's columns and cells: one per kind, under its short heading.
byRoundFields :: Array (Field RoundCounts)
byRoundFields = [ cellOf col.round (show <<< _.round), cellOf col.player (show <<< _.player) ] <> map (\k -> cellOf (shortColumn k) (\x -> show (k.count x.counts))) actionKinds

-- | The run totals' columns and cells: one per kind, under its label, then the total.
totalsFields :: Array (Field PlayerCounts)
totalsFields = [ cellOf col.player (show <<< _.player) ] <> map (\k -> cellOf (labelColumn k) (\x -> show (k.count x.counts))) actionKinds <> [ cellOf totalColumn (show <<< _.total) ]

-- | The columns this module defines.
columns :: Array Column
columns = columnsOf byRoundFields <> columnsOf totalsFields

-- | The per-round table (every Actor in every round), the run totals per Actor, and the run chart.
actions :: StoredRun -> ActionsView
actions r = { chart, totals, byRound }
  where
  ps = players r.manifest.initial
  totalsRows = actionTotals r
  byRound = tableOf "Actions by round" byRoundFields (concatMap (\rec -> map (\e -> { round: rec.round, player: e, counts: countsOf rec e }) ps) (records r))
  totals = tableOf "Count of actions over the whole run, by player. Total is the sum of the five kinds, so a joint candidate Transaction that is also a payment is counted in both." totalsFields totalsRows
  chart =
    { title: "Actions over the run, by player"
    , purpose: "A bar is one player. Each coloured segment is that player's count of actions of one kind over the whole run; the bar's height is the total of the segments. Players are ordered by total, highest first."
    , kinds: map (\k -> { key: k.key, label: k.label }) actionKinds
    , bars: sortBy (\a b -> compare b.total a.total)
        ( map
            ( \x ->
                { player: show x.player
                , total: x.total
                , counts: map (\k -> { key: k.key, count: k.count x.counts }) actionKinds
                }
            )
            totalsRows
        )
    }
