-- | Player actions (docs/06_simulator.md §9.2): per Actor and round, the trades, transforms,
-- | transfers paid, deals signed and Proposals rejected, counted from the recorded events. This is
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

import Core.Query (droppedProposals)
import Core.Slice (Select, Slice, count, describeSlice, event, orKind, runSlice, whereEq, whereNamed)
import Core.Types (EntityId, Id(..), IntentF(..), Proposal, ProposalId)
import Core.World (RejectReason, RoundRecord)
import Data.Array (any, concatMap, elem, filter, find, foldl, length, sortBy)
import Data.Maybe (Maybe, maybe)
import Views.Common (Column, Statistic, StoredRun, Table, col, column, names, players, records)

actionsStat :: Statistic
actionsStat =
  { name: "Actions"
  , question: "What do players do over the run?"
  , definition: "Per player, over the whole run: market trades (buys and sells), production and procurement, payments to other players, deals signed, and proposals rejected."
  , formula: "trades = " <> describeSlice (tradesBy actor) <> "; transforms = " <> describeSlice (transformsBy actor) <> "; transfers = " <> describeSlice (transfersBy actor) <> "; deals = multi-signer Proposals with a Transfer or Transform that the Actor signed and that were neither rejected nor lapsed; rejected = " <> describeSlice (rejectedOwn (const false)) <> "."
  }

-- | The Actor a formula names: the slices below render their filter's value.
actor :: EntityId
actor = Id "the Actor"

tradesBy :: EntityId -> Slice Int
tradesBy e = count (whereEq @"entity" e (event @"Traded"))

transformsBy :: EntityId -> Slice Int
transformsBy e = count (whereEq @"by" e (event @"Transformed"))

transfersBy :: EntityId -> Slice Int
transfersBy e = count (whereEq @"from" e (event @"Transferred"))

-- | Every Rejected or OrderRemoved event: the rejections the Overview reports.
rejections :: Slice Int
rejections = count rejectedEvents

rejectedOwn :: (ProposalId -> Boolean) -> Slice Int
rejectedOwn own = count (whereNamed "the Proposal is the Actor's own" (own <<< _.proposal) rejectedEvents)

-- | One Actor's counts of each kind of action, over a round or a whole run.
type Counts = { trades :: Int, transforms :: Int, transfers :: Int, deals :: Int, rejected :: Int }

-- | The kinds of action counted, in stacking order, with the plain-language label each is shown
-- | under, the short column heading of the per-round table, and the count it takes from a player's
-- | counts. The tables are built from this list, so their headings and cells cannot drift.
actionKinds :: Array { key :: String, label :: String, short :: String, definition :: String, count :: Counts -> Int }
actionKinds =
  [ { key: "trades", label: "Market trades (buys and sells)", short: "Trades", definition: "Market trades the player made: buys and sells on a Market.", count: _.trades }
  , { key: "transforms", label: "Production and procurement", short: "Transforms", definition: "Productions and procurements the player made through a Transformation.", count: _.transforms }
  , { key: "transfers", label: "Payments to other players", short: "Transfers", definition: "Payments the player made to other players, including those inside a deal.", count: _.transfers }
  , { key: "deals", label: "Deals signed", short: "Deals signed", definition: "Deals the player signed.", count: _.deals }
  , { key: "rejected", label: "Proposals rejected", short: "Proposals rejected", definition: ("Proposals of the player the engine rejected. Computed as: " <> describeSlice rejections <> "."), count: _.rejected }
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
type PlayerCounts = { player :: EntityId, counts :: Counts, total :: Int }

-- | Every Rejected or OrderRemoved event in a round: the number the Overview reports.
rejectionCount :: RoundRecord -> Int
rejectionCount rec = runSlice rejections rec.events

-- | The Proposal a round's inputs carry, by id.
proposalOf :: RoundRecord -> ProposalId -> Maybe Proposal
proposalOf rec pid = map _.proposal (find (\s -> s.proposal.id == pid) rec.inputs)

-- | A deal: a Proposal with several signers that moves Money, Resources or goods.
isDeal :: Proposal -> Boolean
isDeal p = length p.signers >= 2 && any movesStuff p.intents
  where
  movesStuff = case _ of
    Transfer _ -> true
    Transform _ -> true
    _ -> false

countsOf :: RoundRecord -> EntityId -> Counts
countsOf rec e =
  { trades: runSlice (tradesBy e) rec.events
  , transforms: runSlice (transformsBy e) rec.events
  , transfers: runSlice (transfersBy e) rec.events
  , deals: length (filter signedDeal rec.inputs)
  , rejected: runSlice (rejectedOwn ownProposal) rec.events
  }
  where
  signedDeal s = elem e s.signatures && isDeal s.proposal && not (elem s.proposal.id dropped)
  dropped = droppedProposals rec.events
  ownProposal pid = maybe false (\p -> p.author == e) (proposalOf rec pid)

rejectedEvents :: Select { round :: Int, proposal :: ProposalId, reason :: RejectReason }
rejectedEvents = orKind (event @"Rejected") (event @"OrderRemoved")

zeroCounts :: Counts
zeroCounts = { trades: 0, transforms: 0, transfers: 0, deals: 0, rejected: 0 }

addCounts :: Counts -> Counts -> Counts
addCounts a b =
  { trades: a.trades + b.trades
  , transforms: a.transforms + b.transforms
  , transfers: a.transfers + b.transfers
  , deals: a.deals + b.deals
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
totalColumn = column "Total" "The sum of the five kinds. A deal that is also a payment is counted in both."

-- | The columns this module defines.
columns :: Array Column
columns = map labelColumn actionKinds <> map shortColumn actionKinds <> [ totalColumn ]

-- | The per-round table (every Actor in every round), the run totals per Actor, and the run chart.
actions :: StoredRun -> ActionsView
actions r = { chart, totals, byRound }
  where
  ps = players r.manifest.initial
  totalsRows = actionTotals r
  byRound =
    { title: "Actions by round"
    , columns: names ([ col.round, col.player ] <> map shortColumn actionKinds)
    , rows: concatMap (\rec -> map (\e -> [ show rec.round, show e ] <> cellsOf (countsOf rec e)) ps) (records r)
    }
  totals =
    { title: "Count of actions over the whole run, by player. Total is the sum of the five kinds, so a deal that is also a payment is counted in both."
    , columns: names ([ col.player ] <> map labelColumn actionKinds <> [ totalColumn ])
    , rows: map (\x -> [ show x.player ] <> cellsOf x.counts <> [ show x.total ]) totalsRows
    }
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
  cellsOf c = map (\k -> show (k.count c)) actionKinds
