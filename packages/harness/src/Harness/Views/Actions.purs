-- | Player actions (docs/06_simulator.md §9.2): per Actor and round, the trades, transforms,
-- | transfers paid, deals signed and Proposals rejected, counted from the recorded events. This is
-- | the one definition of those counts; the run chart, the totals and the Overview's rejection count
-- | read it. Pure.
module Harness.Views.Actions
  ( actionsStat
  , actionKinds
  , rejectionCount
  , actions
  , ActionsChart
  , ActionsView
  ) where

import Prelude

import Core.Types (EntityId, Intent(..), Proposal, ProposalId)
import Core.World (Event(..), RoundRecord)
import Data.Array (any, concatMap, elem, filter, find, foldl, length, sortBy)
import Data.Maybe (Maybe, maybe)
import Harness.Views.Common (Statistic, StoredRun, Table, players)

actionsStat :: Statistic
actionsStat =
  { name: "Actions"
  , question: "What do players do over the run?"
  , definition: "Per player, over the whole run: market trades (buys and sells), production and procurement, payments to other players, deals signed, and proposals rejected."
  , formula: "trades = Traded events with the Actor; transforms = Transformed events by the Actor; transfers = Transferred events the Actor pays (from); deals = multi-signer Proposals with a Transfer or Transform that the Actor signed and that were neither rejected nor lapsed; rejected = Rejected events on the Actor's own Proposals."
  }

-- | One Actor's counts in one round.
type Counts = { trades :: Int, transforms :: Int, transfers :: Int, deals :: Int, rejected :: Int }

-- | The kinds of action counted, in stacking order, with the plain-language label each is shown
-- | under and the count it takes from a player's counts.
actionKinds :: Array { key :: String, label :: String, count :: Counts -> Int }
actionKinds =
  [ { key: "trades", label: "Market trades (buys and sells)", count: _.trades }
  , { key: "transforms", label: "Production and procurement", count: _.transforms }
  , { key: "transfers", label: "Payments to other players", count: _.transfers }
  , { key: "deals", label: "Deals signed", count: _.deals }
  , { key: "rejected", label: "Proposals rejected", count: _.rejected }
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

-- | Every Rejected event in a round: the number the Overview reports.
rejectionCount :: RoundRecord -> Int
rejectionCount rec = length (filter isRejected rec.events)
  where
  isRejected = case _ of
    Rejected _ -> true
    _ -> false

-- | The Proposal a round's inputs carry, by id.
proposalOf :: RoundRecord -> ProposalId -> Maybe Proposal
proposalOf rec pid = map _.proposal (find (\s -> s.proposal.id == pid) rec.inputs)

-- | Whether a Proposal was rejected or lapsed in the round.
settledOff :: RoundRecord -> ProposalId -> Boolean
settledOff rec pid = any
  ( \ev -> case ev of
      Rejected x -> x.proposal == pid
      Lapsed x -> x.proposal == pid
      _ -> false
  )
  rec.events

-- | A deal: a Proposal with several signers that moves Money, Resources or goods.
isDeal :: Proposal -> Boolean
isDeal p = length p.signers >= 2 && any movesStuff p.intents
  where
  movesStuff = case _ of
    Transfer _ -> true
    Transform _ -> true
    _ -> false

tradedBy :: EntityId -> Event -> Boolean
tradedBy e = case _ of
  Traded t -> t.entity == e
  _ -> false

transformedBy :: EntityId -> Event -> Boolean
transformedBy e = case _ of
  Transformed t -> t.by == e
  _ -> false

transferredBy :: EntityId -> Event -> Boolean
transferredBy e = case _ of
  Transferred t -> t.from == e
  _ -> false

countsOf :: RoundRecord -> EntityId -> Counts
countsOf rec e =
  { trades: count (tradedBy e)
  , transforms: count (transformedBy e)
  , transfers: count (transferredBy e)
  , deals: length (filter signedDeal rec.inputs)
  , rejected: count rejectedOwn
  }
  where
  count f = length (filter f rec.events)
  signedDeal s = elem e s.signatures && isDeal s.proposal && not (settledOff rec s.proposal.id)
  rejectedOwn = case _ of
    Rejected x -> maybe false (\p -> p.author == e) (proposalOf rec x.proposal)
    _ -> false

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

-- | The per-round table (every Actor in every round), the run totals per Actor, and the run chart.
actions :: StoredRun -> ActionsView
actions r = { chart, totals, byRound }
  where
  ps = players r.manifest.initial
  -- the Actors' counts in each round, as (round, Actor, counts)
  perRound = concatMap (\rec -> map (\e -> { round: rec.round, actor: e, counts: countsOf rec e }) ps) r.records
  byRound =
    { title: "Actions by round"
    , columns: [ "Round", "Player", "Trades", "Transforms", "Transfers", "Deals signed", "Proposals rejected" ]
    , rows: map (\x -> [ show x.round, show x.actor, show x.counts.trades, show x.counts.transforms, show x.counts.transfers, show x.counts.deals, show x.counts.rejected ]) perRound
    }
  summed = map (\e -> { player: show e, counts: sumFor e }) ps
  totals =
    { title: "Count of actions over the whole run, by player. Total is the sum of the five kinds, so a deal that is also a payment is counted in both."
    , columns: [ "Player" ] <> map _.label actionKinds <> [ "Total" ]
    , rows: map (\x -> [ x.player ] <> map (\k -> show (k.count x.counts)) actionKinds <> [ show (totalOf x.counts) ]) summed
    }
  chart =
    { title: "Actions over the run, by player"
    , purpose: "A bar is one player. Each coloured segment is that player's count of actions of one kind over the whole run; the bar's height is the total of the segments. Players are ordered by total, highest first."
    , kinds: map (\k -> { key: k.key, label: k.label }) actionKinds
    , bars: sortBy (\a b -> compare b.total a.total)
        ( map
            ( \x ->
                { player: x.player
                , total: totalOf x.counts
                , counts: map (\k -> { key: k.key, count: k.count x.counts }) actionKinds
                }
            )
            summed
        )
    }
  sumFor e = foldl addCounts zeroCounts (map _.counts (filter (\x -> x.actor == e) perRound))
