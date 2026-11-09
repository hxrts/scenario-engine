-- | One Actor's balance sheet over the run, round by round (docs/06_simulator.md §8, §9.2). The table
-- | has a row per Resource the Actor holds or an Exchange quotes at any round, and a net-worth row, with
-- | a column per round 0..final. The holdings chart has one line per Resource the Actor holds at some
-- | round, in that Resource's own unit. The net-worth chart is a separate chart, because net worth is in
-- | M and would share no scale with the holdings. Net worth is `Views.Standings.netWorth`, the
-- | one definition. Pure.
module Views.Actor
  ( actorHistory
  , History
  , ActorList
  , actorList
  , ActorChoice
  ) where

import Prelude

import Control.Alt ((<|>))
import Core.Num (Kind(..))
import Core.Map as CoreMap
import Core.Num as Q
import Laws.Query (accountsOf, holding, isAggregate)
import Laws.Types (ActorId, ResourceId)
import Data.String (joinWith)
import Data.Array (any, concatMap, cons, filter, find, fromFoldable, head, nub, range, sort)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Views.Common (Chart, StoredRun, Table, col, completed, listedHolding, names, quantity, series, unitOf, worlds)
import Views.Standings (netWorth)

-- | One Actor's history (`GET /api/runs/:id/Actor/:Actor`): its balance sheet table, and its holdings
-- | and net worth as charts.
type History = { table :: Table, chart :: Chart, netWorth :: Chart }

-- | Every Actor in the run's initial World State, in Actor order: the names `entityHistory` accepts.
actorIds :: StoredRun -> Array ActorId
actorIds r = fromFoldable (Map.keys r.manifest.initial.actors)

-- | An Actor the run has, and whether it is a seat (an Actor, not an Aggregate).
type ActorChoice = { id :: String, seat :: Boolean }

-- | The Actors of a run in Actor order, and the one shown when none is chosen: the first seat
-- | (or the first Actor, when the run has no seat). `default` is "" only for a run with no Actors.
type ActorList = { default :: String, actors :: Array ActorChoice }

actorList :: StoredRun -> ActorList
actorList r =
  { default: fromMaybe "" (map _.id (find _.seat actors) <|> map _.id (head actors))
  , actors
  }
  where
  actors :: Array ActorChoice
  actors = map choice (Map.toUnfoldable r.manifest.initial.actors)
  choice (Tuple e ent) = { id: show e, seat: not (isAggregate ent) }

-- | An Actor's balance sheet after every round, from round 0 to the final round. An Actor the run
-- | does not have is an error that lists the valid ones.
actorHistory :: StoredRun -> ActorId -> Either String History
actorHistory r e
  | not (Map.member e r.manifest.initial.actors) =
      Left ("unknown Actor " <> show e <> "; the Actors are " <> joinWith ", " (map show (actorIds r)))
  | otherwise =
      Right
        { table:
            { title: "Balance sheet of " <> show e <> " by round"
            , columns: names [ col.resource ] <> map (\i -> "Round " <> show i) rounds
            , rows: map resourceRow resources <> [ [ "Net worth" ] <> map (\w -> quantity (netWorth w e)) states ]
            }
        , chart:
            { title: "Holdings of " <> show e
            , unit: "each Resource in its own unit"
            , kind: Quantity
            , series: series r (\w -> map (\res -> Tuple (show res) (holding w e res)) held)
            }
        , netWorth:
            { title: "Net worth of " <> show e
            , unit: unitOf r r.manifest.initial.numeraire
            , kind: Quantity
            , series: series r (\w -> [ Tuple "Net worth" (netWorth w e) ])
            }
        }
      where
      states = worlds r

      rounds :: Array Int
      rounds = range 0 (completed r)

      -- the Resources the Actor holds at some round (a non-zero holding), in Resource order
      held :: Array ResourceId
      held = sort (nub (filter (\res -> listedHolding r.manifest.initial res && any (\w -> holding w e res /= Q.zero) states) (concatMap (\w -> fromFoldable (Map.keys (accountsOf w e))) states)))

      -- the Resources held or quoted by an Exchange at some round: the table's rows
      resources :: Array ResourceId
      resources = sort (nub (held <> concatMap (\w -> concatMap (\m -> [ m.base, m.quote ]) (CoreMap.values w.exchanges)) states))

      resourceRow res = cons (show res) (map (\w -> quantity (holding w e res)) states)
