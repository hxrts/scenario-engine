-- | One Entity's balance sheet over the run, round by round (docs/06_simulator.md §8, §9.2). The table
-- | has a row per Resource the Entity holds or a Market quotes at any round, and a net-worth row, with
-- | a column per round 0..final. The holdings chart has one line per Resource the Entity holds at some
-- | round, in that Resource's own unit. The net-worth chart is a separate chart, because net worth is in
-- | M and would share no scale with the holdings. Net worth is `Analysis.Views.Standings.netWorth`, the
-- | one definition. Pure.
module Analysis.Views.Entity
  ( entityHistory
  , entityIds
  ) where

import Prelude

import Core.Num (Kind(..))
import Core.Num as Q
import Core.Query (holding)
import Core.Types (EntityId, ResourceId)
import Core.World (WorldState)
import Data.Array (any, concatMap, cons, filter, fromFoldable, length, nub, range, sort)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Analysis.Views.Common (Chart, StoredRun, Table, exact, joinComma, quantity, series)
import Analysis.Views.Standings (netWorth)

-- | Every Entity in the run's initial World State, in Entity order: the names `entityHistory` accepts.
entityIds :: StoredRun -> Array EntityId
entityIds r = fromFoldable (Map.keys r.manifest.initial.entities)

-- | An Entity's balance sheet after every round, from round 0 to the final round. An Entity the run
-- | does not have is an error that lists the valid ones.
entityHistory :: StoredRun -> EntityId -> Either String { table :: Table, chart :: Chart, netWorth :: Chart }
entityHistory r e
  | not (Map.member e r.manifest.initial.entities) =
      Left ("unknown entity " <> show e <> "; the entities are " <> joinComma (map show (entityIds r)))
  | otherwise =
      Right
        { table:
            { title: "Balance sheet of " <> show e <> " by round"
            , columns: [ "Resource" ] <> map (\i -> "Round " <> show i) rounds
            , rows: map resourceRow resources <> [ [ "Net worth (M)" ] <> map (\w -> quantity (netWorth w e)) worlds ]
            }
        , chart:
            { title: "Holdings of " <> show e
            , unit: "each Resource in its own unit"
            , kind: Quantity
            , series: series r "holdings" (\w -> map (\res -> Tuple (show res) (holding w e res)) held) exact
            }
        , netWorth:
            { title: "Net worth of " <> show e
            , unit: "M"
            , kind: Quantity
            , series: series r "net worth" (\w -> [ Tuple "Net worth" (netWorth w e) ]) exact
            }
        }
      where
      -- the World States: round 0 is the initial one, then one per completed round (the index is the round)
      worlds :: Array WorldState
      worlds = [ r.manifest.initial ] <> r.snapshots

      rounds :: Array Int
      rounds = range 0 (length r.snapshots)

      -- the Resources the Entity holds at some round (a non-zero holding), in Resource order
      held :: Array ResourceId
      held = sort (nub (filter (\res -> any (\w -> holding w e res /= Q.zero) worlds) (concatMap accountsOf worlds)))

      -- the Resources held or quoted by a Market at some round: the table's rows
      resources :: Array ResourceId
      resources = sort (nub (held <> concatMap (\w -> concatMap (\m -> [ m.base, m.quote ]) (fromFoldable (Map.values w.markets))) worlds))

      accountsOf :: WorldState -> Array ResourceId
      accountsOf w = maybe [] (\en -> fromFoldable (Map.keys en.accounts)) (Map.lookup e w.entities)
      resourceRow res = cons (show res) (map (\w -> quantity (holding w e res)) worlds)
