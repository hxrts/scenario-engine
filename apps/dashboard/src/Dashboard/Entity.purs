-- | The entity balance sheet section (docs/06_simulator.md §9.2): a selector of the run's Entities and
-- | the chosen Entity's holdings and net worth, round by round, as plots. The Entities come from the
-- | balances view at round 0 (its Entity column, every Entity the run has). The history comes from
-- | `/api/runs/:id/entity/:entity`. This module only chooses which Entity to show and where.
module Dashboard.Entity (mkEntity) where

import Prelude

import Dashboard.Api (encode, runPath, useApi)
import Dashboard.Components (as, fetched, metricChart, status)
import Dashboard.Layout as Layout
import Data.Array (elem, find, findIndex, head, index, nub)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Harness.Views.Common (Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React

-- | The charts of an Entity's history (`/api/runs/:id/entity/:entity`); the table is for the API only.
type History = { chart :: Chart, netWorth :: Chart }

-- | A cell of the balances table by column name, or "" when the table has no such column.
cell :: Table -> String -> Array String -> String
cell t name row = fromMaybe "" (findIndex (_ == name) t.columns >>= \i -> index row i)

-- | The Entities the run has, in the balances table's order, each once.
entityChoices :: Table -> Array String
entityChoices t = nub (map (cell t "Entity") t.rows)

-- | The Entity shown when none is chosen: the first Actor (a player or an NPC, not an Aggregate) the run
-- | has. The balances view does not say which Actors are seats, so the first Actor stands for the first seat.
defaultEntity :: Table -> Maybe String
defaultEntity t = case find (\row -> cell t "Kind" row == "Actor") t.rows of
  Just row -> Just (cell t "Entity" row)
  Nothing -> head (entityChoices t)

-- | The section: a selector of the run's Entities, then the chosen Entity's plots. The default is the
-- | first Actor; a choice that the run does not have falls back to the default.
mkEntity :: Component { id :: String }
mkEntity = do
  fetch <- fetched
  component "EntitySection" \p -> React.do
    chosen /\ setChosen <- useState' (Nothing :: Maybe String)
    list <- useApi p.id (runPath p.id <> "/balances?round=0")
    pure $ Layout.section
      { title: "Entity balance sheet"
      , purpose: "One Entity's balance sheet round by round: its holding of each Resource and its net worth, from the start (round 0) to the final round."
      , children: [ body p.id fetch chosen setChosen list ]
      }
  where
  body id fetch chosen setChosen = case _ of
    Nothing -> status Nothing
    Just (Left e) -> status (Just e)
    Just (Right t) -> case shown chosen t of
      Nothing -> status (Just "This run has no Entities.")
      Just e -> D.div
        { className: "flex min-w-0 flex-col gap-stack"
        , children:
            [ selector t e setChosen
            , fetch { path: runPath id <> "/entity/" <> encode e, render: as \(h :: History) -> plots h }
            , D.p { className: "text-sm text-muted-foreground", children: [ D.text "Each line in the holdings plot is one Resource's holding after a round, in that Resource's own unit (Money in M, the others as the Gameboard defines them). Net worth is Money plus each held Resource valued at its Market's marginal price, the same definition as Standings." ] }
            ]
        }
  shown chosen t = case chosen of
    Just c | elem c (entityChoices t) -> Just c
    _ -> defaultEntity t
  selector t e setChosen = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Entity"
        , D.select
            { value: e
            , onChange: handler targetValue setChosen
            , children: entityChoices t <#> \c -> D.option { key: c, value: c, children: [ D.text c ] }
            }
        ]
    }
  plots h = Layout.grid
    [ Layout.panel
        { span: 7
        , title: "Holdings by Resource"
        , purpose: "The holding of each Resource the Entity holds, by round."
        , children: [ metricChart h.chart ]
        }
    , Layout.panel
        { span: 5
        , title: "Net worth"
        , purpose: "The Entity's net worth in M by round, on its own scale."
        , children: [ metricChart h.netWorth ]
        }
    ]
