-- | The entity balance sheet section (docs/06_simulator.md §9.2): a selector of the run's Entities and
-- | the chosen Entity's holdings and net worth, round by round, as plots. The Entities, the seat flags and
-- | the default Entity come from the analysis (`GET /api/runs/:id/entities`); the history comes from
-- | `GET /api/runs/:id/entity/:entity`. This module only chooses which Entity to show and where.
module Dashboard.Entity (mkEntity) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client (History, entities, entity)
import Dashboard.Components (metricChart, stale, status)
import Dashboard.Layout as Layout
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Analysis.Views.Entity (EntityList)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, component, useState', (/\))
import React.Basic.Hooks as React

-- | The Entity shown: the chosen one when the run has it, else the run's default Entity. Nothing when the
-- | run has no Entities.
shown :: Maybe String -> EntityList -> Maybe String
shown chosen l = case chosen of
  Just c | elem c (map _.id l.entities) -> Just c
  _ -> if l.default == "" then Nothing else Just l.default

-- | The section: a selector of the run's Entities, then the chosen Entity's plots.
mkEntity :: Component { id :: String }
mkEntity = do
  history <- mkHistory
  component "EntitySection" \p -> React.do
    list <- useLoad p.id (entities p.id)
    chosen /\ setChosen <- useState' (Nothing :: Maybe String)
    pure $ Layout.section
      { title: "Entity balance sheet"
      , purpose: "One Entity's balance sheet round by round: its holding of each Resource and its net worth, from the start (round 0) to the final round."
      , children: [ body history p.id list chosen setChosen ]
      }
  where
  body history id list chosen setChosen = case list.value of
    Nothing -> status Nothing
    Just (Left e) -> status (Just e)
    Just (Right l) -> case shown chosen l of
      Nothing -> status (Just "This run has no Entities.")
      Just e -> D.div
        { className: "flex min-w-0 flex-col gap-stack"
        , children:
            [ selector l e setChosen
            , history { id, entity: e }
            , D.p { className: "text-sm text-muted-foreground", children: [ D.text "Each line in the holdings plot is one Resource's holding after a round, in that Resource's own unit (Money in M, the others as the Gameboard defines them). Net worth is Money plus each held Resource valued at its Market's marginal price, the same definition as Standings." ] }
            ]
        }
  selector l e setChosen = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Entity"
        , D.select
            { value: e
            , onChange: handler targetValue setChosen
            , children: l.entities <#> \c -> D.option { key: c.id, value: c.id, children: [ D.text c.id ] }
            }
        ]
    }

-- | The history of one Entity: its plots, stale while the next Entity's history loads.
mkHistory :: Component { id :: String, entity :: String }
mkHistory = component "EntityHistory" \p -> React.do
  h <- useLoad (Tuple p.id p.entity) (entity p.id p.entity)
  pure $ stale h.busy case h.value of
    Nothing -> status Nothing
    Just (Left e) -> status (Just e)
    Just (Right hist) -> plots hist

plots :: History -> JSX
plots h = Layout.grid
  [ Layout.panel
      { span: 12
      , title: "Holdings by Resource"
      , purpose: "The holding of each Resource the Entity holds, by round."
      , children: [ metricChart h.chart ]
      }
  , Layout.panel
      { span: 12
      , title: "Net worth"
      , purpose: "The Entity's net worth in " <> h.netWorth.unit <> " by round, on its own scale."
      , children: [ metricChart h.netWorth ]
      }
  ]
