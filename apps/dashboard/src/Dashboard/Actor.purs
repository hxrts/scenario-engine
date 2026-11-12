-- | The Actor balance sheet section (docs/07_simulator.md §9.3): a selector of the run's Actors and
-- | the chosen Actor's holdings and net worth, round by round, as plots. The Actors, the seat flags and
-- | the default Actor come from the analysis (`GET /api/runs/:id/Actors`); the history comes from
-- | `GET /api/runs/:id/Actor/:Actor`. This module only chooses which Actor to show and where.
module Dashboard.Actor (mkActor) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client (actors, actor)
import Dashboard.Components (loaded, metricChart, selectInput, stale, status)
import Dashboard.Layout as Layout
import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Views.Actor (ActorList, History)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useState', (/\))
import React.Basic.Hooks as React

-- | The Actor shown: the chosen one when the run has it, else the run's default Actor. Nothing when the
-- | run has no Actors.
shown :: Maybe String -> ActorList -> Maybe String
shown chosen l = case chosen of
  Just c | elem c (map _.id l.actors) -> Just c
  _ -> if l.default == "" then Nothing else Just l.default

-- | The section: a selector of the run's Actors, then the chosen Actor's plots.
mkActor :: Component { id :: String, version :: Int }
mkActor = do
  history <- mkHistory
  component "ActorSection" \p -> React.do
    list <- useLoad (Tuple p.id p.version) (actors p.id)
    chosen /\ setChosen <- useState' (Nothing :: Maybe String)
    pure $ Layout.section
      { title: "Actor balance sheet"
      , purpose: "One Actor's balance sheet round by round: its holding of each Resource and its net worth, from the start (round 0) to the final round."
      , children: [ body history p list chosen setChosen ]
      }
  where
  body history p list chosen setChosen = loaded list.value \l -> case shown chosen l of
    Nothing -> status (Just "This run has no Actors.")
    Just e -> D.div
      { className: "flex min-w-0 flex-col gap-stack"
      , children:
          [ selector l e setChosen
          , history { id: p.id, version: p.version, actor: e }
          , Layout.purpose "Each line in the holdings plot is one Resource's holding after a round, in that Resource's own unit (Money in M, the others as the composition defines them). Net worth is Money plus each held Resource valued at its Exchange's marginal price, the same definition as Standings."
          ]
      }
  selector l e setChosen = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Actor"
        , selectInput { value: e, options: map _.id l.actors, onChange: setChosen }
        ]
    }

-- | The history of one Actor: its plots, stale while the next Actor's history loads.
mkHistory :: Component { id :: String, version :: Int, actor :: String }
mkHistory = component "ActorHistory" \p -> React.do
  h <- useLoad { id: p.id, version: p.version, actor: p.actor } (actor p.id p.actor)
  pure $ stale h.busy (loaded h.value plots)

plots :: History -> JSX
plots h = Layout.grid
  [ Layout.panel
      { span: 12
      , title: "Holdings by Resource"
      , purpose: "The holding of each Resource the Actor holds, by round."
      , children: [ metricChart h.chart ]
      }
  , Layout.panel
      { span: 12
      , title: "Net worth"
      , purpose: "The Actor's net worth in " <> h.netWorth.unit <> " by round, on its own scale."
      , children: [ metricChart h.netWorth ]
      }
  ]
