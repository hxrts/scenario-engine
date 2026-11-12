-- | The Simulations page (docs/07_simulator.md §9.1): the static suite's runs, every one compared with every other, and
-- | the selected run's results and balance sheets. Runs are played at build time (`just suite`), never from here.
module Dashboard.Simulations (mkSimulations) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (useLoad)
import Dashboard.BalanceSheet (mkLinks)
import Dashboard.Client as Client
import Dashboard.Comparison (mkComparison, mkRunPage, runTable)
import Dashboard.Components (fetched, loaded, note)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)
import Dashboard.Runs (latestReadable, readableIds)
import Data.Maybe (Maybe, fromMaybe)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Schema.Run (RunClass(..))

mkSimulations :: DataTable -> Component { selected :: Maybe String }
mkSimulations grid = do
  fetch <- fetched
  comparison <- mkComparison grid fetch
  runPage <- mkRunPage grid
  links <- mkLinks
  component "Simulations" \p -> React.do
    list <- useLoad unit (Client.runsOf Simulated)
    pure $ Layout.page
      { title: "Simulations"
      , purpose: "The static suite: one run per preset, and one agency run per seat on its alternative preset, played at build time and reused while nothing that decides them has changed. Every run is compared with every other; select one to open its results and balance sheets."
      , children:
          [ loaded list.value \t ->
              let
                current = fromMaybe "" (p.selected <|> latestReadable t)
              in
                if readableIds t == [] then note "No simulated run yet: play the suite with `just suite`."
                else D.div
                  { className: "flex min-w-0 flex-col gap-section"
                  , children:
                      [ Layout.section
                          { title: "Suite runs"
                          , purpose: "Every simulated run, in the order the suite stored them. Click a run to open it."
                          , children: [ runTable grid t { selected: current, select: \id -> navigate (Simulation id) } ]
                          }
                      , Layout.section
                          { title: "Every run compared"
                          , purpose: "All the simulated runs side by side: prices, net worth and standings on shared axes, the key values, and the agency of each seat."
                          , children: [ comparison { ids: [], colours: readableIds t } ]
                          }
                      , Layout.section
                          { title: "Balance sheets"
                          , purpose: "Each Actor's own page for the selected run, to hand to whoever plays it. A page shows only that Actor's data."
                          , children: [ links current ]
                          }
                      , runPage { id: current }
                      ]
                  }
          ]
      }
