-- | One run's results (docs/07_simulator.md §9.3), a presentational view of the overview the run page fetched. A compact
-- | header names the run from the overview's facts; then one section per economic object. Every number comes from the
-- | read API; this module chooses what to show and where.
module Dashboard.Results (mkRunResults, Props) where

import Prelude

import Dashboard.Client (runView)
import Dashboard.Api (Loaded)
import Dashboard.Components (blocksOf, fetched, loaded, mkFigure, note, stale, tableOf)
import Dashboard.DataTable (DataTable)

import Dashboard.Glossary (Lookup, factList, glossaryContext)
import Dashboard.Layout as Layout
import Data.Array (elem, filter, find, null)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Schema.View (viewName)

import Views.Common (Table)
import Views.Keys (ViewKey)
import Views.Keys as Keys

import Views.Overview (Fact, RunOverview)

import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useContext)
import React.Basic.Hooks as React

-- | The run whose results are shown, its version (bumped by an action on it, so every section refetches),
-- | and its overview as the run page loaded it (dimmed while the next run's loads). Comparison is chosen
-- | on the Compare page, not here.
type Props = { id :: String, version :: Int, overview :: Loaded (Either String RunOverview) }

-- | A table with its title (the server's title for it) above it.
titled :: DataTable -> Table -> JSX
titled grid t = Layout.captioned t.title [ tableOf grid t ]

-- | The compact header: the run's id, then the facts that tell one run from another (its name, its preset when that
-- | is not its name, its composition, status, and rounds completed out of its target), from the same overview, so a
-- | run switch never shows a new name over the old facts. An empty fact says what it means, with the reason on hover.
header :: Lookup -> String -> Array Fact -> JSX
header glossary id facts = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.h3 { className: "text-base font-semibold", children: [ D.text ("Run " <> id) ] }
      , factList glossary (filter shown facts)
      ]
  }
  where
  name = maybe "" _.value (find (\f -> f.label == "Name") facts)
  shown f = elem f.label [ "Name", "Composition", "Status", "Rounds completed" ] || (f.label == "Preset" && f.value /= name && f.value /= "")

-- | The results of one run. A run whose overview cannot be read shows its error; the previous run's
-- | results stay on screen, dimmed, until the selected run's overview arrives.
mkRunResults :: DataTable -> Component Props
mkRunResults grid = do
  body <- mkBody grid
  component "RunResults" \p -> React.do
    glossary <- useContext glossaryContext
    pure $ stale p.overview.busy $ loaded p.overview.value \o -> D.div
      { className: "flex min-w-0 flex-col gap-section"
      , children: [ header glossary o.id o.facts, body { id: p.id, version: p.version, rounds: o.rounds } ]
      }

type BodyProps = { id :: String, version :: Int, rounds :: Int }

-- | The sections: the seats' scorecards, then the two economic objects. Each country's regime is read as whether it
-- | reproduces itself, the countries side by side: one chart per dimension of the ratio that decides it, against the
-- | level it is read against. The clearing between them is the circuit, a step at a time. Every chart has the table of
-- | the numbers behind it behind a switch.
mkBody :: DataTable -> Component BodyProps
mkBody grid = do
  fetch <- fetched
  figure <- mkFigure grid
  component "RunResultsBody" \b -> React.do
    let
      keyed path = b.id <> "/" <> path <> "@" <> show b.version

      view :: forall a. ViewKey a -> (a -> JSX) -> JSX
      view key render = fetch { key: keyed (viewName key.view), load: map render <$> runView key b.id Nothing }

      -- one dimension of every country's regime, the countries side by side
      across rs gauge = Layout.columns (map (\r -> figure (gauge r)) rs)
      step s = figure { chart: s.chart, table: s.table, threshold: Nothing }
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-section"
      , children:
          [ Layout.subsection
              { title: "Seat scorecards"
              , purpose: "Each seat's criteria: what was measured, against its bound."
              , children: [ view (Keys.viewKey @"Objectives") (blocksOf grid) ]
              }
          , Layout.subsection
              { title: "National regimes"
              , purpose: "Is each country reproducing itself? Capacity runs only as far as competence (below the line, it is short of one); a substitute gap above the line leaves its own industry idle; absorption is the share of its output its households buy; debt service above the line cannot be paid from sales."
              , children:
                  [ view (Keys.viewKey @"Regimes") \rs ->
                      let
                        held = filter (\r -> not (null r.capability.table.rows)) rs
                      in
                        if null held then note "No country in this run holds productive capacity."
                        else D.div
                          { className: "flex min-w-0 flex-col gap-stack"
                          , children: [ across held _.capability, across held _.substitution, across held _.demand, across held _.finance ]
                          }
                  ]
              }
          , Layout.subsection
              { title: "International clearing"
              , purpose: "The circuit between the countries: what each sells abroad less what it buys, what it is lent, the position that leaves it with, and the exchange rate; then the Rules at the borders."
              , children:
                  [ view (Keys.viewKey @"Clearing") \c -> Layout.tiles [ step c.trade, step c.lending, step c.position ]
                  , Layout.tiles
                      [ view (Keys.viewKey @"Finance") (\f -> figure { chart: f.currentChart, table: f.current, threshold: Nothing })
                      , view (Keys.viewKey @"Clearing") (\c -> step c.rate)
                      ]
                  , view (Keys.viewKey @"Clearing") (\c -> if null c.rules.rows then note "No Rule stands at a border in this run." else titled grid c.rules)
                  ]
              }
          ]
      }
