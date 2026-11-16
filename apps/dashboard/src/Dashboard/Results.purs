-- | One run's results (docs/07_simulator.md §9.3), a presentational view of the overview the run page fetched. A compact
-- | header names the run from the overview's facts; then one section per economic object. Every number comes from the
-- | read API; this module chooses what to show and where.
module Dashboard.Results (mkRunResults, Props) where

import Prelude

import Dashboard.Client (runView)
import Dashboard.Api (Loaded)
import Dashboard.Components (Size(..), blocksOf, fetched, loaded, metricChartAt, mkFigure, note, selectInput, stale, tableOf, withoutColumns)
import Dashboard.DataTable (DataTable)

import Dashboard.Glossary (Lookup, factList, glossaryContext)
import Dashboard.Layout as Layout
import Data.Array (elem, filter, find, head, index, null, range)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Schema.View (viewName)

import Views.Common (Table)
import Views.Keys (ViewKey)
import Views.Keys as Keys

import Views.Overview (Fact, RunOverview)

import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useContext, useState', (/\))
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

-- | The sections: the seats' scorecards, then one per economic object: the headline at the round chosen (the last, to
-- | start), each country's regime, the countries side by side, and the clearing between them. A per-round series is a
-- | chart, with its table behind a switch.
mkBody :: DataTable -> Component BodyProps
mkBody grid = do
  fetch <- fetched
  figure <- mkFigure grid
  component "RunResultsBody" \b -> React.do
    round /\ setRound <- useState' (Nothing :: Maybe String)
    let
      keyed path = b.id <> "/" <> path <> "@" <> show b.version

      view :: forall a. ViewKey a -> (a -> JSX) -> JSX
      view key render = fetch { key: keyed (viewName key.view), load: map render <$> runView key b.id Nothing }

      -- a view shown at the chosen round: its key names the round, so choosing another renders it again
      viewAt :: forall a. ViewKey a -> (a -> JSX) -> JSX
      viewAt key render = fetch { key: keyed (viewName key.view) <> "/round-" <> shown, load: map render <$> runView key b.id Nothing }

      rounds = map show (range 1 b.rounds)
      shown = fromMaybe (show b.rounds) round
      -- the rows of the round chosen, without the round's column: the page states it once, above
      atRound t = withoutColumns [ 0 ] (t { rows = filter (\r -> index r 0 == Just shown) t.rows })
      -- one dimension of every country's regime, the countries side by side
      across rs chart table = Layout.columns (map (\r -> figure { chart: chart r, table: table r }) rs)
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-section"
      , children:
          [ Layout.subsection
              { title: "Seat scorecards"
              , purpose: "Each seat's criteria: what was measured, against its bound."
              , children: [ view (Keys.viewKey @"Objectives") (blocksOf grid) ]
              }
          , Layout.subsection
              { title: "Who is trapped, and how tightly?"
              , purpose: "The headline at the round chosen. A substitute gap above 0 leaves a country's own industry idle; a debt service above 1 cannot be paid from sales; a viability margin below 0 has failed."
              , children:
                  [ D.label { className: "flex w-48 items-center gap-2 text-sm", children: [ D.text "Round", selectInput { value: shown, options: rounds, onChange: \v -> setRound v } ] }
                  , viewAt (Keys.viewKey @"Indicators") \i -> Layout.columns [ titled grid (atRound i.countries), titled grid (atRound i.seats) ]
                  ]
              }
          , Layout.subsection
              { title: "National regimes"
              , purpose: "How each country reproduces itself, side by side: what it can produce, who buys it, and what it owes."
              , children:
                  [ view (Keys.viewKey @"Regimes") \rs ->
                      let
                        held = filter (\r -> not (null r.capability.rows)) rs
                      in
                        if null held then note "No country in this run holds productive capacity."
                        else D.div
                          { className: "flex min-w-0 flex-col gap-stack"
                          , children:
                              [ across held _.capabilityChart _.capability
                              , across held _.demandChart _.demand
                              , across held _.financeChart _.finance
                              ]
                          }
                  ]
              }
          , Layout.subsection
              { title: "International clearing"
              , purpose: "What moves between the countries: prices, the current account, goods, capital, and the Rules at the borders."
              , children:
                  [ Layout.columns
                      [ view (Keys.viewKey @"Exchanges") (\cs -> maybe (D.span_ []) (metricChartAt Small) (head cs))
                      , view (Keys.viewKey @"Finance") (\f -> figure { chart: f.currentChart, table: f.current })
                      ]
                  , Layout.columns
                      [ view (Keys.viewKey @"Clearing") (titled grid <<< _.goods)
                      , view (Keys.viewKey @"Clearing") (titled grid <<< _.capital)
                      ]
                  , view (Keys.viewKey @"Clearing") (\c -> if null c.rules.rows then note "No Rule stands at a border in this run." else titled grid c.rules)
                  ]
              }
          ]
      }
