-- | One run's results (docs/07_simulator.md §9.3), a presentational view of the overview the run page fetched. A compact
-- | header names the run from the overview's facts; then one section per economic object. Every number comes from the
-- | read API; this module chooses what to show and where.
module Dashboard.Results (mkRunResults, Props) where

import Prelude

import Dashboard.Client (runView)
import Dashboard.Api (Loaded)
import Dashboard.Components (fetched, loaded, metricChart, note, selectInput, stale, tableOf)
import Dashboard.DataTable (DataTable)

import Dashboard.Glossary (Lookup, emptyValue, glossaryContext)
import Dashboard.Layout as Layout
import Data.Array (filter, head, index, null, range)
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

-- | The compact header: the run's id, then its facts (name, composition, status, rounds completed out of
-- | its target, game id), both from the same overview, so a run switch never shows a new name over the
-- | old facts. An empty fact says what it means, with the reason on hover.
header :: Lookup -> String -> Array Fact -> JSX
header glossary id facts = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.h3 { className: "text-base font-semibold", children: [ D.text ("Run " <> id) ] }
      , D.dl
          { className: "grid grid-cols-[max-content_1fr] gap-x-6 gap-y-1 text-sm"
          , children: facts >>= \f ->
              [ D.dt { key: f.label, className: "text-muted-foreground", children: [ D.text f.label ] }
              , D.dd { key: f.label <> "-value", className: "min-w-0 break-words", children: [ valueOf f ] }
              ]
          }
      ]
  }
  where
  valueOf f
    | f.value == "" = emptyValue glossary f.label
    | otherwise = D.text f.value

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

-- | The sections, one per economic object: the headline at the round chosen (the last, to start), each country's
-- | regime, the clearing between them, and the seats' scorecards.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> component "RunResultsBody" \b -> React.do
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
    atRound t = t { rows = filter (\r -> index r 0 == Just shown) t.rows }
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children:
        [ Layout.subsection
            { title: "Who is trapped, and how tightly?"
            , purpose: "The run's headline at the round chosen. For each country: its substitute gap (what making a good it imports costs in wages, less what importing it costs: above 0, its own industry stands idle), its net external position, and how much of its own output its households absorb. For each seat: what it owes over what it sells, and how far its every-round criteria cleared their bounds in the worst round so far."
            , children:
                [ D.label { className: "flex w-48 items-center gap-2 text-sm", children: [ D.text "Round", selectInput { value: shown, options: rounds, onChange: \v -> setRound v } ] }
                , viewAt (Keys.viewKey @"Indicators") (titled grid <<< atRound <<< _.countries)
                , viewAt (Keys.viewKey @"Indicators") (titled grid <<< atRound <<< _.seats)
                ]
            }
        , Layout.subsection
            { title: "National regimes"
            , purpose: "How each country reproduces itself, the same three panels for each: its productive capability (capacity, the competence to run it, what ran and what stood idle), its effective demand (what its households earned, spent and saved, and what it sold at home and abroad), and its financial capacity (what it is owed, what it owes, and what it paid on its debts). A country with no productive capacity is left out."
            , children: [ view (Keys.viewKey @"Regimes") \rs -> let held = filter (\r -> not (null r.capability.rows)) rs in if null held then note "No country in this run holds productive capacity." else D.div { className: "flex min-w-0 flex-col gap-section", children: map regimePanels held } ]
            }
        , Layout.subsection
            { title: "International clearing"
            , purpose: "What moves between the countries: the goods traded across each border, the exchange rate and every Exchange's price, the capital lent and repaid across borders, each country's current account, and the Rules that stand at the borders."
            , children:
                [ view (Keys.viewKey @"Clearing") (titled grid <<< _.goods)
                , view (Keys.viewKey @"Exchanges") (\cs -> maybe (D.span_ []) metricChart (head cs))
                , view (Keys.viewKey @"Clearing") (titled grid <<< _.capital)
                , view (Keys.viewKey @"Finance") (titled grid <<< _.current)
                , view (Keys.viewKey @"Clearing") (\c -> if null c.rules.rows then note "No Rule stands at a border in this run." else titled grid c.rules)
                ]
            }
        , Layout.subsection
            { title: "Seat scorecards"
            , purpose: "Each seat's criteria, met, not met or not evaluated, with the measured values. A state's security and industry read its regime's capability panel; its viability and prosperity, its demand panel; a firm's wealth and output, its finance and capability panels."
            , children: [ view (Keys.viewKey @"Objectives") (titled grid) ]
            }
        ]
    }
  where
  regimePanels r = Layout.grid
    [ Layout.panel { span: 12, title: "Capability, " <> r.country, purpose: "Capacity, the competence to run it, units run and units idle.", children: [ tableOf grid r.capability ] }
    , Layout.panel { span: 12, title: "Demand, " <> r.country, purpose: "Household income, spending and saving; sales at home and abroad.", children: [ tableOf grid r.demand ] }
    , Layout.panel { span: 12, title: "Finance, " <> r.country, purpose: "Owed to it, what it owes, and its debt service.", children: [ tableOf grid r.finance ] }
    ]
