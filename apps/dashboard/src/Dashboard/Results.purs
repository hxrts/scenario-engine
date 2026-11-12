-- | One run's results (docs/07_simulator.md §9.2), a presentational view of the overview the run page
-- | fetched. A compact header names the run from the overview's facts; then the sections answer the five
-- | questions in order: the key figures, the marginal prices, the standings, net worth over time, the
-- | actions players take, the drivers, the balance of the game, the objectives, and the final balance
-- | sheet. Every number comes from the read API; this module chooses what to show and where, and renders
-- | each statistic's definition from its record in `Views`.
module Dashboard.Results (mkRunResults, Props) where

import Prelude

import Dashboard.Client (runView)
import Dashboard.Api (Loaded)
import Dashboard.Components (fetched, loaded, metricChart, note, stackedBars, stale, statistic, tableOf)
import Dashboard.DataTable (DataTable)
import Dashboard.Actor (mkActor)
import Dashboard.Events (mkEvents)
import Dashboard.Glossary (Lookup, emptyValue, glossaryContext)
import Dashboard.Layout as Layout
import Data.Array (index, null)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Schema.View (viewName)
import Views.Actions (actionsStat)
import Views.Balance (giniStat, spreadStat)
import Views.Common (Table)
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Drivers (driversStat)
import Views.Overview (Fact, RunOverview)
import Views.Standings (netWorthStat)
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

-- | The compact header: the run's id, then its facts (name, composition, status, rounds completed out of
-- | its target, game id), both from the same overview, so a run switch never shows a new name over the
-- | old facts. An empty fact says what it means, with the reason on hover.
header :: Lookup -> String -> Array Fact -> JSX
header glossary id facts = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children:
      [ D.h2 { className: "text-lg font-semibold", children: [ D.text ("Run " <> id) ] }
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

-- | The sections, in the order of the five questions. Each has one purpose line.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> mkActor >>= \actor -> mkEvents grid >>= \events -> component "RunResultsBody" \b -> React.do
  let
    keyed path = b.id <> "/" <> path <> "@" <> show b.version

    view :: forall a. ViewKey a -> (a -> JSX) -> JSX
    view key render = fetch { key: keyed (viewName key.view), load: map render <$> runView key b.id Nothing }
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children:
        [ Layout.section
            { title: "Who is trapped, and how tightly?"
            , purpose: "The run's headline, by round. For each country: its substitute gap (what making a good it imports costs in wages, less what importing it costs: above 0, its own industry stands idle), its net external position, and how much of its own output its households absorb. For each seat: what it owes over what it sells, and how far its every-round criteria cleared their bounds in the worst round so far."
            , children: [ view (Keys.viewKey @"Indicators") (titled grid <<< _.countries), view (Keys.viewKey @"Indicators") (titled grid <<< _.seats) ]
            }
        , Layout.section
            { title: "National regimes"
            , purpose: "How each country reproduces itself, the same three panels for each: its productive capability (capacity, the competence to run it, what ran and what stood idle), its effective demand (what its households earned, spent and saved, and what it sold at home and abroad), and its financial capacity (what it is owed, what it owes, and what it paid on its debts)."
            , children: [ view (Keys.viewKey @"Regimes") \rs -> if null rs then note "No country in this run has households." else D.div { className: "flex min-w-0 flex-col gap-section", children: map regimePanels rs } ]
            }
        , Layout.section
            { title: "International clearing"
            , purpose: "What moves between the countries: the goods traded across each border, the exchange rate and every Exchange's price, the capital lent and repaid across borders, each country's current account, and the Rules that stand at the borders."
            , children:
                [ view (Keys.viewKey @"Clearing") (titled grid <<< _.goods)
                , view (Keys.viewKey @"Exchanges") (\cs -> D.div { className: "flex min-w-0 flex-col gap-section", children: map metricChart cs })
                , view (Keys.viewKey @"Clearing") (titled grid <<< _.capital)
                , view (Keys.viewKey @"Finance") (titled grid <<< _.current)
                , view (Keys.viewKey @"Clearing") (\c -> if null c.rules.rows then note "No Rule stands at a border in this run." else titled grid c.rules)
                ]
            }
        , Layout.section
            { title: "Seat scorecards"
            , purpose: "Each seat's criteria, met, not met or not evaluated, with the measured values. A state's security and industry read its regime's capability panel; its viability and prosperity, its demand panel; a firm's wealth and output, its finance and capability panels."
            , children: [ view (Keys.viewKey @"Objectives") (titled grid) ]
            }
        , Layout.tabbed
            [ { key: "engine"
              , label: "Engine detail"
              , purpose: "Every generic view of the run: the key figures, prices, standings, actions, drivers, balance, dependence, finance and whole-economy measures, assertions, balance sheets and events."
              , content: D.div
                  { className: "flex min-w-0 flex-col gap-section"
                  , children:
                      [ Layout.section
                          { title: "Key figures"
                          , purpose: "The measure behind each of the five questions, with the formula the sections below use."
                          , children: [ statistic netWorthStat, statistic actionsStat, statistic driversStat, statistic spreadStat, statistic giniStat ]
                          }
                      , Layout.section
                          { title: "Marginal prices"
                          , purpose: "What moves prices: the marginal price of each Exchange after every round, in quote units per base unit."
                          , children: [ view (Keys.viewKey @"Exchanges") (\cs -> maybe (D.span_ []) metricChart (index cs 0)) ]
                          }
                      , Layout.section
                          { title: "Standings"
                          , purpose: "How well each player is doing against the others: net worth after the final round, its change from the start, and its rank."
                          , children: [ view (Keys.viewKey @"Standings") (titled grid) ]
                          }
                      , Layout.section
                          { title: "Net worth over time"
                          , purpose: "One line per player: net worth from the start (round 0) to the final round."
                          , children: [ view (Keys.viewKey @"NetWorth") metricChart ]
                          }
                      , Layout.section
                          { title: "Actions"
                          , purpose: "What each player does over the whole run, by kind of action: one bar per player, and the table gives the exact counts."
                          , children: [ view (Keys.viewKey @"Actions") \a -> D.div { className: "flex min-w-0 flex-col gap-section", children: [ stackedBars a.chart, titled grid a.totals ] } ]
                          }
                      , Layout.section
                          { title: "Drivers"
                          , purpose: "What forces drive gameplay, by round: the rules enacted and repealed, the collective procedure outcomes, and the shortfalls per Resource."
                          , children: [ view (Keys.viewKey @"Drivers") (titled grid) ]
                          }
                      , Layout.section
                          { title: "Representation"
                          , purpose: "How each Aggregate follows its representative: the observation in one round, the factor it stores, and the Aggregate's adjusted purchase in the next."
                          , children: [ view (Keys.viewKey @"Representation") (titled grid) ]
                          }
                      , Layout.section
                          { title: "Collective procedures"
                          , purpose: "Each voted candidate Transaction: who signed, the support it reached against its threshold, and the outcome."
                          , children: [ view (Keys.viewKey @"Procedures") (titled grid) ]
                          }
                      , Layout.section
                          { title: "Balance measures"
                          , purpose: "How evenly final net worth is spread among the players: the spread and the Gini coefficient."
                          , children: [ view (Keys.viewKey @"Balance") (titled grid <<< _.measures) ]
                          }
                      , Layout.section
                          { title: "Dependence"
                          , purpose: "By round, for each Actor and each Resource it has a fixed requirement of: what its own capacity covers, what it must import, what it traded and was paid, what it went short, the bans against it, and the capacity on order."
                          , children: [ view (Keys.viewKey @"Dependence") (\t -> if null t.rows then note "No Actor in this run has a fixed requirement." else titled grid t) ]
                          }
                      , Layout.section
                          { title: "Finance"
                          , purpose: "The bonds open after each round, who lent and who owes, and each country's external accounts: what crossed its border for goods, currency, levies and interest, and its current account in the numeraire."
                          , children: [ view (Keys.viewKey @"Finance") (titled grid <<< _.current), view (Keys.viewKey @"Finance") (titled grid <<< _.external), view (Keys.viewKey @"Finance") (titled grid <<< _.earnings), view (Keys.viewKey @"Finance") (\v -> if null v.bonds.rows then note "No bonds are open in this run." else titled grid v.bonds) ]
                          }
                      , Layout.section
                          { title: "Whole-economy measures"
                          , purpose: "By round and Exchange: what was asked for and what went unfilled, how concentrated net worth is, and how Money is distributed."
                          , children: [ view (Keys.viewKey @"WholeEconomy") (titled grid) ]
                          }
                      , Layout.section
                          { title: "Assertions"
                          , purpose: "Each invariant check that failed: what it expected, what it observed, and the events of that round that concern it."
                          , children: [ view (Keys.viewKey @"Assertions") (\t -> if null t.rows then note "No assertion failed in this run." else titled grid t) ]
                          }
                      , Layout.section
                          { title: "Final balance sheet"
                          , purpose: "Every account's holdings by Resource after the final round, with the holdings at the start of the run for comparison."
                          , children: [ view (Keys.viewKey @"Balances") (titled grid) ]
                          }
                      , actor { id: b.id, version: b.version }
                      , events { id: b.id, version: b.version, rounds: b.rounds }
                      ]
                  }
              }
            ]
        ]
    }
  where
  regimePanels r = Layout.grid
    [ Layout.panel { span: 4, title: "Capability, " <> r.country, purpose: "Capacity, the competence to run it, units run and units idle.", children: [ tableOf grid r.capability ] }
    , Layout.panel { span: 4, title: "Demand, " <> r.country, purpose: "Household income, spending and saving; sales at home and abroad.", children: [ tableOf grid r.demand ] }
    , Layout.panel { span: 4, title: "Finance, " <> r.country, purpose: "Owed to it, what it owes, and its debt service.", children: [ tableOf grid r.finance ] }
    ]
