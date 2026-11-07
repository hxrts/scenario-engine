-- | One run's results (docs/06_simulator.md §9.2): the summary (the key figures, the price chart, the
-- | balances after the final round, the objectives, and the rejections and shortfalls), and the deep
-- | views behind one Details section per topic. Every number comes from the read API; this module only
-- | chooses what to show and where.
module Dashboard.Results (mkRunResults, Props, roundCount, roundTarget, clampRound, rowValue) where

import Prelude

import Dashboard.Api (runPath, useApi)
import Dashboard.Components (as, fetched, fieldList, metricChart, status, tableOf)
import Dashboard.DataTable (DataTable)
import Dashboard.Glossary (emptyCell, fieldDefinition)
import Dashboard.Layout as Layout
import Dashboard.UI as UI
import Data.Array (drop, elem, filter, find, index)
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Effect (Effect)
import Harness.Views.Common (Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React

-- | The run whose results are shown. The comparison controls add the run to the comparison, or remove
-- | it, and link to the comparison when it has runs.
type Props =
  { id :: String
  , inComparison :: Boolean
  , comparisonCount :: Int
  , comparisonHref :: String
  , toggleComparison :: Effect Unit
  }

-- | The economy view model (`/api/runs/:id/economy`).
type Economy = { totals :: Chart, flows :: Table, pressure :: Table }

-- | A value of a row of the Overview (a field and its value).
rowValue :: Table -> String -> Maybe String
rowValue t field = find (\r -> index r 0 == Just field) t.rows >>= \r -> index r 1

-- | The run's completed rounds, from the Overview's `Rounds` row.
roundCount :: Table -> Maybe Int
roundCount t = rowValue t "Rounds" >>= Int.fromString

-- | The run's target rounds, from the Overview's `Rounds target` row, when the server reports one.
roundTarget :: Table -> Maybe Int
roundTarget t = rowValue t "Rounds target" >>= Int.fromString

-- | A requested round, kept within the run's rounds (0 is the initial state).
clampRound :: Int -> Int -> Int
clampRound n r = max 0 (min n r)

-- | The overview fields the summary shows as figures.
isFigure :: String -> Boolean
isFigure f = elem f [ "Rounds", "Rejected Proposals", "Assertions failing" ]
  || isJust (stripPrefix (Pattern "Final price ") f)
  || isJust (stripSuffix (Pattern " procured") f)

-- | A table with its title (the server's title for it) above it.
titled :: DataTable -> Table -> JSX
titled grid t = D.div
  { className: "flex min-w-0 flex-col gap-2"
  , children: [ D.p { className: "text-sm font-medium", children: [ D.text t.title ] }, tableOf grid t ]
  }

-- | The results of one run. The overview is fetched first, because the rounds it reports scope the
-- | other views. A run that cannot be read shows its error.
mkRunResults :: DataTable -> Component Props
mkRunResults grid = do
  body <- mkBody grid
  component "RunResults" \p -> React.do
    overview <- useApi p.id (runPath p.id <> "/overview")
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-stack"
      , children:
          [ D.div
              { className: "flex flex-wrap items-center justify-between gap-2"
              , children:
                  [ D.h2 { className: "text-lg font-semibold", children: [ D.text ("Run " <> p.id) ] }
                  , D.div
                      { className: "flex flex-wrap items-center gap-2"
                      , children:
                          [ element UI.button
                              { variant: if p.inComparison then "secondary" else "outline"
                              , size: "sm"
                              , onClick: handler_ p.toggleComparison
                              , children: [ D.text (if p.inComparison then "Remove from comparison" else "Add to comparison") ]
                              }
                          ] <>
                            ( if p.comparisonCount > 0 then [ D.a { href: p.comparisonHref, className: "text-sm underline", children: [ D.text ("Open comparison (" <> show p.comparisonCount <> ")") ] } ]
                              else []
                            )
                      }
                  ]
              }
          , case overview of
              Nothing -> status Nothing
              Just (Left e) -> status (Just e)
              Just (Right (t :: Table)) -> body { id: p.id, table: t }
          ]
      }

type BodyProps = { id :: String, table :: Table }

-- | The summary, then the deep views. The round selector (for balance sheets and events) is clamped
-- | to the run's rounds.
mkBody :: DataTable -> Component BodyProps
mkBody grid = fetched >>= \fetch -> component "RunResultsBody" \b -> React.do
  requested /\ setRequested <- useState' (Nothing :: Maybe Int)
  let
    n = fromMaybe 0 (roundCount b.table)
    r = clampRound n (fromMaybe n requested)
    path view = runPath b.id <> "/" <> view
    figures = filter (\row -> maybe false isFigure (index row 0)) b.table.rows
    tile row =
      let
        field = fromMaybe "" (index row 0)
        value = fromMaybe "" (index row 1)
        e = emptyCell field
      in
        Layout.tile
          { label: field
          , title: fieldDefinition field
          , value: if value == "" then D.span { className: "italic text-muted-foreground", title: e.reason, children: [ D.text e.label ] } else D.text value
          }
    roundInput label = D.label
      { className: "flex items-center gap-2 text-sm"
      , children:
          [ D.text label
          , element UI.input { className: "w-20", "aria-label": label, value: show r, onChange: handler targetValue (\v -> setRequested (Int.fromString =<< v)) }
          , D.text (" of " <> show n)
          ]
      }
    summary =
      [ Layout.grid (figures <#> tile)
      , Layout.section
          { title: "Price"
          , purpose: "The marginal price of each Market after every round: where its supply and demand meet, in quote units per base unit."
          , children: [ fetch { path: path "markets", render: as \(cs :: Array Chart) -> maybe (D.span_ []) metricChart (index cs 0) } ]
          }
      , Layout.section
          { title: "Balances"
          , purpose: "Every Actor's and Aggregate's holdings after the final round, with the change from the start. An Aggregate is a residual representation of its category."
          , children: [ fetch { path: path "balances", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.section
          { title: "Objectives"
          , purpose: "Each Actor's objective criteria, each met, not met, or not evaluated. Objectives are measured from the run and never change the economy."
          , children: [ fetch { path: path "objectives", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.section
          { title: "Rejections and shortfalls"
          , purpose: "Per round: Energy shortfalls, and the Proposals the engine rejected or that lapsed without executing."
          , children: [ fetch { path: path "economy", render: as \(e :: Economy) -> titled grid e.pressure } ]
          }
      ]
    deep =
      [ Layout.details
          { title: "Overview"
          , purpose: "Every overview field of this run. A field with no recorded value says not recorded, with the reason on hover."
          , children: [ fieldList b.table ]
          }
      , Layout.details
          { title: "Markets"
          , purpose: "Reserves and traded volume for each Market, by round."
          , children: [ fetch { path: path "markets", render: as \(cs :: Array Chart) -> D.div_ (map metricChart (drop 1 cs)) } ]
          }
      , Layout.details
          { title: "Facilities"
          , purpose: "Each order the facilities filled: who ordered it, what it bought, and the Money paid."
          , children: [ fetch { path: path "facilities", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "World economy"
          , purpose: "Resource totals across all accounts, and Circulation: the Money moved between Entity categories (Actor and Aggregate)."
          , children: [ fetch { path: path "economy", render: as \(e :: Economy) -> D.div_ [ metricChart e.totals, titled grid e.flows ] } ]
          }
      , Layout.details
          { title: "Balance sheets by round"
          , purpose: "Every balance sheet after the round you choose, with the holdings at the start of the run for comparison."
          , children: [ roundInput "Round", fetch { path: path ("balances?round=" <> show r), render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Representation"
          , purpose: "The four-stage record of each Representation: the observation, the stored factor, the Aggregate's next purchase, and the influence ratio."
          , children: [ fetch { path: path "representation", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Procedures"
          , purpose: "Each collective procedure an Aggregate held: the members who signed, the support against the threshold, and whether it executed or lapsed."
          , children: [ fetch { path: path "procedures", render: as \(t :: Table) -> titled grid t } ]
          }
      , Layout.details
          { title: "Events"
          , purpose: "The engine's events in the round you choose, in canonical phase order."
          , children: [ roundInput "Event round", fetch { path: path ("events?round=" <> show r), render: as \(t :: Table) -> titled grid t } ]
          }
      ]
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-section"
    , children: summary <> deep
    }
