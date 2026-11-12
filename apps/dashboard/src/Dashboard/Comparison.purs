-- | What the Simulations and Live pages share (docs/07_simulator.md §9): a run list as a table, one run's results, and
-- | the comparison's plots and tables. Every view model comes from the read API, through `Dashboard.Client`.
module Dashboard.Comparison (Fetch, runTable, mkRunPage, mkComparison, plots) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Components (diffTable, groupedBars, note, overlayChart, placeholderFrame, selectInput, status, tableOf)
import Dashboard.DataTable (DataTable, defaults)
import Dashboard.Layout as Layout
import Dashboard.Results (mkRunResults)
import Data.Array (elem, find, index, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, useState', (/\))
import React.Basic.Hooks as React
import Views.Balance (giniStat, spreadStat)
import Views.Compare (CompareView, compatibilityPurpose)
import Views.Overview (RunList)

-- | A tab: its label, the one-sentence purpose shown above its content, and the view it fetches.
-- | A fetched view: its key, and its load, which renders what it reads (`Fetched` in `Dashboard.Components`).
type Fetch = { key :: String, load :: Aff (Either String JSX) } -> JSX

-- | The run list's id column: a run's id is its first cell, and a click on it selects the run.
runKeyColumn :: Int
runKeyColumn = 0

-- | The run list as a table. A row click selects that run; the selected row is highlighted.
runTable :: DataTable -> RunList -> { selected :: String, select :: String -> Effect Unit } -> JSX
runTable grid t s = D.div
  { className: "overflow-x-auto"
  , children:
      [ grid
          ( (defaults t)
              { onRow = Just \row -> s.select (fromMaybe "" (index row runKeyColumn))
              , activeRow = \row -> index row runKeyColumn == Just s.selected
              }
          )
      ]
  }

-- | The run page: the selected run's overview, fetched once, and its results.
mkRunPage :: DataTable -> Component { id :: String }
mkRunPage grid = do
  results <- mkRunResults grid
  component "RunPage" \p -> React.do
    overview <- useLoad p.id (Client.overview p.id)
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-gutter"
      , children: [ results { id: p.id, version: 0, overview } ]
      }

-- | The plots and tables of the comparison, each section with its purpose line. The plots answer, in
-- | order: how each Exchange's price moves, how each player's net worth moves, who ends ahead, how even
-- | the outcome is, and what the key values are. Until a comparison is loaded, each plot's frame is
-- | shown in the same place and size, so nothing moves when the data arrives. The tables in the tabs
-- | hold the rest.
plots :: DataTable -> Fetch -> Array String -> Maybe (Either String CompareView) -> Maybe String -> (Maybe String -> Effect Unit) -> JSX
plots grid fetch ids shown picked setPicked = D.div
  { className: "flex min-w-0 flex-col gap-section"
  , children: skippedRuns <>
      [ Layout.section
          { title: "Prices by exchange"
          , purpose: "How each Exchange's marginal price moves after every round: one line per run, on the same axes."
          , children: loadedWith (Layout.grid [ item "Prices by exchange" waiting ]) \c ->
              [ Layout.grid (c.exchanges <#> \m -> item m.title (overlayChart ids c.baseline m)) ]
          }
      , Layout.section
          { title: "Net worth over time"
          , purpose: "One player's net worth from the start (round 0) to the final round, with one line per run. Choose the player."
          , children:
              [ D.div { className: "flex h-9 items-center", children: loadedWith (D.span_ []) (\c -> [ playerPicker c ]) }
              , D.div { children: loadedWith waiting (\c -> netWorthPlot c) }
              ]
          }
      , Layout.section
          { title: "Final standings"
          , purpose: "Each player's net worth after the final round: one group per player, with one bar per run."
          , children: loadedWith waiting \c -> [ groupedBars ids c.baseline "Player" c.standings ]
          }
      , Layout.section
          { title: "Spread and Gini"
          , purpose: balancePurpose
          , children: loadedWith (balance waiting waiting) \c -> [ balance (groupedBars ids c.baseline "Measure" c.balance.spread) (groupedBars ids c.baseline "Measure" c.balance.gini) ]
          }
      , Layout.section
          { title: "Key values"
          , purpose: "One row per measure or player, one column per run; each column is headed by the run's name and id. A run keeps its colour in the plots."
          , children: loadedWith waiting \c -> [ tableOf grid c.keyValues ]
          }
      , Layout.section
          { title: "Details"
          , purpose: "The comparison's full tables, one tab each."
          , children:
              [ Layout.tabbed
                  [ { key: "final", label: "Final state", purpose: "Every account's holding after the final round, one column per run; differing values are highlighted.", content: D.div { children: loadedWith waiting \c -> [ diffTable grid c.final ] } }
                  , { key: "diff", label: "Baseline diff", purpose: "The holdings that differ from the first run, for each run, as a change.", content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.diff ] } }
                  , { key: "config", label: "Configuration", purpose: "The specification fields that differ between the runs; differing values are highlighted.", content: D.div { children: loadedWith waiting \c -> [ diffTable grid c.config ] } }
                  , { key: "compatibility", label: "Compatibility", purpose: compatibilityPurpose, content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.compatibility ] } }
                  , { key: "seeds", label: "Seed variation", purpose: "Does the outcome depend on the seed? The spread and Gini of the runs that share this run's specification and differ only in seed, with the lowest, highest and range of each. With one run, no range is reported.", content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.seeds ] } }
                  , { key: "agency", label: "Agency", purpose: "For each agency run among those compared: how far one seat playing the alternative preset moves the run, against the preset run it deviates from, and its objective criteria under each.", content: D.div { className: "flex min-w-0 flex-col gap-stack", children: loadedWith waiting \c -> if null c.agency.rows then [ note "No agency run is among the runs compared." ] else [ Layout.captioned c.agency.title [ tableOf grid c.agency ], Layout.captioned c.agencyCriteria.title [ tableOf grid c.agencyCriteria ] ] } }
                  ]
              ]
          }
      ]
  }
  where
  -- the requested runs that could not be read, each with its reason (docs/07_simulator.md §10.1)
  skippedRuns = case shown of
    Just (Right c) | not (null c.skipped) ->
      [ Layout.section
          { title: "Skipped runs"
          , purpose: "These runs could not be read and are not in the comparison."
          , children: [ D.ul { className: "list-disc pl-5 text-sm", children: c.skipped <#> \s -> D.li { key: s, children: [ D.text s ] } } ]
          }
      ]
    _ -> []

  message = "Loading the comparison…"
  waiting = placeholderFrame message

  -- the loaded comparison's content; until one is loaded, the waiting content of the same size shows
  loadedWith :: JSX -> (CompareView -> Array JSX) -> Array JSX
  loadedWith waitingContent f = case shown of
    Just (Right c) -> f c
    Just (Left e) -> [ status (Just e) ]
    Nothing -> [ waitingContent ]

  item title child = D.div
    { className: "col-span-12 min-w-0 lg:col-span-6"
    , children: [ Layout.captioned title [ child ] ]
    }

  -- the unit of the spread, which the loaded comparison names; until one is loaded, the copy omits it
  spreadUnit = case shown of
    Just (Right c) -> Just c.balance.unit
    _ -> Nothing
  balancePurpose = "How even final net worth is among the players in each run. "
    <> maybe "" (\u -> "The spread is in " <> u <> ". ") spreadUnit
    <> giniStat.definition

  -- the two balance plots side by side, each in its own panel
  balance spreadPlot giniPlot = Layout.grid
    [ Layout.panel { span: 6, title: "Spread", purpose: spreadStat.definition, children: [ spreadPlot ] }
    , Layout.panel { span: 6, title: "Gini coefficient", purpose: giniStat.definition, children: [ giniPlot ] }
    ]

  playerPicker c = D.label
    { className: "flex items-center gap-2 text-sm"
    , children:
        [ D.text "Player"
        , selectInput { value: chosen c, options: map _.player c.netWorth, onChange: setPicked }
        ]
    }

  netWorthPlot c = case find (\nw -> nw.player == chosen c) c.netWorth of
    Nothing -> [ waiting ]
    Just nw -> [ overlayChart ids c.baseline nw.chart ]

  -- the player shown: the one picked, when the comparison has them, else the first
  chosen c = case picked of
    Just p | elem p (map _.player c.netWorth) -> p
    _ -> maybe "" _.player (index c.netWorth 0)

-- | The comparison of the runs named (every simulated run when none is), with its plots. The player picked for net
-- | worth is kept here; the last comparison shows until the next arrives.
mkComparison :: DataTable -> Fetch -> Component { ids :: Array String, colours :: Array String }
mkComparison grid fetch = component "Comparison" \{ ids, colours } -> React.do
  comparison <- useLoad (String.joinWith "," ids) (Client.compare ids)
  picked /\ setPicked <- useState' (Nothing :: Maybe String)
  pure (plots grid fetch colours comparison.value picked setPicked)
