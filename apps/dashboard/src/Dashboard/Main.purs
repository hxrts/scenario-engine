-- | The dashboard (docs/07_simulator.md §9–§10). Three pages in the top bar, one hash route each:
-- | Runs (`#/runs`, or `#/runs/:id` with that run selected: the run list beside the selected run's
-- | results and actions), Prepare (launch a run of the composition), and Compare (runs side by
-- | side). Every view model comes from the read API, through `Dashboard.Client`. The app loads the
-- | glossary once, before any page renders.
module Dashboard.Main (main) where

import Prelude

import Dashboard.BalanceSheet (mkBalanceSheet, mkBalanceSheets)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location, document)
import Data.String (Pattern(..), split)
import Data.Array (elem, filter, find, index, length, null)

import Control.Alt ((<|>))
import Dashboard.Api (Loaded, useLoad)
import Dashboard.Client as Client
import Dashboard.Components (diffTable, fetched, groupedBars, loaded, note, overlayChart, placeholderFrame, selectInput, status, tableOf)
import Dashboard.DataTable (DataTable, defaults, mkDataTable)
import Dashboard.Glossary (glossaryContext)
import Dashboard.Layout as Layout
import Dashboard.Palette (runSwatch)
import Dashboard.Results (mkRunResults)
import Dashboard.Route (Route(..), href, navigate, route)
import Dashboard.Runs (latestReadable, readableIds)
import Dashboard.Theme (pageMarker, themeToggle)
import Dashboard.Public (mkLanding, mkLogin)
import Dashboard.UI as UI
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple)
import Effect (Effect)

import Effect.Aff (Aff)

import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Views.Balance (giniStat, spreadStat)
import Views.Compare (CompareView, compatibilityPurpose)
import Views.Overview (RunList)

import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)

import React.Basic.Hooks (Component, Hook, JSX, component, element, provider, useEffectOnce, useState', (/\))
import React.Basic.Hooks.Aff (UseAff)
import React.Basic.Hooks as React
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)

-- | A tab: its label, the one-sentence purpose shown above its content, and the view it fetches.
-- | A fetched view: its key, and its load, which renders what it reads (`Fetched` in `Dashboard.Components`).
type Fetch = { key :: String, load :: Aff (Either String JSX) } -> JSX

-- | The run list's id column: a run's id is its first cell, and a click on it selects the run.
runKeyColumn :: Int
runKeyColumn = 0

-- | The run list, every run, in creation order.
listRuns :: Aff (Either String RunList)
listRuns = Client.runs

-- | The run list, loaded for a key: a new key reloads it. The previous list
-- | stays on screen, dimmed, until the new one arrives. Every page that lists runs uses this hook.
useRunList :: Int -> Hook (UseAff Int (Tuple Int (Either String RunList))) (Loaded (Either String RunList))
useRunList version = useLoad version listRuns

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
      { className: "col-span-12 flex min-w-0 flex-col gap-gutter lg:col-span-8"
      , children: [ results { id: p.id, version: 0, overview } ]
      }

-- | Runs: the run list beside the selected run, which is the route's run (`#/runs/:id`) or, with no
-- | selection, the latest readable run. The selected run's results come first; its manage
-- | actions are stacked under them. Selecting a row changes the route, so a selected run has
-- | its own address. Archive, resume and delete bump `version`, which reloads the list, the selected
-- | run's overview, and every section of it.
mkRuns :: DataTable -> Component { selected :: Maybe String }
mkRuns grid = do
  runPage <- mkRunPage grid
  component "Runs" \p -> React.do
    everyRun <- useRunList 0
    pure $ Layout.page
      { title: "Runs"
      , purpose: "Every stored run, with the selected run's results beside the list. With no run selected, the latest readable run is shown."
      , children:
          [ loaded everyRun.value \all ->
              let
                t = all
                current = fromMaybe "" (p.selected <|> latestReadable t)
              in
                Layout.grid
                  [ Layout.panel
                      { span: 4
                      , title: "Runs"
                      , purpose: "Every stored run in the order they were created. Click a run to show its results."
                      , children:
                          [ runTable grid t { selected: current, select: \id -> navigate (Run id) }
                          ]
                      }
                  , if current == "" then Layout.panel
                      { span: 8
                      , title: "Selected run"
                      , purpose: "The selected run's results appear here."
                      , children: [ Layout.purpose "No run yet." ]
                      }
                    else runPage { id: current }
                  ]
          ]
      }

-- | Compare: the runs ticked in the choice list (the route's ids; the first is the baseline), and the
-- | comparison's plots and tables. The comparison is fetched whenever the ids change, and the last one
-- | loaded stays on screen until the new one arrives, so ticking a run does not move the page. Every
-- | plot sits in a frame of fixed height.
mkCompare :: DataTable -> Component (Array String)
mkCompare grid = do
  fetch <- fetched
  comparison <- mkComparison grid fetch
  component "Compare" \ids -> React.do
    everyRun <- useRunList 0
    let chooser baseline = loaded everyRun.value \t -> choices t ids baseline
    pure $ Layout.page
      { title: "Compare a Run set"
      , purpose: "Runs of one Run set side by side: prices, net worth and standings on shared axes, then the key values in one table."
      , children:
          [ if null ids then React.fragment [ chooser Nothing, plots grid fetch [] Nothing Nothing (\_ -> pure unit) ]
            else comparison { ids, chooser }
          ]
      }

-- | The comparison of the ticked runs, fetched for their ids. The view stays mounted while the ids change
-- | (so the last comparison shows until the next arrives); the player picked for net worth is kept here.
mkComparison :: DataTable -> Fetch -> Component { ids :: Array String, chooser :: Maybe String -> JSX }
mkComparison grid fetch = component "Comparison" \{ ids, chooser } -> React.do
  comparison <- useLoad (String.joinWith "," ids) (Client.compare ids)
  picked /\ setPicked <- useState' (Nothing :: Maybe String)
  pure $ React.fragment [ chooser (baselineOf comparison.value), plots grid fetch ids comparison.value picked setPicked ]
  where
  baselineOf = case _ of
    Just (Right c) -> Just c.baseline
    _ -> Nothing

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

  message = if null ids then "Tick runs in the list above to compare them as a Run set." else "Loading the comparison…"
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

-- | The choice list: every readable run as a checkbox, so no id is typed. Ticking a run adds it to the
-- | comparison (the route's ids, in the order ticked; the first is the baseline), and unticking removes
-- | it: a run is removed only by unticking it. Each tick changes the route. A ticked run shows the
-- | colour its plots and tables use.
choices :: RunList -> Array String -> Maybe String -> JSX
choices t ids baseline = Layout.section
  { title: "Choose runs"
  , purpose: "Every readable run. Tick the runs of a Run set to compare them; untick one to remove it. The first run ticked is the baseline."
  , children:
      [ D.div
          { className: "grid grid-cols-1 gap-2 sm:grid-cols-2 lg:grid-cols-3"
          , children: readableIds t <#> \id -> D.label
              { key: id
              , className: "flex items-center gap-2 text-sm"
              , children:
                  [ element UI.checkbox
                      { checked: elem id ids
                      , onCheckedChange: mkEffectFn1 (\on -> navigate (Compare (toggled id on)))
                      }
                  , D.span { className: "size-3 shrink-0 rounded-full " <> runSwatch ids id }
                  , D.span { className: "font-medium", children: [ D.text id ] }
                  , D.span { className: "text-muted-foreground", children: [ D.text (if baseline == Just id then "baseline" else "") ] }
                  ]
              }
          }
      ]
  }
  where
  toggled id on
    | on = if elem id ids then ids else ids <> [ id ]
    | otherwise = filter (_ /= id) ids

-- | The top bar: the app name, then the three pages (Runs, Prepare, and Compare with how many runs it
-- | holds). Every item is a link on every page. The comparison set lives in the App, so every page sees it.
shell :: Route -> Array String -> JSX -> JSX -> JSX
shell current compareSet themeButton page = D.div
  { className: "min-h-screen"
  , children:
      [ Layout.topBar
          { brand: "Scenario Engine"
          , home: href Runs
          , nav:
              [ navLink "Runs" Runs (isRuns current)
              , navLink "Balance sheets" BalanceSheets (current == BalanceSheets)
              , navLink (if compareSet == [] then "Compare" else "Compare (" <> show (length compareSet) <> ")") (Compare compareSet) (isCompare current)
              ]
          , themeButton
          }
      , D.main { className: "min-w-0", children: [ page ] }
      ]
  }
  where
  isRuns = case _ of
    Run _ -> true
    Runs -> true
    _ -> false
  isCompare = case _ of
    Compare _ -> true
    _ -> false
  -- the current page is marked by a dot beneath it, not by weight or an underline
  navLink label r active = D.a
    { href: href r
    , className:
        if active then "relative text-foreground after:absolute after:-bottom-2 after:left-1/2 after:size-1 after:-translate-x-1/2 after:rounded-full after:bg-foreground"
        else "text-muted-foreground hover:text-foreground"
    , children: [ D.text label ]
    }

-- | The app: the routes and the comparison set (the ids of the Compare route, shown in the top bar). The
-- | glossary loads first: until it has, the app shows only its status; the pages then read it through
-- | `glossaryContext`. `#/runs` and `#/runs/:id` render the same Runs component, so its list keeps its
-- | state as a run is selected.
mkApp :: Component Unit
mkApp = do
  grid <- mkDataTable
  runsPage <- mkRuns grid
  compare <- mkCompare grid
  balanceSheets <- mkBalanceSheets
  toggle <- themeToggle
  component "App" \_ -> React.do
    glossary <- useLoad unit Client.glossary
    current /\ setCurrent <- useState' Runs
    compareSet /\ setCompareSet <- useState' ([] :: Array String)
    useEffectOnce $ matchesWith (parse route) \_ r -> do
      setCurrent r
      case r of
        Compare ids -> setCompareSet ids
        _ -> pure unit
    let
      page = case current of
        Runs -> runsPage { selected: Nothing }
        Run id -> runsPage { selected: Just id }
        Compare ids -> compare ids
        BalanceSheets -> balanceSheets unit
    pure $ loaded glossary.value \g -> provider glossaryContext (Just g) [ shell current compareSet (toggle unit) page ]

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      r <- createRoot e
      path <- pathname =<< location =<< window
      marked <- pageMarker
      case filter (_ /= "") (split (Pattern "/") path) of
        [ "balancesheet", run, token ] -> do
          grid <- mkDataTable
          page <- mkBalanceSheet grid
          renderRoot r (page { run, token })
        _ | marked == "landing" -> mkLanding >>= \page -> renderRoot r (page unit)
        _ | marked == "login" || marked == "login-failed" -> mkLogin >>= \page -> renderRoot r (page { failed: marked == "login-failed" })
        _ -> do
          app <- mkApp
          renderRoot r (app unit)
