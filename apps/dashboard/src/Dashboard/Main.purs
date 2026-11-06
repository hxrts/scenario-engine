-- | The dashboard (docs/06_simulator.md §9–§10). Four pages, one hash route each: the run list (home,
-- | with the latest run's summary), Prepare (launch and derive runs), View (one run's results and
-- | actions), and Compare (runs side by side). Every view model comes from the read API.
module Dashboard.Main (main) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (encode, get, post, runPath, useApi, useFetch)
import Dashboard.Components (as, diffTable, fetched, metricChart, status, tableOf)
import Dashboard.DataTable (DataTable, mkDataTable)
import Dashboard.Launch (LaunchForm, LaunchRequest, emptyForm, validate)
import Dashboard.Layout as Layout
import Dashboard.Presets (presetNames)
import Dashboard.Results (mkRunResults, roundCount, roundTarget)
import Dashboard.Route (Route(..), href, route)
import Dashboard.RunActions as Actions
import Dashboard.Runs (RunInfo, derivedFrom, mergeRuns, runInfo, runInfos, sourceRow)
import Dashboard.Theme (themeToggle)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array (elem, filter, find, findIndex, head, index, null, reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Harness.Views.Common (Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.Hash (matchesWith, setHash)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | A tab: its label, the one-sentence purpose shown above its content, and the view it fetches.
type Tab = { key :: String, label :: String, purpose :: String, path :: String, render :: Render }

type Render = Json -> JSX

type Fetched = { path :: String, render :: Render } -> JSX

-- | The run's column in the run list (`Table.columns` of `/api/runs`) holds its id.
runKeyColumn :: Int
runKeyColumn = 0

-- | The comparison view model (`/api/compare`); each part is one of its fields.
type CompareView = { trajectory :: Chart, final :: Table, diff :: Table, config :: Table, compatibility :: Table }

-- | The run list. Without `archived` it is the runs that are not archived; with it, the archived
-- | runs are listed too (the list's `?archived=true`), in one table.
listRuns :: Boolean -> Aff (Either String Table)
listRuns archived = do
  shown <- get "/api/runs"
  if not archived then pure shown
  else do
    more <- get "/api/runs?archived=true"
    pure (mergeRuns <$> shown <*> more)

-- | The most recent run (the list is in creation order) that can be read: a run the server could not
-- | decode has the status `unreadable`, and has no results to show.
latestReadable :: Table -> Maybe String
latestReadable t = do
  si <- findIndex (_ == "Status") t.columns
  row <- find (\r -> index r si /= Just "unreadable") (reverse t.rows)
  index row runKeyColumn

-- | A Source cell links to the source run when the list has it.
runLinks :: Table -> Int -> Array String -> Maybe String
runLinks t i row
  | Just i == findIndex (_ == "Source") t.columns = do
      s <- sourceRow t row
      if elem s (map _.id (runInfos t)) then Just (href (Run s)) else Nothing
  | otherwise = Nothing

-- | A link to a run's page.
runLink :: String -> JSX
runLink id = D.a { href: href (Run id), className: "underline", children: [ D.text id ] }

-- | A comparison of the given runs, one tab per part. Each tab has its purpose line.
compareTabs :: DataTable -> Array String -> String -> Array Tab
compareTabs grid ids market =
  [ part "Trajectory" "The marginal price of one Market in each compared run, by round." (comparePath <> "&market=" <> encode market) (\c -> metricChart c.trajectory)
  , part "Final state" "Every account's holding after the final round, one column per run; differing values are highlighted." comparePath (\c -> diffTable grid { baseline: 2 } c.final)
  , part "Baseline diff" "The holdings that differ from the first run, for each run, as a change." comparePath (\c -> tableOf grid c.diff)
  , part "Configuration" "The specification settings that differ between the runs; differing values are highlighted." comparePath (\c -> diffTable grid { baseline: 1 } c.config)
  , part "Compatibility" "Whether each run can be combined with the first: the same Gameboard, engine, rounds, units, Markets and Actor selection." comparePath (\c -> tableOf grid c.compatibility)
  , { key: "agency", label: "Agency (fixed baseline)", purpose: "The agency analysis against a fixed baseline, for each Actor's alternatives.", path: "/api/agency", render: as \(t :: Table) -> tableOf grid t }
  ]
  where
  comparePath = "/api/compare?runs=" <> joinIds
  joinIds = String.joinWith "," (map encode ids)
  part label purpose path f = { key: label, label, purpose, path, render: as (f :: CompareView -> JSX) }

-- | The tabs, each showing its purpose line and then its fetched view.
tabbed :: Fetched -> Array Tab -> JSX
tabbed fetch ts = element UI.tabs
  { defaultValue: maybe "" _.key (index ts 0)
  , children:
      [ element UI.tabsList { children: ts <#> \t -> element UI.tabsTrigger { key: t.key, value: t.key, children: [ D.text t.label ] } } ]
        <> (ts <#> \t -> element UI.tabsContent { key: t.key, value: t.key, children: [ D.p { className: "mb-3 text-sm text-muted-foreground", children: [ D.text t.purpose ] }, fetch { path: t.path, render: t.render } ] })
  }

-- | The run list as a table. A row click selects that run; the selected row is highlighted.
runTable :: DataTable -> Table -> { selected :: String, select :: String -> Effect Unit } -> JSX
runTable grid t s = D.div
  { className: "overflow-x-auto"
  , children:
      [ grid
          { columns: t.columns
          , rows: t.rows
          , link: runLinks t
          , mark: \_ _ -> false
          , onRow: Just \row -> s.select (fromMaybe "" (index row runKeyColumn))
          , activeRow: \row -> index row runKeyColumn == Just s.selected
          }
      ]
  }

-- | A labelled text input, or a preset select, for the launch form.
launchFields :: LaunchForm -> ((LaunchForm -> LaunchForm) -> Effect Unit) -> Maybe String -> Effect Unit -> JSX
launchFields form setForm failure launch = D.div
  { className: "flex flex-col gap-stack"
  , children:
      [ field "Name" form.name (\v f -> f { name = v })
      , presetField
      , field "Player seed" form.playerSeed (\v f -> f { playerSeed = v })
      , field "Rounds (blank for the scenario default)" form.rounds (\v f -> f { rounds = v })
      , maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] }) failure
      , element UI.button { onClick: handler_ launch, children: [ D.text "Run" ] }
      ]
  }
  where
  field label value set = D.label { className: "flex flex-col gap-1 text-sm", children: [ D.text label, element UI.input { value, onChange: handler targetValue (\v -> setForm (set (fromMaybe "" v))) } ] }
  presetField = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Preset"
        , D.select
            { value: form.preset
            , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
            , onChange: handler targetValue (\v -> setForm (\f -> f { preset = fromMaybe f.preset v }))
            , children: presetNames <#> \p -> D.option { key: p, value: p, children: [ D.text p ] }
            }
        ]
    }

-- | Prepare: launch a run, or derive one from a run (duplicate, rerun, variant or counterfactual).
mkPrepare :: DataTable -> Component Unit
mkPrepare _ = do
  actions <- Actions.mkRunActions
  component "Prepare" \_ -> React.do
    form /\ setForm <- useState emptyForm
    failure /\ setFailure <- useState' (Nothing :: Maybe String)
    picked /\ setPicked <- useState' (Nothing :: Maybe String)
    runs <- useFetch unit (listRuns false)
    let
      -- A bad form is reported without a request; a launch that the server accepts opens the new run.
      launch = case validate form of
        Left e -> setFailure (Just e)
        Right req -> launchAff_ (submit req)

      submit :: LaunchRequest -> Aff Unit
      submit req = do
        reply <- post "/api/runs" req :: Aff (Either String { id :: String })
        liftEffect case reply of
          Left e -> setFailure (Just e)
          Right r -> setHash (print route (Run r.id))
    pure $ Layout.page
      { title: "Prepare a run"
      , purpose: "Launch a new run from a preset, or derive one from an existing run as a duplicate, a rerun, a variant or a counterfactual."
      , children:
          [ Layout.grid
              [ Layout.panel
                  { span: 5
                  , title: "Launch a run"
                  , purpose: "A new run from a preset. Blank seeds and rounds keep the server's draws and the scenario's default."
                  , children: [ launchFields form setForm failure launch ]
                  }
              , case runs of
                  Just (Right (t :: Table)) ->
                    let
                      source = picked <|> latestReadable t
                    in
                      Layout.panel
                        { span: 7
                        , title: "Derive from a run"
                        , purpose: "Choose the source run. The new run copies its spec, with any field you change, and opens when it is created."
                        , children:
                            [ sourceSelect t source setPicked
                            , case source of
                                Just id -> actions { id, derive: true, archived: false, rounds: Nothing, target: Nothing, onChanged: pure unit }
                                Nothing -> D.p { className: "text-sm text-muted-foreground", children: [ D.text "No run to derive from yet." ] }
                            ]
                        }
                  Just (Left e) -> Layout.panel { span: 7, title: "Derive from a run", purpose: "Choose the source run.", children: [ status (Just e) ] }
                  Nothing -> Layout.panel { span: 7, title: "Derive from a run", purpose: "Choose the source run.", children: [ status Nothing ] }
              ]
          ]
      }
  where
  sourceSelect t current setPicked = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Source run"
        , D.select
            { value: fromMaybe "" current
            , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
            , onChange: handler targetValue (\v -> setPicked v)
            , children: runInfos t <#> \r -> D.option { key: r.id, value: r.id, children: [ D.text r.id ] }
            }
        ]
    }

-- | Home: the run list, and the selected run's summary (the latest readable run until one is
-- | selected). Selecting a run changes the summary in place: no page change.
mkHome :: DataTable -> Component { compareSet :: Array String, toggle :: String -> Effect Unit }
mkHome grid = do
  results <- mkRunResults grid
  component "Home" \p -> React.do
    selected /\ setSelected <- useState' (Nothing :: Maybe String)
    archived /\ setArchived <- useState' false
    runs <- useFetch archived (listRuns archived)
    pure $ Layout.page
      { title: "Simulation runs"
      , purpose: "Every stored run, with the latest run's results beside the list. Select a run to show its results here, or open it in View."
      , children:
          [ case runs of
              Just (Right (t :: Table)) ->
                let
                  current = fromMaybe "" (selected <|> latestReadable t)
                in
                  Layout.grid
                    [ Layout.panel
                        { span: 4
                        , title: "Runs"
                        , purpose: "Every stored run in the order they were created. Click a run to show its results."
                        , children:
                            [ D.label
                                { className: "flex items-center gap-2 text-sm"
                                , children: [ element UI.checkbox { checked: archived, onCheckedChange: mkEffectFn1 setArchived }, D.text "Show archived runs" ]
                                }
                            , runTable grid t { selected: current, select: \id -> setSelected (Just id) }
                            , D.a { href: href Prepare, className: "text-sm underline", children: [ D.text "Prepare a run" ] }
                            ]
                        }
                    , Layout.panel
                        { span: 8
                        , title: "Latest run"
                        , purpose: "The selected run's key results: its figures, price, balances, objectives, and rejections and shortfalls."
                        , children:
                            [ if current == "" then D.p { className: "text-sm text-muted-foreground", children: [ D.text "No run yet. Prepare a run to see its results here." ] }
                              else D.div
                                { key: current
                                , children:
                                    [ results
                                        { id: current
                                        , details: false
                                        , inComparison: elem current p.compareSet
                                        , comparisonCount: Array.length p.compareSet
                                        , comparisonHref: href (Compare p.compareSet)
                                        , toggleComparison: p.toggle current
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
              Just (Left e) -> status (Just e)
              Nothing -> status Nothing
          ]
      }

-- | View: one run's lineage and actions, and its results with the deep views in Details.
mkView :: DataTable -> Component { id :: String, compareSet :: Array String, toggle :: String -> Effect Unit }
mkView grid = do
  results <- mkRunResults grid
  actions <- Actions.mkRunActions
  component "View" \p -> React.do
    version /\ setVersion <- useState' 0
    overview <- useApi (Tuple version p.id) (runPath p.id <> "/overview")
    runs <- useFetch (Tuple version p.id) (listRuns true)
    pure $ Layout.page
      { title: "Run " <> p.id
      , purpose: "This run's results and its actions. The key results come first; each deeper topic is under its Details."
      , children:
          [ case overview, runs of
              Just (Left e), _ -> status (Just e)
              _, Just (Left e) -> status (Just e)
              Just (Right (t :: Table)), Just (Right (all :: Table)) ->
                let
                  info = runInfo all p.id
                in
                  Layout.grid
                    [ Layout.panel
                        { span: 5
                        , title: "Lineage"
                        , purpose: "The run this one was derived from, and the runs derived from it."
                        , children: [ lineage all p.id info ]
                        }
                    , Layout.panel
                        { span: 7
                        , title: "Manage this run"
                        , purpose: "Archive, resume or delete this run. Derive, duplicate and variant runs from Prepare."
                        , children:
                            [ actions
                                { id: p.id
                                , derive: false
                                , archived: maybe false _.archived info
                                , rounds: roundCount t
                                , target: roundTarget t
                                , onChanged: setVersion (version + 1)
                                }
                            ]
                        }
                    , Layout.panel
                        { span: 12
                        , title: "Results"
                        , purpose: "The run's results: the same summary as the home page, with each deeper topic under Details."
                        , children:
                            [ results
                                { id: p.id
                                , details: true
                                , inComparison: elem p.id p.compareSet
                                , comparisonCount: Array.length p.compareSet
                                , comparisonHref: href (Compare p.compareSet)
                                , toggleComparison: p.toggle p.id
                                }
                            ]
                        }
                    ]
              _, _ -> status Nothing
          ]
      }

-- | Lineage: the source run this one was derived from, and the runs derived from it.
lineage :: Table -> String -> Maybe RunInfo -> JSX
lineage all id info = D.div
  { className: "flex flex-col gap-2 text-sm"
  , children:
      [ case info of
          Just i | i.source /= "" -> D.p_ [ D.text "Derived from ", runLink i.source ]
          _ -> D.p_ [ D.text "Not derived from another run (an original run)." ]
      , case derivedFrom all id of
          [] -> D.p_ [ D.text "No runs are derived from this run." ]
          ids -> D.div_ [ D.p_ [ D.text "Derived runs" ], D.ul { className: "list-disc pl-5", children: ids <#> \r -> D.li { key: r, children: [ runLink r ] } } ]
      ]
  }

-- | Compare: the runs chosen for comparison, a market selector for the trajectory, and the parts as
-- | tabs. Removing a run changes the route's id list.
mkCompare :: DataTable -> Component (Array String)
mkCompare grid = do
  fetch <- fetched
  component "Compare" \ids -> React.do
    markets <- useApi ids (runPath (fromMaybe "" (head ids)) <> "/markets")
    picked /\ setPicked <- useState' (Nothing :: Maybe String)
    pure $ Layout.page
      { title: "Compare runs"
      , purpose: "Runs side by side: their price trajectories, final holdings, configuration differences, and compatibility."
      , children:
          [ if null ids then Layout.section
              { title: "No runs selected"
              , purpose: "Add a run to the comparison from its results, then open the comparison."
              , children: [ D.a { href: href Runs, className: "underline", children: [ D.text "Go to the run list" ] } ]
              }
            else Layout.section
              { title: "Runs compared"
              , purpose: "The runs in this comparison; the first is the baseline. Remove a run to take it out."
              , children:
                  [ D.div
                      { className: "flex flex-wrap gap-2"
                      , children: ids <#> \id -> D.div
                          { key: id
                          , className: "flex items-center gap-2 rounded-md border px-3 py-1 text-sm"
                          , children:
                              [ runLink id
                              , element UI.button
                                  { size: "sm"
                                  , variant: "ghost"
                                  , onClick: handler_ (setHash (print route (Compare (filter (_ /= id) ids))))
                                  , children: [ D.text "Remove" ]
                                  }
                              ]
                          }
                      }
                  , case markets of
                      Just (Right (cs :: Array Chart)) -> marketPicker cs picked setPicked ids grid fetch
                      Just (Left e) -> status (Just e)
                      Nothing -> status Nothing
                  ]
              }
          ]
      }

-- | The market selector and the tabs, for the chosen market (the trajectory's default is the Atlantic energy market).
marketPicker :: Array Chart -> Maybe String -> (Maybe String -> Effect Unit) -> Array String -> DataTable -> Fetched -> JSX
marketPicker cs picked setPicked ids grid fetch = D.div
  { className: "flex min-w-0 flex-col gap-stack"
  , children:
      [ D.label
          { className: "text-sm"
          , children:
              [ D.text "Market "
              , D.select
                  { value: market
                  , onChange: handler targetValue (\v -> setPicked v)
                  , children: names <#> \m -> D.option { key: m, value: m, children: [ D.text m ] }
                  }
              ]
          }
      , tabbed fetch (compareTabs grid ids market)
      ]
  }
  where
  names = maybe [] (map _.name <<< _.series) (head cs)
  market = case picked of
    Just m | elem m names -> m
    _ -> if elem "energy-atlantic" names then "energy-atlantic" else fromMaybe "" (head names)

-- | The top bar: the app name, then the pages. View links to the run last viewed, and Compare shows how
-- | many runs it holds. The comparison set and the viewed run live in the App, so every page sees them.
shell :: Route -> Array String -> Maybe String -> JSX -> JSX -> JSX
shell current compareSet viewed themeButton page = D.div
  { className: "min-h-screen"
  , children:
      [ D.header
          { className: "border-b"
          , children:
              [ D.div
                  { className: "mx-auto flex w-full max-w-page flex-wrap items-center gap-x-6 gap-y-2 px-gutter py-3"
                  , children:
                      [ D.a { href: href Runs, className: "font-semibold", children: [ D.text "Scenario Plan" ] }
                      , D.nav
                          { className: "flex flex-wrap items-center gap-4 text-sm"
                          , children:
                              [ navLink "Runs" Runs (current == Runs)
                              , navLink "Prepare" Prepare (current == Prepare)
                              , case viewed of
                                  Just id -> navLink "View" (Run id) (isView current)
                                  Nothing -> D.span { className: "text-muted-foreground", children: [ D.text "View" ] }
                              , navLink (if compareSet == [] then "Compare" else "Compare (" <> show (Array.length compareSet) <> ")") (Compare compareSet) (isCompare current)
                              ]
                          }
                      , D.div { className: "ml-auto", children: [ themeButton ] }
                      ]
                  }
              ]
          }
      , D.main { className: "min-w-0", children: [ page ] }
      ]
  }
  where
  isView = case _ of
    Run _ -> true
    _ -> false
  isCompare = case _ of
    Compare _ -> true
    _ -> false
  navLink label r active = D.a
    { href: href r
    , className: if active then "font-medium underline underline-offset-4" else "text-muted-foreground hover:text-foreground"
    , children: [ D.text label ]
    }

-- | The app: the routes, the comparison set (added from any run, and taken from a compare route), and
-- | the last viewed run (for the top bar's View link).
mkApp :: Component Unit
mkApp = do
  grid <- mkDataTable
  home <- mkHome grid
  prepare <- mkPrepare grid
  view <- mkView grid
  compare <- mkCompare grid
  toggle <- themeToggle
  component "App" \_ -> React.do
    current /\ setCurrent <- useState' Runs
    compareSet /\ setCompareSet <- useState' ([] :: Array String)
    viewed /\ setViewed <- useState' (Nothing :: Maybe String)
    useEffectOnce $ matchesWith (parse route) \_ r -> do
      setCurrent r
      case r of
        Run id -> setViewed (Just id)
        Compare ids -> setCompareSet ids
        _ -> pure unit
    let
      toggleComparison id = setCompareSet (if elem id compareSet then filter (_ /= id) compareSet else compareSet <> [ id ])
    pure $ shell current compareSet viewed (toggle unit) case current of
      Runs -> home { compareSet, toggle: toggleComparison }
      Prepare -> prepare unit
      Run id -> D.div { key: id, children: [ view { id, compareSet, toggle: toggleComparison } ] }
      Compare ids -> D.div { key: String.joinWith "," ids, children: [ compare ids ] }

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      app <- mkApp
      r <- createRoot e
      renderRoot r (app unit)
