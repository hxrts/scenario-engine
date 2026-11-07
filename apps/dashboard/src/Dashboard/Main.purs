-- | The dashboard (docs/06_simulator.md §9–§10). Three pages in the top bar, one hash route each:
-- | Runs (`#/runs`, or `#/runs/:id` with that run selected: the run list beside the selected run's
-- | results, lineage and actions), Prepare (launch and derive runs), and Compare (runs side by side).
-- | Every view model comes from the read API.
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
import Dashboard.Runs (RunInfo, derivedFrom, mergeRuns, readableIds, runInfo, runInfos, sourceRow)
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
type CompareView = { trajectory :: Chart, final :: Table, diff :: Table, config :: Table, compatibility :: Table, seeds :: Table }

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
  , part "Seed variation" "Does the outcome depend on the seed? The spread and Gini of the runs that share this run's specification and differ only in seed, with the lowest, highest and range of each. With one run, no range is reported." comparePath (\c -> tableOf grid c.seeds)
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

-- | The launch form's fields and its Run button, with the line that says what is required. A required
-- | field's label carries an asterisk and the word `required`; an optional one says `optional` and
-- | what it defaults to.
launchFields :: LaunchForm -> ((LaunchForm -> LaunchForm) -> Effect Unit) -> Maybe String -> Effect Unit -> JSX
launchFields form setForm failure launch = D.div
  { className: "flex flex-col gap-stack"
  , children:
      [ field { label: "Name", required: true, note: "required" } form.name (\v f -> f { name = v })
      , presetField
      , field { label: "Player seed", required: false, note: "optional, a seed is drawn by the server when blank" } form.playerSeed (\v f -> f { playerSeed = v })
      , field { label: "Rounds", required: false, note: "optional, defaults to the scenario's round count when blank" } form.rounds (\v f -> f { rounds = v })
      , maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] }) failure
      , element UI.button { onClick: handler_ launch, children: [ D.text "Run" ] }
      , D.p { className: "text-sm text-muted-foreground", children: [ D.text "Required: Name and Preset. Player seed and Rounds are optional and take their defaults when blank." ] }
      ]
  }
  where
  field f value set = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ fieldHeading f
        , element UI.input { value, "aria-required": f.required, onChange: handler targetValue (\v -> setForm (set (fromMaybe "" v))) }
        ]
    }
  fieldHeading f = D.span
    { className: "flex flex-col"
    , children:
        [ D.span { className: "font-medium", children: [ D.text f.label, D.span { className: "text-destructive", children: [ D.text (if f.required then " *" else "") ] } ] }
        , D.span { className: "text-xs text-muted-foreground", children: [ D.text f.note ] }
        ]
    }
  presetField = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ fieldHeading { label: "Preset", required: true, note: "required, " <> emptyForm.preset <> " is preselected" }
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

-- | The manage panel's actions for one run: archive, resume and delete. The run's rounds and target
-- | come from its overview, so Resume is offered only while the run is shorter than its target.
mkManage :: Component { id :: String, archived :: Boolean, onChanged :: Effect Unit }
mkManage = do
  actions <- Actions.mkRunActions
  component "Manage" \p -> React.do
    overview <- useApi p.id (runPath p.id <> "/overview")
    pure $ case overview of
      Just (Right (t :: Table)) -> actions { id: p.id, derive: false, archived: p.archived, rounds: roundCount t, target: roundTarget t, onChanged: p.onChanged }
      Just (Left e) -> status (Just e)
      Nothing -> status Nothing

-- | Runs: the run list beside the selected run, which is the route's run (`#/runs/:id`) or, with no
-- | selection, the latest readable run. The selected run's results come first; its lineage and its
-- | manage actions are stacked under them. Selecting a row changes the route, so a selected run has
-- | its own address. Archive, resume and delete bump `version`, which refetches the list and remounts
-- | the selected run's panels.
mkRuns :: DataTable -> Component { selected :: Maybe String }
mkRuns grid = do
  results <- mkRunResults grid
  manage <- mkManage
  component "Runs" \p -> React.do
    archived /\ setArchived <- useState' false
    version /\ setVersion <- useState' 0
    shown <- useFetch (Tuple archived version) (listRuns archived)
    everyRun <- useFetch version (listRuns true)
    pure $ Layout.page
      { title: "Runs"
      , purpose: "Every stored run, with the selected run's results, lineage and actions beside the list. With no run selected, the latest readable run is shown."
      , children:
          [ case shown, everyRun of
              Just (Left e), _ -> status (Just e)
              _, Just (Left e) -> status (Just e)
              Just (Right (t :: Table)), Just (Right (all :: Table)) ->
                let
                  current = fromMaybe "" (p.selected <|> latestReadable t)
                  info = runInfo all current
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
                            , runTable grid t { selected: current, select: \id -> setHash (print route (Run id)) }
                            , D.a { href: href Prepare, className: "text-sm underline", children: [ D.text "Prepare a run" ] }
                            ]
                        }
                    , if current == "" then Layout.panel
                        { span: 8
                        , title: "Selected run"
                        , purpose: "The selected run's results appear here."
                        , children: [ D.p { className: "text-sm text-muted-foreground", children: [ D.text "No run yet. Prepare a run to see its results here." ] } ]
                        }
                      else D.div
                        { key: current <> "#" <> show version
                        , className: "col-span-12 flex min-w-0 flex-col gap-gutter lg:col-span-8"
                        , children:
                            [ results { id: current, status: maybe "" _.status info }
                            , Layout.panel
                                { span: 12
                                , title: "Lineage"
                                , purpose: "The run this one was derived from, and the runs derived from it."
                                , children: [ lineage all current info ]
                                }
                            , Layout.panel
                                { span: 12
                                , title: "Manage this run"
                                , purpose: "Archive, resume or delete this run. Derive, duplicate and variant runs from Prepare."
                                , children: [ manage { id: current, archived: maybe false _.archived info, onChanged: setVersion (version + 1) } ]
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
    everyRun <- useFetch unit (listRuns true)
    picked /\ setPicked <- useState' (Nothing :: Maybe String)
    pure $ Layout.page
      { title: "Compare runs"
      , purpose: "Runs side by side: their price trajectories, final holdings, configuration differences, and compatibility."
      , children:
          [ case everyRun of
              Just (Right (t :: Table)) -> choices t ids
              Just (Left e) -> status (Just e)
              Nothing -> status Nothing
          , if null ids then Layout.section
              { title: "No runs selected"
              , purpose: "Tick runs in the list above to compare them."
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

-- | The Compare page's choices: every readable run as a checkbox, so no id is typed. Ticking a run adds
-- | it to the comparison (the route's ids, in the order ticked; the first is the baseline), and
-- | unticking removes it. Each tick changes the route.
choices :: Table -> Array String -> JSX
choices t ids = Layout.section
  { title: "Choose runs"
  , purpose: "Every readable run. Tick the runs to compare; the first run ticked is the baseline."
  , children:
      [ D.div
          { className: "flex flex-col gap-2"
          , children: readableIds t <#> \id -> D.label
              { key: id
              , className: "flex items-center gap-2 text-sm"
              , children:
                  [ element UI.checkbox
                      { checked: elem id ids
                      , onCheckedChange: mkEffectFn1 (\on -> setHash (print route (Compare (toggled id on))))
                      }
                  , D.text id
                  ]
              }
          }
      ]
  }
  where
  toggled id on
    | on = if elem id ids then ids else ids <> [ id ]
    | otherwise = filter (_ /= id) ids

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

-- | The top bar: the app name, then the three pages (Runs, Prepare, and Compare with how many runs it
-- | holds). Every item is a link on every page. The comparison set lives in the App, so every page sees it.
shell :: Route -> Array String -> JSX -> JSX -> JSX
shell current compareSet themeButton page = D.div
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
                              [ navLink "Runs" Runs (isRuns current)
                              , navLink "Prepare" Prepare (current == Prepare)
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

-- | The app: the routes and the comparison set (the ids of the Compare route, shown in the top bar).
-- | `#/runs` and `#/runs/:id` render the same Runs component, so its list keeps its state as a run is
-- | selected.
mkApp :: Component Unit
mkApp = do
  grid <- mkDataTable
  runsPage <- mkRuns grid
  prepare <- mkPrepare grid
  compare <- mkCompare grid
  toggle <- themeToggle
  component "App" \_ -> React.do
    current /\ setCurrent <- useState' Runs
    compareSet /\ setCompareSet <- useState' ([] :: Array String)
    useEffectOnce $ matchesWith (parse route) \_ r -> do
      setCurrent r
      case r of
        Compare ids -> setCompareSet ids
        _ -> pure unit
    pure $ shell current compareSet (toggle unit) case current of
      Runs -> runsPage { selected: Nothing }
      Run id -> runsPage { selected: Just id }
      Prepare -> prepare unit
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
