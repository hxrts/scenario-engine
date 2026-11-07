-- | The dashboard (docs/06_simulator.md §9–§10). Three pages in the top bar, one hash route each:
-- | Runs (`#/runs`, or `#/runs/:id` with that run selected: the run list beside the selected run's
-- | results, lineage and actions), Prepare (launch a run of the scenario), and Compare (runs side by
-- | side). Every view model comes from the read API.
module Dashboard.Main (main) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (encode, get, post, runPath, useApi, useFetch)
import Dashboard.Components (as, diffTable, fetched, groupedBars, overlayChart, placeholderFrame, status, tableOf)
import Dashboard.DataTable (DataTable, mkDataTable)
import Dashboard.Launch (LaunchForm, LaunchRequest, Scenario, emptyForm, nextRunName, roundChoices, roundValue, scenarioOf, summary, validate)
import Dashboard.Layout as Layout
import Dashboard.Presets (presetNames)
import Dashboard.Results (mkRunResults, roundCount, roundTarget)
import Dashboard.Route (Route(..), href, route)
import Dashboard.RunActions as Actions
import Dashboard.Runs (mergeRuns, readableIds, runInfo, runInfos, sourceRow)
import Dashboard.Theme (themeToggle)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array (elem, filter, find, findIndex, index, null, reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Effect.Uncurried (mkEffectFn1)
import Harness.Views.Common (Bars, Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useEffect, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.Hash (matchesWith, setHash)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | A tab: its label, the one-sentence purpose shown above its content, and the view it fetches.
type Tab = { key :: String, label :: String, purpose :: String, content :: JSX }

type Render = Json -> JSX

type Fetched = { path :: String, render :: Render } -> JSX

-- | The run's column in the run list (`Table.columns` of `/api/runs`) holds its id.
runKeyColumn :: Int
runKeyColumn = 0

-- | The comparison view model (`/api/compare`); each part is one of its fields.
type CompareView =
  { markets :: Array Chart
  , netWorth :: Array { player :: String, chart :: Chart }
  , standings :: Bars
  , balance :: { spread :: Bars, gini :: Bars }
  , keyValues :: Table
  , final :: Table
  , diff :: Table
  , config :: Table
  , compatibility :: Table
  , seeds :: Table
  }

-- | The run list. Without `archived` it is the runs that are not archived; with it, the archived
-- | runs are listed too (the list's `?archived=true`), in one table.
listRuns :: Boolean -> Aff (Either String Table)
listRuns archived = do
  shown <- get "/api/runs"
  if not archived then pure shown
  else do
    more <- get "/api/runs?archived=true"
    pure (mergeRuns <$> shown <*> more)

-- | The run list without its archived runs (the Archived column says "yes" for them).
withoutArchived :: Table -> Table
withoutArchived t = case findIndex (_ == "Archived") t.columns of
  Just ai -> t { rows = filter (\r -> index r ai /= Just "yes") t.rows }
  Nothing -> t

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

-- | The tabs: each shows its purpose line, then its content.
tabbed :: Array Tab -> JSX
tabbed ts = element UI.tabs
  { defaultValue: maybe "" _.key (index ts 0)
  , children:
      [ element UI.tabsList { children: ts <#> \t -> element UI.tabsTrigger { key: t.key, value: t.key, children: [ D.text t.label ] } } ]
        <> (ts <#> \t -> element UI.tabsContent { key: t.key, value: t.key, children: [ D.p { className: "mb-3 text-sm text-muted-foreground", children: [ D.text t.purpose ] }, t.content ] })
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

-- | The launch form's view: the form, the scenario it launches, the default name, the failure
-- | message, and the Run action.
type LaunchView =
  { form :: LaunchForm
  , setForm :: (LaunchForm -> LaunchForm) -> Effect Unit
  , scenario :: Scenario
  , defaultName :: String
  , failure :: Maybe String
  , launch :: Effect Unit
  }

-- | The launch form's fields, each with its default, then the summary line under them and the Run
-- | button. A required field's label carries an asterisk and the word `required`; an optional one says
-- | `optional` and its default. The round choices and the game seed come from the scenario.
-- | The launch action: what Run will play, any error, and the Run button. It sits in its own column.
launchAction :: LaunchView -> JSX
launchAction v = D.div
  { className: "flex flex-col gap-stack"
  , children:
      [ D.p { className: "text-sm font-medium", children: [ D.text (summary v.scenario.rounds v.form) ] }
      , maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] }) v.failure
      , element UI.button { onClick: handler_ v.launch, children: [ D.text "Run" ] }
      , D.p { className: "text-sm text-muted-foreground", children: [ D.text "Required: Preset and Scenario. Every other field has a default, so Run needs no edits." ] }
      ]
  }

launchFields :: LaunchView -> JSX
launchFields v = D.div
  { className: "flex flex-col gap-stack"
  , children:
      [ textField { label: "Name", required: false, note: "optional, defaults to " <> v.defaultName <> ", the next run id" } v.form.name (\x f -> f { name = x })
      , selectField { label: "Preset", required: true, note: "required, " <> emptyForm.preset <> " by default: every seat plays this preset" } v.form.preset presetNames (\x f -> f { preset = x })
      , selectField { label: "Scenario", required: true, note: "required, the one scenario the server runs" } scenarioName [ scenarioName ] (\_ f -> f)
      , selectField
          { label: "Rounds", required: false, note: "optional, defaults to " <> show v.scenario.rounds.default <> " rounds; allowed " <> show v.scenario.rounds.min <> " to " <> show v.scenario.rounds.max }
          (show (roundValue v.scenario.rounds v.form.rounds))
          (map show (roundChoices v.scenario.rounds))
          (\x f -> f { rounds = x })
      , textField { label: "Player seed", required: false, note: "optional, blank draws a seed once at launch and stores it with the run" } v.form.playerSeed (\x f -> f { playerSeed = x })
      , D.div
          { className: "flex flex-col gap-1 text-sm"
          , children:
              [ fieldHeading { label: "Game seed", required: false, note: "default: the scenario's seed. Read-only, because the server takes no game seed at launch" }
              , D.span { className: "font-mono", children: [ D.text (show v.scenario.gameSeed) ] }
              ]
          }
      ]
  }
  where
  scenarioName = v.scenario.name <> "@" <> v.scenario.version
  textField f value set = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ fieldHeading f
        , element UI.input { value, "aria-required": f.required, onChange: handler targetValue (\x -> v.setForm (set (fromMaybe "" x))) }
        ]
    }
  -- a select; `set` receives the chosen value
  selectField f value options set = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ fieldHeading f
        , D.select
            { value
            , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
            , onChange: handler targetValue (\x -> v.setForm (set (fromMaybe value x)))
            , children: options <#> \o -> D.option { key: o, value: o, children: [ D.text o ] }
            }
        ]
    }
  fieldHeading f = D.span
    { className: "flex flex-col"
    , children:
        [ D.span { className: "font-medium", children: [ D.text f.label, D.span { className: "text-destructive", children: [ D.text (if f.required then " *" else "") ] } ] }
        , D.span { className: "text-xs text-muted-foreground", children: [ D.text f.note ] }
        ]
    }

-- | Prepare: launch a run of the scenario. The form starts with its defaults (the next run name, the
-- | preset Conservative, the scenario's round count, a drawn player seed), so Run needs no edits. A
-- | launch the server accepts opens the new run.
mkPrepare :: Component Unit
mkPrepare = component "Prepare" \_ -> React.do
  form /\ setForm <- useState emptyForm
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  runs <- useFetch unit (listRuns true)
  scenario <- useFetch unit (map (_ >>= scenarioOf) (get "/api/scenario" :: Aff (Either String Json)))
  let
    -- the default name is known once the run list is; until then it is "run"
    loadedName = case runs of
      Just (Right t) -> nextRunName (map _.id (runInfos t))
      _ -> ""
    defaultName = if loadedName == "" then "run" else loadedName

    -- a bad form is reported without a request; a launch that the server accepts opens the new run
    launch = case scenario of
      Just (Right s) -> case validate defaultName s.rounds form of
        Left e -> setFailure (Just e)
        Right req -> setFailure Nothing *> launchAff_ (submit req)
      _ -> setFailure (Just "The scenario has not loaded, so the run cannot be launched.")

    submit :: LaunchRequest -> Aff Unit
    submit req = do
      reply <- post "/api/runs" req :: Aff (Either String { id :: String })
      liftEffect case reply of
        Left e -> setFailure (Just e)
        Right r -> setHash (print route (Run r.id))
  -- the name field shows the default once it is known; a name the user has typed is kept
  useEffect loadedName do
    if loadedName == "" then pure unit else setForm (\f -> if f.name == "" then f { name = loadedName } else f)
    pure (pure unit)
  pure $ Layout.page
    { title: "Prepare a run"
    , purpose: "Launch a run of the scenario from a preset. Every field has a default, so Run needs no edits."
    , children:
        [ Layout.grid
            [ Layout.panel
                { span: 6
                , title: "Options"
                , purpose: "Each option's default is shown beside it. Only Preset and Scenario are required."
                , children:
                    [ case scenario of
                        Just (Right s) -> launchFields { form, setForm, scenario: s, defaultName, failure, launch }
                        Just (Left e) -> status (Just e)
                        Nothing -> status Nothing
                    ]
                }
            , Layout.panel
                { span: 6
                , title: "Launch"
                , purpose: "The summary says what Run will play. Run opens the new run when the server accepts it."
                , children:
                    [ case scenario of
                        Just (Right s) -> launchAction { form, setForm, scenario: s, defaultName, failure, launch }
                        Just (Left e) -> status (Just e)
                        Nothing -> status Nothing
                    ]
                }
            ]
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
      Just (Right (t :: Table)) -> actions { id: p.id, archived: p.archived, rounds: roundCount t, target: roundTarget t, onChanged: p.onChanged }
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
    -- every run, fetched on each change. The previous list stays on screen until the new one
    -- arrives, so an archive or delete does not blank the page; the archived box only filters rows
    everyRun /\ setEveryRun <- useState' (Nothing :: Maybe (Either String Table))
    useEffect version do
      fiber <- launchAff do
        result <- listRuns true
        liftEffect (setEveryRun (Just result))
      pure (launchAff_ (killFiber (error "superseded") fiber))
    pure $ Layout.page
      { title: "Runs"
      , purpose: "Every stored run, with the selected run's results and actions beside the list. With no run selected, the latest readable run is shown."
      , children:
          [ case everyRun of
              Just (Left e) -> status (Just e)
              Just (Right (all :: Table)) ->
                let
                  t = if archived then all else withoutArchived all
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
                            ]
                        }
                    , if current == "" then Layout.panel
                        { span: 8
                        , title: "Selected run"
                        , purpose: "The selected run's results appear here."
                        , children: [ D.p { className: "text-sm text-muted-foreground", children: [ D.text "No run yet. Prepare a run to see its results here." ] } ]
                        }
                      else D.div
                        { key: show version
                        , className: "col-span-12 flex min-w-0 flex-col gap-gutter lg:col-span-8"
                        , children:
                            [ results { id: current, status: maybe "" _.status info }
                            , Layout.panel
                                { span: 12
                                , title: "Manage this run"
                                , purpose: "Resume, archive or delete this run."
                                , children: [ manage { id: current, archived: maybe false _.archived info, onChanged: setVersion (version + 1) } ]
                                }
                            ]
                        }
                    ]
              Nothing -> status Nothing
          ]
      }

-- | Compare: the runs ticked in the choice list (the route's ids; the first is the baseline), and the
-- | comparison's plots and tables. The comparison is fetched whenever the ids change, and the last one
-- | loaded stays on screen until the new one arrives, so ticking a run does not move the page. Every
-- | plot sits in a frame of fixed height.
mkCompare :: DataTable -> Component (Array String)
mkCompare grid = do
  fetch <- fetched
  component "Compare" \ids -> React.do
    everyRun <- useFetch unit (listRuns true)
    shown /\ setShown <- useState' (Nothing :: Maybe (Either String CompareView))
    picked /\ setPicked <- useState' (Nothing :: Maybe String)
    useEffect (String.joinWith "," ids) do
      if null ids then setShown Nothing *> pure (pure unit)
      else do
        fiber <- launchAff do
          result <- (get (comparePath ids) :: Aff (Either String CompareView))
          liftEffect (setShown (Just result))
        pure (launchAff_ (killFiber (error "superseded") fiber))
    pure $ Layout.page
      { title: "Compare runs"
      , purpose: "Runs side by side: prices, net worth and standings on shared axes, then the key values in one table."
      , children:
          [ case everyRun of
              Just (Right (t :: Table)) -> choices t ids
              Just (Left e) -> status (Just e)
              Nothing -> status Nothing
          , plots grid fetch ids shown picked (setPicked)
          ]
      }

-- | The comparison's request: the ids, each encoded as one path segment.
comparePath :: Array String -> String
comparePath ids = "/api/compare?runs=" <> String.joinWith "," (map encode ids)

-- | A plot in its grid cell: a title on one line, then the plot. Half width from `lg`, so a plot and
-- | its placeholder take the same box.
item :: String -> JSX -> JSX
item title child = D.div
  { className: "col-span-12 flex min-w-0 flex-col gap-2 lg:col-span-6"
  , children: [ D.p { className: "text-sm font-medium", children: [ D.text title ] }, child ]
  }

-- | The plots and tables of the comparison, each section with its purpose line. The plots answer, in
-- | order: how each Market's price moves, how each player's net worth moves, who ends ahead, how even
-- | the outcome is, and what the key values are. Until a comparison is loaded, each plot's frame is
-- | shown in the same place and size, so nothing moves when the data arrives. The tables in the tabs
-- | hold the rest.
plots :: DataTable -> Fetched -> Array String -> Maybe (Either String CompareView) -> Maybe String -> (Maybe String -> Effect Unit) -> JSX
plots grid fetch ids shown picked setPicked = D.div
  { className: "flex min-w-0 flex-col gap-section"
  , children:
      [ Layout.section
          { title: "Prices by market"
          , purpose: "How each Market's marginal price moves after every round: one line per run, on the same axes."
          , children: loadedWith (Layout.grid [ item "Prices by market" waiting ]) \c ->
              [ Layout.grid (c.markets <#> \m -> item m.title (overlayChart ids m)) ]
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
          , children: loadedWith waiting \c -> [ groupedBars ids "Player" c.standings ]
          }
      , Layout.section
          { title: "Spread and Gini"
          , purpose: "How even final net worth is among the players in each run. The spread is in M; the Gini coefficient runs from 0 (equal) to 1 (one player holds everything)."
          , children: loadedWith (balance waiting waiting) \c -> [ balance (groupedBars ids "Measure" c.balance.spread) (groupedBars ids "Measure" c.balance.gini) ]
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
              [ tabbed
                  [ { key: "final", label: "Final state", purpose: "Every account's holding after the final round, one column per run; differing values are highlighted.", content: D.div { children: loadedWith waiting \c -> [ diffTable grid { baseline: 2 } c.final ] } }
                  , { key: "diff", label: "Baseline diff", purpose: "The holdings that differ from the first run, for each run, as a change.", content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.diff ] } }
                  , { key: "config", label: "Configuration", purpose: "The specification settings that differ between the runs; differing values are highlighted.", content: D.div { children: loadedWith waiting \c -> [ diffTable grid { baseline: 1 } c.config ] } }
                  , { key: "compatibility", label: "Compatibility", purpose: "Whether each run can be combined with the first: the same Gameboard, engine, rounds, units, Markets and Actor selection.", content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.compatibility ] } }
                  , { key: "seeds", label: "Seed variation", purpose: "Does the outcome depend on the seed? The spread and Gini of the runs that share this run's specification and differ only in seed, with the lowest, highest and range of each. With one run, no range is reported.", content: D.div { children: loadedWith waiting \c -> [ tableOf grid c.seeds ] } }
                  , { key: "agency", label: "Agency (fixed baseline)", purpose: "The agency analysis against a fixed baseline, for each Actor's alternatives.", content: fetch { path: "/api/agency", render: as \(t :: Table) -> tableOf grid t } }
                  ]
              ]
          }
      ]
  }
  where
  message = if null ids then "Tick runs in the list above to compare them." else "Loading the comparison…"
  waiting = placeholderFrame message

  -- the loaded comparison's content; until one is loaded, the waiting content of the same size shows
  loadedWith :: JSX -> (CompareView -> Array JSX) -> Array JSX
  loadedWith waitingContent f = case shown of
    Just (Right c) -> f c
    Just (Left e) -> [ status (Just e) ]
    Nothing -> [ waitingContent ]

  -- the two balance plots side by side, each in its own panel
  balance spreadPlot giniPlot = Layout.grid
    [ Layout.panel { span: 6, title: "Spread", purpose: "Highest minus lowest final net worth, in M.", children: [ spreadPlot ] }
    , Layout.panel { span: 6, title: "Gini coefficient", purpose: "Inequality of final net worth, from 0 to 1.", children: [ giniPlot ] }
    ]

  playerPicker c = D.label
    { className: "flex items-center gap-2 text-sm"
    , children:
        [ D.text "Player"
        , D.select
            { value: chosen c
            , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
            , onChange: handler targetValue (\v -> setPicked v)
            , children: c.netWorth <#> \p -> D.option { key: p.player, value: p.player, children: [ D.text p.player ] }
            }
        ]
    }

  netWorthPlot c = case find (\nw -> nw.player == chosen c) c.netWorth of
    Nothing -> [ waiting ]
    Just nw -> [ overlayChart ids nw.chart ]

  -- the player shown: the one picked, when the comparison has them, else the first
  chosen c = case picked of
    Just p | elem p (map _.player c.netWorth) -> p
    _ -> maybe "" _.player (index c.netWorth 0)

-- | The choice list: every readable run as a checkbox, so no id is typed. Ticking a run adds it to the
-- | comparison (the route's ids, in the order ticked; the first is the baseline), and unticking removes
-- | it: a run is removed only by unticking it. Each tick changes the route. A ticked run shows the
-- | colour its plots and tables use.
choices :: Table -> Array String -> JSX
choices t ids = Layout.section
  { title: "Choose runs"
  , purpose: "Every readable run. Tick a run to compare it; untick it to remove it. The first run ticked is the baseline."
  , children:
      [ D.div
          { className: "grid grid-cols-1 gap-2 sm:grid-cols-2 lg:grid-cols-3"
          , children: readableIds t <#> \id -> D.label
              { key: id
              , className: "flex items-center gap-2 text-sm"
              , children:
                  [ element UI.checkbox
                      { checked: elem id ids
                      , onCheckedChange: mkEffectFn1 (\on -> setHash (print route (Compare (toggled id on))))
                      }
                  , D.span { className: "size-3 shrink-0 rounded-full " <> swatch id }
                  , D.span { className: "font-medium", children: [ D.text id ] }
                  , D.span { className: "text-muted-foreground", children: [ D.text (if index ids 0 == Just id then "baseline" else "") ] }
                  ]
              }
          }
      ]
  }
  where
  toggled id on
    | on = if elem id ids then ids else ids <> [ id ]
    | otherwise = filter (_ /= id) ids
  -- the run's colour: the same position in the ids as the plots use (Dashboard.Chart.colourOf)
  swatch id = swatchClass (fromMaybe 0 (findIndex (_ == id) ids) `mod` 5)

-- | A colour swatch's class: one theme chart colour, named in full so Tailwind finds it.
swatchClass :: Int -> String
swatchClass n = case n of
  0 -> "bg-[var(--chart-1)]"
  1 -> "bg-[var(--chart-2)]"
  2 -> "bg-[var(--chart-3)]"
  3 -> "bg-[var(--chart-4)]"
  _ -> "bg-[var(--chart-5)]"

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
                      [ D.a { href: href Runs, className: "font-semibold", children: [ D.text "Scenario Engine" ] }
                      , D.nav
                          { className: "flex flex-wrap items-center gap-4 text-sm"
                          , children:
                              [ navLink "Prepare" Prepare (current == Prepare)
                              , navLink "Runs" Runs (isRuns current)
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
  prepare <- mkPrepare
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
      Compare ids -> compare ids

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      app <- mkApp
      r <- createRoot e
      renderRoot r (app unit)
