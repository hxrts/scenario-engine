-- | The dashboard (docs/06_simulator.md §9–§10). Pages are configuration lists over the
-- | generic components; every view model comes from the read API.
module Dashboard.Main (main) where

import Prelude

import Dashboard.Api (encode, get, post, runPath, useApi, useFetch)
import Dashboard.Components (Render, as, card, diffTable, metricChart, panel, plainTable, recordTable, runPicker, status)
import Dashboard.DataTable (DataTable, mkDataTable)
import Dashboard.Launch (LaunchForm, LaunchRequest, emptyForm, validate)
import Dashboard.Presets (presetNames)
import Dashboard.Route (Route(..), href, route)
import Dashboard.RunActions as Actions
import Dashboard.Runs (RunInfo, derivedFrom, mergeRuns, runInfo, runInfos, sourceRow)
import Dashboard.Theme (themeToggle)
import Dashboard.UI as UI
import Data.Array (elem, find, findIndex, head, index, null)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Harness.Views (Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Tab = { key :: String, label :: String, path :: String, render :: Render }

type Fetch = { path :: String, render :: Render } -> JSX

-- | The run's column in the run list (`Table.columns` of `/api/runs`) holds its id.
runKeyColumn :: Int
runKeyColumn = 0

-- | The comparison view model (`/api/compare`); each part is one of its fields.
type CompareView = { trajectory :: Chart, final :: Table, diff :: Table, config :: Table, compatibility :: Table }

table :: DataTable -> Render
table grid = as (plainTable grid)

charts :: Render
charts = as (\(cs :: Array Chart) -> D.div_ (map metricChart cs))

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

-- | The run list. Without `archived` it is the runs that are not archived; with it, the archived
-- | runs are listed too (the list's `?archived=true`), in one table.
listRuns :: Boolean -> Aff (Either String Table)
listRuns archived = do
  shown <- get "/api/runs"
  if not archived then pure shown
  else do
    more <- get "/api/runs?archived=true"
    pure (mergeRuns <$> shown <*> more)

-- | Run Details (§9): one tab per view model. `?round` applies to round-scoped views.
runTabs :: DataTable -> String -> Int -> Array Tab
runTabs grid id n =
  [ tab "overview" "Overview" (table grid)
  , tab ("balances?round=" <> show n) "Balance sheets" (table grid)
  , tab "markets" "Markets" charts
  , tab "facilities" "Facilities" (table grid)
  , tab "economy" "World economy" (as \(e :: { totals :: Chart, flows :: Table, pressure :: Table }) -> D.div_ [ metricChart e.totals, plainTable grid e.flows, plainTable grid e.pressure ])
  , tab "representation" "Representation" (table grid)
  , tab "procedures" "Procedures" (table grid)
  , tab "objectives" "Objectives" (table grid)
  , tab ("events?round=" <> show n) "Events" (table grid)
  ]
  where
  tab view label render = { key: label, label, path: runPath id <> "/" <> view, render }

-- | Comparison (§10): one fetch per part, plus the agency baseline. The trajectory is for one market.
compareTabs :: DataTable -> Array String -> String -> Array Tab
compareTabs grid ids market =
  [ part "Trajectory" (comparePath <> "&market=" <> encode market) (\c -> metricChart c.trajectory)
  , part "Final state" comparePath (\c -> diffTable grid { baseline: 2 } c.final)
  , part "Baseline diff" comparePath (\c -> plainTable grid c.diff)
  , part "Configuration" comparePath (\c -> diffTable grid { baseline: 1 } c.config)
  , part "Compatibility" comparePath (\c -> plainTable grid c.compatibility)
  , { key: "agency", label: "Agency (fixed baseline)", path: "/api/agency", render: table grid }
  ]
  where
  comparePath = "/api/compare?runs=" <> joinIds
  joinIds = String.joinWith "," (map encode ids)
  part label path f = { key: label, label, path, render: as (f :: CompareView -> JSX) }

tabbed :: Fetch -> Array Tab -> JSX
tabbed show' ts = element UI.tabs
  { defaultValue: maybe "" _.key (index ts 0)
  , children:
      [ element UI.tabsList { children: ts <#> \t -> element UI.tabsTrigger { key: t.key, value: t.key, children: [ D.text t.label ] } } ]
        <> (ts <#> \t -> element UI.tabsContent { key: t.key, value: t.key, children: [ show' { path: t.path, render: t.render } ] })
  }

-- | A link to a run's page.
runLink :: String -> JSX
runLink id = D.a { href: href (Run id), className: "underline", children: [ D.text id ] }

-- | The run list's links: the id links to its run, and a Source cell links to the source run when
-- | the list has it.
runLinks :: Table -> Int -> Array String -> Maybe String
runLinks t i row
  | i == runKeyColumn = href <<< Run <$> index row runKeyColumn
  | Just i == findIndex (_ == "Source") t.columns = do
      s <- sourceRow t row
      if elem s (map _.id (runInfos t)) then Just (href (Run s)) else Nothing
  | otherwise = Nothing

-- | Simulation Runs (§9.1): the run list, run selection for comparison, and launching.
mkRuns :: DataTable -> Component Unit
mkRuns grid = component "Runs" \_ -> React.do
  version /\ setVersion <- useState' 0
  selected /\ setSelected <- useState' []
  open /\ setOpen <- useState' false
  form /\ setForm <- useState emptyForm
  failure /\ setFailure <- useState' Nothing
  archived /\ setArchived <- useState' false
  runs <- useFetch (Tuple archived version) (listRuns archived)
  let
    -- A bad form is reported without a request; the server's reply, on success or error, is shown or refreshed.
    launch = case validate form of
      Left e -> setFailure (Just e)
      Right req -> launchAff_ (submit req)

    submit :: LaunchRequest -> Aff Unit
    submit req = do
      reply <- post "/api/runs" req :: Aff (Either String { id :: String })
      liftEffect case reply of
        Left e -> setFailure (Just e)
        Right _ -> do
          setFailure Nothing
          setOpen false
          setVersion (version + 1)
  pure $ D.div
    { className: "flex flex-col gap-4"
    , children:
        [ D.label
            { className: "flex items-center gap-2 text-sm"
            , children: [ element UI.checkbox { checked: archived, onCheckedChange: mkEffectFn1 setArchived }, D.text "Show archived runs" ]
            }
        , case runs of
            Just (Right (t :: Table)) -> D.div
              { className: "flex flex-col gap-4"
              , children:
                  [ recordTable grid (runLinks t) t
                  , card "Compare"
                      [ runPicker { runs: t, keyColumn: runKeyColumn, selected, onChange: setSelected }
                      , D.a { href: href (Compare selected), className: if null selected then "pointer-events-none opacity-50" else "underline", children: [ D.text "Compare selected" ] }
                      ]
                  , launchSheet { open, setOpen, form, setForm, failure, launch }
                  ]
              }
            Just (Left e) -> status (Just e)
            Nothing -> status Nothing
        ]
    }

type LaunchSheet =
  { open :: Boolean
  , setOpen :: Boolean -> Effect Unit
  , form :: LaunchForm
  , setForm :: (LaunchForm -> LaunchForm) -> Effect Unit
  , failure :: Maybe String
  , launch :: Effect Unit
  }

-- | The launch form, in a Sheet opened from the run list. A rejected launch keeps the form open
-- | and shows the server's message (or the form's own validation message).
launchSheet :: LaunchSheet -> JSX
launchSheet s = element UI.sheet
  { open: s.open
  , onOpenChange: mkEffectFn1 s.setOpen
  , children:
      [ element UI.sheetTrigger { asChild: true, children: [ element UI.button { children: [ D.text "Launch a run" ] } ] }
      , element UI.sheetContent
          { children:
              [ element UI.sheetHeader { children: [ element UI.sheetTitle { children: [ D.text "Launch a run" ] } ] }
              , D.div
                  { className: "flex flex-col gap-4 p-4"
                  , children:
                      [ field "Name" s.form.name (\v f -> f { name = v })
                      , presetField
                      , field "Player seed" s.form.playerSeed (\v f -> f { playerSeed = v })
                      , field "Rounds (blank for the scenario default)" s.form.rounds (\v f -> f { rounds = v })
                      , maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] }) s.failure
                      , element UI.button { onClick: handler_ s.launch, children: [ D.text "Run" ] }
                      ]
                  }
              ]
          }
      ]
  }
  where
  field label value set = D.label { className: "flex flex-col gap-1 text-sm", children: [ D.text label, element UI.input { value, onChange: handler targetValue (\v -> s.setForm (set (fromMaybe "" v))) } ] }
  presetField = D.label
    { className: "flex flex-col gap-1 text-sm"
    , children:
        [ D.text "Preset"
        , D.select
            { value: s.form.preset
            , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
            , onChange: handler targetValue (\v -> s.setForm (\f -> f { preset = fromMaybe f.preset v }))
            , children: presetNames <#> \p -> D.option { key: p, value: p, children: [ D.text p ] }
            }
        ]
    }

-- | Lineage (§9.2): the source run this one was derived from, and the runs derived from it. Both come
-- | from the run list's Source column.
lineage :: Table -> String -> Maybe RunInfo -> JSX
lineage all id info = card "Lineage"
  [ D.div
      { className: "flex flex-col gap-2 text-sm"
      , children:
          [ case info of
              Just i | i.source /= "" -> D.p_ [ D.text "Derived from ", runLink i.source ]
              _ -> D.p_ [ D.text "Not derived from another run." ]
          , case derivedFrom all id of
              [] -> D.p_ [ D.text "No runs are derived from this run." ]
              ids -> D.div_ [ D.p_ [ D.text "Derived runs" ], D.ul { className: "list-disc pl-5", children: ids <#> \r -> D.li { key: r, children: [ runLink r ] } } ]
          ]
      }
  ]

-- | One run's page: its lineage and actions, its round (a selector clamped to the run's rounds), and its view tabs.
-- | Archive, resume and the derivations refresh the page through the version in the fetch keys.
mkRunPage :: DataTable -> Fetch -> (Actions.Props -> JSX) -> Component String
mkRunPage grid show' actions = component "Run" \id -> React.do
  version /\ setVersion <- useState' 0
  requested /\ setRequested <- useState' (Nothing :: Maybe Int)
  overview <- useApi (Tuple version id) (runPath id <> "/overview")
  runs <- useFetch (Tuple version id) (listRuns true)
  pure case overview, runs of
    Just (Left e), _ -> status (Just e)
    _, Just (Left e) -> status (Just e)
    Just (Right (t :: Table)), Just (Right (all :: Table)) ->
      let
        n = fromMaybe 0 (roundCount t)
        r = clampRound n (fromMaybe n requested)
        info = runInfo all id
      in
        D.div_
          [ D.div
              { className: "grid gap-4 md:grid-cols-2"
              , children:
                  [ lineage all id info
                  , actions
                      { id
                      , archived: maybe false _.archived info
                      , rounds: roundCount t
                      , target: roundTarget t
                      , onChanged: setVersion (version + 1)
                      }
                  ]
              }
          , D.label
              { className: "text-sm"
              , children:
                  [ D.text "Round "
                  , element UI.input { className: "w-20", "aria-label": "Round", value: show r, onChange: handler targetValue (\v -> setRequested (Int.fromString =<< v)) }
                  , D.text (" of " <> show n)
                  ]
              }
          , tabbed show' (runTabs grid id r)
          ]
    _, _ -> status Nothing

-- | A comparison of runs, with a market selector for the trajectory. The markets come from the first run.
mkComparePage :: DataTable -> Fetch -> Component (Array String)
mkComparePage grid show' = component "Compare" \ids -> React.do
  markets <- useApi ids (runPath (fromMaybe "" (head ids)) <> "/markets")
  picked /\ setPicked <- useState' (Nothing :: Maybe String)
  pure case markets of
    Just (Right (cs :: Array Chart)) ->
      let
        names = maybe [] (map _.name <<< _.series) (head cs)
        market = case picked of
          Just m | elem m names -> m
          _ -> if elem "energy-atlantic" names then "energy-atlantic" else fromMaybe "" (head names)
      in
        D.div_
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
          , tabbed show' (compareTabs grid ids market)
          ]
    Just (Left e) -> status (Just e)
    Nothing -> status Nothing

mkApp :: Component Unit
mkApp = do
  grid <- mkDataTable
  runsPage <- mkRuns grid
  fetchPanel <- panel
  actionsPanel <- Actions.mkRunActions
  runPage <- mkRunPage grid fetchPanel actionsPanel
  comparePage <- mkComparePage grid fetchPanel
  toggle <- themeToggle
  component "App" \_ -> React.do
    current /\ setCurrent <- useState' Runs
    useEffectOnce (matchesWith (parse route) \_ r -> setCurrent r)
    -- Keyed wrappers remount the page when the id changes, so per-run state (the round) resets.
    pure $ shell current (toggle unit) case current of
      Runs -> runsPage unit
      Run id -> D.div { key: id, children: [ runPage id ] }
      Compare ids -> D.div { key: String.joinWith "," ids, children: [ comparePage ids ] }

-- | The dashboard block's layout: a sidebar, and an inset with a header and the page.
shell :: Route -> JSX -> JSX -> JSX
shell current themeButton page = element UI.sidebarProvider
  { children:
      [ element UI.sidebar
          { children:
              [ element UI.sidebarHeader { children: [ D.span { className: "px-2 font-semibold", children: [ D.text "Scenario Plan" ] } ] }
              , element UI.sidebarContent { children: [ element UI.sidebarGroup { children: [ element UI.sidebarMenu { children: [ nav "Simulation runs" Runs ] } ] } ] }
              ]
          }
      , element UI.sidebarInset
          { children:
              [ D.header
                  { className: "flex h-12 items-center gap-2 border-b px-4"
                  , children: [ element UI.sidebarTrigger {}, element UI.separator { orientation: "vertical", className: "h-4" }, D.h1 { className: "font-medium", children: [ D.text title ] }, D.div { className: "ml-auto", children: [ themeButton ] } ]
                  }
              , D.main { className: "flex flex-col gap-4 p-6", children: [ page ] }
              ]
          }
      ]
  }
  where
  title = case current of
    Runs -> "Simulation runs"
    Run id -> "Run " <> id
    Compare ids -> "Comparing " <> String.joinWith ", " ids
  nav label r = element UI.sidebarMenuItem { key: label, children: [ element UI.sidebarMenuButton { asChild: true, isActive: r == current, children: [ D.a { href: href r, children: [ D.text label ] } ] } ] }

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      app <- mkApp
      r <- createRoot e
      renderRoot r (app unit)
