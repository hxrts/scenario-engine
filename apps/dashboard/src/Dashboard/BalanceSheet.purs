-- | The balanceSheet pages (docs/07_simulator.md §9): the page each Actor is given, which shows only its own
-- | data and links nowhere else, and the admin page that lists every Actor's link for a Run.
module Dashboard.BalanceSheet (mkBalanceSheet, mkBalanceSheets) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Components (loaded, metricChart, selectInput, status, tableOf)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Control.Alt ((<|>))
import Data.Array (last)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React
import Views.Common (Table)
import Views.BalanceSheet (BalanceSheet)

-- | The page an Actor is given: what it has done, has available, and can do.
mkBalanceSheet :: DataTable -> Component { run :: String, token :: String }
mkBalanceSheet grid = component "BalanceSheet" \p -> React.do
  value <- useLoad p (Client.balanceSheet p.run p.token)
  pure $ D.main_ [ loaded value.value page ]
  where
  page :: BalanceSheet -> _
  page x = Layout.page
    { title: x.actor <> " Balance Sheet"
    , purpose: x.kind
    , children:
        [ Layout.grid
            [ Layout.panel { span: 6, title: "Holdings", purpose: "Every Resource it holds, round by round.", children: [ metricChart x.chart ] }
            , Layout.panel { span: 6, title: "Net worth", purpose: "What its holdings are worth at the round's prices.", children: [ metricChart x.netWorth ] }
            , Layout.panel { span: 12, title: "What it has available", purpose: "Every Resource it holds, and what it owes or is owed.", children: [ Layout.captioned "Holdings by round" [ table x.holdings ], Layout.captioned "Obligations" [ table x.owed ] ] }
            , Layout.panel { span: 4, title: "What it can do", purpose: "Every action it is allowed.", children: [ table x.capabilities ] }
            , Layout.panel { span: 8, title: "Where it can do it", purpose: "The Exchanges it trades on and the Transformations it runs.", children: [ Layout.captioned "Exchanges" [ table x.exchanges ], Layout.captioned "Transformations" [ table x.transformations ] ] }
            , Layout.panel { span: 12, title: "Prices", purpose: "The price on each Exchange it trades on, round by round.", children: [ metricChart x.prices ] }
            , Layout.panel { span: 12, title: "Its objective", purpose: "Each criterion it is scored on, and whether it is met.", children: [ table x.objective ] }
            , Layout.panel { span: 12, title: "What it has done", purpose: "Every Transaction it proposed or was asked to sign, by round.", children: [ table x.done ] }
            ]
        ]
    }

  table :: Table -> _
  table = tableOf grid

-- | The admin page: a Run, and a link to each of its Actors' balanceSheets.
mkBalanceSheets :: Component Unit
mkBalanceSheets = do
  linksOf <- mkLinks
  component "BalanceSheets" \_ -> React.do
    list <- useLoad unit Client.runs
    chosen /\ setChosen <- useState' (Nothing :: Maybe String)
    pure $ Layout.page
      { title: "Balance sheets"
      , purpose: "Each Actor's own page, to hand to whoever plays it. A page shows only that Actor's data."
      , children:
          [ loaded list.value \l ->
              let
                ids = map _.id l.runs
                current = fromMaybe "" (chosen <|> last ids)
              in
                if ids == [] then status (Just "There are no runs yet.")
                else Layout.grid
                  [ Layout.panel
                      { span: 4
                      , title: "Run"
                      , purpose: "The run whose Actors' pages are listed."
                      , children: [ D.label { className: "flex flex-col gap-1 text-sm", children: [ D.text "Run", selectInput { value: current, options: ids, onChange: setChosen } ] } ]
                      }
                  , Layout.panel { span: 8, title: "Actors", purpose: "Each Actor of the run, and the link to its page.", children: [ linksOf current ] }
                  ]
          ]
      }

mkLinks :: Component String
mkLinks = component "BalanceSheetLinks" \id -> React.do
  links <- useLoad id (Client.balanceSheetLinks id)
  pure $ loaded links.value \ls -> D.div
    { className: "overflow-x-auto rounded-md border"
    , children:
        [ D.table
            { className: "w-full text-sm"
            , children:
                [ D.thead
                    { className: "border-b bg-muted/50 text-left text-muted-foreground"
                    , children: [ D.tr_ (map (\h -> D.th { className: "px-3 py-2 font-medium", children: [ D.text h ] }) [ "Actor", "Kind", "Page" ]) ]
                    }
                , D.tbody_ (map (\x -> D.tr { className: "border-b last:border-0", children: [ cell [ D.text x.actor ], cell [ D.text x.kind ], cell [ D.a { href: x.path, className: "font-medium underline-offset-4 hover:underline", children: [ D.text "Open" ] } ] ] }) ls)
                ]
            }
        ]
    }
  where
  cell children = D.td { className: "px-3 py-2", children }
