-- | The balanceSheet pages (docs/06_simulator.md §9): the page each Actor is given, which shows only its own
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
  pure $ D.main { className: "mx-auto w-full max-w-page px-gutter py-6", children: [ loaded value.value page ] }
  where
  page :: BalanceSheet -> _
  page x = D.div
    { className: "flex min-w-0 flex-col gap-stack"
    , children:
        [ D.h1 { className: "text-2xl font-semibold", children: [ D.text ("Balance sheet of " <> x.actor) ] }
        , D.p { className: "text-muted-foreground", children: [ D.text x.kind ] }
        , Layout.section { title: "What it has available", purpose: "Every Resource it holds, and what it owes or is owed.", children: [ metricChart x.chart, metricChart x.netWorth, table x.holdings, table x.owed ] }
        , Layout.section { title: "What it can do", purpose: "What it may do now, and where.", children: [ table x.capabilities, table x.exchanges, table x.transformations, metricChart x.prices, table x.objective ] }
        , Layout.section { title: "What it has done", purpose: "Every Transaction it proposed or was asked to sign, by round.", children: [ table x.done ] }
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
    pure $ Layout.section
      { title: "Balance sheets"
      , purpose: "Each Actor's own page, to hand to whoever plays it."
      , children:
          [ loaded list.value \l ->
              let
                ids = map _.id l.runs
                current = fromMaybe "" (chosen <|> last ids)
              in
                if ids == [] then status (Just "There are no runs yet.")
                else D.div
                  { className: "flex flex-col gap-stack"
                  , children:
                      [ D.label { className: "flex flex-col gap-1 text-sm", children: [ D.text "Run", selectInput { value: current, options: ids, onChange: setChosen } ] }
                      , linksOf current
                      ]
                  }
          ]
      }

mkLinks :: Component String
mkLinks = component "BalanceSheetLinks" \id -> React.do
  links <- useLoad id (Client.balanceSheetLinks id)
  pure $ loaded links.value \ls -> D.table
    { className: "w-full text-sm"
    , children:
        [ D.tbody_ (map (\x -> D.tr_ [ D.td_ [ D.text x.actor ], D.td_ [ D.text x.kind ], D.td_ [ D.a { href: x.path, className: "underline", children: [ D.text x.path ] } ] ]) ls) ]
    }
