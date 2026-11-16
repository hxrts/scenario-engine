-- | The balance sheet pages (docs/07_simulator.md §9): the page each Actor is given, which shows only its own data and
-- | links nowhere else, and the balance sheets panel the Simulations and Live pages show for their selected run, which
-- | lists every Actor's link.
module Dashboard.BalanceSheet (mkBalanceSheet, mkLinks) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Components (loaded, metricChart, tableOf)
import Dashboard.DataTable (DataTable, defaults)
import Data.Array (index)
import Data.Maybe (Maybe(..))
import Dashboard.Layout as Layout
import Dashboard.Theme (themeToggle)

import React.Basic.DOM as D
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Views.Common (Table)
import Views.BalanceSheet (BalanceSheet, linksTable)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- | The page an Actor is given: what it has done, has available, and can do.
mkBalanceSheet :: DataTable -> Component { run :: String, token :: String }
mkBalanceSheet grid = do
  toggle <- themeToggle
  component "BalanceSheet" \p -> React.do
    value <- useLoad p (Client.balanceSheet p.run p.token)
    -- the bar links only back to this page, which links nowhere else
    pure $ D.div_
      [ Layout.topBar { brand: "⋲", home: "/balancesheet/" <> p.run <> "/" <> p.token, nav: [], themeButton: toggle unit }
      , D.main_ [ loaded value.value page ]
      ]
  where
  page :: BalanceSheet -> _
  page x = Layout.page
    { title: x.actor <> " Balance Sheet"
    , purpose: x.kind
    , children:
        [ Layout.grid
            [ Layout.panel { span: 12, title: "Holdings", purpose: "Every Resource it holds, round by round.", children: [ metricChart x.chart ] }
            , Layout.panel { span: 12, title: "Net worth", purpose: "What its holdings are worth at the round's prices.", children: [ metricChart x.netWorth ] }
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

-- | Each Actor's page, shown as its whole address in the shared table, so it can be copied and sent to the player.
mkLinks :: DataTable -> Component String
mkLinks grid = (origin =<< location =<< window) >>= \here -> component "BalanceSheetLinks" \id -> React.do
  links <- useLoad id (Client.balanceSheetLinks id)
  pure $ loaded links.value \ls -> grid ((defaults (linksTable here ls)) { link = \i row -> if i == 2 then index row 2 else Nothing })
