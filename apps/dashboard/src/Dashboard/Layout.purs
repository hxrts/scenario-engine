-- | The one layout system every page uses (see app.css, which defines the tokens): a 12-column grid,
-- | named spacing (`gap-gutter`, `gap-stack`, `gap-section`), and a max content width (`max-w-page`).
-- |   * page: the page's heading and purpose line, and its content in a single column of sections
-- |   * section: a titled group of content, stacked with `gap-stack`
-- |   * grid: a 12-column grid of panels
-- |   * panel: a card placed on `span` of the 12 columns (full width below `lg`)
module Dashboard.Layout (Tab, topBar, page, section, grid, panel, tabbed, caption, captioned, purpose) where

import Prelude

import Dashboard.UI as UI
import React.Basic.DOM as D
import Data.Array (index)
import Data.Maybe (maybe)
import React.Basic.Hooks (JSX, element)

-- | The bar across the top of every page: the brand, its links, and the theme button at the far end.
topBar :: { brand :: String, home :: String, nav :: Array JSX, themeButton :: JSX } -> JSX
topBar b = D.header
  { className: "border-b"
  , children:
      [ D.div
          { className: "mx-auto flex w-full max-w-page flex-wrap items-center gap-x-6 gap-y-2 px-gutter py-3"
          , children:
              [ D.a { href: b.home, className: "font-semibold", children: [ D.text b.brand ] }
              , D.nav { className: "flex flex-wrap items-center gap-4 text-sm", children: b.nav }
              , D.div { className: "ml-auto", children: [ b.themeButton ] }
              ]
          }
      ]
  }

-- | A page: its title, a one-sentence purpose, and its sections.
page :: { title :: String, purpose :: String, children :: Array JSX } -> JSX
page p = D.div
  { className: "mx-auto flex w-full max-w-page min-w-0 flex-col gap-section px-gutter py-section"
  , children:
      [ D.header
          { className: "flex flex-col gap-1"
          , children:
              [ D.h1 { className: "text-2xl font-semibold tracking-tight", children: [ D.text p.title ] }
              , purpose p.purpose
              ]
          }
      ] <> p.children
  }

-- | A titled group of content inside a page.
section :: { title :: String, purpose :: String, children :: Array JSX } -> JSX
section s = D.section
  { className: "flex min-w-0 flex-col gap-stack"
  , children:
      [ D.div
          { className: "flex flex-col gap-1"
          , children:
              [ D.h2 { className: "text-lg font-semibold", children: [ D.text s.title ] }
              , purpose s.purpose
              ]
          }
      , D.div { className: "flex min-w-0 flex-col gap-stack", children: s.children }
      ]
  }

-- | A tab: its key, its label, its purpose line, and its content.
type Tab = { key :: String, label :: String, purpose :: String, content :: JSX }

-- | Tabs, the first open: each shows its purpose line, then its content.
tabbed :: Array Tab -> JSX
tabbed ts = element UI.tabs
  { defaultValue: maybe "" _.key (index ts 0)
  , children:
      [ element UI.tabsList { children: ts <#> \t -> element UI.tabsTrigger { key: t.key, value: t.key, children: [ D.text t.label ] } } ]
        <> (ts <#> \t -> element UI.tabsContent { key: t.key, value: t.key, children: [ D.div { className: "mb-3", children: [ purpose t.purpose ] }, t.content ] })
  }

-- | The 12-column grid. Panels place themselves with `panel`.
grid :: Array JSX -> JSX
grid children = D.div { className: "grid grid-cols-12 gap-gutter", children }

-- | A card on `span` of the 12 columns. Below the `lg` breakpoint every panel is full width.
panel :: { span :: Int, title :: String, purpose :: String, children :: Array JSX } -> JSX
panel p = D.section
  { className: spanClass p.span <> " min-w-0"
  , children:
      [ element UI.card
          { className: "h-full gap-stack py-stack"
          , children:
              [ element UI.cardHeader
                  { children:
                      [ element UI.cardTitle { children: [ D.text p.title ] }
                      , purpose p.purpose
                      ]
                  }
              , element UI.cardContent { className: "flex min-w-0 flex-col gap-stack", children: p.children }
              ]
          }
      ]
  }

-- | Each span is written out in full so Tailwind finds the class names in the compiled output.
spanClass :: Int -> String
spanClass n = case n of
  4 -> "col-span-12 lg:col-span-4"
  5 -> "col-span-12 lg:col-span-5"
  6 -> "col-span-12 lg:col-span-6"
  7 -> "col-span-12 lg:col-span-7"
  8 -> "col-span-12 lg:col-span-8"
  _ -> "col-span-12"

-- | A one-line purpose, under a heading or a title.
purpose :: String -> JSX
purpose t = D.p { className: "text-sm text-muted-foreground", children: [ D.text t ] }

-- | A caption: a small, medium-weight line that names a plot or a table.
caption :: String -> JSX
caption t = D.p { className: "text-sm font-medium", children: [ D.text t ] }

-- | A caption above its content.
captioned :: String -> Array JSX -> JSX
captioned t children = D.div { className: "flex min-w-0 flex-col gap-2", children: [ caption t ] <> children }
