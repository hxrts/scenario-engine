-- | Tailwind-first element helpers, in the style of yoga-react-tailwind: the class string
-- | comes first and children last, so a view reads as its declarative structure.
-- | Class names are plain strings and are not checked at compile time.
module Dashboard.Tailwind
  ( div, span, p, h1, h2, h3, pre, label, main, header, section
  , ul, li, strong, small
  , a, button
  ) where

import React.Basic.DOM as D
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX)

div :: String -> Array JSX -> JSX
div cn children = D.div { className: cn, children }

span :: String -> Array JSX -> JSX
span cn children = D.span { className: cn, children }

p :: String -> Array JSX -> JSX
p cn children = D.p { className: cn, children }

h1 :: String -> Array JSX -> JSX
h1 cn children = D.h1 { className: cn, children }

h2 :: String -> Array JSX -> JSX
h2 cn children = D.h2 { className: cn, children }

h3 :: String -> Array JSX -> JSX
h3 cn children = D.h3 { className: cn, children }

pre :: String -> Array JSX -> JSX
pre cn children = D.pre { className: cn, children }

label :: String -> Array JSX -> JSX
label cn children = D.label { className: cn, children }

main :: String -> Array JSX -> JSX
main cn children = D.main { className: cn, children }

header :: String -> Array JSX -> JSX
header cn children = D.header { className: cn, children }

section :: String -> Array JSX -> JSX
section cn children = D.section { className: cn, children }

ul :: String -> Array JSX -> JSX
ul cn children = D.ul { className: cn, children }

li :: String -> Array JSX -> JSX
li cn children = D.li { className: cn, children }

strong :: String -> Array JSX -> JSX
strong cn children = D.strong { className: cn, children }

small :: String -> Array JSX -> JSX
small cn children = D.small { className: cn, children }

-- | Links and buttons take their attributes as a record, since both need more than a class.
a :: String -> String -> Array JSX -> JSX
a cn href children = D.a { className: cn, href, children }

button :: String -> EventHandler -> Array JSX -> JSX
button cn onClick children = D.button { className: cn, onClick, children }
