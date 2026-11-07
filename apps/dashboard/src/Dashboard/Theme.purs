-- | Light/dark toggle. The preference is stored in localStorage; dark is the default, and
-- | index.html applies it before the app renders so the page never flashes light.
module Dashboard.Theme (themeToggle) where

import Prelude

import Dashboard.UI as UI
import Effect (Effect)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, element, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as D

foreign import readStoredTheme :: Effect String
foreign import storeTheme :: String -> Effect Unit

themeToggle :: Component Unit
themeToggle = component "ThemeToggle" \_ -> React.do
  dark /\ setDark <- useState' true
  useEffectOnce do
    stored <- readStoredTheme
    setDark (stored /= "light")
    pure (pure unit)
  let
    flip = do
      storeTheme (if dark then "light" else "dark")
      setDark (not dark)
  -- a bare glyph: no button component, border or background, only the character
  pure $ D.button
    { className: "text-lg leading-none text-foreground hover:opacity-70 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring rounded-sm"
    , title: if dark then "Switch to light mode" else "Switch to dark mode"
    , onClick: handler_ flip
    , children: [ D.text "◑" ]
    }
