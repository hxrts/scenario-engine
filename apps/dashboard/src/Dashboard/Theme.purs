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
  pure $ element UI.button
    { className: "text-sm"
    , variant: "outline"
    , size: "sm"
    -- an emoji keeps its own colours, so the icon looks the same in light and dark mode
    , "aria-label": if dark then "Switch to light mode" else "Switch to dark mode"
    , title: if dark then "Switch to light mode" else "Switch to dark mode"
    , onClick: handler_ flip
    , children: [ D.text "🌗" ]
    }
